library(terra)
library(sf)
library(exactextractr)
library(data.table)
library(osmextract)
library(dplyr)
library(progressr)

handlers(global = TRUE)
handlers("progress")

# -----------------------------------------------------------------------
# Paths - UPDATE THESE
# -----------------------------------------------------------------------
yerevan_shp <- sf::st_read("C:/Users/mrealehatem/Documents/GitHub/ArmeniaPollutionAnalysis/Yerevan Boundary/boundary.gpkg",
                           layer = "boundary-polygon-lvl4")
pop_path       <- "C:/Users/mrealehatem/Documents/GitHub/ArmeniaPollutionAnalysis/WorldPopData/arm_age_data/pop.tif"
landcover_path <- "C:/Users/mrealehatem/Downloads/worldcover.tif"
chm_paths      <- paste0("C:/Users/mrealehatem/Downloads/veg", 17:25, ".tif")
roads_pbf      <- "C:/Users/mrealehatem/Downloads/armenia.pbf"

BUFFER_RADII <- c(100, 200, 500)

# -----------------------------------------------------------------------
# 0. Roads — read once, reproject once
# -----------------------------------------------------------------------
cat("Reading roads...\n")
armenia_roads_oe <- oe_read(
  roads_pbf,
  layer      = "lines",
  extra_tags = c("lanes", "maxspeed")
)
armenia_roads_oe <- armenia_roads_oe[!is.na(armenia_roads_oe$highway), ]

yerevan <- yerevan_shp |> st_transform(32638)
armenia_roads_oe <- st_transform(armenia_roads_oe, 32638)

yerevan_roads <- st_intersection(armenia_roads_oe, yerevan)
yerevan_roads$lanes <- as.numeric(yerevan_roads$lanes)

routes <- yerevan_roads |>
  filter(lanes >= 4 | highway %in% c("primary", "trunk"))

cat("✓ Roads ready:", nrow(yerevan_roads), "segments\n")

# -----------------------------------------------------------------------
# 1. Create 100 m grid + centroids
# -----------------------------------------------------------------------
grid_raw  <- st_make_grid(yerevan, cellsize = 100, what = "polygons")
grid_100m <- st_sf(grid_id = seq_along(grid_raw), geometry = grid_raw)
grid_100m <- st_intersection(grid_100m, yerevan)
cents     <- st_centroid(grid_100m)
n_cells   <- nrow(grid_100m)
cat("Grid:", n_cells, "cells\n")

# Pre-compute all buffer sets once — reused across all sections
bufs_list <- setNames(
  lapply(BUFFER_RADII, function(r) {
    b        <- st_buffer(cents, r)
    b$buf_id <- seq_len(nrow(b))
    b
  }),
  paste0("r", BUFFER_RADII)
)
cat("✓ Buffers pre-computed\n")

# -----------------------------------------------------------------------
# 2. Population — reproject buffers to raster CRS, never touch the raster
# -----------------------------------------------------------------------
cat("\n[2/5] Population...\n")
pop_rast <- terra::rast(pop_path)

with_progress({
  p <- progressor(steps = length(BUFFER_RADII))
  for (r in BUFFER_RADII) {
    bufs_native <- st_transform(bufs_list[[paste0("r", r)]], terra::crs(pop_rast))
    vals        <- exact_extract(pop_rast, bufs_native, "mean", progress = FALSE)
    grid_100m[[paste0("pop_mean_", r, "m")]] <- vals
    p(message = sprintf("pop %dm done", r))
  }
})
cat("✓ Population extracted\n")

# -----------------------------------------------------------------------
# 3. Vegetation height and volume — terra + reproject buffers, not raster
# -----------------------------------------------------------------------
cat("\n[3/5] Vegetation...\n")
existing_chm <- chm_paths[file.exists(chm_paths)]
cat(" Found", length(existing_chm), "CHM files\n")

with_progress({
  p <- progressor(steps = length(existing_chm) * length(BUFFER_RADII))
  
  for (chm_path in existing_chm) {
    year_suffix <- regmatches(
      chm_path,
      regexpr("(?<=veg)\\d{2}", chm_path, perl = TRUE)
    )
    
    chm_rast <- terra::rast(chm_path)  # lazy load — no reprojection of raster
    
    for (r in BUFFER_RADII) {
      # Reproject buffers to match raster CRS — fast, small object
      bufs_native <- st_transform(bufs_list[[paste0("r", r)]], terra::crs(chm_rast))
      
      veg_vals <- exact_extract(chm_rast, bufs_native, "mean", progress = FALSE)
      
      grid_100m[[paste0("veg", year_suffix, "_height_", r, "m")]] <- veg_vals
      grid_100m[[paste0("veg", year_suffix, "_volume_", r, "m")]] <- veg_vals * (pi * r^2)
      p(message = sprintf("veg%s %dm done", year_suffix, r))
    }
  }
})
cat("✓ Vegetation extracted\n")

# -----------------------------------------------------------------------
# 4. Landcover — fast built-in "frac" (single C++ pass per radius)
# -----------------------------------------------------------------------
cat("\n[4/5] Landcover...\n")
if (file.exists(landcover_path)) {
  lc_rast <- terra::rast(landcover_path)
  # Reproject raster once here only — justified because "frac" needs consistent
  # pixel alignment, and we do it once not per-loop
  lc_rast <- terra::project(lc_rast, "epsg:32638", method = "near")
  
  for (r in BUFFER_RADII) {
    cat(sprintf("  %dm buffer (progress below):\n", r))
    
    # Native C++ progress bar via progress = TRUE
    lc_frac <- exact_extract(
      lc_rast,
      bufs_list[[paste0("r", r)]],
      "frac",
      progress = TRUE
    )
    
    # Rename frac_<val> → lc<val>_pct_<r>m, scale 0–1 → 0–100
    names(lc_frac) <- gsub("^frac_(.+)$", paste0("lc\\1_pct_", r, "m"), names(lc_frac))
    lc_frac        <- lc_frac * 100
    
    grid_100m <- cbind(grid_100m, lc_frac)
    cat(sprintf("  ✓ %dm done (%d classes)\n", r, ncol(lc_frac)))
  }
  cat("✓ Landcover extracted\n")
}

# -----------------------------------------------------------------------
# 5. Road length within buffers + distance to nearest primary road
# -----------------------------------------------------------------------
cat("\n[5/5] Roads...\n")

# 5a. Road length per buffer radius
with_progress({
  p <- progressor(steps = length(BUFFER_RADII))
  
  for (r in BUFFER_RADII) {
    bufs <- bufs_list[[paste0("r", r)]]
    
    # Use full sf object (not st_geometry) so attributes survive intersection
    road_clip <- st_intersection(yerevan_roads, bufs["buf_id"])
    
    # Keep only line geometries
    road_clip <- road_clip[
      st_geometry_type(road_clip) %in% c("LINESTRING", "MULTILINESTRING"), 
    ]
    
    # Build clean data.table directly
    len_df <- data.table(
      buf_id  = as.integer(road_clip$buf_id),
      seg_len = as.numeric(st_length(road_clip))
    )
    
    # Sum lengths per buffer cell
    len_by_buf <- len_df[, .(seg_len = sum(seg_len, na.rm = TRUE)), by = buf_id]
    
    # Left-join to full grid, fill missing with 0
    result <- merge(
      data.table(buf_id = seq_len(n_cells)),
      len_by_buf,
      by    = "buf_id",
      all.x = TRUE
    )
    result[is.na(seg_len), seg_len := 0]
    
    grid_100m[[paste0("road_length_", r, "m")]] <- result$seg_len
    p(message = sprintf("road length %dm done", r))
  }
})

cat("✓ Road lengths computed\n")


# 5b. Distance to nearest primary road
primary_roads <- filter(yerevan_roads, highway == "primary")

if (nrow(primary_roads) > 0) {
  cat("  Computing distance to nearest primary road...\n")
  
  nearest_idx <- st_nearest_feature(cents, primary_roads)
  
  # Single vectorised call — extract diagonal of matched pairs
  near_geom     <- primary_roads[nearest_idx, ]
  dist_primary  <- as.numeric(diag(st_distance(cents, near_geom)))
  
  grid_100m$dist_primary_road_m <- dist_primary
  cat("✓ Distance to primary road computed\n")
} else {
  warning("No primary roads found — dist_primary_road_m set to NA")
  grid_100m$dist_primary_road_m <- NA_real_
}

# -----------------------------------------------------------------------
# 6. Export
# -----------------------------------------------------------------------
cat("\nExporting...\n")
output_df <- st_drop_geometry(grid_100m) |> as.data.table()
output_path <- "C:/Users/mrealehatem/Documents/GitHub/ArmeniaPollutionAnalysis/yerevan_grid_full_covariates.csv"
fwrite(output_df, output_path)
gpkg_path <- "C:/Users/mrealehatem/Documents/GitHub/ArmeniaPollutionAnalysis/yerevan_grid_full_covariates.gpkg"
st_write(grid_100m, gpkg_path, delete_dsn = TRUE)
cat("✅ Spatial file saved:", gpkg_path, "\n")
cat("✅ Saved:", ncol(output_df), "columns for", nrow(output_df), "grid cells\n")
cat("   →", output_path, "\n")
