library(terra)
library(sf)
library(exactextractr)
library(data.table)
library(osmextract)
library(dplyr)
library(progressr)
library(elevatr)      # NEW: for elevation lookup

handlers(global = TRUE)
handlers("progress")

# -----------------------------------------------------------------------
# Paths - UPDATE THESE
# -----------------------------------------------------------------------
yerevan_shp <- sf::st_read("C:/Users/mrealehatem/Documents/GitHub/ArmeniaPollutionAnalysis/Yerevan Boundary/boundary.gpkg",
                           layer = "boundary-polygon-lvl4")
pop_path       <- "C:/Users/mrealehatem/Documents/GitHub/ArmeniaPollutionAnalysis/WorldPopData/arm_age_data/pop.tif"
landcover_path <- "C:/Users/mrealehatem/Downloads/worldcover.tif"

# CHANGED: only 2023–2025 (years 23, 24, 25)
chm_paths      <- paste0("C:/Users/mrealehatem/Downloads/veg", 23:25, ".tif")

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
cat("\n[2/6] Population...\n")
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
# 3. Vegetation height and volume — 2023–2025 only
# -----------------------------------------------------------------------
cat("\n[3/6] Vegetation (2023–2025)...\n")
existing_chm <- chm_paths[file.exists(chm_paths)]
cat(" Found", length(existing_chm), "CHM files\n")

if (length(existing_chm) == 0) {
  warning("No CHM files found for years 23–25. Check that veg23.tif, veg24.tif, veg25.tif exist.")
} else {
  # CHANGED: print which years are actually being read
  year_labels <- regmatches(
    existing_chm,
    regexpr("(?<=veg)\\d{2}", existing_chm, perl = TRUE)
  )
  cat(" Reading vegetation for years:", paste(paste0("20", year_labels), collapse = ", "), "\n")
  
  with_progress({
    p <- progressor(steps = length(existing_chm) * length(BUFFER_RADII))
    
    for (chm_path in existing_chm) {
      year_suffix <- regmatches(
        chm_path,
        regexpr("(?<=veg)\\d{2}", chm_path, perl = TRUE)
      )
      
      cat(sprintf("  Loading veg%s.tif (year 20%s)...\n", year_suffix, year_suffix))
      chm_rast <- terra::rast(chm_path)
      
      for (r in BUFFER_RADII) {
        bufs_native <- st_transform(bufs_list[[paste0("r", r)]], terra::crs(chm_rast))
        
        veg_vals <- exact_extract(chm_rast, bufs_native, "mean", progress = FALSE)
        
        grid_100m[[paste0("veg", year_suffix, "_height_", r, "m")]] <- veg_vals
        grid_100m[[paste0("veg", year_suffix, "_volume_", r, "m")]] <- veg_vals * (pi * r^2)
        p(message = sprintf("veg%s %dm done", year_suffix, r))
      }
    }
  })
  cat("✓ Vegetation extracted\n")
}

# -----------------------------------------------------------------------
# 4. Landcover — fast built-in "frac" + print land use categories found
# -----------------------------------------------------------------------
cat("\n[4/6] Landcover...\n")
if (file.exists(landcover_path)) {
  lc_rast <- terra::rast(landcover_path)
  lc_rast <- terra::project(lc_rast, "epsg:32638", method = "near")
  
  for (r in BUFFER_RADII) {
    cat(sprintf("  %dm buffer (progress below):\n", r))
    
    lc_frac <- exact_extract(
      lc_rast,
      bufs_list[[paste0("r", r)]],
      "frac",
      progress = TRUE
    )
    
    # CHANGED: print detected land use categories (once, on first radius pass)
    if (r == BUFFER_RADII[1]) {
      raw_classes <- gsub("^frac_", "", names(lc_frac))
      cat(sprintf("  Land use categories detected (%d total): %s\n",
                  length(raw_classes),
                  paste(raw_classes, collapse = ", ")))
    }
    
    names(lc_frac) <- gsub("^frac_(.+)$", paste0("lc\\1_pct_", r, "m"), names(lc_frac))
    lc_frac        <- lc_frac * 100
    
    grid_100m <- cbind(grid_100m, lc_frac)
    cat(sprintf("  ✓ %dm done (%d classes)\n", r, ncol(lc_frac)))
  }
  cat("✓ Landcover extracted\n")
}

# -----------------------------------------------------------------------
# 5. Elevation — elevatr::get_elev_point at centroid coords           NEW
# -----------------------------------------------------------------------
cat("\n[5/6] Elevation...\n")

# elevatr needs WGS84 (EPSG:4326)
cents_wgs84 <- st_transform(cents, 4326)

# get_elev_point queries the AWS Terrain Tiles at zoom=14 (~10 m resolution)
# src="aws" is free, no API key required
elev_df <- elevatr::get_elev_point(
  locations = cents_wgs84,
  src       = "aws",
  z         = 14          # zoom level: higher = finer resolution, slower
)

grid_100m$elevation_m <- elev_df$elevation
cat(sprintf("✓ Elevation extracted (min: %.1f m, max: %.1f m, mean: %.1f m)\n",
            min(elev_df$elevation, na.rm = TRUE),
            max(elev_df$elevation, na.rm = TRUE),
            mean(elev_df$elevation, na.rm = TRUE)))

# -----------------------------------------------------------------------
# 6. Road length within buffers + distance to nearest primary road
# -----------------------------------------------------------------------
cat("\n[6/6] Roads...\n")

with_progress({
  p <- progressor(steps = length(BUFFER_RADII))
  
  for (r in BUFFER_RADII) {
    bufs <- bufs_list[[paste0("r", r)]]
    
    road_clip <- st_intersection(yerevan_roads, bufs["buf_id"])
    road_clip <- road_clip[
      st_geometry_type(road_clip) %in% c("LINESTRING", "MULTILINESTRING"), 
    ]
    
    len_df <- data.table(
      buf_id  = as.integer(road_clip$buf_id),
      seg_len = as.numeric(st_length(road_clip))
    )
    
    len_by_buf <- len_df[, .(seg_len = sum(seg_len, na.rm = TRUE)), by = buf_id]
    
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

primary_roads <- filter(yerevan_roads, highway == "primary")

if (nrow(primary_roads) > 0) {
  cat("  Computing distance to nearest primary road...\n")
  
  nearest_idx  <- st_nearest_feature(cents, primary_roads)
  near_geom    <- primary_roads[nearest_idx, ]
  dist_primary <- as.numeric(diag(st_distance(cents, near_geom)))
  
  grid_100m$dist_primary_road_m <- dist_primary
  cat("✓ Distance to primary road computed\n")
} else {
  warning("No primary roads found — dist_primary_road_m set to NA")
  grid_100m$dist_primary_road_m <- NA_real_
}

# -----------------------------------------------------------------------
# 7. Export
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