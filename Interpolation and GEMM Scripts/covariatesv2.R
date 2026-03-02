library(terra)
library(sf)
library(exactextractr)
library(data.table)

# Paths - UPDATE THESE
yerevan_shp    <- "C:/Users/mrealehatem/OneDrive/AUA/Air pollution/Data/am_yerevan.shp"
pop_path       <- "C:/Users/mrealehatem/Documents/GitHub/ArmeniaPollutionAnalysis/WorldPopData/arm_age_data/pop.tif"
landcover_path <- "C:/Users/mrealehatem/Downloads/worldcover.tif"
chm_paths      <- paste0("C:/Users/mrealehatem/Downloads/veg", 17:25, ".tif")
roads_pbf      <- "C:/Users/mrealehatem/Downloads/armenia.pbf"

library(osmextract)
library(dplyr)

# --- Read & prep roads ---
armenia_roads_oe <- oe_read(
  roads_pbf,
  layer     = "lines",
  extra_tags = c("lanes", "maxspeed")
)
armenia_roads_oe <- armenia_roads_oe[!is.na(armenia_roads_oe$highway), ]

yerevan <- st_read(yerevan_shp) |> st_transform(32638)

if (st_crs(armenia_roads_oe) != st_crs(yerevan)) {
  armenia_roads_oe <- st_transform(armenia_roads_oe, st_crs(yerevan))
}

yerevan_roads <- st_intersection(armenia_roads_oe, yerevan)

yerevan_roads$lanes <- as.numeric(yerevan_roads$lanes)
routes <- yerevan_roads %>%
  filter(lanes >= 4 | highway %in% c("primary", "trunk"))

# 1. Create 100m grid
grid_raw  <- st_make_grid(yerevan, cellsize = 100, what = "polygons")
grid_100m <- st_sf(grid_id = 1:length(grid_raw), geometry = grid_raw)
grid_100m <- st_intersection(grid_100m, yerevan)
cents     <- st_centroid(grid_100m)
cat("Grid:", nrow(grid_100m), "cells\n")

# 2. Population (mean in 100m, 200m, 500m buffers)
pop_rast <- raster::raster(pop_path)
pop_rast <- raster::projectRaster(pop_rast, crs = "+proj=utm +zone=38 +datum=WGS84")

for (r in c(100, 200, 500)) {
  bufs <- st_buffer(cents, r)
  vals <- exact_extract(pop_rast, bufs, "mean", progress = FALSE)
  grid_100m[[paste0("pop_mean_", r, "m")]] <- vals
}
cat("✓ Population extracted\n")

# 3. Vegetation height and volume
for (i in seq_along(chm_paths)) {
  year        <- 2016 + i
  year_suffix <- substr(as.character(year), 3, 4)
  
  if (file.exists(chm_paths[i])) {
    chm_rast <- raster::raster(chm_paths[i])
    chm_rast <- raster::projectRaster(chm_rast, crs = "+proj=utm +zone=38 +datum=WGS84")
    
    for (r in c(100, 200, 500)) {
      bufs     <- st_buffer(cents, r)
      veg_vals <- exact_extract(chm_rast, bufs, "mean", progress = FALSE)
      grid_100m[[paste0("veg", year_suffix, "_height_", r, "m")]] <- veg_vals
      grid_100m[[paste0("veg", year_suffix, "_volume_", r, "m")]] <- veg_vals * (pi * r^2)
    }
    cat("✓ Vegetation extracted for", year, "\n")
  }
}

# 4. Landcover (% per class in buffers)
if (file.exists(landcover_path)) {
  library(progressr)
  
  lc_rast   <- terra::rast(landcover_path)
  lc_rast   <- terra::project(lc_rast, "epsg:32638")
  lc_classes <- terra::unique(lc_rast)[, 1]
  lc_classes <- lc_classes[!is.na(lc_classes)]
  
  with_progress({
    p <- progressor(steps = length(c(100, 200, 500)))
    
    for (r in c(100, 200, 500)) {
      bufs       <- st_buffer(cents, r)
      lc_extract <- exact_extract(lc_rast, bufs, function(values, coverage_fraction) {
        total_coverage <- sum(coverage_fraction, na.rm = TRUE)
        sapply(lc_classes, function(lc_class) {
          sum((values == lc_class) * coverage_fraction, na.rm = TRUE) /
            total_coverage * 100
        })
      }, progress = FALSE)
      
      if (is.matrix(lc_extract)) {
        for (j in seq_along(lc_classes))
          grid_100m[[paste0("lc", lc_classes[j], "_pct_", r, "m")]] <- lc_extract[, j]
      } else {
        for (j in seq_along(lc_classes))
          grid_100m[[paste0("lc", lc_classes[j], "_pct_", r, "m")]] <-
            sapply(lc_extract, function(x) x[j])
      }
      
      p(message = sprintf("Completed %dm buffer (%d classes)", r, length(lc_classes)))
    }
  })
  cat("✓ Landcover extracted\n")
}

# -----------------------------------------------------------------------
# 5. Road length within buffers + distance to nearest primary road  (NEW)
# -----------------------------------------------------------------------

# 5a. Ensure roads are in UTM 32638 (metric CRS) for accurate length/distance
yerevan_roads_utm <- st_transform(yerevan_roads, 32638)

# 5b. Road length (metres) within 100 m, 200 m, 500 m buffers
#     Strategy: for each buffer radius, intersect buffer polygons with the
#     full road network, then sum st_length() per buffer id.

for (r in c(100, 200, 500)) {
  bufs      <- st_buffer(cents, r)
  bufs$buf_id <- seq_len(nrow(bufs))          # stable join key
  
  # Intersect roads with buffers → each road segment clipped to its buffer(s)
  road_clip <- st_intersection(
    st_geometry(yerevan_roads_utm),            # just geometry for speed
    bufs["buf_id"]                             # keep buf_id attribute
  )
  
  # Sum clipped segment lengths per buffer
  road_clip$seg_len <- as.numeric(st_length(road_clip))
  
  len_by_buf <- aggregate(seg_len ~ buf_id,
                          data = st_drop_geometry(road_clip),
                          FUN  = sum)
  
  # Left-join so cells with zero roads get 0
  result <- merge(
    data.frame(buf_id = seq_len(nrow(cents))),
    len_by_buf,
    by    = "buf_id",
    all.x = TRUE
  )
  result$seg_len[is.na(result$seg_len)] <- 0
  
  grid_100m[[paste0("road_length_", r, "m")]] <- result$seg_len
  cat("✓ Road length computed for", r, "m buffer\n")
}

# 5c. Distance (metres) from each grid centroid to the nearest primary road
primary_roads <- yerevan_roads_utm %>%
  filter(highway == "primary")

if (nrow(primary_roads) > 0) {
  # st_nearest_feature returns the index of the nearest primary road for each centroid
  cents_utm   <- st_transform(cents, 32638)
  nearest_idx <- st_nearest_feature(cents_utm, primary_roads)
  
  # st_distance between each centroid and its matched primary road geometry
  dist_to_primary <- mapply(
    function(pt_idx, rd_idx) {
      as.numeric(
        st_distance(cents_utm[pt_idx, ], primary_roads[rd_idx, ])
      )
    },
    seq_len(nrow(cents_utm)),
    nearest_idx
  )
  
  grid_100m$dist_primary_road_m <- dist_to_primary
  cat("✓ Distance to nearest primary road computed\n")
} else {
  warning("No primary roads found in yerevan_roads — dist_primary_road_m skipped.")
  grid_100m$dist_primary_road_m <- NA_real_
}

# -----------------------------------------------------------------------
# 6. Export
# -----------------------------------------------------------------------
output_df <- st_drop_geometry(grid_100m) |> as.data.table()
fwrite(output_df, "yerevan_grid_full_covariates.csv")
cat("\n✅ Saved:", ncol(output_df), "columns for", nrow(output_df), "grid cells\n")
