library(terra)
library(sf)
library(exactextractr)
library(data.table)

# Paths - UPDATE THESE
yerevan_shp <- "C:/Users/mrealehatem/OneDrive/AUA/Air pollution/Data/am_yerevan.shp"
pop_path <- "C:/Users/mrealehatem/Documents/GitHub/ArmeniaPollutionAnalysis/WorldPopData/arm_age_data/pop.tif"
landcover_path <- "C:/Users/mrealehatem/Downloads/worldcover.tif"
chm_paths <- paste0("C:/Users/mrealehatem/Downloads/veg", 17:25, ".tif")
library(osmextract)
roads_pbf <- "C:/Users/mrealehatem/Downloads/armenia.pbf"
# Read the lines layer and automatically pull the highway tags
armenia_roads_oe <- oe_read(
  roads_pbf, 
  layer = "lines"
)
armenia_roads_oe <- oe_read(
  roads_pbf, 
  layer = "lines",
  extra_tags = c("lanes", "maxspeed") # This creates $lanes and $maxspeed columns
)
armenia_roads_oe <- armenia_roads_oe[!is.na(armenia_roads_oe$highway), ]
yerevan <- st_read(yerevan_shp) |> st_transform(32638)
if (st_crs(armenia_roads_oe) != st_crs(yerevan)) {
  yerevan <- st_transform(yerevan, st_crs(armenia_roads_oe))
}
yerevan_roads <- st_intersection(armenia_roads_oe, yerevan)
library(dplyr)
yerevan_roads$lanes <- as.numeric(yerevan_roads$lanes)
routes <- yerevan_roads %>%
  filter(lanes >= 4 | highway %in% c("primary", "trunk"))

# 1. Create 100m grid
grid_raw <- st_make_grid(yerevan, cellsize = 100, what = "polygons")
grid_100m <- st_sf(grid_id = 1:length(grid_raw), geometry = grid_raw)
grid_100m <- st_intersection(grid_100m, yerevan)
cents <- st_centroid(grid_100m)
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
  year <- 2016 + i
  year_suffix <- substr(as.character(year), 3, 4)
  
  if(file.exists(chm_paths[i])) {
    chm_rast <- raster::raster(chm_paths[i])
    chm_rast <- raster::projectRaster(chm_rast, crs = "+proj=utm +zone=38 +datum=WGS84")
    
    for (r in c(100, 200, 500)) {
      bufs <- st_buffer(cents, r)
      veg_vals <- exact_extract(chm_rast, bufs, "mean", progress = FALSE)
      grid_100m[[paste0("veg", year_suffix, "_height_", r, "m")]] <- veg_vals
      
      buffer_area <- pi * r^2
      grid_100m[[paste0("veg", year_suffix, "_volume_", r, "m")]] <- veg_vals * buffer_area
    }
    cat("✓ Vegetation extracted for", year, "\\n")
  }
}


# 4. Landcover (% urban in buffers)
if(file.exists(landcover_path)) {
  library(terra)
  library(sf)
  library(exactextractr)
  library(progressr)
  
  lc_rast <- terra::rast(landcover_path)
  lc_rast <- terra::project(lc_rast, "epsg:32638")
  
  # Memory-efficient way to get unique classes
  lc_classes <- terra::unique(lc_rast)[,1]
  lc_classes <- lc_classes[!is.na(lc_classes)]
  
  buffer_sizes <- c(100, 200, 500)
  
  with_progress({
    p <- progressor(steps = length(buffer_sizes))
    
    for (r in buffer_sizes) {
      #Add printout 
      bufs <- st_buffer(cents, r)
      #And here
      # Calculate all classes simultaneously
      lc_extract <- exact_extract(lc_rast, bufs, function(values, coverage_fraction) {
        total_coverage <- sum(coverage_fraction, na.rm = TRUE)
        sapply(lc_classes, function(lc_class) {
          sum((values == lc_class) * coverage_fraction, na.rm = TRUE) / total_coverage * 100
        })
      }, progress = FALSE)
      
      # Assign results
      if(is.matrix(lc_extract)) {
        for (j in seq_along(lc_classes)) {
          grid_100m[[paste0("lc", lc_classes[j], "_pct_", r, "m")]] <- lc_extract[, j]
        }
      } else {
        for (j in seq_along(lc_classes)) {
          grid_100m[[paste0("lc", lc_classes[j], "_pct_", r, "m")]] <- 
            sapply(lc_extract, function(x) x[j])
        }
      }
      
      p(message = sprintf("Completed %dm buffer (%d classes)", r, length(lc_classes)))
    }
  })
  
  cat("✓ Landcover extracted\n")
}


# 6. Export
output_df <- st_drop_geometry(grid_100m) |> as.data.table()
fwrite(output_df, "yerevan_grid_full_covariates.csv")
cat("\n✅ Saved:", ncol(output_df), "columns for", nrow(output_df), "grid cells\n")
