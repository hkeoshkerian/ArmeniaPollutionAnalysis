# Efficient raster stack creation for all timestamps
library(readr)
library(dplyr)
library(sf)
library(gstat)
library(raster)

# Read and combine air pollution data
data24 <- read_csv("C:/Users/mrealehatem/Downloads/sensor_avg_hourly_2024.csv")
data24$timestamp <- gsub("\u00A0", " ", data24$timestamp)
data24$timestamp <- as.POSIXct(data24$timestamp, format = "%m/%d/%Y %H:%M")

data25 <- read_csv("C:/Users/mrealehatem/Downloads/sensor_avg_hourly_2025.csv")
data25$timestamp <- gsub("\u00A0", " ", data25$timestamp)
data25$timestamp <- as.POSIXct(data25$timestamp, format = "%Y-%m-%d %H:%M:%S")

data <- rbind(data24, data25)

# Read and filter sensors
sensors <- read_csv("C:/Users/mrealehatem/Downloads/sensors.csv")
sensors.c <- sensors %>% filter(provider == "clarity" & is_suspicious == FALSE)
data.c <- data %>% filter(sensor_id %in% sensors.c$id)

# Filter to valid timestamps with >40 observations
start_time <- as.POSIXct("2024-08-01 00:00:00")
end_time   <- as.POSIXct("2025-06-15 23:59:59")

valid_timestamps <- data.c %>%
  filter(!is.na(timestamp), timestamp >= start_time, timestamp <= end_time) %>%
  group_by(timestamp) %>%
  summarise(count = n(), .groups = 'drop') %>%
  filter(count > 40) %>%
  pull(timestamp)

data.v <- data.c %>%
  filter(timestamp %in% valid_timestamps) %>%
  merge(sensors.c, by.x = "sensor_id", by.y = "id", all.x = TRUE)

# Setup spatial objects
yerevan_boundary <- st_read("C:/Users/mrealehatem/OneDrive/AUA/Air pollution/Data/am_yerevan.shp")
yerevan_proj <- st_transform(yerevan_boundary, 32638)

# Create raster template
yerevan_bbox <- st_bbox(yerevan_proj)
raster_template <- raster(xmn = yerevan_bbox[1], xmx = yerevan_bbox[3],
                          ymn = yerevan_bbox[2], ymx = yerevan_bbox[4],
                          resolution = 100, crs = st_crs(yerevan_proj)$proj4string)
grid_sp <- as(raster_template, "SpatialPixelsDataFrame")

# Function to create kriged raster for one timestamp
krige_timestamp <- function(ts) {
  data_sub <- data.v %>% filter(timestamp == ts & !is.na(pm2.5))
  if (nrow(data_sub) < 2) return(list(pred = NA, var = NA))  # Not enough points to interpolate
  data_sub_sf <- st_as_sf(data_sub, coords = c("longitude", "latitude"), crs = 4326) %>%
    st_transform(32638)
  data_sub_sp <- as(data_sub_sf, "Spatial")
  
  variogram_emp <- variogram(pm2.5 ~ 1, data_sub_sp)
  variogram_fit <- fit.variogram(variogram_emp, model = vgm(psill = 1, "Sph", range = 1000, nugget = 0))
  krige_result <- krige(pm2.5 ~ 1, data_sub_sp, grid_sp, model = variogram_fit)
  
  pred_raster <- mask(crop(raster(krige_result["var1.pred"]), yerevan_proj), yerevan_proj)
  var_raster <- mask(crop(raster(krige_result["var1.var"]), yerevan_proj), yerevan_proj)
  return(list(pred = pred_raster, var = var_raster))
}


# Apply to all timestamps and create stacks
cat("Processing", length(valid_timestamps), "timestamps...\n")
results <- lapply(valid_timestamps, krige_timestamp)

# Create raster stacks
pred_stack <- stack(lapply(results, function(x) x$pred))
var_stack <- stack(lapply(results, function(x) x$var))

# Name layers with timestamps
names(pred_stack) <- paste0("pm25_", gsub("[^0-9]", "", valid_timestamps))
names(var_stack) <- paste0("var_", gsub("[^0-9]", "", valid_timestamps))

# Save stacks
writeRaster(pred_stack, "C:/Users/mrealehatem/OneDrive/AUA/Air pollution/pm25_yerevan_stack.tif", overwrite = TRUE)
writeRaster(var_stack, "C:/Users/mrealehatem/OneDrive/AUA/Air pollution/pm25_variance_stack.tif", overwrite = TRUE)

cat("Created raster stacks with", nlayers(pred_stack), "layers each\n")
