# Load required libraries
library(terra)

# Set the folder path
folder_path <- "C:\\Users\\mrealehatem\\Documents\\GitHub\\ArmeniaPollutionAnalysis\\WorldPopData\\arm_age_data"

# List all .tif files matching the pattern
tif_files <- list.files(folder_path, pattern = "arm_t_.*_2025_CN_100m_R2025A_v1\\.tif$", full.names = TRUE)

# Extract age values from filenames
# Pattern: arm_t_[AGE]_2025_CN_100m_R2025A_v1.tif
ages <- as.numeric(gsub(".*arm_t_([0-9]+)_2025_CN_100m_R2025A_v1\\.tif.*", "\\1", tif_files))

# Sort files by age
sorted_indices <- order(ages)
tif_files <- tif_files[sorted_indices]
ages <- ages[sorted_indices]

# Create age labels (adjust as needed for your age groupings)
# This example assumes age represents the start of a 10-year cohort
# Modify this if your ages represent something different
age_labels <- paste0(ages, "-", ages + 9)

# Read and stack all rasters
pop_raster <- rast(tif_files)

# Rename layers with age cohort names
names(pop_raster) <- age_labels

# Verify the stack
print(pop_raster)
print(names(pop_raster))

# Save as multi-layer GeoTIFF (optional)
writeRaster(pop_raster, "pop.tif", overwrite = TRUE)

# Check file was created
cat("Stacked raster saved as pop.tif\n")
cat("Number of layers:", nlayers(pop_raster), "\n")
cat("Layer names:", names(pop_raster), "\n")