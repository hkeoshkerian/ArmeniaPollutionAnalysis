# ====================================================================
# GEMM Implementation - Annual Average PM2.5 with All-Cause Mortality
# ====================================================================

library(terra)
library(readxl)
library(ggplot2)

cat("Loading data...\n")
pop_raster_raw <- rast("C:/Users/mrealehatem/Documents/GitHub/ArmeniaPollutionAnalysis/WorldPopData/arm_age_data/pop.tif")
pm25_stack <- rast("C:/Users/mrealehatem/OneDrive/AUA/Air pollution/pm25_yerevan_stack.tif")

# Check CRS match and reproject if needed
cat("Pop CRS:", crs(pop_raster_raw), "\n")
cat("PM2.5 CRS:", crs(pm25_stack), "\n")

if(crs(pop_raster_raw) != crs(pm25_stack)) {
  cat("Reprojecting pop_raster to match pm25_stack CRS...\n")
  pop_raster_raw <- project(pop_raster_raw, crs(pm25_stack))
}

# Crop to PM2.5 extent (UTM coordinates)
pop_raster <- crop(pop_raster_raw, ext(pm25_stack))


cat("Loading coefficients...\n")
coeff_df <- read_excel("C:/Users/mrealehatem/Documents/GitHub/ArmeniaPollutionAnalysis/GEMMcoefficients.xlsx")
colnames(coeff_df) <- c("Cause", "AgeGroup", "Theta", "Theta_SD", "Alpha", "Mu", "Nu","Deaths23","Pop23","DeathRate")
coeff_df <- coeff_df[complete.cases(coeff_df), ]

# Filter for all-cause mortality only
coeff_df <- coeff_df[coeff_df$Cause == "NCD+LRI", ]

#if(nrow(coeff_df) == 0) {
#  stop("No 'All-cause' mortality data found in coefficients file!")
#}

# Age mapping lookup
age_map <- c("0-9"="0-25", "1-10"="0-25", "5-14"="0-25", "10-19"="0-25", "15-24"="0-25",
             "20-29"="25-30", "25-34"="30-35", "30-39"="35-40", "35-44"="40-45", "40-49"="45-50",
             "45-54"="50-55", "50-59"="55-60", "55-64"="60-65", "60-69"="65-70", "65-74"="70-75",
             "70-79"="75-80", "75-84"="80+", "80-89"="80+", "85-94"="80+", "90-99"="80+")

# Pre-build lookup as matrix for speed
causes_list <- unique(coeff_df$Cause)
coef_ages <- unique(coeff_df$AgeGroup)

coef_matrix <- array(NA, dim=c(length(causes_list), length(coef_ages), 5))
dimnames(coef_matrix) <- list(causes_list, coef_ages, c("theta", "theta_sd", "alpha", "mu", "nu"))

for(i in 1:length(causes_list)) {
  for(j in 1:length(coef_ages)) {
    row <- coeff_df[coeff_df$Cause == causes_list[i] & coeff_df$AgeGroup == coef_ages[j], ]
    if(nrow(row) > 0) {
      coef_matrix[i, j, ] <- c(row$Theta[1], row$Theta_SD[1], row$Alpha[1], row$Mu[1], row$Nu[1])
    }
  }
}

# GEMM function
calculate_gemm_rr <- function(pm25, theta, alpha, mu, nu, cf = 2.4) {
  z <- pm25 - cf
  z[z < 0] <- 0
  omega <- 1 / (1 + exp(-(z - mu) / nu))
  exp(theta * log((z / alpha) + 1) * omega)
}

# Baseline all-cause mortality rate (8.4 per 100,000 per year)
baseline_rate <- 744.03 / 100000 #From 2023 statistical atlas, Yerevan specific, all cause

# ====================================================================
# Step 1: Calculate annual average PM2.5 for each grid cell
# ====================================================================
cat("Calculating annual average PM2.5...\n")
start_time <- Sys.time()

pm25_resampled <- resample(pm25_stack, pop_raster, method = "bilinear")
pm25_annual_avg <- mean(pm25_resampled, na.rm = TRUE)

cat("Annual average PM2.5 calculated\n")

# ====================================================================
# Step 2: Calculate mortality by age group and grid cell
# ====================================================================
cat("Calculating excess mortality by population grid cell...\n")

# Extract death rates for each age group from coefficients
death_rates <- coeff_df[, c("AgeGroup", "DeathRate")]
colnames(death_rates) <- c("AgeGroup", "DeathRate")

cat("\nDeath rates by age group:\n")
print(death_rates)

# Average PM2.5 to population grid resolution
pm25_pop_grid <- aggregate(pm25_annual_avg, fact = ncell(pm25_annual_avg) / ncell(pop_raster[[1]]), fun = "mean")

# Initialize storage for population cell results
n_pop_cells <- ncell(pop_raster)
deaths_by_pop_cell <- rep(0, n_pop_cells)
var_by_pop_cell <- rep(0, n_pop_cells)
pm25_pop_values <- values(pm25_pop_grid)

# For each age group
for(ag in names(pop_raster)) {
  cat("Processing age group:", ag, "\n")
  
  pop_layer <- pop_raster[[ag]]
  pop_values <- values(pop_layer)
  coef_age <- age_map[ag]
  
  # Find the death rate for this age group
  age_death_rate <- death_rates$DeathRate[death_rates$AgeGroup == coef_age]
  
  if(length(age_death_rate) > 0 && !is.na(age_death_rate)) {
    
    coef_age_idx <- which(coef_ages == coef_age)
    
    if(!is.na(coef_matrix[1, coef_age_idx, 1])) {
      theta <- coef_matrix[1, coef_age_idx, 1]
      theta_sd <- coef_matrix[1, coef_age_idx, 2]
      alpha <- coef_matrix[1, coef_age_idx, 3]
      mu <- coef_matrix[1, coef_age_idx, 4]
      nu <- coef_matrix[1, coef_age_idx, 5]
      
      # Calculate RR for averaged PM2.5
      rr_values <- calculate_gemm_rr(pm25_pop_values, theta, alpha, mu, nu)
      paf_values <- (rr_values - 1) / rr_values
      paf_values[is.na(paf_values)] <- 0
      
      # Mortality calculations using age-group-specific death rate
      death_values <- paf_values * pop_values * age_death_rate
      var_values <- (pop_values * age_death_rate)^2 * theta_sd^2
      
      deaths_by_pop_cell <- deaths_by_pop_cell + death_values
      var_by_pop_cell <- var_by_pop_cell + var_values
      
      cat("  ✓ Death rate for", coef_age, "=", age_death_rate, "\n")
    } else {
      cat("  ⚠ No GEMM coefficients for age group", coef_age, "\n")
    }
  } else {
    cat("  ⚠ No death rate found for age group", coef_age, "\n")
  }
}

# Create rasters at population grid resolution
deaths_raster <- pop_raster[[1]]
values(deaths_raster) <- deaths_by_pop_cell

var_raster <- pop_raster[[1]]
values(var_raster) <- var_by_pop_cell

elapsed <- as.numeric(Sys.time() - start_time, units = "mins")
cat("Processing completed in", round(elapsed, 1), "minutes\n\n")

# ====================================================================
# Step 3: Load PM2.5 variance stack and aggregate to population grid
# ====================================================================
cat("Loading and processing variance stack...\n")

# Load variance stack
var_stack <- rast("C:/Users/mrealehatem/OneDrive/AUA/Air pollution/pm25_variance_stack.tif")

# Reproject if needed
if(crs(var_stack) != crs(pm25_stack)) {
  var_stack <- project(var_stack, crs(pm25_stack))
}

# Resample to match PM2.5 grid
var_stack_resampled <- resample(var_stack, pm25_annual_avg, method = "bilinear")

# Average variance to population grid resolution
var_pop_grid <- aggregate(var_stack_resampled, fact = ncell(var_stack_resampled) / ncell(pop_raster[[1]]), fun = "mean")

# deaths_raster and var_raster already created in Step 2
cat("Variance raster aggregated to population grid\n")

# ====================================================================
# Step 4: Calculate total excess mortality
# ====================================================================
total_deaths <- sum(deaths_by_pop_cell, na.rm = TRUE)
total_se <- sqrt(sum(var_by_pop_cell, na.rm = TRUE))
ci_lower <- total_deaths - 1.96 * total_se
ci_upper <- total_deaths + 1.96 * total_se

cat("\n=== Annual Excess Mortality Results ===\n")
cat("Total Deaths:", round(total_deaths, 2), "\n")
cat("SE:", round(total_se, 2), "\n")
cat("95% CI: [", round(ci_lower, 2), ", ", round(ci_upper, 2), "]\n\n")

results <- data.frame(
  Metric = c("Total Deaths", "SE", "CI Lower", "CI Upper"),
  Value = c(
    round(total_deaths, 2),
    round(total_se, 2),
    round(ci_lower, 2),
    round(ci_upper, 2)
  )
)

print(results)
write.csv(results, "C:/Users/mrealehatem/Documents/GitHub/ArmeniaPollutionAnalysis/yerevan_annual_mortality.csv", row.names = FALSE)

# ====================================================================
# Step 5: Create heatmap of deaths per capita on OpenStreetMap
# ====================================================================
cat("Creating visualizations...\n")

library(leaflet)
library(leafem)

# Sum population across all age groups
pop_total <- pop_raster[[1]]
values(pop_total) <- 0
for(ag in names(pop_raster)) {
  pop_total <- pop_total + pop_raster[[ag]]
}

# Calculate deaths per 1,000 capita
deaths_per_capita <- deaths_raster
values(deaths_per_capita) <- (values(deaths_raster) / values(pop_total)) * 1000

# Convert zeros and NaN to NA
values(deaths_per_capita)[values(deaths_per_capita) == 0 | is.nan(values(deaths_per_capita))] <- NA

# Also mask out cells with no PM2.5 data
values(deaths_per_capita)[is.na(values(pm25_pop_grid))] <- NA

# Reproject to WGS84 (EPSG:4326) for leaflet
deaths_per_capita_wgs84 <- project(deaths_per_capita, "EPSG:4326")

# Create color palette
pal <- colorNumeric(viridis::viridis(256), 
                    values(deaths_per_capita_wgs84), 
                    na.color = NA)

# Create leaflet map
heatmap_leaflet <- leaflet() %>%
  addProviderTiles("OpenStreetMap.Mapnik") %>%  # Add OSM basemap
  addRasterImage(deaths_per_capita_wgs84, 
                 colors = pal, 
                 opacity = 0.7,
                 method = "ngb") %>%
  addLegend(pal = pal, 
            values = values(deaths_per_capita_wgs84),
            title = "Excess Deaths<br>per 1,000 people<br>(Annual)",
            position = "bottomright") %>%
  setView(lng = 44.5086, lat = 40.1792, zoom = 12)  # Yerevan coordinates

# Save as HTML
htmlwidgets::saveWidget(heatmap_leaflet, 
                        "C:/Users/mrealehatem/Documents/GitHub/ArmeniaPollutionAnalysis/mortality_heatmap_interactive.html")

cat("Interactive heatmap saved\n")
heatmap_leaflet
