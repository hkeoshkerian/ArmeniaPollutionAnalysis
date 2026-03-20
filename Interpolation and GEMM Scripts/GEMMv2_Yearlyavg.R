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
# ====================================================================
# GEMM Visualization Suite
# ====================================================================

library(ggplot2)
library(terra)

# Helper: convert SpatRaster to data frame for ggplot
rast_to_df <- function(r, value_name = "value") {
  df <- as.data.frame(r, xy = TRUE)
  colnames(df)[3] <- value_name
  df
}

# ====================================================================
# PLOT 1 – Annual Average PM2.5 Map
# ====================================================================
df1 <- rast_to_df(pm25_annual_avg, "PM25")

p1 <- ggplot(df1, aes(x = x, y = y, fill = PM25)) +
  geom_raster() +
  scale_fill_distiller(palette = "YlOrRd", direction = 1,
                       na.value = "gray90", name = "PM₂.₅ (µg/m³)") +
  coord_equal() +
  labs(title = "Annual Average PM₂.₅ – Yerevan",
       x = "Longitude", y = "Latitude") +
  theme_minimal()

# ====================================================================
# PLOT 2 – Deaths per 1,000 Capita Map
# ====================================================================
df2 <- rast_to_df(deaths_per_capita_wgs84, "DeathsPer1k")

p2 <- ggplot(df2, aes(x = x, y = y, fill = DeathsPer1k)) +
  geom_raster() +
  scale_fill_viridis_c(na.value = "gray90", name = "Deaths / 1k") +
  coord_equal() +
  labs(title = "PM₂.₅ Excess Deaths per 1,000 – Yerevan",
       x = "Longitude", y = "Latitude") +
  theme_minimal()

# ====================================================================
# PLOT 3 – Total Excess Deaths Map
# ====================================================================
df3 <- rast_to_df(deaths_raster, "Deaths")

p3 <- ggplot(df3, aes(x = x, y = y, fill = Deaths)) +
  geom_raster() +
  scale_fill_distiller(palette = "Reds", direction = 1,
                       na.value = "gray90", name = "Deaths") +
  coord_equal() +
  labs(title = "Total Excess Deaths per Grid Cell – Yerevan",
       x = "Longitude", y = "Latitude") +
  theme_minimal()

# ====================================================================
# PLOT 4 – Uncertainty (SE) Map
# ====================================================================
se_raster <- deaths_raster
values(se_raster) <- sqrt(var_by_pop_cell)
df4 <- rast_to_df(se_raster, "SE")

p4 <- ggplot(df4, aes(x = x, y = y, fill = SE)) +
  geom_raster() +
  scale_fill_distiller(palette = "Blues", direction = 1,
                       na.value = "gray90", name = "SE (deaths)") +
  coord_equal() +
  labs(title = "Mortality Estimate Uncertainty (SE) – Yerevan",
       x = "Longitude", y = "Latitude") +
  theme_minimal()

# ====================================================================
# PLOT 5 – Age-Group Deaths Bar Chart
# NOTE: Add these two inserts into your existing loop first (see below)
# ====================================================================

# --- INSERT BEFORE your for(ag in names(pop_raster)) loop ---
age_deaths_summary <- data.frame(AgeGroup = character(),
                                 CoefAge  = character(),
                                 Deaths   = numeric(),
                                 stringsAsFactors = FALSE)

# --- INSERT AT THE END INSIDE the if(!is.na(coef_matrix[...])) block ---
age_deaths_summary <- rbind(age_deaths_summary, data.frame(
  AgeGroup = ag,
  CoefAge  = coef_age,
  Deaths   = sum(death_values, na.rm = TRUE)
))

# --- After the loop ---
age_deaths_agg <- aggregate(Deaths ~ CoefAge, data = age_deaths_summary, FUN = sum)
age_deaths_agg$CoefAge <- factor(age_deaths_agg$CoefAge,
                                 levels = c("0-25","25-30","30-35","35-40","40-45",
                                            "45-50","50-55","55-60","60-65","65-70",
                                            "70-75","75-80","80+"))

p5 <- ggplot(age_deaths_agg, aes(x = CoefAge, y = Deaths)) +
  geom_col(fill = "steelblue") +
  labs(title = "PM₂.₅ Excess Deaths by Age Group",
       x = "Age Group", y = "Excess Deaths (annual)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ====================================================================
# PLOT 6 – GEMM Dose-Response Curves (RR and PAF vs. PM2.5)
# ====================================================================
pm25_seq      <- seq(0, 100, length.out = 500)
cf            <- 2.4
selected_ages <- c("0-25", "45-50", "65-70", "80+")

curve_df <- do.call(rbind, lapply(selected_ages, function(ag) {
  idx   <- which(coef_ages == ag)
  theta <- coef_matrix[1, idx, 1]
  alpha <- coef_matrix[1, idx, 3]
  mu    <- coef_matrix[1, idx, 4]
  nu    <- coef_matrix[1, idx, 5]
  rr    <- calculate_gemm_rr(pm25_seq, theta, alpha, mu, nu, cf)
  data.frame(PM25 = pm25_seq, RR = rr, PAF = (rr - 1) / rr, AgeGroup = ag)
}))

p6a <- ggplot(curve_df, aes(x = PM25, y = RR, color = AgeGroup)) +
  geom_line(linewidth = 1) +
  geom_vline(xintercept = cf, linetype = "dashed", color = "gray50") +
  labs(title = "GEMM Relative Risk vs. PM₂.₅",
       x = "PM₂.₅ (µg/m³)", y = "Relative Risk (RR)",
       color = "Age Group") +
  theme_minimal()

p6b <- ggplot(curve_df, aes(x = PM25, y = PAF, color = AgeGroup)) +
  geom_line(linewidth = 1) +
  geom_vline(xintercept = cf, linetype = "dashed", color = "gray50") +
  labs(title = "Population Attributable Fraction vs. PM₂.₅",
       x = "PM₂.₅ (µg/m³)", y = "PAF",
       color = "Age Group") +
  theme_minimal()

# ====================================================================
# PLOT 7 – Grid-Cell PM2.5 vs. Excess Deaths Scatterplot
# ====================================================================
scatter_df <- data.frame(
  PM25   = as.vector(values(pm25_pop_grid)),
  Deaths = deaths_by_pop_cell
)
scatter_df <- scatter_df[!is.na(scatter_df$PM25) & scatter_df$Deaths > 0, ]

p7 <- ggplot(scatter_df, aes(x = PM25, y = Deaths)) +
  geom_point(alpha = 0.4, size = 1.2, color = "tomato") +
  geom_smooth(method = "loess", se = TRUE, color = "black") +
  labs(title = "Grid-Cell PM₂.₅ vs. Excess Deaths",
       x = "Annual Avg PM₂.₅ (µg/m³)", y = "Excess Deaths") +
  theme_minimal()

# ====================================================================
# PLOT 8 – Baseline vs. PM2.5-Attributable Deaths (Pie Chart)
# ====================================================================
baseline_deaths <- sum(values(pop_total), na.rm = TRUE) * baseline_rate
attr_deaths     <- sum(deaths_by_pop_cell, na.rm = TRUE)

summary_df <- data.frame(
  Category = c("Baseline (non-PM₂.₅)", "PM₂.₅ Attributable"),
  Deaths   = c(baseline_deaths - attr_deaths, attr_deaths)
)

p8 <- ggplot(summary_df, aes(x = "", y = Deaths, fill = Category)) +
  geom_col(width = 1) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("steelblue", "tomato")) +
  labs(title = "Share of Annual Deaths Attributable to PM₂.₅",
       fill = NULL) +
  theme_void()

# ====================================================================
# PLOT 9 – Total Deaths with 95% CI
# ====================================================================
ci_df <- data.frame(
  Estimate = total_deaths,
  Lower    = ci_lower,
  Upper    = ci_upper,
  Label    = "All-cause NCD+LRI"
)

p9 <- ggplot(ci_df, aes(x = Label, y = Estimate)) +
  geom_col(fill = "steelblue", width = 0.4) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.1, linewidth = 1) +
  labs(title = "Annual PM₂.₅ Excess Deaths – Yerevan (95% CI)",
       x = NULL, y = "Excess Deaths") +
  theme_minimal()

# ====================================================================
# Print all plots
# ====================================================================
print(p1)
print(p2)
print(p3)
print(p4)
print(p5)
print(p6a)
print(p6b)
print(p7)
print(p8)
print(p9)

# ====================================================================
# Optional: Save all to files
# ====================================================================
ggsave("plot1_pm25_map.png",           p1,   width = 8, height = 6, dpi = 300)
ggsave("plot2_deaths_per_capita.png",  p2,   width = 8, height = 6, dpi = 300)
ggsave("plot3_deaths_total.png",       p3,   width = 8, height = 6, dpi = 300)
ggsave("plot4_uncertainty.png",        p4,   width = 8, height = 6, dpi = 300)
ggsave("plot5_age_bar.png",            p5,   width = 8, height = 5, dpi = 300)
ggsave("plot6a_rr_curve.png",          p6a,  width = 8, height = 5, dpi = 300)
ggsave("plot6b_paf_curve.png",         p6b,  width = 8, height = 5, dpi = 300)
ggsave("plot7_scatter.png",            p7,   width = 7, height = 5, dpi = 300)
ggsave("plot8_pie.png",                p8,   width = 6, height = 6, dpi = 300)
ggsave("plot9_ci_bar.png",             p9,   width = 5, height = 5, dpi = 300)
