# ====================================================================
# GEMM Implementation - Annual Average PM2.5 with Age-Cohort Outputs
# Fixed version with OSM basemap overlays for all spatial maps
# ====================================================================

library(terra)
library(readxl)
library(ggplot2)
library(dplyr)
library(leaflet)
library(leafem)
library(htmlwidgets)
library(viridis)
library(ggspatial)   # annotation_map_tile() for OSM basemap
library(sf)          # needed by ggspatial

# ====================================================================
# File paths
# ====================================================================
pop_path  <- "C:/Users/mrealehatem/Documents/GitHub/ArmeniaPollutionAnalysis/WorldPopData/arm_age_data/pop.tif"
pm25_path <- "C:/Users/mrealehatem/OneDrive/AUA/Air pollution/pm25_yerevan_stack.tif"
var_path  <- "C:/Users/mrealehatem/OneDrive/AUA/Air pollution/pm25_variance_stack.tif"
coef_path <- "C:/Users/mrealehatem/Documents/GitHub/ArmeniaPollutionAnalysis/GEMMcoefficients.xlsx"
out_dir   <- "C:/Users/mrealehatem/Documents/GitHub/ArmeniaPollutionAnalysis"

# ====================================================================
# Load data
# ====================================================================
cat("Loading data...\n")
pop_raster_raw <- rast(pop_path)
pm25_stack     <- rast(pm25_path)

if (crs(pop_raster_raw) != crs(pm25_stack)) {
  cat("Reprojecting population raster to match PM2.5 CRS...\n")
  pop_raster_raw <- project(pop_raster_raw, crs(pm25_stack))
}

pop_raster <- crop(pop_raster_raw, ext(pm25_stack))

cat("Loading coefficients...\n")
coeff_all <- read_excel(coef_path)
colnames(coeff_all) <- c("Cause","AgeGroup","Theta","Theta_SD","Alpha","Mu","Nu",
                         "Deaths23","Pop23","DeathRate")
coeff_all <- coeff_all[complete.cases(coeff_all), ]
coeff_df  <- coeff_all %>% filter(Cause == "NCD+LRI")

if (nrow(coeff_df) == 0) stop("No rows found for Cause == 'NCD+LRI'.")

# ====================================================================
# Age mapping
# ====================================================================
age_map <- c(
  "0-9"="0-25","1-10"="0-25","5-14"="0-25","10-19"="0-25","15-24"="0-25",
  "20-29"="25-30","25-34"="30-35","30-39"="35-40","35-44"="40-45","40-49"="45-50",
  "45-54"="50-55","50-59"="55-60","55-64"="60-65","60-69"="65-70","65-74"="70-75",
  "70-79"="75-80","75-84"="80+","80-89"="80+","85-94"="80+","90-99"="80+"
)

age_order <- c("0-25","25-30","30-35","35-40","40-45","45-50","50-55","55-60",
               "60-65","65-70","70-75","75-80","80+")

# ====================================================================
# Coefficient matrix
# ====================================================================
causes_list <- unique(coeff_df$Cause)
coef_ages   <- unique(coeff_df$AgeGroup)
coef_matrix <- array(NA, dim = c(length(causes_list), length(coef_ages), 5),
                     dimnames = list(causes_list, coef_ages,
                                     c("theta","theta_sd","alpha","mu","nu")))
for (i in seq_along(causes_list)) {
  for (j in seq_along(coef_ages)) {
    row <- coeff_df[coeff_df$Cause == causes_list[i] & coeff_df$AgeGroup == coef_ages[j], ]
    if (nrow(row) > 0)
      coef_matrix[i, j, ] <- c(row$Theta[1], row$Theta_SD[1], row$Alpha[1], row$Mu[1], row$Nu[1])
  }
}

# ====================================================================
# GEMM function
# ====================================================================
calculate_gemm_rr <- function(pm25, theta, alpha, mu, nu, cf = 2.4) {
  z <- pm25 - cf; z[z < 0] <- 0
  omega <- 1 / (1 + exp(-(z - mu) / nu))
  exp(theta * log((z / alpha) + 1) * omega)
}

baseline_rate <- 744.03 / 100000

# ====================================================================
# Step 1: Annual average PM2.5 (at population grid resolution)
# ====================================================================
cat("Calculating annual average PM2.5...\n")
start_time <- Sys.time()

pm25_resampled  <- resample(pm25_stack, pop_raster[[1]], method = "bilinear")
pm25_annual_avg <- mean(pm25_resampled, na.rm = TRUE)

# ====================================================================
# Step 2: Mortality loop
# ====================================================================
cat("Calculating excess mortality...\n")

death_rates <- coeff_df[, c("AgeGroup","DeathRate")]
n_pop_cells       <- ncell(pop_raster[[1]])
deaths_by_pop_cell <- rep(0, n_pop_cells)
var_by_pop_cell    <- rep(0, n_pop_cells)
pm25_pop_values    <- values(pm25_annual_avg)

age_deaths_summary <- data.frame(
  RasterAgeGroup = character(), CoefAge = character(),
  Deaths = numeric(), BaselineDeaths = numeric(),
  MeanRR = numeric(), MeanPAF = numeric(), MeanAbsoluteRisk = numeric(),
  stringsAsFactors = FALSE
)

for (ag in names(pop_raster)) {
  if (!ag %in% names(age_map)) { cat("  ⚠ No mapping:", ag, "\n"); next }
  
  pop_values   <- values(pop_raster[[ag]])
  coef_age     <- unname(age_map[ag])
  age_dr       <- death_rates$DeathRate[death_rates$AgeGroup == coef_age]
  if (length(age_dr) == 0 || is.na(age_dr[1])) { cat("  ⚠ No death rate:", coef_age, "\n"); next }
  age_dr       <- age_dr[1]
  idx          <- which(coef_ages == coef_age)
  if (length(idx) == 0 || is.na(coef_matrix[1, idx, 1])) { cat("  ⚠ No coefficients:", coef_age, "\n"); next }
  
  theta    <- coef_matrix[1, idx, 1]; theta_sd <- coef_matrix[1, idx, 2]
  alpha    <- coef_matrix[1, idx, 3]; mu       <- coef_matrix[1, idx, 4]
  nu       <- coef_matrix[1, idx, 5]
  
  rr_values              <- calculate_gemm_rr(pm25_pop_values, theta, alpha, mu, nu)
  paf_values             <- (rr_values - 1) / rr_values
  paf_values[is.na(paf_values)] <- 0
  
  baseline_death_values  <- pop_values * age_dr
  death_values           <- paf_values * baseline_death_values
  var_values             <- (baseline_death_values)^2 * theta_sd^2
  var_values[is.na(var_values)]   <- 0
  death_values[is.na(death_values)] <- 0
  
  deaths_by_pop_cell <- deaths_by_pop_cell + death_values
  var_by_pop_cell    <- var_by_pop_cell    + var_values
  
  age_deaths_summary <- rbind(age_deaths_summary, data.frame(
    RasterAgeGroup = ag, CoefAge = coef_age,
    Deaths = sum(death_values, na.rm = TRUE),
    BaselineDeaths = sum(baseline_death_values, na.rm = TRUE),
    MeanRR = mean(rr_values, na.rm = TRUE),
    MeanPAF = mean(paf_values, na.rm = TRUE),
    MeanAbsoluteRisk = mean(paf_values * age_dr, na.rm = TRUE),
    stringsAsFactors = FALSE
  ))
  
  cat("  ✓", ag, "->", coef_age, "| death rate =", age_dr, "\n")
}

# Aggregate
age_deaths_agg <- age_deaths_summary %>%
  group_by(CoefAge) %>%
  summarise(Deaths = sum(Deaths), BaselineDeaths = sum(BaselineDeaths),
            MeanRR = mean(MeanRR), MeanPAF = mean(MeanPAF),
            MeanAbsoluteRisk = mean(MeanAbsoluteRisk), .groups = "drop") %>%
  mutate(CoefAge = factor(CoefAge, levels = age_order))

cause_deaths_summary <- data.frame(
  Cause = "NCD+LRI",
  Deaths = sum(age_deaths_summary$Deaths),
  BaselineDeaths = sum(age_deaths_summary$BaselineDeaths),
  stringsAsFactors = FALSE
)

deaths_raster <- pop_raster[[1]]; values(deaths_raster) <- deaths_by_pop_cell
var_raster    <- pop_raster[[1]]; values(var_raster)    <- var_by_pop_cell
se_raster     <- pop_raster[[1]]; values(se_raster)     <- sqrt(var_by_pop_cell)

elapsed <- as.numeric(Sys.time() - start_time, units = "mins")
cat("Done in", round(elapsed, 1), "minutes\n")

# ====================================================================
# Step 3: Variance stack
# ====================================================================
var_stack <- rast(var_path)
if (crs(var_stack) != crs(pm25_stack)) var_stack <- project(var_stack, crs(pm25_stack))
var_pop_grid <- mean(resample(var_stack, pop_raster[[1]], method = "bilinear"), na.rm = TRUE)

# ====================================================================
# Step 4: Summary stats
# ====================================================================
total_deaths <- sum(deaths_by_pop_cell, na.rm = TRUE)
total_se     <- sqrt(sum(var_by_pop_cell, na.rm = TRUE))
ci_lower     <- total_deaths - 1.96 * total_se
ci_upper     <- total_deaths + 1.96 * total_se

cat("\n=== Annual Excess Mortality Results ===\n")
cat("Total Deaths:", round(total_deaths, 2), "\n")
cat("SE:", round(total_se, 2), "\n")
cat("95% CI: [", round(ci_lower, 2), ",", round(ci_upper, 2), "]\n")

results <- data.frame(
  Metric = c("Total Deaths","SE","CI Lower","CI Upper"),
  Value  = round(c(total_deaths, total_se, ci_lower, ci_upper), 2)
)
print(results)

write.csv(results,              file.path(out_dir, "yerevan_annual_mortality.csv"),       row.names = FALSE)
write.csv(age_deaths_summary,   file.path(out_dir, "age_deaths_summary_raw.csv"),         row.names = FALSE)
write.csv(age_deaths_agg,       file.path(out_dir, "age_deaths_summary_aggregated.csv"),  row.names = FALSE)
write.csv(cause_deaths_summary, file.path(out_dir, "cause_deaths_summary.csv"),           row.names = FALSE)

# ====================================================================
# Step 5: Interactive leaflet map
# ====================================================================
pop_total <- sum(pop_raster, na.rm = TRUE)

deaths_per_capita <- deaths_raster
values(deaths_per_capita) <- (values(deaths_raster) / values(pop_total)) * 1000
bad <- which(values(deaths_per_capita) == 0 | is.nan(values(deaths_per_capita)) | is.infinite(values(deaths_per_capita)))
if (length(bad) > 0) values(deaths_per_capita)[bad] <- NA
values(deaths_per_capita)[is.na(values(pm25_annual_avg))] <- NA

deaths_per_capita_wgs84 <- project(deaths_per_capita, "EPSG:4326")

pal_leaflet <- colorNumeric(viridis::viridis(256),
                            values(deaths_per_capita_wgs84), na.color = NA)

heatmap_leaflet <- leaflet() %>%
  addProviderTiles("OpenStreetMap.Mapnik") %>%
  addRasterImage(deaths_per_capita_wgs84, colors = pal_leaflet, opacity = 0.7, method = "ngb") %>%
  addLegend(pal = pal_leaflet, values = values(deaths_per_capita_wgs84),
            title = "Excess Deaths<br>per 1,000 people<br>(Annual)", position = "bottomright") %>%
  setView(lng = 44.5086, lat = 40.1792, zoom = 12)

htmlwidgets::saveWidget(heatmap_leaflet, file.path(out_dir, "mortality_heatmap_interactive.html"))
cat("Interactive heatmap saved\n")

# ====================================================================
# Helper: project raster to WGS84 and convert to data frame for ggplot
# ====================================================================
rast_to_wgs84_df <- function(r, value_name = "value") {
  r_wgs84 <- project(r, "EPSG:4326", method = "bilinear")
  df <- as.data.frame(r_wgs84, xy = TRUE, na.rm = FALSE)
  colnames(df)[3] <- value_name
  df
}

# ====================================================================
# Shared ggplot theme for basemap maps
# ====================================================================
map_theme <- theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text  = element_text(size = 8, color = "gray40"),
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(size = 9, color = "gray40")
  )

# ====================================================================
# PLOT 1 – Annual Average PM2.5 over OSM basemap
# ====================================================================
df1 <- rast_to_wgs84_df(pm25_annual_avg, "PM25")

p1 <- ggplot(df1, aes(x = x, y = y, fill = PM25)) +
  annotation_map_tile(type = "osm", zoom = 12, progress = "none") +
  geom_raster(alpha = 0.75) +
  scale_fill_distiller(palette = "YlOrRd", direction = 1,
                       na.value = NA, name = "PM2.5\n(µg/m³)") +
  coord_sf(crs = 4326, expand = FALSE) +
  labs(title = "Annual Average PM2.5 – Yerevan",
       subtitle = "OSM basemap | semi-transparent raster overlay") +
  map_theme

# ====================================================================
# PLOT 2 – Deaths per 1,000 over OSM basemap
# ====================================================================
df2 <- rast_to_wgs84_df(deaths_per_capita, "DeathsPer1k")

p2 <- ggplot(df2, aes(x = x, y = y, fill = DeathsPer1k)) +
  annotation_map_tile(type = "osm", zoom = 12, progress = "none") +
  geom_raster(alpha = 0.75) +
  scale_fill_viridis_c(na.value = NA, name = "Deaths\nper 1k") +
  coord_sf(crs = 4326, expand = FALSE) +
  labs(title = "PM2.5 Excess Deaths per 1,000 – Yerevan") +
  map_theme

# ====================================================================
# PLOT 3 – Total excess deaths over OSM basemap
# ====================================================================
df3 <- rast_to_wgs84_df(deaths_raster, "Deaths")

p3 <- ggplot(df3, aes(x = x, y = y, fill = Deaths)) +
  annotation_map_tile(type = "osm", zoom = 12, progress = "none") +
  geom_raster(alpha = 0.75) +
  scale_fill_distiller(palette = "Reds", direction = 1,
                       na.value = NA, name = "Deaths") +
  coord_sf(crs = 4326, expand = FALSE) +
  labs(title = "Total Excess Deaths per Grid Cell – Yerevan") +
  map_theme

# ====================================================================
# PLOT 4 – Uncertainty (SE) over OSM basemap
# ====================================================================
df4 <- rast_to_wgs84_df(se_raster, "SE")

p4 <- ggplot(df4, aes(x = x, y = y, fill = SE)) +
  annotation_map_tile(type = "osm", zoom = 12, progress = "none") +
  geom_raster(alpha = 0.75) +
  scale_fill_distiller(palette = "Blues", direction = 1,
                       na.value = NA, name = "SE\n(deaths)") +
  coord_sf(crs = 4326, expand = FALSE) +
  labs(title = "Mortality Estimate Uncertainty (SE) – Yerevan") +
  map_theme

# ====================================================================
# Non-spatial plots (unchanged)
# ====================================================================
p10 <- ggplot(cause_deaths_summary, aes(x = reorder(Cause, Deaths), y = Deaths, fill = Cause)) +
  geom_col(width = 0.7, show.legend = FALSE) +
  coord_flip() +
  labs(title = "PM2.5 Attributable Deaths by Cause", x = NULL, y = "Attributable deaths") +
  theme_minimal()

p11 <- ggplot(cause_deaths_summary, aes(x = reorder(Cause, BaselineDeaths), y = BaselineDeaths, fill = Cause)) +
  geom_col(width = 0.7, show.legend = FALSE) +
  coord_flip() +
  labs(title = "Baseline Mortality by Cause", x = NULL, y = "Baseline deaths") +
  theme_minimal()

p12 <- ggplot(age_deaths_agg, aes(x = CoefAge, y = Deaths, fill = CoefAge)) +
  geom_col(width = 0.75, show.legend = FALSE) +
  labs(title = "PM2.5 Attributable Deaths by Age Cohort", x = "Age cohort", y = "Attributable deaths") +
  theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))

p13 <- ggplot(age_deaths_agg, aes(x = CoefAge, y = MeanAbsoluteRisk, group = 1)) +
  geom_line(linewidth = 1, color = "#2C7FB8") +
  geom_point(size = 2.5, color = "#2C7FB8") +
  labs(title = "GEMM Absolute Risk by Age Cohort", x = "Age cohort", y = "Mean absolute risk") +
  theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))

p14 <- ggplot(age_deaths_agg, aes(x = CoefAge, y = BaselineDeaths, fill = CoefAge)) +
  geom_col(width = 0.75, show.legend = FALSE) +
  labs(title = "Baseline Mortality by Age Cohort", x = "Age cohort", y = "Baseline deaths") +
  theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))

pm25_seq <- seq(0, 100, length.out = 500); cf <- 2.4
selected_ages <- c("0-25","45-50","65-70","80+")

curve_df <- do.call(rbind, lapply(selected_ages, function(ag) {
  idx <- which(coef_ages == ag)
  rr  <- calculate_gemm_rr(pm25_seq, coef_matrix[1,idx,1], coef_matrix[1,idx,3],
                           coef_matrix[1,idx,4], coef_matrix[1,idx,5], cf)
  data.frame(PM25 = pm25_seq, RR = rr, PAF = (rr - 1) / rr, AgeGroup = ag)
}))

p6a <- ggplot(curve_df, aes(x = PM25, y = RR, color = AgeGroup)) +
  geom_line(linewidth = 1) +
  geom_vline(xintercept = cf, linetype = "dashed", color = "gray50") +
  labs(title = "GEMM Relative Risk vs. PM2.5", x = "PM2.5 (µg/m³)", y = "RR", color = "Age Group") +
  theme_minimal()

p6b <- ggplot(curve_df, aes(x = PM25, y = PAF, color = AgeGroup)) +
  geom_line(linewidth = 1) +
  geom_vline(xintercept = cf, linetype = "dashed", color = "gray50") +
  labs(title = "PAF vs. PM2.5", x = "PM2.5 (µg/m³)", y = "PAF", color = "Age Group") +
  theme_minimal()

scatter_df <- data.frame(PM25 = as.vector(values(pm25_annual_avg)), Deaths = deaths_by_pop_cell)
scatter_df <- scatter_df[!is.na(scatter_df$PM25) & scatter_df$Deaths > 0, ]

p7 <- ggplot(scatter_df, aes(x = PM25, y = Deaths)) +
  geom_point(alpha = 0.4, size = 1.2, color = "tomato") +
  geom_smooth(method = "loess", se = TRUE, color = "black") +
  labs(title = "Grid-Cell PM2.5 vs. Excess Deaths", x = "PM2.5 (µg/m³)", y = "Excess Deaths") +
  theme_minimal()

summary_df <- data.frame(
  Category = c("Baseline (non-PM2.5)","PM2.5 Attributable"),
  Deaths   = c(max(sum(values(pop_total), na.rm = TRUE) * baseline_rate - total_deaths, 0), total_deaths)
)

p8 <- ggplot(summary_df, aes(x = "", y = Deaths, fill = Category)) +
  geom_col(width = 1) + coord_polar(theta = "y") +
  scale_fill_manual(values = c("steelblue","tomato")) +
  labs(title = "Share of Deaths Attributable to PM2.5", fill = NULL) + theme_void()

ci_df <- data.frame(Estimate = total_deaths, Lower = ci_lower, Upper = ci_upper, Label = "NCD+LRI")

p9 <- ggplot(ci_df, aes(x = Label, y = Estimate)) +
  geom_col(fill = "steelblue", width = 0.4) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.1, linewidth = 1) +
  labs(title = "PM2.5 Excess Deaths – Yerevan (95% CI)", x = NULL, y = "Excess Deaths") +
  theme_minimal()

# ====================================================================
# Print
# ====================================================================
for (p in list(p1, p2, p3, p4, p10, p11, p12, p13, p14, p6a, p6b, p7, p8, p9)) print(p)

# ====================================================================
# Save
# ====================================================================
ggsave(file.path(out_dir, "plot1_pm25_map.png"),               p1,  width = 9, height = 7, dpi = 300)
ggsave(file.path(out_dir, "plot2_deaths_per_capita.png"),       p2,  width = 9, height = 7, dpi = 300)
ggsave(file.path(out_dir, "plot3_deaths_total.png"),            p3,  width = 9, height = 7, dpi = 300)
ggsave(file.path(out_dir, "plot4_uncertainty.png"),             p4,  width = 9, height = 7, dpi = 300)
ggsave(file.path(out_dir, "plot10_pm25_deaths_by_cause.png"),   p10, width = 8, height = 5, dpi = 300)
ggsave(file.path(out_dir, "plot11_baseline_by_cause.png"),      p11, width = 8, height = 5, dpi = 300)
ggsave(file.path(out_dir, "plot12_pm25_deaths_by_age.png"),     p12, width = 9, height = 5.5, dpi = 300)
ggsave(file.path(out_dir, "plot13_gemm_risk_by_age.png"),       p13, width = 9, height = 5.5, dpi = 300)
ggsave(file.path(out_dir, "plot14_baseline_by_age.png"),        p14, width = 9, height = 5.5, dpi = 300)
ggsave(file.path(out_dir, "plot6a_rr_curve.png"),               p6a, width = 8, height = 5, dpi = 300)
ggsave(file.path(out_dir, "plot6b_paf_curve.png"),              p6b, width = 8, height = 5, dpi = 300)
ggsave(file.path(out_dir, "plot7_scatter.png"),                 p7,  width = 7, height = 5, dpi = 300)
ggsave(file.path(out_dir, "plot8_pie.png"),                     p8,  width = 6, height = 6, dpi = 300)
ggsave(file.path(out_dir, "plot9_ci_bar.png"),                  p9,  width = 5, height = 5, dpi = 300)

cat("\nAll outputs saved successfully.\n")

# ====================================================================
# PLOT: Deaths per capita per unit PM2.5 (per-person mortality efficiency map)
# ====================================================================

# Total population per grid cell
pop_total_values <- values(pop_total)

# Deaths per capita per µg/m³:
# (attributable deaths / population) / PM2.5 concentration
# Mask cells below counterfactual, zero population, or zero deaths
deaths_per_cap_pm25_values <- ifelse(
  !is.na(pm25_values_grid) & pm25_values_grid > 2.4 &
    !is.na(pop_total_values) & pop_total_values > 0 &
    deaths_values > 0,
  (deaths_values / pop_total_values) / pm25_values_grid,
  NA
)

deaths_per_pm25_raster <- deaths_raster
values(deaths_per_pm25_raster) <- deaths_per_cap_pm25_values

# Scale to per 100,000 people per µg/m³ for readable legend values
scale_factor <- 100000
values(deaths_per_pm25_raster) <- values(deaths_per_pm25_raster) * scale_factor

# Reproject to WGS84
deaths_per_pm25_wgs84 <- project(deaths_per_pm25_raster, "EPSG:4326", method = "bilinear")

df_dpp <- as.data.frame(deaths_per_pm25_wgs84, xy = TRUE, na.rm = FALSE)
colnames(df_dpp)[3] <- "DeathsPerCapPM25"

# Clip top 1% to prevent extreme outliers dominating colour scale
q99 <- quantile(df_dpp$DeathsPerCapPM25, 0.99, na.rm = TRUE)
df_dpp$DeathsPerCapPM25_clipped <- pmin(df_dpp$DeathsPerCapPM25, q99)

p_dpp <- ggplot(df_dpp, aes(x = x, y = y, fill = DeathsPerCapPM25_clipped)) +
  annotation_map_tile(type = "osm", zoom = 12, progress = "none") +
  geom_raster(alpha = 0.78) +
  scale_fill_distiller(
    palette   = "RdYlGn",
    direction = -1,
    na.value  = NA,
    name      = "Deaths per\n100k pop\nper µg/m³"
  ) +
  coord_sf(crs = 4326, expand = FALSE) +
  labs(
    title    = "PM2.5 Per-Capita Mortality Efficiency – Yerevan",
    subtitle = "Excess deaths per 100,000 people per µg/m³ of annual average PM2.5",
    caption  = "Values above 99th percentile clipped. Cells below GEMM counterfactual (2.4 µg/m³) or with zero population excluded."
  ) +
  map_theme

print(p_dpp)

ggsave(
  file.path(out_dir, "plot_deaths_per_capita_per_pm25_map.png"),
  p_dpp,
  width = 9, height = 7, dpi = 300
)
cat("Deaths per capita per PM2.5 map saved.\n")