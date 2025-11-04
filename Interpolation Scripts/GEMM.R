# ====================================================================
# GEMM Implementation in R for Hourly PM2.5-Attributable Mortality
# Across a 100 m Grid of Yerevan with Age-Cohort–Specific GEMM Parameters
# ====================================================================

# 0. Install/load required packages
# install.packages(c("terra"))
library(terra)

# ====================================================================
# 1. DEFINE THE GEMM RELATIVE RISK FUNCTION
# ====================================================================
calculate_gemm_rr <- function(pm25, theta, alpha, mu, nu, cf = 2.4) {
  z     <- pmax(0, pm25 - cf)
  omega <- 1 / (1 + exp(-(z - mu) / nu))
  Tz    <- log((z / alpha) + 1) * omega
  exp(theta * Tz)
}

# ====================================================================
# 2. LOAD AGE-COHORT–SPECIFIC GEMM PARAMETERS FROM SI TABLE S2
# ====================================================================
gemm_params <- list(
  "25-34" = list(theta = 0.1585, alpha = 1.6, mu = 15.5, nu = 36.8),  # age midpoint 27.5
  "35-44" = list(theta = 0.1577, alpha = 1.6, mu = 15.5, nu = 36.8),  # 32.5
  "45-54" = list(theta = 0.1570, alpha = 1.6, mu = 15.5, nu = 36.8),  # 37.5
  "55-64" = list(theta = 0.1558, alpha = 1.6, mu = 15.5, nu = 36.8),  # 42.5
  "65-74" = list(theta = 0.1532, alpha = 1.6, mu = 15.5, nu = 36.8),  # 47.5
  "75-84" = list(theta = 0.1499, alpha = 1.6, mu = 15.5, nu = 36.8),  # 52.5
  "85+"   = list(theta = 0.1462, alpha = 1.6, mu = 15.5, nu = 36.8)   # 57.5 (use nearest available)
)

# ====================================================================
# 3. LOAD RASTERS
# ====================================================================
# Population: multi-layer GeoTIFF with one layer per age cohort (names = keys of gemm_params)
pop_raster <- rast("pop.tif")                 # layers: "25-34","35-44",...,"85+"

# Hourly PM2.5: one layer per hour (8760 layers)
pm25_stack <- rast("pollution.tif")

# Baseline mortality rates (annual) per age cohort
baseline_rates <- data.frame(
  age_group   = names(gemm_params),
  annual_rate = c(0.0005, 0.0012, 0.0028, 0.0075, 0.0185, 0.0450, 0.1200)
) %>%
  mutate(hourly_rate = annual_rate / (365 * 24))

# ====================================================================
# 4. CALCULATE HOURLY ATTRIBUTABLE DEATHS RASTER FOR ONE COHORT
# ====================================================================
calc_hourly_deaths_for_cohort <- function(age_group) {
  params      <- gemm_params[[age_group]]
  hourly_rate <- baseline_rates$hourly_rate[baseline_rates$age_group == age_group]
  pop_layer   <- pop_raster[[age_group]]
  
  # Compute RR stack
  rr_stack <- lapp(pm25_stack, function(pm) {
    calculate_gemm_rr(pm,
                      theta = params$theta,
                      alpha = params$alpha,
                      mu    = params$mu,
                      nu    = params$nu)
  })
  
  # Compute PAF stack
  paf_stack <- (rr_stack - 1) / rr_stack
  
  # Compute deaths/hour stack: pop * hourly_rate * paf
  deaths_stack <- paf_stack * pop_layer * hourly_rate
  
  return(deaths_stack)
}

# ====================================================================
# 5. CALCULATE AND SUM HOURLY DEATHS FOR ALL AGE COHORTS
# ====================================================================
death_stacks <- lapply(names(gemm_params), calc_hourly_deaths_for_cohort)
total_hourly_deaths <- Reduce(`+`, death_stacks)

# ====================================================================
# 6. AGGREGATE TO ANNUAL DEATHS (SUM OVER HOURS)
# ====================================================================
annual_deaths_raster <- app(total_hourly_deaths, fun = sum, cores = 4)

# ====================================================================
# 7. OUTPUT RESULTS
# ====================================================================
writeRaster(total_hourly_deaths,  "yerevan_hourly_deaths.tif", overwrite = TRUE)
writeRaster(annual_deaths_raster, "yerevan_annual_deaths.tif", overwrite = TRUE)

total_deaths <- global(annual_deaths_raster, fun = "sum", na.rm = TRUE)
cat("Total annual PM2.5-attributable deaths in Yerevan:", total_deaths, "\n")
