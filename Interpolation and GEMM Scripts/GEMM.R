# ====================================================================
# GEMM Implementation
# ====================================================================

library(terra)
library(readxl)

cat("Loading data...\n")
pop_raster_raw <- rast("C:/Users/mrealehatem/Documents/GitHub/ArmeniaPollutionAnalysis/WorldPopData/arm_age_data/pop.tif")
pm25_stack <- rast("C:/Users/mrealehatem/OneDrive/AUA/Air pollution/pm25_yerevan_stack.tif")

# Single crop operation
pop_raster <- crop(pop_raster_raw, ext(pm25_stack))

cat("Loading coefficients...\n")
coeff_df <- read_excel("C:/Users/mrealehatem/Documents/GitHub/ArmeniaPollutionAnalysis/GEMMcoefficients.xlsx")
colnames(coeff_df) <- c("Cause", "AgeGroup", "Theta", "Theta_SD", "Alpha", "Mu", "Nu")
coeff_df <- coeff_df[complete.cases(coeff_df), ]

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

calculate_gemm_rr <- function(pm25, theta, alpha, mu, nu, cf = 2.4) {
  z <- pm25 - cf
  z[z < 0] <- 0
  omega <- 1 / (1 + exp(-(z - mu) / nu))
  exp(theta * log((z / alpha) + 1) * omega)
}
# GEMM(z)=exp{θlog(z/α+1)/(1+exp{-(z-µ)/ν})}, where z=max(0, PM2.5-2.4µg/m3


baseline_rate <- (8.4/100000) / (365 * 24)

# Resample PM2.5 only once
pm25_resampled <- resample(pm25_stack, pop_raster, method = "bilinear")

cat("Processing", nlyr(pm25_resampled), "hours...\n\n")
start_time <- Sys.time()

# Accumulate only totals (not hourly maps)
annual_deaths <- 0
annual_var <- 0

for(hr in 1:nlyr(pm25_resampled)) {
  if(hr %% 100 == 0) {
    elapsed <- as.numeric(Sys.time() - start_time, units = "mins")
    cat("Hour", hr, "-", round(elapsed, 1), "min\n")
  }
  
  pm25_layer <- pm25_resampled[[hr]]
  
  for(ag in names(pop_raster)) {
    pop_layer <- pop_raster[[ag]]
    coef_age <- age_map[ag]
    
    for(cause_idx in 1:length(causes_list)) {
      coef_age_idx <- which(coef_ages == coef_age)
      
      if(!is.na(coef_matrix[cause_idx, coef_age_idx, 1])) {
        theta <- coef_matrix[cause_idx, coef_age_idx, 1]
        theta_sd <- coef_matrix[cause_idx, coef_age_idx, 2]
        alpha <- coef_matrix[cause_idx, coef_age_idx, 3]
        mu <- coef_matrix[cause_idx, coef_age_idx, 4]
        nu <- coef_matrix[cause_idx, coef_age_idx, 5]
        
        rr <- calculate_gemm_rr(pm25_layer, theta, alpha, mu, nu)
        paf <- (rr - 1) / rr
        
        death <- paf * pop_layer * baseline_rate
        var <- (pop_layer * baseline_rate)^2 * theta_sd^2
        
        annual_deaths <- annual_deaths + sum(values(death), na.rm = TRUE)
        annual_var <- annual_var + sum(values(var), na.rm = TRUE)
      }
    }
  }
  
  if(hr %% 500 == 0) gc()
}

total_time <- as.numeric(Sys.time() - start_time, units = "mins")

cat("\n=== Results ===\n")
cat("Time:", round(total_time, 1), "min\n")
cat("Deaths:", round(annual_deaths, 2), "\n")
cat("SE:", round(sqrt(annual_var), 2), "\n")
cat("95% CI: [", round(annual_deaths - 1.96*sqrt(annual_var), 2), ", ",
    round(annual_deaths + 1.96*sqrt(annual_var), 2), "]\n\n")

results <- data.frame(
  Metric = c("Deaths", "SE", "CI Lower", "CI Upper"),
  Value = c(
    round(annual_deaths, 2),
    round(sqrt(annual_var), 2),
    round(annual_deaths - 1.96 * sqrt(annual_var), 2),
    round(annual_deaths + 1.96 * sqrt(annual_var), 2)
  )
)

print(results)
write.csv(results, "C:/Users/mrealehatem/Documents/GitHub/ArmeniaPollutionAnalysis/yerevan_mortality.csv", row.names = FALSE)
