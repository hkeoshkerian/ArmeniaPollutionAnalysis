# ====================================================================
# GEMM Implementation - DIAGNOSTIC VERSION
# Step-by-step with visualizations and data checks
# ====================================================================

library(terra)
library(readxl)
library(ggplot2)
library(dplyr)

cat("\n========== LOADING & CHECKING DATA ==========\n")

pop_raster_raw <- rast("C:/Users/mrealehatem/Documents/GitHub/ArmeniaPollutionAnalysis/WorldPopData/arm_age_data/pop.tif")
pm25_stack <- rast("C:/Users/mrealehatem/OneDrive/AUA/Air pollution/pm25_yerevan_stack.tif")

cat("Pop raster info:\n")
print(pop_raster_raw)
cat("\nPM2.5 stack info:\n")
print(pm25_stack)

# Check CRS match
if(crs(pop_raster_raw) != crs(pm25_stack)) {
  pop_raster_raw <- project(pop_raster_raw, crs(pm25_stack))
  cat("\n✓ Reprojected population raster\n")
}

pop_raster <- crop(pop_raster_raw, ext(pm25_stack))
cat("\nCropped pop raster info:\n")
print(pop_raster)

cat("\n========== STEP 1: ANNUAL AVERAGE PM2.5 ==========\n")

pm25_resampled <- resample(pm25_stack, pop_raster, method = "bilinear")
pm25_annual_avg <- mean(pm25_resampled, na.rm = TRUE)

# Check PM2.5 values
pm25_vals <- values(pm25_annual_avg)
cat("PM2.5 Summary Statistics:\n")
cat("  Min:", min(pm25_vals, na.rm = TRUE), "\n")
cat("  Max:", max(pm25_vals, na.rm = TRUE), "\n")
cat("  Mean:", mean(pm25_vals, na.rm = TRUE), "\n")
cat("  NA cells:", sum(is.na(pm25_vals)), "/", length(pm25_vals), "\n")
cat("  % NA:", round((sum(is.na(pm25_vals)) / length(pm25_vals)) * 100, 2), "%\n")

# Visualize PM2.5 distribution
png("C:/Users/mrealehatem/Documents/GitHub/ArmeniaPollutionAnalysis/diag_01_pm25_dist.png", width=800, height=600)
hist(pm25_vals[!is.na(pm25_vals)], breaks=50, main="Distribution of Annual Average PM2.5", 
     xlab="PM2.5 (µg/m³)", col="steelblue", border="white")
dev.off()
cat("✓ Saved PM2.5 distribution plot\n")

cat("\n========== STEP 2: POPULATION DATA ==========\n")

coeff_df <- read_excel("C:/Users/mrealehatem/Documents/GitHub/ArmeniaPollutionAnalysis/GEMMcoefficients.xlsx")
colnames(coeff_df) <- c("Cause", "AgeGroup", "Theta", "Theta_SD", "Alpha", "Mu", "Nu")
coeff_df <- coeff_df[complete.cases(coeff_df), ]
coeff_df <- coeff_df[coeff_df$Cause == "NCD+LRI", ]

cat("Coefficients available for age groups:", paste(unique(coeff_df$AgeGroup), collapse=", "), "\n")
cat("Number of layers in pop_raster:", nlyr(pop_raster), "\n")
cat("Layer names:", paste(names(pop_raster), collapse=", "), "\n")

# Age mapping
age_map <- c("0-9"="0-25", "1-10"="0-25", "5-14"="0-25", "10-19"="0-25", "15-24"="0-25",
             "20-29"="25-30", "25-34"="30-35", "30-39"="35-40", "35-44"="40-45", "40-49"="45-50",
             "45-54"="50-55", "50-59"="55-60", "55-64"="60-65", "60-69"="65-70", "65-74"="70-75",
             "70-79"="75-80", "75-84"="80+", "80-89"="80+", "85-94"="80+", "90-99"="80+")

# Check each age layer
cat("\nPopulation summary by age group:\n")
total_pop <- 0
for(ag in names(pop_raster)) {
  pop_layer <- pop_raster[[ag]]
  pop_vals <- values(pop_layer)
  layer_sum <- sum(pop_vals, na.rm = TRUE)
  layer_na <- sum(is.na(pop_vals))
  total_pop <- total_pop + layer_sum
  
  cat(sprintf("  %s: Pop = %s | NA cells = %d\n", 
              ag, formatC(layer_sum, format="f", big.mark=","), layer_na))
}
cat("\nTOTAL POPULATION:", formatC(total_pop, format="f", big.mark=","), "\n")

cat("\n========== STEP 3: BASELINE MORTALITY RATE ==========\n")

baseline_rate <- 7.44 / 100000
cat("Baseline mortality rate:", baseline_rate, "deaths/person/year\n")
cat("Expected annual deaths (baseline only):\n")
cat("  For 1M people:", formatC(baseline_rate * 1000000, format="f", big.mark=","), "\n")
cat("  For total population (", formatC(total_pop, format="f", big.mark=","), "):", 
    formatC(baseline_rate * total_pop, format="f", digits=2), "\n")

cat("\n========== STEP 4: COEFFICIENT MATRIX & GEMM FUNCTION ==========\n")

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

cat("Coefficients loaded. Sample values:\n")
print(coef_matrix[1, , ])

# GEMM function
calculate_gemm_rr <- function(pm25, theta, alpha, mu, nu, cf = 2.4) {
  z <- pm25 - cf
  z[z < 0] <- 0
  omega <- 1 / (1 + exp(-(z - mu) / nu))
  exp(theta * log((z / alpha) + 1) * omega)
}

# Test GEMM with typical values
test_pm25 <- c(5, 15, 25, 35, 45)
test_theta <- coeff_df$Theta[1]
test_alpha <- coeff_df$Alpha[1]
test_mu <- coeff_df$Mu[1]
test_nu <- coeff_df$Nu[1]

cat("\nGEMM Function Test (sample coefficient from", unique(coeff_df$AgeGroup)[1], "):\n")
cat("Theta =", test_theta, "| Alpha =", test_alpha, "| Mu =", test_mu, "| Nu =", test_nu, "\n\n")

test_rr <- calculate_gemm_rr(test_pm25, test_theta, test_alpha, test_mu, test_nu)
test_paf <- (test_rr - 1) / test_rr

test_df <- data.frame(
  PM25 = test_pm25,
  RR = round(test_rr, 4),
  PAF = round(test_paf, 4),
  PAF_Pct = round(test_paf * 100, 2)
)
print(test_df)
cat("⚠ KEY CHECK: Are PAF values reasonable (typically 0.5-15% for PM2.5)?\n")

cat("\n========== STEP 5: CALCULATION WITH DIAGNOSTICS ==========\n")

pm25_by_cell <- values(pm25_annual_avg)
n_cells <- ncell(pop_raster)
deaths_by_cell <- rep(0, n_cells)
var_by_cell <- rep(0, n_cells)

diag_list <- list()
diag_idx <- 1

for(ag in names(pop_raster)) {
  pop_layer <- pop_raster[[ag]]
  pop_values <- values(pop_layer)
  coef_age <- age_map[ag]
  coef_age_idx <- which(coef_ages == coef_age)
  
  if(length(coef_age_idx) > 0 && !is.na(coef_matrix[1, coef_age_idx, 1])) {
    theta <- coef_matrix[1, coef_age_idx, 1]
    theta_sd <- coef_matrix[1, coef_age_idx, 2]
    alpha <- coef_matrix[1, coef_age_idx, 3]
    mu <- coef_matrix[1, coef_age_idx, 4]
    nu <- coef_matrix[1, coef_age_idx, 5]
    
    # Calculate RR and PAF
    rr_values <- calculate_gemm_rr(pm25_by_cell, theta, alpha, mu, nu)
    paf_values <- (rr_values - 1) / rr_values
    paf_values[is.na(paf_values)] <- 0
    
    # Calculate deaths
    death_values <- paf_values * pop_values * baseline_rate
    var_values <- (pop_values * baseline_rate)^2 * theta_sd^2
    
    deaths_by_cell <- deaths_by_cell + death_values
    var_by_cell <- var_by_cell + var_values
    
    # Store diagnostics
    valid_cells <- !is.na(death_values) & !is.na(pop_values) & pop_values > 0
    
    diag_list[[diag_idx]] <- data.frame(
      AgeGroup = ag,
      CoefAge = coef_age,
      Theta = theta,
      PAF_Mean = round(mean(paf_values[valid_cells], na.rm=TRUE), 6),
      PAF_Max = round(max(paf_values[valid_cells], na.rm=TRUE), 6),
      Deaths_Total = round(sum(death_values, na.rm=TRUE), 4),
      Deaths_Mean = round(mean(death_values[valid_cells], na.rm=TRUE), 6),
      Deaths_Max = round(max(death_values[valid_cells], na.rm=TRUE), 6),
      Pop_Total = round(sum(pop_values, na.rm=TRUE), 0),
      N_Valid_Cells = sum(valid_cells)
    )
    diag_idx <- diag_idx + 1
    
    cat(sprintf("✓ %s: Deaths = %.2f | Population = %s | Mean PAF = %.4f%%\n", 
                ag, sum(death_values, na.rm=TRUE), 
                formatC(sum(pop_values, na.rm=TRUE), format="f", big.mark=","),
                mean(paf_values[valid_cells], na.rm=TRUE) * 100))
  } else {
    cat("✗ WARNING:", ag, "→ no coefficient for age group", coef_age, "\n")
  }
}

diag_df <- do.call(rbind, diag_list)
rownames(diag_df) <- NULL

cat("\n\nDETAILED DIAGNOSTICS BY AGE GROUP:\n")
print(diag_df)
write.csv(diag_df, "C:/Users/mrealehatem/Documents/GitHub/ArmeniaPollutionAnalysis/diag_by_age_group.csv", row.names = FALSE)

cat("\n========== FINAL RESULTS & VALIDATION ==========\n")

total_deaths <- sum(deaths_by_cell, na.rm = TRUE)
total_se <- sqrt(sum(var_by_cell, na.rm = TRUE))
ci_lower <- total_deaths - 1.96 * total_se
ci_upper <- total_deaths + 1.96 * total_se
expected_baseline <- baseline_rate * total_pop

cat("Total Annual Excess Deaths:", round(total_deaths, 2), "\n")
cat("Standard Error:", round(total_se, 2), "\n")
cat("95% CI: [", round(ci_lower, 2), ", ", round(ci_upper, 2), "]\n")
cat("Expected baseline deaths (all causes):", round(expected_baseline, 2), "\n")
cat("Excess as % of baseline:", round((total_deaths / expected_baseline) * 100, 2), "%\n")

# Spatial analysis
deaths_raster <- pm25_annual_avg
values(deaths_raster) <- deaths_by_cell

deaths_df <- as.data.frame(deaths_raster, xy = TRUE)
colnames(deaths_df) <- c("x", "y", "Deaths")
deaths_df$PM25 <- pm25_by_cell
deaths_df_clean <- deaths_df[!is.na(deaths_df$Deaths) & !is.na(deaths_df$PM25), ]

cat("\nSpatial distribution of deaths:\n")
cat("  Cells with deaths > 0:", sum(deaths_df_clean$Deaths > 0, na.rm=TRUE), "\n")
cat("  Cells with deaths = 0:", sum(deaths_df_clean$Deaths == 0, na.rm=TRUE), "\n")
cat("  Mean deaths per cell:", round(mean(deaths_df_clean$Deaths, na.rm=TRUE), 6), "\n")
cat("  Median deaths per cell:", round(median(deaths_df_clean$Deaths, na.rm=TRUE), 6), "\n")
cat("  Max deaths per cell:", round(max(deaths_df_clean$Deaths, na.rm=TRUE), 6), "\n")

# Visualizations
png("C:/Users/mrealehatem/Documents/GitHub/ArmeniaPollutionAnalysis/diag_02_deaths_distribution.png", width=900, height=600)
par(mfrow=c(1, 2))
hist(deaths_df_clean$Deaths[deaths_df_clean$Deaths > 0], breaks=50, main="Deaths per Cell (Deaths > 0)", 
     xlab="Annual Excess Deaths", col="darkred", border="white")
plot(deaths_df_clean$PM25, deaths_df_clean$Deaths, main="Deaths vs PM2.5", 
     xlab="PM2.5 (µg/m³)", ylab="Deaths per Cell", col="steelblue", alpha=0.5)
par(mfrow=c(1, 1))
dev.off()

cat("\n✓ Diagnostics complete. Check outputs folder for diagnostic plots.\n")
