# ====================================================================
# GEMM - DEBUGGING VERSION (Find the problem)
# ====================================================================

library(terra)
library(readxl)
# ====================================================================
# Clip Population to Pollution Extent & Check Population Counts
# ====================================================================

library(terra)
library(readxl)

cat("Loading rasters...\n")
pop_raster_raw <- rast("C:/Users/mrealehatem/Documents/GitHub/ArmeniaPollutionAnalysis/WorldPopData/arm_age_data/pop.tif")
pm25_stack <- rast("C:/Users/mrealehatem/OneDrive/AUA/Air pollution/pm25_yerevan_stack.tif")

cat("Original Population Extent:\n")
cat("  Xmin:", ext(pop_raster_raw)$xmin, "\n")
cat("  Xmax:", ext(pop_raster_raw)$xmax, "\n")
cat("  Ymin:", ext(pop_raster_raw)$ymin, "\n")
cat("  Ymax:", ext(pop_raster_raw)$ymax, "\n\n")

cat("PM2.5 Pollution Extent:\n")
cat("  Xmin:", ext(pm25_stack)$xmin, "\n")
cat("  Xmax:", ext(pm25_stack)$xmax, "\n")
cat("  Ymin:", ext(pm25_stack)$ymin, "\n")
cat("  Ymax:", ext(pm25_stack)$ymax, "\n\n")

cat("Trimming Population Raster...\n")
pop_raster <- crop(pop_raster_raw, ext(pm25_stack))

cat("New Population Extent (trimmed to PM2.5):\n")
cat("  Xmin:", ext(pop_raster)$xmin, "\n")
cat("  Xmax:", ext(pop_raster)$xmax, "\n")
cat("  Ymin:", ext(pop_raster)$ymin, "\n")
cat("  Ymax:", ext(pop_raster)$ymax, "\n\n")

cat("Age Groups in Population Raster:\n")
cat("  Population layers:", nlyr(pop_raster), "\n")
cat("  Raster layer names:", names(pop_raster), "\n\n")

cat("Population Summary (in pollution area):\n")
pop_0_9 <- pop_raster[["0-9"]]
cat("  Population 0-9 sum:", sum(values(pop_0_9), na.rm=TRUE), "\n")
cat("  Population 0-9 min:", min(values(pop_0_9), na.rm=TRUE), "\n")
cat("  Population 0-9 max:", max(values(pop_0_9), na.rm=TRUE), "\n")
cat("  Population 0-9 NA values:", sum(is.na(values(pop_0_9))), "\n")
cat("  Extent of 0-9:", ext(pop_0_9), "\n\n")

cat("STEP 1: Load rasters\n")
pop_raster <- rast("C:/Users/mrealehatem/Documents/GitHub/ArmeniaPollutionAnalysis/WorldPopData/arm_age_data/pop.tif")
pm25_stack <- rast("C:/Users/mrealehatem/OneDrive/AUA/Air pollution/pm25_yerevan_stack.tif")

cat("  Population layers:", nlyr(pop_raster), "\n")
cat("  PM2.5 layers:", nlyr(pm25_stack), "\n")
cat("  Pop layer names:", names(pop_raster)[1:5], "\n\n")

cat("STEP 2: Load coefficients\n")
coeff_path <- "C:/Users/mrealehatem/Documents/GitHub/ArmeniaPollutionAnalysis/GEMMcoefficients.xlsx"
coeff_df <- read_excel(coeff_path)
colnames(coeff_df) <- c("Cause", "AgeGroup", "Theta", "Theta_SD", "Alpha", "Mu", "Nu")
coeff_df <- coeff_df[complete.cases(coeff_df), ]

cat("  Rows in coefficient table:", nrow(coeff_df), "\n")
cat("  Unique causes:", paste(unique(coeff_df$Cause), collapse=", "), "\n")
cat("  Unique age groups in coefficients:", paste(unique(coeff_df$AgeGroup), collapse=", "), "\n\n")

cat("STEP 3: Check age mapping\n")
age_mapping <- data.frame(
  raster_group = c("0-9",  "1-10",  "5-14",  "10-19", "15-24", "20-29", "25-34", "30-39", "35-44", "40-49", 
                   "45-54", "50-59", "55-64", "60-69", "65-74", "70-79", "75-84", "80-89", "85-94", "90-99"),
  coefficient_group = c("0-25",  "0-25",  "0-25", "25-30", "25-30", "30-35", "30-35", "35-40", "40-45", "45-50",
                        "50-55", "55-60", "60-65", "65-70", "70-75", "75-80", "80+", "80+", "80+", "80+")
)

cat("  First 5 raster groups:", paste(age_mapping$raster_group[1:5], collapse=", "), "\n")
cat("  Mapping to coefficient groups:", paste(age_mapping$coefficient_group[1:5], collapse=", "), "\n\n")

cat("STEP 4: Check population data\n")
pop_0_9 <- pop_raster[["0-9"]]
cat("  Population 0-9 sum:", sum(values(pop_0_9), na.rm=TRUE), "\n")
cat("  Population 0-9 min:", min(values(pop_0_9), na.rm=TRUE), "\n")
cat("  Population 0-9 max:", max(values(pop_0_9), na.rm=TRUE), "\n")
cat("  Population 0-9 NA values:", sum(is.na(values(pop_0_9))), "\n\n")

cat("STEP 5: Check PM2.5 data\n")
pm25_hour1 <- pm25_stack[[1]]
cat("  PM2.5 hour 1 sum:", sum(values(pm25_hour1), na.rm=TRUE), "\n")
cat("  PM2.5 hour 1 min:", min(values(pm25_hour1), na.rm=TRUE), "\n")
cat("  PM2.5 hour 1 max:", max(values(pm25_hour1), na.rm=TRUE), "\n")
cat("  PM2.5 hour 1 NA values:", sum(is.na(values(pm25_hour1))), "\n\n")

cat("STEP 6: Check coefficient lookup\n")
coeff_lookup <- list()
for (cause in unique(coeff_df$Cause)) {
  for (coef_age in unique(coeff_df$AgeGroup)) {
    row <- coeff_df[coeff_df$Cause == cause & coeff_df$AgeGroup == coef_age, ]
    if (nrow(row) > 0) {
      key <- paste(cause, coef_age, sep = "|")
      coeff_lookup[[key]] <- list(
        theta = as.numeric(row$Theta[1]),
        theta_sd = as.numeric(row$Theta_SD[1]),
        alpha = as.numeric(row$Alpha[1]),
        mu = as.numeric(row$Mu[1]),
        nu = as.numeric(row$Nu[1])
      )
    }
  }
}

cat("  Total coefficient combinations:", length(coeff_lookup), "\n")
cat("  Example key: NCD+LRI|0-25\n")
test_key <- "NCD+LRI|0-25"
if (!is.null(coeff_lookup[[test_key]])) {
  cat("  Found:", coeff_lookup[[test_key]]$theta, "\n")
} else {
  cat("  NOT FOUND!\n")
}
cat("\n")

cat("STEP 7: Manual calculation test\n")
calculate_gemm_rr <- function(pm25, theta, alpha, mu, nu, cf = 2.4) {
  z <- pm25 - cf
  z[z < 0] <- 0
  omega <- 1 / (1 + exp(-(z - mu) / nu))
  Tz <- log((z / alpha) + 1) * omega
  exp(theta * Tz)
}

# Test with first hour, first age group
ag <- "0-9"
coef_age <- "0-25"
key <- "NCD+LRI|0-25"

if (!is.null(coeff_lookup[[key]])) {
  coeff <- coeff_lookup[[key]]
  
  cat("  Using coefficient:", key, "\n")
  cat("  Theta:", coeff$theta, "\n")
  cat("  Alpha:", coeff$alpha, "\n")
  cat("  Mu:", coeff$mu, "\n")
  cat("  Nu:", coeff$nu, "\n\n")
  
  rr <- calculate_gemm_rr(pm25_hour1, coeff$theta, coeff$alpha, coeff$mu, coeff$nu)
  cat("  RR sum:", sum(values(rr), na.rm=TRUE), "\n")
  cat("  RR min:", min(values(rr), na.rm=TRUE), "\n")
  cat("  RR max:", max(values(rr), na.rm=TRUE), "\n\n")
  
  paf <- (rr - 1) / rr
  cat("  PAF sum:", sum(values(paf), na.rm=TRUE), "\n")
  cat("  PAF min:", min(values(paf), na.rm=TRUE), "\n")
  cat("  PAF max:", max(values(paf), na.rm=TRUE), "\n\n")
  
  baseline_rate <- 0.0005 / (365 * 24)
  cat("  Baseline rate:", baseline_rate, "\n\n")
  
  death <- paf * pop_0_9 * baseline_rate
  cat("  Death sum:", sum(values(death), na.rm=TRUE), "\n")
  cat("  Death min:", min(values(death), na.rm=TRUE), "\n")
  cat("  Death max:", max(values(death), na.rm=TRUE), "\n")
} else {
  cat("  ERROR: Coefficient key not found!\n")
}
