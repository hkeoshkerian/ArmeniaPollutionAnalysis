# ====================================================================
# GEMM (Global Exposure Mortality Model) Implementation in R
# for Calculating Annual PM2.5-Attributable Mortality
# ====================================================================

# Load required libraries
library(dplyr)
library(tidyr)

# ====================================================================
# 1. DEFINE GEMM HAZARD RATIO FUNCTION
# ====================================================================

# The GEMM hazard ratio function based on Burnett et al. (2018)
# Formula: RR(z) = exp{ theta * log((z/alpha) + 1) * omega(z) }
# where z = max(0, PM2.5 - counterfactual)
# and omega(z) = 1 / (1 + exp(-(z - mu) / nu))

calculate_gemm_rr <- function(pm25_concentration, 
                              theta, 
                              alpha, 
                              mu, 
                              nu, 
                              counterfactual = 2.4) {
  
  # Calculate z (exposure above counterfactual)
  z <- pmax(0, pm25_concentration - counterfactual)
  
  # Calculate omega (logistic weighting function)
  omega <- 1 / (1 + exp(-(z - mu) / nu))
  
  # Calculate transformation T(z)
  T_z <- log((z / alpha) + 1) * omega
  
  # Calculate relative risk (hazard ratio)
  RR <- exp(theta * T_z)
  
  return(RR)
}

# ====================================================================
# 2. GEMM PARAMETERS BY CAUSE OF DEATH
# ====================================================================

# Note: These parameter values should come from the original GEMM paper
# supplementary materials (Burnett et al., 2018, PNAS)
# Values below are PLACEHOLDERS - you need to obtain actual values

gemm_parameters <- list(
  # Non-accidental mortality (NCD+LRI)
  NCD_LRI = list(
    theta = 0.15,    # PLACEHOLDER - get from paper
    alpha = 10.0,    # PLACEHOLDER - get from paper
    mu = 50.0,       # PLACEHOLDER - get from paper
    nu = 20.0        # PLACEHOLDER - get from paper
  ),
  
  # Ischemic Heart Disease (IHD)
  IHD = list(
    theta = 0.18,    # PLACEHOLDER
    alpha = 12.0,    # PLACEHOLDER
    mu = 45.0,       # PLACEHOLDER
    nu = 18.0        # PLACEHOLDER
  ),
  
  # Stroke
  Stroke = list(
    theta = 0.14,    # PLACEHOLDER
    alpha = 11.0,    # PLACEHOLDER
    mu = 48.0,       # PLACEHOLDER
    nu = 19.0        # PLACEHOLDER
  ),
  
  # COPD
  COPD = list(
    theta = 0.16,    # PLACEHOLDER
    alpha = 9.5,     # PLACEHOLDER
    mu = 52.0,       # PLACEHOLDER
    nu = 21.0        # PLACEHOLDER
  ),
  
  # Lung Cancer
  Lung_Cancer = list(
    theta = 0.13,    # PLACEHOLDER
    alpha = 10.5,    # PLACEHOLDER
    mu = 46.0,       # PLACEHOLDER
    nu = 17.0        # PLACEHOLDER
  ),
  
  # Lower Respiratory Infections (LRI)
  LRI = list(
    theta = 0.19,    # PLACEHOLDER
    alpha = 8.0,     # PLACEHOLDER
    mu = 40.0,       # PLACEHOLDER
    nu = 15.0        # PLACEHOLDER
  )
)

# ====================================================================
# 3. CALCULATE POPULATION ATTRIBUTABLE FRACTION (PAF)
# ====================================================================

calculate_paf <- function(relative_risk) {
  # PAF = (RR - 1) / RR = 1 - (1/RR)
  paf <- (relative_risk - 1) / relative_risk
  return(paf)
}

# ====================================================================
# 4. CALCULATE ATTRIBUTABLE DEATHS
# ====================================================================

calculate_attributable_deaths <- function(pm25_data, 
                                          population_data,
                                          baseline_mortality_rates,
                                          cause = "NCD_LRI") {
  
  # Get GEMM parameters for the specified cause
  params <- gemm_parameters[[cause]]
  
  # Merge all data
  merged_data <- pm25_data %>%
    left_join(population_data, by = c("region", "age_group")) %>%
    left_join(baseline_mortality_rates, by = c("region", "age_group", "cause"))
  
  # Calculate relative risk for each row
  merged_data <- merged_data %>%
    mutate(
      relative_risk = calculate_gemm_rr(
        pm25_concentration = pm25_annual_mean,
        theta = params$theta,
        alpha = params$alpha,
        mu = params$mu,
        nu = params$nu,
        counterfactual = 2.4
      ),
      # Calculate PAF
      paf = calculate_paf(relative_risk),
      # Calculate attributable deaths
      # Deaths = Population * Baseline_Mortality_Rate * PAF
      attributable_deaths = population * baseline_mortality_rate * paf
    )
  
  return(merged_data)
}

# ====================================================================
# 5. MAIN FUNCTION TO CALCULATE ANNUAL MORTALITY BURDEN
# ====================================================================

calculate_annual_mortality_burden <- function(pm25_data,
                                              population_data,
                                              mortality_data,
                                              causes = c("NCD_LRI")) {
  
  results_list <- list()
  
  for (cause in causes) {
    cat("Calculating mortality for:", cause, "\n")
    
    # Calculate attributable deaths for this cause
    result <- calculate_attributable_deaths(
      pm25_data = pm25_data,
      population_data = population_data,
      baseline_mortality_rates = mortality_data,
      cause = cause
    )
    
    results_list[[cause]] <- result
  }
  
  # Combine results
  all_results <- bind_rows(results_list, .id = "cause_of_death")
  
  # Summarize by region
  summary_by_region <- all_results %>%
    group_by(region, cause_of_death) %>%
    summarize(
      total_population = sum(population),
      mean_pm25 = weighted.mean(pm25_annual_mean, population),
      total_attributable_deaths = sum(attributable_deaths),
      .groups = "drop"
    )
  
  return(list(
    detailed_results = all_results,
    summary = summary_by_region
  ))
}

# ====================================================================
# 6. EXAMPLE DATA STRUCTURE AND USAGE
# ====================================================================

# Example: PM2.5 data structure
# Each row represents a geographic region/grid cell and time period
pm25_data_example <- data.frame(
  region = c("Region_A", "Region_A", "Region_B", "Region_B"),
  age_group = c("25-64", "65+", "25-64", "65+"),
  pm25_annual_mean = c(15.5, 15.5, 35.2, 35.2)  # μg/m³
)

# Example: Population data structure (by region and age group)
population_data_example <- data.frame(
  region = c("Region_A", "Region_A", "Region_B", "Region_B"),
  age_group = c("25-64", "65+", "25-64", "65+"),
  population = c(500000, 100000, 800000, 150000)
)

# Example: Baseline mortality rates (deaths per person per year)
# These should come from GBD or national health statistics
mortality_data_example <- data.frame(
  region = c(rep("Region_A", 4), rep("Region_B", 4)),
  age_group = rep(c("25-64", "25-64", "65+", "65+"), 2),
  cause = rep(c("NCD_LRI", "IHD"), 4),
  baseline_mortality_rate = c(
    0.008, 0.003, 0.045, 0.015,  # Region A
    0.009, 0.0035, 0.050, 0.018  # Region B
  )
)

# ====================================================================
# 7. RUN THE ANALYSIS
# ====================================================================

# Calculate mortality burden for multiple causes
results <- calculate_annual_mortality_burden(
  pm25_data = pm25_data_example,
  population_data = population_data_example,
  mortality_data = mortality_data_example,
  causes = c("NCD_LRI", "IHD")
)

# View summary results
print(results$summary)

# Export results
write.csv(results$summary, "gemm_mortality_summary.csv", row.names = FALSE)
write.csv(results$detailed_results, "gemm_mortality_detailed.csv", row.names = FALSE)

# ====================================================================
# 8. SENSITIVITY ANALYSIS WITH CONFIDENCE INTERVALS
# ====================================================================

# For uncertainty analysis, you would typically:
# 1. Bootstrap the GEMM parameters (theta, alpha, mu, nu)
# 2. Run Monte Carlo simulations
# 3. Calculate 95% confidence intervals

calculate_mortality_with_uncertainty <- function(pm25_data,
                                                 population_data,
                                                 mortality_data,
                                                 n_simulations = 1000) {
  
  mortality_estimates <- matrix(NA, nrow = n_simulations, ncol = 1)
  
  for (i in 1:n_simulations) {
    # Sample parameters from uncertainty distributions
    # (distributions should come from GEMM paper supplementary materials)
    theta_sample <- rnorm(1, mean = 0.15, sd = 0.02)  # EXAMPLE
    alpha_sample <- rnorm(1, mean = 10.0, sd = 1.5)   # EXAMPLE
    mu_sample <- rnorm(1, mean = 50.0, sd = 5.0)      # EXAMPLE
    nu_sample <- rnorm(1, mean = 20.0, sd = 3.0)      # EXAMPLE
    
    # Calculate mortality with sampled parameters
    # (simplified - you would update the parameters in the function)
    result <- calculate_attributable_deaths(
      pm25_data, population_data, mortality_data, "NCD_LRI"
    )
    
    mortality_estimates[i] <- sum(result$attributable_deaths)
  }
  
  # Calculate summary statistics
  return(data.frame(
    mean_deaths = mean(mortality_estimates),
    median_deaths = median(mortality_estimates),
    lower_95ci = quantile(mortality_estimates, 0.025),
    upper_95ci = quantile(mortality_estimates, 0.975)
  ))
}

# ====================================================================
# 9. VISUALIZATION
# ====================================================================

library(ggplot2)

# Plot concentration-response curve
plot_gemm_curve <- function(params, max_pm25 = 100) {
  pm25_seq <- seq(0, max_pm25, by = 0.5)
  
  rr_values <- calculate_gemm_rr(
    pm25_concentration = pm25_seq,
    theta = params$theta,
    alpha = params$alpha,
    mu = params$mu,
    nu = params$nu,
    counterfactual = 2.4
  )
  
  plot_data <- data.frame(
    PM25 = pm25_seq,
    Relative_Risk = rr_values
  )
  
  ggplot(plot_data, aes(x = PM25, y = Relative_Risk)) +
    geom_line(color = "blue", size = 1.2) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
    labs(
      title = "GEMM Concentration-Response Function",
      x = expression(paste("PM"[2.5], " Concentration (μg/m"^3, ")")),
      y = "Relative Risk (Hazard Ratio)"
    ) +
    theme_minimal()
}

# Example usage
plot_gemm_curve(gemm_parameters$NCD_LRI)
