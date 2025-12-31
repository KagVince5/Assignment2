# ============================================================================
# QUESTION 1(b): Population Parameter Identification
# ============================================================================
# Assignment by: Vincent Teoh Chia Win (B032410980)
# Data Source: Department of Statistics Malaysia (DOSM)
# ============================================================================

# Load required libraries
library(ggplot2)

# Load data
data <- read.csv("C:/Users/Vincent/Desktop/Assignment2/Data/electricity_consumption.csv")

# Filter for domestic (local_domestic) electricity consumption in 2023
data$date <- as.Date(data$date)
data$year <- format(data$date, "%Y")

# Extract 2023 domestic consumption data
domestic_2023 <- data[data$sector == "local_domestic" & data$year == "2023", ]
consumption_values <- domestic_2023$consumption

# ============================================================================
# QUESTION 1(b): POPULATION PARAMETER IDENTIFICATION
# ============================================================================

cat("\n")
cat("=" , rep("=", 70), "\n", sep="")
cat("QUESTION 1(b): POPULATION PARAMETER IDENTIFICATION\n")
cat("=" , rep("=", 70), "\n", sep="")

# ============================================================================
# ANSWER: THE POPULATION PARAMETER BEING ESTIMATED
# ============================================================================

cat("\n")
cat("-" , rep("-", 70), "\n", sep="")
cat("THE POPULATION PARAMETER BEING ESTIMATED:\n")
cat("-" , rep("-", 70), "\n", sep="")

cat("\n")
cat("The population parameter being estimated is:\n")
cat("\n")
cat("   μ (mu) = THE POPULATION MEAN of monthly domestic electricity\n")
cat("            consumption in Malaysia\n")
cat("\n")

# ============================================================================
# EXPLANATION: POPULATION MEAN VS POPULATION PROPORTION
# ============================================================================

cat("-" , rep("-", 70), "\n", sep="")
cat("EXPLANATION: WHY IT IS A POPULATION MEAN (NOT A PROPORTION)\n")
cat("-" , rep("-", 70), "\n", sep="")

cat("\n")
cat("This represents a POPULATION MEAN (μ), NOT a population proportion (p).\n")
cat("\n")
cat("REASONS:\n")
cat("\n")
cat("1. NATURE OF THE VARIABLE:\n")
cat("   - The variable 'monthly domestic electricity consumption' is a\n")
cat("     QUANTITATIVE CONTINUOUS variable measured in GWh (Gigawatt-hours)\n")
cat("   - It represents measurable quantities, not categories or counts\n")
cat("\n")
cat("2. WHAT WE ARE ESTIMATING:\n")
cat("   - We want to estimate the TRUE AVERAGE (mean) monthly domestic\n")
cat("     electricity consumption across ALL time periods in Malaysia\n")
cat("   - The sample mean (X̄) is used to estimate the population mean (μ)\n")
cat("\n")
cat("3. POPULATION MEAN vs POPULATION PROPORTION:\n")
cat("\n")
cat("   POPULATION MEAN (μ):                 POPULATION PROPORTION (p):\n")
cat("   ----------------------               --------------------------\n")
cat("   - Used for quantitative data         - Used for categorical data\n")
cat("   - Measures central tendency          - Measures fraction/percentage\n")
cat("   - Variable can take any numeric      - Variable has only two outcomes\n")
cat("     value within a range                 (success/failure, yes/no)\n")
cat("   - Example: average consumption       - Example: proportion of months\n")
cat("                                           with consumption > 3500 GWh\n")
cat("\n")
cat("4. IN THIS CONTEXT:\n")
cat("   - We are measuring AVERAGE monthly consumption (a continuous quantity)\n")
cat("   - We are NOT counting how many months fall into a specific category\n")
cat("   - Therefore, the parameter is μ (population mean)\n")
cat("\n")

# ============================================================================
# MATHEMATICAL REPRESENTATION
# ============================================================================

cat("-" , rep("-", 70), "\n", sep="")
cat("MATHEMATICAL REPRESENTATION:\n")
cat("-" , rep("-", 70), "\n", sep="")

cat("\n")
cat("Population Parameter:    μ = true mean monthly domestic electricity\n")
cat("                             consumption in Malaysia\n")
cat("\n")
cat("Point Estimator:         X̄ = sample mean (calculated from sample data)\n")
cat("\n")
cat("Estimation Formula:      X̄ = (ΣXᵢ) / n\n")
cat("\n")

# Calculate and display the sample mean as point estimate
sample_mean <- mean(consumption_values)
sample_n <- length(consumption_values)
sample_sum <- sum(consumption_values)

cat("-" , rep("-", 70), "\n", sep="")
cat("SAMPLE CALCULATION (Point Estimate of μ):\n")
cat("-" , rep("-", 70), "\n", sep="")

cat("\n")
cat(sprintf("Sum of observations (ΣXᵢ):     %.4f GWh\n", sample_sum))
cat(sprintf("Sample size (n):                %d observations\n", sample_n))
cat(sprintf("Sample Mean (X̄):               %.4f GWh\n", sample_mean))
cat("\n")
cat("Therefore:\n")
cat(sprintf("   X̄ = %.4f / %d = %.4f GWh\n", sample_sum, sample_n, sample_mean))
cat("\n")
cat(sprintf("The sample mean X̄ = %.4f GWh is our POINT ESTIMATE of the\n", sample_mean))
cat("population mean μ (the true average monthly domestic electricity\n")
cat("consumption in Malaysia).\n")
cat("\n")

# ============================================================================
# SUMMARY BOX
# ============================================================================

cat("=" , rep("=", 70), "\n", sep="")
cat("SUMMARY OF QUESTION 1(b):\n")
cat("=" , rep("=", 70), "\n", sep="")

cat("\n")
cat("╔══════════════════════════════════════════════════════════════════════╗\n")
cat("║  POPULATION PARAMETER:  μ (mu) - Population Mean                     ║\n")
cat("║                                                                      ║\n")
cat("║  DEFINITION:  The true average monthly domestic electricity         ║\n")
cat("║               consumption in Malaysia (in GWh)                       ║\n")
cat("║                                                                      ║\n")
cat("║  TYPE:        POPULATION MEAN (not a proportion)                     ║\n")
cat("║                                                                      ║\n")
cat("║  REASON:      The variable is quantitative and continuous,          ║\n")
cat("║               measuring electricity consumption in GWh,             ║\n")
cat("║               not a categorical/binary outcome                       ║\n")
cat("║                                                                      ║\n")
cat(sprintf("║  POINT ESTIMATE: X̄ = %.4f GWh                               ║\n", sample_mean))
cat("╚══════════════════════════════════════════════════════════════════════╝\n")
cat("\n")

cat("=" , rep("=", 70), "\n", sep="")
cat("END OF QUESTION 1(b) ANALYSIS\n")
cat("=" , rep("=", 70), "\n", sep="")
