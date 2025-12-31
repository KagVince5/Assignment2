# ============================================================================
# QUESTION 1(c): Sample Mean Calculation and Point Estimate Explanation
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
n <- length(consumption_values)

# ============================================================================
# QUESTION 1(c): SAMPLE MEAN CALCULATION
# ============================================================================

cat("\n")
cat("=" , rep("=", 70), "\n", sep="")
cat("QUESTION 1(c): SAMPLE MEAN AND POINT ESTIMATE\n")
cat("=" , rep("=", 70), "\n", sep="")

# ============================================================================
# STEP 1: DISPLAY THE RAW DATA
# ============================================================================

cat("\n")
cat("-" , rep("-", 70), "\n", sep="")
cat("STEP 1: RAW DATA (2023 Monthly Domestic Electricity Consumption)\n")
cat("-" , rep("-", 70), "\n", sep="")

months <- c("January", "February", "March", "April", "May", "June",
            "July", "August", "September", "October", "November", "December")

cat("\n")
cat("Month          | Consumption (Xᵢ) in GWh\n")
cat("---------------|------------------------\n")
for (i in 1:n) {
  cat(sprintf("%-14s | %.6f\n", months[i], consumption_values[i]))
}
cat("---------------|------------------------\n")

# ============================================================================
# STEP 2: CALCULATION OF SAMPLE MEAN
# ============================================================================

cat("\n")
cat("-" , rep("-", 70), "\n", sep="")
cat("STEP 2: SAMPLE MEAN CALCULATION\n")
cat("-" , rep("-", 70), "\n", sep="")

# Calculate sum of all values
sum_x <- sum(consumption_values)

cat("\n")
cat("Formula for Sample Mean:\n")
cat("\n")
cat("        ΣXᵢ      X₁ + X₂ + X₃ + ... + Xₙ\n")
cat("  X̄ = ───── = ─────────────────────────────\n")
cat("         n                n\n")
cat("\n")

cat("Where:\n")
cat("  X̄  = Sample mean\n")
cat("  Xᵢ = Individual observations (i = 1, 2, 3, ..., n)\n")
cat("  n  = Sample size (number of observations)\n")
cat("  Σ  = Summation symbol (sum of all values)\n")
cat("\n")

# Step-by-step calculation
cat("-" , rep("-", 70), "\n", sep="")
cat("STEP-BY-STEP CALCULATION:\n")
cat("-" , rep("-", 70), "\n", sep="")

cat("\n")
cat("Step 2.1: Sum all monthly consumption values (ΣXᵢ)\n")
cat("\n")
cat("  ΣXᵢ = X₁ + X₂ + X₃ + X₄ + X₅ + X₆ + X₇ + X₈ + X₉ + X₁₀ + X₁₁ + X₁₂\n")
cat("\n")
cat(sprintf("  ΣXᵢ = %.6f + %.6f + %.6f + %.6f +\n", 
            consumption_values[1], consumption_values[2], 
            consumption_values[3], consumption_values[4]))
cat(sprintf("        %.6f + %.6f + %.6f + %.6f +\n", 
            consumption_values[5], consumption_values[6], 
            consumption_values[7], consumption_values[8]))
cat(sprintf("        %.6f + %.6f + %.6f + %.6f\n", 
            consumption_values[9], consumption_values[10], 
            consumption_values[11], consumption_values[12]))
cat("\n")
cat(sprintf("  ΣXᵢ = %.6f GWh\n", sum_x))
cat("\n")

cat("Step 2.2: Identify the sample size (n)\n")
cat("\n")
cat(sprintf("  n = %d (12 monthly observations for year 2023)\n", n))
cat("\n")

cat("Step 2.3: Calculate the sample mean (X̄)\n")
cat("\n")

# Calculate sample mean
sample_mean <- sum_x / n

cat(sprintf("        ΣXᵢ     %.6f\n", sum_x))
cat(sprintf("  X̄ = ───── = ──────────────\n"))
cat(sprintf("         n          %d\n", n))
cat("\n")
cat(sprintf("  X̄ = %.6f GWh\n", sample_mean))
cat("\n")

# Verify with R's mean function
r_mean <- mean(consumption_values)
cat(sprintf("Verification using R's mean() function: %.6f GWh\n", r_mean))
cat("\n")

# ============================================================================
# STEP 3: MEANING AS A POINT ESTIMATE
# ============================================================================

cat("-" , rep("-", 70), "\n", sep="")
cat("STEP 3: MEANING OF SAMPLE MEAN AS A POINT ESTIMATE\n")
cat("-" , rep("-", 70), "\n", sep="")

cat("\n")
cat("DEFINITION OF POINT ESTIMATE:\n")
cat("─────────────────────────────\n")
cat("A point estimate is a SINGLE VALUE calculated from sample data that\n")
cat("serves as our 'best guess' or approximation of an unknown population\n")
cat("parameter.\n")
cat("\n")

cat("THE SAMPLE MEAN AS A POINT ESTIMATE:\n")
cat("────────────────────────────────────\n")
cat("\n")
cat(sprintf("  Sample Mean (X̄) = %.4f GWh\n", sample_mean))
cat("\n")
cat("This value is our POINT ESTIMATE for the population mean (μ).\n")
cat("\n")

cat("INTERPRETATION:\n")
cat("───────────────\n")
cat("\n")
cat(sprintf("The sample mean of %.4f GWh means:\n", sample_mean))
cat("\n")
cat("1. WHAT IT REPRESENTS:\n")
cat(sprintf("   On average, Malaysian households consumed approximately %.2f GWh\n", sample_mean))
cat("   of electricity per month during 2023.\n")
cat("\n")
cat("2. AS A POINT ESTIMATE:\n")
cat(sprintf("   We use X̄ = %.4f GWh as our single best estimate of the\n", sample_mean))
cat("   true population mean (μ), which represents the average monthly\n")
cat("   domestic electricity consumption across ALL time periods.\n")
cat("\n")
cat("3. WHY IT'S CALLED A 'POINT' ESTIMATE:\n")
cat("   - It provides a SINGLE numeric value (a 'point' on the number line)\n")
cat("   - Unlike interval estimates which give a range of values\n")
cat(sprintf("   - This single point (%.4f GWh) is our best approximation of μ\n", sample_mean))
cat("\n")

cat("PROPERTIES OF THE SAMPLE MEAN AS AN ESTIMATOR:\n")
cat("──────────────────────────────────────────────\n")
cat("\n")
cat("The sample mean (X̄) is considered a GOOD estimator because it is:\n")
cat("\n")
cat("1. UNBIASED:\n")
cat("   - E(X̄) = μ (The expected value of X̄ equals the population mean)\n")
cat("   - On average, X̄ neither overestimates nor underestimates μ\n")
cat("\n")
cat("2. CONSISTENT:\n")
cat("   - As sample size increases, X̄ converges to μ\n")
cat("   - Larger samples give more accurate estimates\n")
cat("\n")
cat("3. EFFICIENT:\n")
cat("   - X̄ has the smallest variance among all unbiased estimators\n")
cat("   - It uses sample information optimally\n")
cat("\n")

# ============================================================================
# STEP 4: VISUAL REPRESENTATION
# ============================================================================

cat("-" , rep("-", 70), "\n", sep="")
cat("STEP 4: GRAPHICAL ILLUSTRATION\n")
cat("-" , rep("-", 70), "\n", sep="")

# Create visualization
plot_data <- data.frame(
  Month = factor(months, levels = months),
  Consumption = consumption_values
)

# Create plot showing data points and sample mean
point_estimate_plot <- ggplot(plot_data, aes(x = Month, y = Consumption)) +
  geom_bar(stat = "identity", fill = "#3498db", alpha = 0.7, width = 0.6) +
  geom_hline(yintercept = sample_mean, color = "#e74c3c", size = 1.2, linetype = "solid") +
  geom_text(aes(label = sprintf("%.2f", Consumption)), vjust = -0.3, size = 3) +
  annotate("text", x = 9, y = sample_mean + 80, 
           label = paste("Sample Mean (X̄) =", round(sample_mean, 2), "GWh"),
           color = "#e74c3c", size = 4, fontface = "bold") +
  annotate("text", x = 9, y = sample_mean - 80, 
           label = "Point Estimate of Population Mean (μ)",
           color = "#e74c3c", size = 3.5, fontface = "italic") +
  labs(title = "Figure 3: Sample Mean as Point Estimate of Population Mean",
       subtitle = "2023 Monthly Domestic Electricity Consumption in Malaysia",
       x = "Month",
       y = "Electricity Consumption (GWh)",
       caption = paste("Prepared by: Vincent Teoh Chia Win (B032410980)\n",
                      "Red line represents the sample mean (point estimate)")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
        plot.subtitle = element_text(size = 10, hjust = 0.5, color = "gray40")) +
  ylim(0, max(consumption_values) * 1.15)

print(point_estimate_plot)

# Save the plot
ggsave("C:/Users/Vincent/Desktop/Assignment2/Output/figure3_point_estimate.png",
       plot = point_estimate_plot, width = 12, height = 7, dpi = 300)

cat("\n")
cat("Graph saved: Output/figure3_point_estimate.png\n")
cat("\n")

# ============================================================================
# SUMMARY
# ============================================================================

cat("=" , rep("=", 70), "\n", sep="")
cat("SUMMARY OF QUESTION 1(c):\n")
cat("=" , rep("=", 70), "\n", sep="")

cat("\n")
cat("╔══════════════════════════════════════════════════════════════════════╗\n")
cat("║  SAMPLE MEAN CALCULATION:                                            ║\n")
cat("║  ─────────────────────────                                           ║\n")
cat(sprintf("║  Sum of observations (ΣXᵢ):  %.4f GWh                        ║\n", sum_x))
cat(sprintf("║  Sample size (n):             %d observations                       ║\n", n))
cat(sprintf("║  Sample Mean (X̄):            %.4f GWh                         ║\n", sample_mean))
cat("║                                                                      ║\n")
cat("║  MEANING AS POINT ESTIMATE:                                          ║\n")
cat("║  ──────────────────────────                                          ║\n")
cat(sprintf("║  The sample mean X̄ = %.4f GWh is our SINGLE BEST ESTIMATE     ║\n", sample_mean))
cat("║  of the population mean (μ), representing the true average          ║\n")
cat("║  monthly domestic electricity consumption in Malaysia.              ║\n")
cat("║                                                                      ║\n")
cat("║  It is called a 'point' estimate because it provides ONE specific   ║\n")
cat("║  value (a point on the number line) as our best approximation of    ║\n")
cat("║  the unknown population parameter.                                  ║\n")
cat("╚══════════════════════════════════════════════════════════════════════╝\n")
cat("\n")

cat("=" , rep("=", 70), "\n", sep="")
cat("END OF QUESTION 1(c) ANALYSIS\n")
cat("=" , rep("=", 70), "\n", sep="")
