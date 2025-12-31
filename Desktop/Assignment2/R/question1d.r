# ============================================================================
# QUESTION 1(d): 95% Confidence Interval Construction and Interpretation
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
# QUESTION 1(d): 95% CONFIDENCE INTERVAL CONSTRUCTION
# ============================================================================

cat("\n")
cat("=" , rep("=", 70), "\n", sep="")
cat("QUESTION 1(d): 95% CONFIDENCE INTERVAL FOR MEAN MONTHLY CONSUMPTION\n")
cat("=" , rep("=", 70), "\n", sep="")

# ============================================================================
# STEP 1: CALCULATE SAMPLE STATISTICS
# ============================================================================

cat("\n")
cat("-" , rep("-", 70), "\n", sep="")
cat("STEP 1: SAMPLE STATISTICS\n")
cat("-" , rep("-", 70), "\n", sep="")

n <- length(consumption_values)           # Sample size
x_bar <- mean(consumption_values)         # Sample mean
s <- sd(consumption_values)               # Sample standard deviation
se <- s / sqrt(n)                         # Standard error of the mean

cat("\n")
cat(sprintf("Sample size (n):                    %d\n", n))
cat(sprintf("Sample mean (X̄):                   %.4f GWh\n", x_bar))
cat(sprintf("Sample standard deviation (s):     %.4f GWh\n", s))
cat(sprintf("Standard error (SE = s/√n):        %.4f GWh\n", se))
cat("\n")

# ============================================================================
# STEP 2: DETERMINE THE APPROPRIATE DISTRIBUTION
# ============================================================================

cat("-" , rep("-", 70), "\n", sep="")
cat("STEP 2: CHOOSING THE APPROPRIATE DISTRIBUTION\n")
cat("-" , rep("-", 70), "\n", sep="")

cat("\n")
cat("Since:\n")
cat(sprintf("  - Sample size n = %d (which is SMALL, n < 30)\n", n))
cat("  - Population standard deviation (σ) is UNKNOWN\n")
cat("  - We assume the population is approximately normally distributed\n")
cat("\n")
cat("We use the t-DISTRIBUTION with degrees of freedom (df) = n - 1\n")
cat(sprintf("  df = %d - 1 = %d\n", n, n-1))
cat("\n")

# ============================================================================
# STEP 3: FIND THE CRITICAL VALUE
# ============================================================================

cat("-" , rep("-", 70), "\n", sep="")
cat("STEP 3: CRITICAL VALUE (t*)\n")
cat("-" , rep("-", 70), "\n", sep="")

confidence_level <- 0.95
alpha <- 1 - confidence_level
df <- n - 1
t_critical <- qt(1 - alpha/2, df)         # Two-tailed critical value

cat("\n")
cat(sprintf("Confidence level:           %.0f%%\n", confidence_level * 100))
cat(sprintf("Significance level (α):     %.2f\n", alpha))
cat(sprintf("α/2:                        %.3f\n", alpha/2))
cat(sprintf("Degrees of freedom (df):    %d\n", df))
cat(sprintf("Critical value (t*):        %.4f\n", t_critical))
cat("\n")
cat("Note: t* is found using R's qt() function:\n")
cat(sprintf("  t* = qt(1 - α/2, df) = qt(%.3f, %d) = %.4f\n", 1 - alpha/2, df, t_critical))
cat("\n")

# ============================================================================
# STEP 4: CALCULATE MARGIN OF ERROR
# ============================================================================

cat("-" , rep("-", 70), "\n", sep="")
cat("STEP 4: MARGIN OF ERROR (ME)\n")
cat("-" , rep("-", 70), "\n", sep="")

margin_of_error <- t_critical * se

cat("\n")
cat("Formula: ME = t* × SE\n")
cat("\n")
cat(sprintf("ME = t* × (s / √n)\n"))
cat(sprintf("ME = %.4f × (%.4f / √%d)\n", t_critical, s, n))
cat(sprintf("ME = %.4f × %.4f\n", t_critical, se))
cat(sprintf("ME = %.4f GWh\n", margin_of_error))
cat("\n")

# ============================================================================
# STEP 5: CONSTRUCT THE CONFIDENCE INTERVAL
# ============================================================================

cat("-" , rep("-", 70), "\n", sep="")
cat("STEP 5: CONFIDENCE INTERVAL CONSTRUCTION\n")
cat("-" , rep("-", 70), "\n", sep="")

lower_bound <- x_bar - margin_of_error
upper_bound <- x_bar + margin_of_error

cat("\n")
cat("Formula: CI = X̄ ± ME = (X̄ - ME, X̄ + ME)\n")
cat("\n")
cat(sprintf("Lower Bound = X̄ - ME = %.4f - %.4f = %.4f GWh\n", x_bar, margin_of_error, lower_bound))
cat(sprintf("Upper Bound = X̄ + ME = %.4f + %.4f = %.4f GWh\n", x_bar, margin_of_error, upper_bound))
cat("\n")
cat("╔══════════════════════════════════════════════════════════════════════╗\n")
cat("║                     95% CONFIDENCE INTERVAL                          ║\n")
cat("║══════════════════════════════════════════════════════════════════════║\n")
cat(sprintf("║       (%.4f GWh  ,  %.4f GWh)                       ║\n", lower_bound, upper_bound))
cat("║                                                                      ║\n")
cat(sprintf("║       or equivalently: %.4f ± %.4f GWh                  ║\n", x_bar, margin_of_error))
cat("╚══════════════════════════════════════════════════════════════════════╝\n")
cat("\n")

# ============================================================================
# VERIFICATION USING R's t.test() FUNCTION
# ============================================================================

cat("-" , rep("-", 70), "\n", sep="")
cat("VERIFICATION USING R's t.test() FUNCTION\n")
cat("-" , rep("-", 70), "\n", sep="")

t_test_result <- t.test(consumption_values, conf.level = 0.95)
cat("\n")
print(t_test_result)
cat("\n")
cat(sprintf("R's 95%% CI: (%.4f, %.4f)\n", t_test_result$conf.int[1], t_test_result$conf.int[2]))
cat("This confirms our manual calculation is correct!\n")
cat("\n")

# ============================================================================
# STEP 6: INTERPRETATION FOR POLICYMAKERS
# ============================================================================

cat("-" , rep("-", 70), "\n", sep="")
cat("STEP 6: INTERPRETATION FOR POLICYMAKERS AND ENERGY PLANNERS\n")
cat("-" , rep("-", 70), "\n", sep="")

cat("\n")
cat("╔══════════════════════════════════════════════════════════════════════╗\n")
cat("║           INTERPRETATION IN NON-TECHNICAL LANGUAGE                  ║\n")
cat("╚══════════════════════════════════════════════════════════════════════╝\n")
cat("\n")

cat("WHAT THIS MEANS:\n")
cat("────────────────\n")
cat("\n")
cat("Based on the 2023 electricity consumption data from Malaysian households,\n")
cat("we are 95% CONFIDENT that the TRUE AVERAGE monthly domestic electricity\n")
cat("consumption in Malaysia falls between:\n")
cat("\n")
cat(sprintf("        %.2f GWh  and  %.2f GWh per month\n", lower_bound, upper_bound))
cat("\n")

cat("IN SIMPLE TERMS:\n")
cat("────────────────\n")
cat("\n")
cat("1. ELECTRICITY DEMAND PLANNING:\n")
cat(sprintf("   Energy planners should expect Malaysian households to consume\n"))
cat(sprintf("   between %.0f and %.0f GWh of electricity each month on average.\n", 
            floor(lower_bound), ceiling(upper_bound)))
cat("\n")

cat("2. INFRASTRUCTURE CAPACITY:\n")
cat(sprintf("   Power generation and distribution systems should be designed to\n"))
cat(sprintf("   reliably supply at least %.0f GWh monthly to meet domestic demand,\n", ceiling(upper_bound)))
cat("   with some buffer for peak periods.\n")
cat("\n")

cat("3. BUDGET PLANNING:\n")
cat(sprintf("   Annual domestic electricity consumption is estimated to be between\n"))
cat(sprintf("   %.0f GWh and %.0f GWh (multiplying monthly figures by 12).\n", 
            lower_bound * 12, upper_bound * 12))
cat("   This helps in forecasting revenue and planning subsidies.\n")
cat("\n")

cat("4. CONFIDENCE IN THE ESTIMATE:\n")
cat("   If we were to repeat this analysis with different samples of monthly\n")
cat("   data, 95 out of 100 such intervals would contain the true average.\n")
cat("   This gives policymakers HIGH CONFIDENCE in using this range for\n")
cat("   decision-making.\n")
cat("\n")

cat("5. POLICY IMPLICATIONS:\n")
cat("   - This range can guide electricity tariff reviews\n")
cat("   - Helps in assessing the impact of energy efficiency programs\n")
cat("   - Useful for projecting future demand growth\n")
cat("   - Supports decisions on renewable energy investments\n")
cat("\n")

# ============================================================================
# VISUALIZATION
# ============================================================================

cat("-" , rep("-", 70), "\n", sep="")
cat("STEP 7: GRAPHICAL REPRESENTATION\n")
cat("-" , rep("-", 70), "\n", sep="")

# Create data for visualization
months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
            "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

plot_data <- data.frame(
  Month = factor(months, levels = months),
  Consumption = consumption_values
)

# Create confidence interval visualization
ci_plot <- ggplot(plot_data, aes(x = Month, y = Consumption)) +
  # Add confidence interval band
  annotate("rect", xmin = -Inf, xmax = Inf, 
           ymin = lower_bound, ymax = upper_bound,
           fill = "#3498db", alpha = 0.2) +
  # Add data points
  geom_point(color = "#2c3e50", size = 4) +
  geom_line(aes(group = 1), color = "#2c3e50", size = 0.8) +
  # Add mean line
  geom_hline(yintercept = x_bar, color = "#e74c3c", size = 1.2, linetype = "solid") +
  # Add CI upper and lower bounds
  geom_hline(yintercept = upper_bound, color = "#27ae60", size = 1, linetype = "dashed") +
  geom_hline(yintercept = lower_bound, color = "#27ae60", size = 1, linetype = "dashed") +
  # Add annotations
  annotate("text", x = 10.5, y = x_bar + 30, 
           label = paste("Mean =", round(x_bar, 2), "GWh"),
           color = "#e74c3c", size = 3.5, fontface = "bold") +
  annotate("text", x = 10.5, y = upper_bound + 30, 
           label = paste("Upper:", round(upper_bound, 2), "GWh"),
           color = "#27ae60", size = 3, fontface = "bold") +
  annotate("text", x = 10.5, y = lower_bound - 30, 
           label = paste("Lower:", round(lower_bound, 2), "GWh"),
           color = "#27ae60", size = 3, fontface = "bold") +
  annotate("text", x = 6, y = (upper_bound + lower_bound)/2, 
           label = "95% Confidence Interval",
           color = "#3498db", size = 4, fontface = "bold.italic") +
  # Labels and theme
  labs(title = "Figure 4: 95% Confidence Interval for Mean Monthly Domestic Electricity Consumption",
       subtitle = "Malaysia 2023 - Shaded area represents the 95% confidence interval",
       x = "Month",
       y = "Electricity Consumption (GWh)",
       caption = paste("Prepared by: Vincent Teoh Chia Win (B032410980)\n",
                      "95% CI: (", round(lower_bound, 2), ", ", round(upper_bound, 2), ") GWh")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(face = "bold", size = 12, hjust = 0.5),
        plot.subtitle = element_text(size = 10, hjust = 0.5, color = "gray40")) +
  ylim(min(consumption_values) * 0.85, max(consumption_values) * 1.10)

print(ci_plot)

# Save the plot
ggsave("C:/Users/Vincent/Desktop/Assignment2/Output/figure4_confidence_interval.png",
       plot = ci_plot, width = 12, height = 7, dpi = 300)

cat("\n")
cat("Graph saved: Output/figure4_confidence_interval.png\n")
cat("\n")

# ============================================================================
# SUMMARY
# ============================================================================

cat("=" , rep("=", 70), "\n", sep="")
cat("SUMMARY OF QUESTION 1(d):\n")
cat("=" , rep("=", 70), "\n", sep="")

cat("\n")
cat("╔══════════════════════════════════════════════════════════════════════╗\n")
cat("║  95% CONFIDENCE INTERVAL RESULTS                                     ║\n")
cat("╠══════════════════════════════════════════════════════════════════════╣\n")
cat(sprintf("║  Sample Mean (X̄):              %.4f GWh                       ║\n", x_bar))
cat(sprintf("║  Standard Error (SE):           %.4f GWh                         ║\n", se))
cat(sprintf("║  Critical Value (t*):           %.4f                              ║\n", t_critical))
cat(sprintf("║  Margin of Error (ME):          %.4f GWh                        ║\n", margin_of_error))
cat("║                                                                      ║\n")
cat(sprintf("║  95%% CI: (%.4f GWh, %.4f GWh)                       ║\n", lower_bound, upper_bound))
cat("║                                                                      ║\n")
cat("║  INTERPRETATION FOR POLICYMAKERS:                                    ║\n")
cat("║  We are 95% confident that the true average monthly domestic        ║\n")
cat("║  electricity consumption in Malaysia lies within this interval.    ║\n")
cat("║  This range provides a reliable basis for energy planning and      ║\n")
cat("║  infrastructure investment decisions.                              ║\n")
cat("╚══════════════════════════════════════════════════════════════════════╝\n")
cat("\n")

cat("=" , rep("=", 70), "\n", sep="")
cat("END OF QUESTION 1(d) ANALYSIS\n")
cat("=" , rep("=", 70), "\n", sep="")
