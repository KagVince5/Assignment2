# ============================================================================
# QUESTION 1: Malaysian Electricity Consumption Analysis
# ============================================================================
# Assignment by: Vincent Teoh Chia Win (B032410980)
# Data Source: Department of Statistics Malaysia (DOSM)
# ============================================================================

# Load required libraries
library(ggplot2)

# Set working directory and load data
data <- read.csv("C:/Users/Vincent/Desktop/Assignment2/Data/electricity_consumption.csv")

# View data structure
head(data)
str(data)

# ============================================================================
# QUESTION 1(a): Population, Sample, and Variable of Interest
# ============================================================================

# Filter for domestic (local_domestic) electricity consumption in 2023
# The question specifically asks for "domestic (local household)" consumption
data$date <- as.Date(data$date)
data$year <- format(data$date, "%Y")
data$month <- format(data$date, "%B")

# Extract 2023 domestic consumption data
domestic_2023 <- data[data$sector == "local_domestic" & data$year == "2023", ]
print("2023 Domestic Electricity Consumption Data:")
print(domestic_2023)

# ============================================================================
# ANSWERS TO QUESTION 1(a):
# ============================================================================

cat("\n")
cat("=" , rep("=", 70), "\n", sep="")
cat("QUESTION 1(a) ANSWERS:\n")
cat("=" , rep("=", 70), "\n", sep="")

# I. POPULATION
cat("\nI. POPULATION:\n")
cat("   The population consists of ALL monthly domestic (local household)\n")
cat("   electricity consumption values in Malaysia across ALL time periods.\n")
cat("   This includes all historical and future monthly consumption data\n")
cat("   that could potentially be collected from all Malaysian households.\n")

# II. SAMPLE
cat("\nII. SAMPLE:\n")
cat("   The sample is the 12 monthly observations of domestic electricity\n")
cat("   consumption in Malaysia for the year 2023.\n")
cat("   Sample size: n = 12 observations\n")

# III. VARIABLE OF INTEREST
cat("\nIII. VARIABLE OF INTEREST:\n")
cat("   The variable of interest is the monthly domestic (local household)\n")
cat("   electricity consumption in Malaysia, measured in GWh (Gigawatt-hours).\n")
cat("   This is a quantitative continuous variable.\n")
cat("\n")

# ============================================================================
# DISPLAY THE 2023 DOMESTIC DATA
# ============================================================================

cat("=" , rep("=", 70), "\n", sep="")
cat("2023 DOMESTIC ELECTRICITY CONSUMPTION DATA (in GWh):\n")
cat("=" , rep("=", 70), "\n", sep="")

# Create a clean display table
consumption_values <- domestic_2023$consumption
months <- c("January", "February", "March", "April", "May", "June",
            "July", "August", "September", "October", "November", "December")

for (i in 1:length(consumption_values)) {
  cat(sprintf("%2d. %-12s: %.4f GWh\n", i, months[i], consumption_values[i]))
}

# ============================================================================
# DESCRIPTIVE STATISTICS FOR THE SAMPLE
# ============================================================================

cat("\n")
cat("=" , rep("=", 70), "\n", sep="")
cat("DESCRIPTIVE STATISTICS FOR 2023 DOMESTIC CONSUMPTION:\n")
cat("=" , rep("=", 70), "\n", sep="")

cat(sprintf("Sample Size (n):     %d observations\n", length(consumption_values)))
cat(sprintf("Minimum:             %.4f GWh\n", min(consumption_values)))
cat(sprintf("Maximum:             %.4f GWh\n", max(consumption_values)))
cat(sprintf("Range:               %.4f GWh\n", max(consumption_values) - min(consumption_values)))
cat(sprintf("Mean:                %.4f GWh\n", mean(consumption_values)))
cat(sprintf("Median:              %.4f GWh\n", median(consumption_values)))
cat(sprintf("Standard Deviation:  %.4f GWh\n", sd(consumption_values)))
cat(sprintf("Variance:            %.4f GWh²\n", var(consumption_values)))
cat(sprintf("Total Consumption:   %.4f GWh\n", sum(consumption_values)))

# ============================================================================
# VISUALIZATION: Monthly Domestic Electricity Consumption 2023
# ============================================================================

# Create a data frame for plotting
plot_data <- data.frame(
  Month = factor(months, levels = months),
  Consumption = consumption_values
)

# Bar Chart
bar_plot <- ggplot(plot_data, aes(x = Month, y = Consumption, fill = Consumption)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = sprintf("%.2f", Consumption)), 
            vjust = -0.3, size = 3) +
  scale_fill_gradient(low = "#4575b4", high = "#d73027") +
  labs(title = "Figure 1: Monthly Domestic Electricity Consumption in Malaysia (2023)",
       subtitle = "Data Source: Department of Statistics Malaysia (DOSM)",
       x = "Month",
       y = "Electricity Consumption (GWh)",
       caption = paste("Prepared by: Vincent Teoh Chia Win (B032410980)\n",
                      "Total Annual Consumption:", round(sum(consumption_values), 2), "GWh")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
        plot.subtitle = element_text(size = 10, hjust = 0.5, color = "gray40"),
        legend.position = "none") +
  ylim(0, max(consumption_values) * 1.15)

print(bar_plot)

# Save the bar chart
ggsave("C:/Users/Vincent/Desktop/Assignment2/Output/figure1_domestic_consumption_2023.png", 
       plot = bar_plot, width = 12, height = 7, dpi = 300)

# Line Chart with trend
line_plot <- ggplot(plot_data, aes(x = Month, y = Consumption, group = 1)) +
  geom_line(color = "#2c7bb6", size = 1.2) +
  geom_point(color = "#d7191c", size = 3) +
  geom_text(aes(label = sprintf("%.2f", Consumption)), 
            vjust = -1, size = 3) +
  geom_hline(yintercept = mean(consumption_values), 
             linetype = "dashed", color = "darkgreen", size = 0.8) +
  annotate("text", x = 10, y = mean(consumption_values) + 50, 
           label = paste("Mean =", round(mean(consumption_values), 2), "GWh"),
           color = "darkgreen", size = 3.5) +
  labs(title = "Figure 2: Trend of Monthly Domestic Electricity Consumption (2023)",
       subtitle = "Data Source: Department of Statistics Malaysia (DOSM)",
       x = "Month",
       y = "Electricity Consumption (GWh)",
       caption = paste("Prepared by: Vincent Teoh Chia Win (B032410980)\n",
                      "Mean Monthly Consumption:", round(mean(consumption_values), 2), "GWh")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
        plot.subtitle = element_text(size = 10, hjust = 0.5, color = "gray40")) +
  ylim(min(consumption_values) * 0.9, max(consumption_values) * 1.15)

print(line_plot)

# Save the line chart
ggsave("C:/Users/Vincent/Desktop/Assignment2/Output/figure2_consumption_trend_2023.png", 
       plot = line_plot, width = 12, height = 7, dpi = 300)

cat("\n")
cat("=" , rep("=", 70), "\n", sep="")
cat("GRAPHS SAVED:\n")
cat("=" , rep("=", 70), "\n", sep="")
cat("1. Figure 1: Bar chart saved to Output/figure1_domestic_consumption_2023.png\n")
cat("2. Figure 2: Line chart saved to Output/figure2_consumption_trend_2023.png\n")

# ============================================================================
# APPENDIX: RAW DATA AND CALCULATIONS
# ============================================================================

cat("\n")
cat("=" , rep("=", 70), "\n", sep="")
cat("APPENDIX: DATA AND CALCULATIONS\n")
cat("=" , rep("=", 70), "\n", sep="")

cat("\nA. Raw Data (2023 Domestic Electricity Consumption):\n")
cat("-" , rep("-", 50), "\n", sep="")
print(domestic_2023[, c("date", "sector", "consumption")])

cat("\nB. Summary Statistics Calculations:\n")
cat("-" , rep("-", 50), "\n", sep="")
cat("Sum of all values (ΣX):", sum(consumption_values), "\n")
cat("Number of observations (n):", length(consumption_values), "\n")
cat("Mean (X̄ = ΣX/n):", sum(consumption_values)/length(consumption_values), "\n")
cat("Sorted values for median:", sort(consumption_values), "\n")
cat("Median ((6th + 7th)/2):", median(consumption_values), "\n")

# Variance calculation step by step
cat("\nC. Variance Calculation (Sample Variance):\n")
cat("-" , rep("-", 50), "\n", sep="")
mean_val <- mean(consumption_values)
deviations <- consumption_values - mean_val
squared_deviations <- deviations^2
cat("Mean (X̄):", mean_val, "\n")
cat("Deviations (X - X̄):\n")
print(round(deviations, 4))
cat("Squared Deviations (X - X̄)²:\n")
print(round(squared_deviations, 4))
cat("Sum of Squared Deviations Σ(X - X̄)²:", sum(squared_deviations), "\n")
cat("Sample Variance s² = Σ(X - X̄)²/(n-1):", var(consumption_values), "\n")
cat("Sample Standard Deviation s = √s²:", sd(consumption_values), "\n")

cat("\n")
cat("=" , rep("=", 70), "\n", sep="")
cat("END OF QUESTION 1 ANALYSIS\n")
cat("=" , rep("=", 70), "\n", sep="")
