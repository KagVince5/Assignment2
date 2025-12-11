# Question 1 Part D - Comparative Analysis
# Compares mean population values between two selected states (Selangor and Kelantan)

# Load required libraries
if (!require(ggplot2)) install.packages("ggplot2", repos = "https://cloud.r-project.org")
if (!require(dplyr)) install.packages("dplyr", repos = "https://cloud.r-project.org")
library(ggplot2)
library(dplyr)

# 1. Load Data
data <- read.csv("../xls/population_state.csv", stringsAsFactors = FALSE)

# Basic Cleanup
data$date <- as.Date(data$date)
latest_date <- max(data$date)
cat("Using data from date:", as.character(latest_date), "\n\n")

# 2. Select Two States for Comparison
# Choice: Selangor vs Kelantan
selected_states <- c("Selangor", "Kelantan")

# Filter Data: Latest date, overall sex, overall age, overall ethnicity
df_compare <- data %>%
    filter(
        date == latest_date,
        state %in% selected_states,
        sex == "both",
        age == "overall",
        ethnicity == "overall"
    ) %>%
    select(state, population)

# 3. Calculate Mean Population (Since it's one year, it's just the total pop for that year)
# The question asks for "mean population values".
# Interpretation: If we are comparing just ONE time point (latest), the "mean" is the value itself.
# However, if the question implies comparing over time, we would average over years.
# Given Part B/C focused on "Current" snapshot, we will stick to the latest year value
# but present it as the "Mean/Average Population" for consistency with the question wording,
# or we can average over the available history if we want a robust "mean".
# Let's stick to the SNAPSHOT value as used in previous parts to be consistent, relative to the "Latest Year".
# BUT, to strictly follow "Calculate mean", let's average over the LAST 5 YEARS to give it some statistical weight?
# No, let's keep it simple: Compare the "Population Value" implied as the metric.
# Re-reading prompt: "Compare their mean population values." likely implies specific groups (like districts) but we only have State level.
# So "Mean" of a single state value is just the value.
# ALTERNATIVELY, maybe it means mean across districts? But we don't have district data.
# Let's just use the latest population value and label it clearly.
# actually, let's look at the data structure. It has "date". Maybe comparing mean over the entire timeline available in CSV?
# That makes more sense for "Mean Population".
# Let's calculate the Mean Population over all years in the dataset for these two states.

df_all_years <- data %>%
    filter(
        state %in% selected_states,
        sex == "both",
        age == "overall",
        ethnicity == "overall"
    )

comparison_stats <- df_all_years %>%
    group_by(state) %>%
    summarise(mean_population = mean(population, na.rm = TRUE))

# 4. Present comparison in a simple table
cat("--- COMPARISON TABLE: MEAN POPULATION (All Years) ---\n")
print(comparison_stats)
cat("\n")

# 5. Plot a bar chart to show the difference
# Use the calculated mean values
png_filename <- "comparison_chart.png"
p <- ggplot(comparison_stats, aes(x = state, y = mean_population, fill = state)) +
    geom_bar(stat = "identity", width = 0.5) +
    labs(
        title = "Mean Population Comparison (All Years)",
        subtitle = "Selangor vs Kelantan",
        x = "State", y = "Mean Population (Thousands)"
    ) +
    theme_minimal() +
    scale_fill_manual(values = c("Kelantan" = "orange", "Selangor" = "blue"))

ggsave(png_filename, plot = p, width = 6, height = 6)
cat(sprintf("Bar chart saved to %s\n", png_filename))

# 6. Write a short explanation
explanation <- paste0(
    "--- EXPLANATION ---\n",
    "The data shows a significant difference in mean population between Selangor and Kelantan. ",
    "Selangor has a much higher average population due to rapid urbanization and its status as a primary economic hub in Malaysia (Central Region), attracting significant migration. ",
    "In contrast, Kelantan (East Coast Region) has a more agrarian economy and lower urbanization rates, resulting in a lower average population density."
)
cat("\n")
cat(explanation)
cat("\n")
