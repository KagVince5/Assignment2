# ==============================================================================
# Question 3(a): Descriptive Analysis
# Course: BITI 2233/BAXI 2233 Statistics and Probability
# ==============================================================================

# Load required libraries
library(ggplot2)
library(dplyr)

# Set seed for reproducibility
set.seed(123)

# Generate sample data for DBMS A and DBMS B
# Number of queries per type
n_queries <- 30

# Create data for different query types
query_types <- rep(c("SELECT", "UPDATE", "JOIN"), each = n_queries)

# DBMS A execution times (slightly slower)
dbms_a_select <- rnorm(n_queries, mean = 75, sd = 12)
dbms_a_update <- rnorm(n_queries, mean = 90, sd = 15)
dbms_a_join <- rnorm(n_queries, mean = 110, sd = 18)

# DBMS B execution times (generally faster)
dbms_b_select <- rnorm(n_queries, mean = 68, sd = 11)
dbms_b_update <- rnorm(n_queries, mean = 82, sd = 14)
dbms_b_join <- rnorm(n_queries, mean = 95, sd = 16)

# Combine all data
dbms_a_times <- c(dbms_a_select, dbms_a_update, dbms_a_join)
dbms_b_times <- c(dbms_b_select, dbms_b_update, dbms_b_join)

# Create dataframe
query_data <- data.frame(
  Query_Type = rep(query_types, 2),
  DBMS = rep(c("DBMS A", "DBMS B"), each = length(query_types)),
  Execution_Time = c(dbms_a_times, dbms_b_times)
)

# Save to CSV
write.csv(query_data, "dbms_comparison.csv", row.names = FALSE)

# View first few rows
cat("Dataset Preview:\n")
head(query_data, 10)

# ==============================================================================
# Part (a)(i): Calculate Descriptive Statistics
# ==============================================================================

cat("\n========================================\n")
cat("DESCRIPTIVE STATISTICS\n")
cat("========================================\n\n")

# Calculate descriptive statistics for DBMS A
dbms_a_stats <- query_data %>%
  filter(DBMS == "DBMS A") %>%
  summarise(
    Mean = mean(Execution_Time),
    Median = median(Execution_Time),
    SD = sd(Execution_Time),
    Min = min(Execution_Time),
    Max = max(Execution_Time)
  )

# Calculate descriptive statistics for DBMS B
dbms_b_stats <- query_data %>%
  filter(DBMS == "DBMS B") %>%
  summarise(
    Mean = mean(Execution_Time),
    Median = median(Execution_Time),
    SD = sd(Execution_Time),
    Min = min(Execution_Time),
    Max = max(Execution_Time)
  )

# Display results
cat("DBMS A Statistics:\n")
print(dbms_a_stats)
cat("\nDBMS B Statistics:\n")
print(dbms_b_stats)

# Combined summary by DBMS
cat("\nCombined Summary:\n")
summary_stats <- query_data %>%
  group_by(DBMS) %>%
  summarise(
    Mean = mean(Execution_Time),
    Median = median(Execution_Time),
    SD = sd(Execution_Time)
  )
print(summary_stats)

# ==============================================================================
# Part (a)(ii): Create Visualizations
# ==============================================================================

cat("\n========================================\n")
cat("CREATING VISUALIZATIONS\n")
cat("========================================\n\n")

# 1. Bar chart showing average execution time
cat("Creating Bar Chart...\n")
avg_data <- query_data %>%
  group_by(DBMS) %>%
  summarise(Avg_Time = mean(Execution_Time))

p1 <- ggplot(avg_data, aes(x = DBMS, y = Avg_Time, fill = DBMS)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = round(Avg_Time, 2)), vjust = -0.5, size = 5) +
  labs(title = "Average Query Execution Time Comparison",
       x = "Database Management System",
       y = "Average Execution Time (ms)") +
  theme_minimal() +
  scale_fill_manual(values = c("DBMS A" = "steelblue", "DBMS B" = "coral"))

print(p1)

# 2. Boxplot comparing execution times
cat("Creating Boxplot...\n")
p2 <- ggplot(query_data, aes(x = DBMS, y = Execution_Time, fill = DBMS)) +
  geom_boxplot() +
  labs(title = "Distribution of Query Execution Times by DBMS",
       x = "Database Management System",
       y = "Execution Time (ms)") +
  theme_minimal() +
  scale_fill_manual(values = c("DBMS A" = "steelblue", "DBMS B" = "coral"))

print(p2)

# 3. Grouped bar chart by query type
cat("Creating Grouped Bar Chart...\n")
avg_by_type <- query_data %>%
  group_by(DBMS, Query_Type) %>%
  summarise(Avg_Time = mean(Execution_Time), .groups = 'drop')

p3 <- ggplot(avg_by_type, aes(x = Query_Type, y = Avg_Time, fill = DBMS)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_text(aes(label = round(Avg_Time, 1)), 
            position = position_dodge(width = 0.7), 
            vjust = -0.5, size = 3) +
  labs(title = "Average Execution Times by Query Type and DBMS",
       x = "Query Type",
       y = "Average Execution Time (ms)") +
  theme_minimal() +
  scale_fill_manual(values = c("DBMS A" = "steelblue", "DBMS B" = "coral"))

print(p3)

cat("\nDescriptive analysis complete!\n")
cat("Data saved to: dbms_comparison.csv\n")