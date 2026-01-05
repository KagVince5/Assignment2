# ==============================================================================
# Question 3(b): 95% Confidence Interval
# ==============================================================================

library(dplyr)

# Read the data
query_data <- read.csv("dbms_comparison.csv")

# Extract execution times for each DBMS
times_a <- query_data %>% filter(DBMS == "DBMS A") %>% pull(Execution_Time)
times_b <- query_data %>% filter(DBMS == "DBMS B") %>% pull(Execution_Time)

cat("\n95% CONFIDENCE INTERVAL ANALYSIS\n")
cat(rep("=", 50), sep="")
cat("\n\n")

# Calculate sample statistics
mean_a <- mean(times_a)
mean_b <- mean(times_b)
sd_a <- sd(times_a)
sd_b <- sd(times_b)
n_a <- length(times_a)
n_b <- length(times_b)

cat("Sample Statistics:\n")
cat(sprintf("  DBMS A: Mean = %.2f ms, SD = %.2f ms, n = %d\n", mean_a, sd_a, n_a))
cat(sprintf("  DBMS B: Mean = %.2f ms, SD = %.2f ms, n = %d\n", mean_b, sd_b, n_b))
cat(sprintf("  Difference (A - B): %.2f ms\n", mean_a - mean_b))

# Perform t-test to get confidence interval
t_test_result <- t.test(times_a, times_b, conf.level = 0.95, var.equal = TRUE)

cat("\n95% Confidence Interval for Difference:\n")
cat(sprintf("  [%.2f, %.2f] ms\n", t_test_result$conf.int[1], t_test_result$conf.int[2]))

