# ==============================================================================
# Question 3(c): Hypothesis Testing
# ==============================================================================

run_hypothesis_test <- function() {
  library(dplyr)
  
  # Read the data
  query_data <- read.csv("dbms_comparison.csv")
  times_a <- query_data %>% filter(DBMS == "DBMS A") %>% pull(Execution_Time)
  times_b <- query_data %>% filter(DBMS == "DBMS B") %>% pull(Execution_Time)
  
  # Part (i): Two-Sample t-Test
  
  cat("\nTWO-SAMPLE T-TEST\n")
  cat(rep("=", 50), sep="")
  cat("\n\n")
  
  cat("Hypotheses:\n")
  cat("  H0: μA = μB (No difference in mean execution times)\n")
  cat("  H1: μA ≠ μB (There is a difference)\n")
  cat("  Significance Level: α = 0.05\n\n")
  
  # Perform t-test
  t_test <- t.test(times_a, times_b, var.equal = TRUE, alternative = "two.sided")
  
  cat("Test Results:\n")
  cat(sprintf("  Mean DBMS A: %.2f ms\n", mean(times_a)))
  cat(sprintf("  Mean DBMS B: %.2f ms\n", mean(times_b)))
  cat(sprintf("  Difference: %.2f ms\n", mean(times_a) - mean(times_b)))
  cat(sprintf("  t-statistic: %.4f\n", t_test$statistic))
  cat(sprintf("  Degrees of Freedom: %d\n", t_test$parameter))
  cat(sprintf("  P-value: %.4f\n\n", t_test$p.value))
  
  cat("Decision:\n")
  if (t_test$p.value < 0.05) {
    cat("  REJECT H0 (p-value < 0.05)\n\n")
    cat("Conclusion:\n")
    cat("  There is a statistically significant difference between DBMS A and DBMS B.\n")
    if (mean(times_a) > mean(times_b)) {
      cat("  DBMS B performs better than DBMS A.\n")
    } else {
      cat("  DBMS A performs better than DBMS B.\n")
    }
  } else {
    cat("  FAIL TO REJECT H0 (p-value ≥ 0.05)\n\n")
    cat("Conclusion:\n")
    cat("  No statistically significant difference between DBMS A and DBMS B.\n")
  }
  
  # Part (ii): Subgroup Analysis by Query Type
  
  cat("\n\nSUBGROUP ANALYSIS BY QUERY TYPE\n")
  cat(rep("=", 50), sep="")
  cat("\n\n")
  
  query_types <- c("SELECT", "UPDATE", "JOIN")
  
  for (qtype in query_types) {
    cat(qtype, "Queries:\n")
    
    # Filter data
    type_a <- query_data %>% 
      filter(DBMS == "DBMS A" & Query_Type == qtype) %>% 
      pull(Execution_Time)
    
    type_b <- query_data %>% 
      filter(DBMS == "DBMS B" & Query_Type == qtype) %>% 
      pull(Execution_Time)
    
    # Perform t-test
    type_test <- t.test(type_a, type_b, var.equal = TRUE)
    
    cat(sprintf("  Mean DBMS A: %.2f ms\n", mean(type_a)))
    cat(sprintf("  Mean DBMS B: %.2f ms\n", mean(type_b)))
    cat(sprintf("  Difference: %.2f ms\n", mean(type_a) - mean(type_b)))
    cat(sprintf("  t-statistic: %.4f\n", type_test$statistic))
    cat(sprintf("  p-value: %.4f\n", type_test$p.value))
    
    if (type_test$p.value < 0.05) {
      cat("  Result: Significant difference\n\n")
    } else {
      cat("  Result: No significant difference\n\n")
    }
  }
  
  # Summary table
  cat("Summary Table:\n")
  summary_table <- query_data %>%
    group_by(Query_Type, DBMS) %>%
    summarise(
      Mean = round(mean(Execution_Time), 2),
      SD = round(sd(Execution_Time), 2),
      .groups = 'drop'
    )
  print(summary_table)
  
  cat("\n")
  cat(rep("=", 50), sep="")
  cat("\n")
}

run_hypothesis_test()