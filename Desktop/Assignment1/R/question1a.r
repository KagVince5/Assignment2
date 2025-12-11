# ============================================================
# Question 1: Data Exploration and Variable Classification
# Dataset: Malaysia Population Data
# ============================================================

## 1. Load the dataset into R
data <- read.csv("../xls/population_state.csv")
cat("=== 1. Dataset Loaded Successfully ===\n\n")

## 2. Display the first five (5) rows of the dataset
cat("=== 2. First 5 Rows of the Dataset ===\n\n")
print(head(data, 5))
cat("\n")

## 3. Report the total number of rows and columns
cat("=== 3. Dataset Dimensions ===\n\n")
num_rows <- nrow(data)
num_cols <- ncol(data)
cat("Total number of rows:", num_rows, "\n")
cat("Total number of columns:", num_cols, "\n\n")
cat("Column names:", paste(colnames(data), collapse = ", "), "\n\n")

## 4. Identify and classify each variable as qualitative or quantitative
cat("=== 4. Variable Classification (Qualitative vs Quantitative) ===\n\n")
cat("Data Types:\n")
print(sapply(data, class))
cat("\n")
cat("Variable Classification:\n")
cat("-------------------------\n")
cat("1. state      : Qualitative (categorical - state names)\n")
cat("2. date       : Qualitative (categorical - date as string, could be treated as time/interval)\n")
cat("3. sex        : Qualitative (categorical - gender categories)\n")
cat("4. age        : Qualitative (categorical - age group)\n")
cat("5. ethnicity  : Qualitative (categorical - ethnicity group)\n")
cat("6. population : Quantitative (numeric - population count)\n\n")

## 5. State the scale of measurement for each variable
cat("=== 5. Scale of Measurement for Each Variable ===\n\n")
cat("1. state      : Nominal (names of states)\n")
cat("2. date       : Interval (dates, can be ordered and differences are meaningful)\n")
cat("3. sex        : Nominal (categories: male, female, both)\n")
cat("4. age        : Ordinal (ordered age groups)\n")
cat("5. ethnicity  : Nominal (ethnic group names)\n")
cat("6. population : Ratio (numeric, true zero, meaningful ratios)\n\n")
cat("3. age         : Qualitative (categorical - age group categories)\n")
cat("4. ethnicity   : Qualitative (categorical - ethnic group categories)\n")
cat("5. population  : Quantitative (numerical - count of people in thousands)\n\n")

# 5. State the scale of measurement for each variable
# ------------------------------------------------------------
cat("=== 5. Scale of Measurement for Each Variable ===\n\n")

cat("Variable       | Type         | Scale of Measurement | Justification\n")
cat("---------------|--------------|----------------------|----------------------------------\n")
cat("date           | Qualitative  | Interval             | Dates have meaningful differences\n")
cat("               |              |                      | but no true zero point\n")
cat("sex            | Qualitative  | Nominal              | Categories with no natural order\n")
cat("               |              |                      | (male, female, both)\n")
cat("age            | Qualitative  | Ordinal              | Age groups have natural order\n")
cat("               |              |                      | (0-4 < 5-9 < 10-14, etc.)\n")
cat("ethnicity      | Qualitative  | Nominal              | Categories with no natural order\n")
cat("               |              |                      | (malay, chinese, indian, etc.)\n")
cat("               |              |                      | (e.g., 2000 is twice 1000)\n\n")

## 6. Write a short paragraph (about 100 words) describing what the dataset shows about Malaysia population.
cat("=== 6. Dataset Description ===\n\n")
description <- "The dataset provides a comprehensive demographic snapshot of Malaysia, detailing population statistics across various states ("
description <- paste0(description, "e.g., Johor, Kedah, Selangor). It categorizes data by key variables including date, gender (male, female), ")
description <- paste0(description, "age groups, and ethnicity. By analyzing this structured data, we can observe regional population distributions, ")
description <- paste0(description, "identify disparities between states (e.g., urban vs rural density), and track demographic compositions such as ")
description <- paste0(description, "gender ratios and ethnic diversity. The inclusion of the 'date' variable also allows for potential time-series ")
description <- paste0(description, "analysis to understand population growth trends over time.")

cat(description, "\n\n")
