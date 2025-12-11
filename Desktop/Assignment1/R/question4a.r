# Question 4a: Baby Weights Analysis
# ============================================================

# Load required libraries
if (!require(ggplot2)) install.packages("ggplot2", repos = "https://cloud.r-project.org")
if (!require(dplyr)) install.packages("dplyr", repos = "https://cloud.r-project.org")
library(ggplot2)
library(dplyr)

# ============================================================
# 1. Load and Sample Data
# ============================================================
cat("=== 1. Data Loading and Random Selection ===\n")

# Check if file exists (handle running from 'R' folder or root folder)
possible_paths <- c("../xls/Babies_Weight_2.txt", "xls/Babies_Weight_2.txt", "./Babies_Weight_2.txt")
file_path <- possible_paths[file.exists(possible_paths)][1]

if (is.na(file_path)) {
    stop("Error: 'Babies_Weight_2.txt' not found. Checked: ", paste(possible_paths, collapse = ", "))
}
message("Found data file at: ", file_path)

# Determine image output directory based on input path logic
if (grepl("\\.\\./", file_path)) {
    img_dir <- "../images/"
} else {
    img_dir <- "images/"
}
if (!dir.exists(img_dir)) dir.create(img_dir)

# Load data (assuming tab or space delimited, checking structure)
# The file extension is .txt, usually read.table works.
# Based on typical assignments, it might have headers "weight" and "gender".
data_full <- read.table(file_path, header = TRUE)
# Checks and sanitizes column names
colnames(data_full) <- tolower(trimws(colnames(data_full)))
cat("Column names detected:", paste(colnames(data_full), collapse = ", "), "\n")

# Randomly select 500 data points without replacement
set.seed(123) # For reproducibility
data_sample <- data_full[sample(nrow(data_full), 500, replace = FALSE), ]

cat("Sample size:", nrow(data_sample), "\n")
cat("First 10 rows of selected data (take a screenshot):\n")
print(head(data_sample, 10))
cat("\n")

# Assign labels for gender (0 = male, 1 = female) for plotting
data_sample$gender_label <- factor(data_sample$gender, levels = c(0, 1), labels = c("Male", "Female"))

# ============================================================
# 2. Histograms
# ============================================================

# (i) Histogram of the weights of all babies
p1 <- ggplot(data_sample, aes(x = weight)) +
    geom_histogram(binwidth = 5, fill = "skyblue", color = "black", alpha = 0.7) +
    labs(title = "Histogram of All Baby Weights", x = "Weight (ounces)", y = "Frequency") +
    theme_minimal()

ggsave(paste0(img_dir, "q4a_hist_all.png"), plot = p1, width = 8, height = 6, bg = "white")
cat("(i) Created histogram of all weights:", paste0(img_dir, "q4a_hist_all.png"), "\n")

# (ii) Histogram of weight of male and female babies separately
p2 <- ggplot(data_sample, aes(x = weight, fill = gender_label)) +
    geom_histogram(binwidth = 5, color = "black", alpha = 0.6, position = "identity") +
    facet_wrap(~gender_label) +
    labs(title = "Histogram of Baby Weights by Gender", x = "Weight (ounces)", y = "Frequency") +
    theme_minimal() +
    theme(legend.position = "none")

ggsave(paste0(img_dir, "q4a_hist_gender.png"), plot = p2, width = 10, height = 6, bg = "white")
cat("(ii) Created histogram by gender:", paste0(img_dir, "q4a_hist_gender.png"), "\n\n")

# ============================================================
# 3. Statistical Analysis
# ============================================================
cat("=== Statistical Analysis ===\n")

# (iii) Calculate Mean and Standard Deviation
stats <- data_sample %>%
    group_by(gender_label) %>%
    summarise(
        mean_weight = mean(weight),
        sd_weight = sd(weight)
    )

print(stats)
cat("\n")

# Extract values for calculations
mu_female <- stats$mean_weight[stats$gender_label == "Female"]
sd_female <- stats$sd_weight[stats$gender_label == "Female"]
mu_male <- stats$mean_weight[stats$gender_label == "Male"]
sd_male <- stats$sd_weight[stats$gender_label == "Male"]

cat(sprintf("Male stats: Mean = %.2f, SD = %.2f\n", mu_male, sd_male))
cat(sprintf("Female stats: Mean = %.2f, SD = %.2f\n\n", mu_female, sd_female))

# --- Probability Calculations (assuming Normal Distribution) ---

# (iv) Probability of randomly picked female baby weight >= 132
# P(X >= 132) = 1 - P(X < 132)
prob_iv <- 1 - pnorm(132, mean = mu_female, sd = sd_female)
cat(sprintf("(iv) P(Female >= 132): %.4f\n", prob_iv))

# (v) Probability of randomly picked female baby weight between 120 and 130
# P(120 <= X <= 130) = P(X <= 130) - P(X < 120)
prob_v <- pnorm(130, mean = mu_female, sd = sd_female) - pnorm(120, mean = mu_female, sd = sd_female)
cat(sprintf("(v) P(120 <= Female <= 130): %.4f\n", prob_v))

# (vi) Probability of randomly picked female baby weight between 130 and 140
prob_vi <- pnorm(140, mean = mu_female, sd = sd_female) - pnorm(130, mean = mu_female, sd = sd_female)
cat(sprintf("(vi) P(130 <= Female <= 140): %.4f\n", prob_vi))

# (vii) Conclusion for (v) and (vi)
cat("(vii) Conclusion: ")
if (prob_v > prob_vi) {
    cat("Since P(120-130) > P(130-140), it is more likely to find a female baby in the 120-130oz range than 130-140oz.\n")
} else {
    cat("Since P(130-140) > P(120-130), it is more likely to find a female baby in the 130-140oz range.\n")
}
cat("\n")

# (viii) Probability of male baby weight < 128
prob_viii <- pnorm(128, mean = mu_male, sd = sd_male)
cat(sprintf("(viii) P(Male < 128): %.4f\n", prob_viii))

# (ix) Probability of male baby weight between 130 and 140
prob_ix <- pnorm(140, mean = mu_male, sd = sd_male) - pnorm(130, mean = mu_male, sd = sd_male)
cat(sprintf("(ix) P(130 <= Male <= 140): %.4f\n", prob_ix))

# (x) Conclusion for (vi) and (ix) (Comparing Female 130-140 vs Male 130-140)
cat("(x) Conclusion: ")
if (prob_ix > prob_vi) {
    cat(sprintf("Male babies (%.4f) have a HIGHER probability of being in the 130-140oz range than female babies (%.4f).\n", prob_ix, prob_vi))
} else {
    cat(sprintf("Female babies (%.4f) have a HIGHER probability of being in the 130-140oz range than male babies (%.4f).\n", prob_vi, prob_ix))
}
cat("\n")

# --- Quantile Calculations ---

# (xi) Weight of female babies that 85% are lower than this value (85th percentile)
q_xi <- qnorm(0.85, mean = mu_female, sd = sd_female)
cat(sprintf("(xi) Female 85th Percentile: %.2f oz\n", q_xi))

# (xii) Two values for middle 60% of female babies (Symmetrically distributed)
# Middle 60% means 20% tails on each side. Lower bound = 20th percentile (0.2), Upper bound = 80th percentile (0.8).
q_xii_lower <- qnorm(0.20, mean = mu_female, sd = sd_female)
q_xii_upper <- qnorm(0.80, mean = mu_female, sd = sd_female)
cat(sprintf("(xii) Female Middle 60%%: Between %.2f and %.2f oz\n", q_xii_lower, q_xii_upper))

# (xiii) Weight of male babies that 80% are HIGHER than this value.
# If 80% are higher, then 20% are lower. This is the 20th percentile.
q_xiii <- qnorm(0.20, mean = mu_male, sd = sd_male)
cat(sprintf("(xiii) Male weight where 80%% are higher (20th Percentile): %.2f oz\n", q_xiii))

# (xiv) Two values for middle 60% of male babies
q_xiv_lower <- qnorm(0.20, mean = mu_male, sd = sd_male)
q_xiv_upper <- qnorm(0.80, mean = mu_male, sd = sd_male)
cat(sprintf("(xiv) Male Middle 60%%: Between %.2f and %.2f oz\n", q_xiv_lower, q_xiv_upper))

# (xv) Conclusion comparing (xii) and (xiv)
cat("(xv) Conclusion: ")
range_female <- q_xii_upper - q_xii_lower
range_male <- q_xiv_upper - q_xiv_lower
cat(sprintf(
    "The range for the middle 60%% of males (%.2f - %.2f = %.2f) vs females (%.2f - %.2f = %.2f). ",
    q_xiv_lower, q_xiv_upper, range_male, q_xii_lower, q_xii_upper, range_female
))
if (range_male > range_female) {
    cat("Since the male range is wider, male weights have greater variability (dispersion) in the central 60% distribution.\n")
} else {
    cat("Since the female range is wider, female weights have greater variability (dispersion) in the central 60% distribution.\n")
}
