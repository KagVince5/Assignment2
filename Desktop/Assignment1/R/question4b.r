# Question 4b: Normal Distribution Analysis (Metal Rods)
# ============================================================

# Load required libraries
if (!require(ggplot2)) install.packages("ggplot2", repos = "https://cloud.r-project.org")
library(ggplot2)

# Parameters
true_mean <- 50
true_sd <- 2

# ============================================================
# (i) Simulation
# ============================================================
cat("=== (i) Simulation of 300 Random Observations ===\n")

# Simulate 300 random observations from N(50, 2^2)
set.seed(42) # For reproducibility
simulated_data <- rnorm(300, mean = true_mean, sd = true_sd)

# Display first 10 simulated lengths
cat("First 10 simulated lengths:\n")
print(head(simulated_data, 10))
cat("\n")

# Compute sample mean and standard deviation
sample_mean <- mean(simulated_data)
sample_sd <- sd(simulated_data)

cat(sprintf("Sample Mean: %.4f (Theoretical: %.0f)\n", sample_mean, true_mean))
cat(sprintf("Sample SD:   %.4f (Theoretical: %.0f)\n", sample_sd, true_sd))

# Compare and conclude
cat("Conclusion: The sample mean and standard deviation are very close to the theoretical values.\n")
cat("This illustrates the Law of Large Numbers, where as the sample size increases, the sample statistics converge to the population parameters.\n\n")


# ============================================================
# (ii) Visualization
# ============================================================
cat("=== (ii) Visualization ===\n")

df <- data.frame(length = simulated_data)

# Create histogram with density curve and theoretical normal curve
p <- ggplot(df, aes(x = length)) +
    # Histogram with density scaling
    geom_histogram(aes(y = ..density..), binwidth = 0.5, fill = "lightblue", color = "black", alpha = 0.6) +
    # Empirical density curve (from data)
    geom_density(aes(color = "Empirical Density"), size = 1) +
    # Theoretical normal curve
    stat_function(
        fun = dnorm, args = list(mean = true_mean, sd = true_sd),
        aes(color = "Theoretical Normal"), size = 1, linetype = "dashed"
    ) +
    labs(
        title = "Distribution of Simulated Metal Rod Lengths",
        subtitle = "Overlay of Empirical Density vs Theoretical Normal Curve",
        x = "Length (mm)",
        y = "Density",
        color = "Legend"
    ) +
    scale_color_manual(values = c("Empirical Density" = "blue", "Theoretical Normal" = "red")) +
    theme_minimal()

# Determine image output directory
# Assuming script is run from R/ folder or root, try to put images in ../images or images/
if (dir.exists("../images")) {
    img_path <- "../images/q4b_distribution.png"
} else if (dir.exists("images")) {
    img_path <- "images/q4b_distribution.png"
} else {
    dir.create("images")
    img_path <- "images/q4b_distribution.png"
}

ggsave(img_path, plot = p, width = 8, height = 6, bg = "white")
cat(sprintf("Histogram saved to: %s\n", img_path))

cat("Comment on Shape/Center/Spread:\n")
cat("- Shape: The histogram and density curve show a symmetric, bell-shaped distribution.\n")
cat("- Center: The distribution is centered approximately around the mean of 50 mm.\n")
cat("- Spread: The data spreads out from the center, with most values falling within 46 mm to 54 mm (using 2 SD rule).\n\n")


# ============================================================
# (iii) Probabilities (Using R functions)
# ============================================================
cat("=== (iii) Probabilities ===\n")

# 1. P(X < 48)
prob_1 <- pnorm(48, mean = true_mean, sd = true_sd)
cat(sprintf("1. P(X < 48): %.4f\n", prob_1))

# 2. P(X > 52) = 1 - P(X <= 52)
prob_2 <- 1 - pnorm(52, mean = true_mean, sd = true_sd)
# Alternatively: pnorm(52, mean = true_mean, sd = true_sd, lower.tail = FALSE)
cat(sprintf("2. P(X > 52): %.4f\n", prob_2))

# 3. P(49 < X < 51) = P(X < 51) - P(X < 49)
prob_3 <- pnorm(51, mean = true_mean, sd = true_sd) - pnorm(49, mean = true_mean, sd = true_sd)
cat(sprintf("3. P(49 < X < 51): %.4f\n\n", prob_3))


# ============================================================
# (iv) Quantiles/Percentiles
# ============================================================
cat("=== (iv) Quantiles ===\n")

# 1. The length below which 10% of rods fall (10th percentile)
q1 <- qnorm(0.10, mean = true_mean, sd = true_sd)
cat(sprintf("1. Length below which 10%% fall: %.4f mm\n", q1))

# 2. The length above which 5% of rods fall (95th percentile)
q2 <- qnorm(0.95, mean = true_mean, sd = true_sd)
cat(sprintf("2. Length above which 5%% fall: %.4f mm\n", q2))

# 3. The middle 95% range of lengths
# This corresponds to the range between 2.5th and 97.5th percentiles
q3_lower <- qnorm(0.025, mean = true_mean, sd = true_sd)
q3_upper <- qnorm(0.975, mean = true_mean, sd = true_sd)
cat(sprintf("3. Middle 95%% range: [%.4f mm, %.4f mm]\n\n", q3_lower, q3_upper))


# ============================================================
# (v) Standard Normal Transformation & Reflection
# ============================================================
cat("=== (v) Standard Normal Transformation & Reflection ===\n")

val_x <- 53
# 1. Convert X = 53 mm into a Z-score
z_score <- (val_x - true_mean) / true_sd
cat(sprintf("1. Z-score for X=53: %.4f\n", z_score))

# Calculate P(X < 53) using Z-score on Standard Normal (mean=0, sd=1)
prob_z <- pnorm(z_score, mean = 0, sd = 1)
cat(sprintf("   P(Z < %.4f) from Standard Normal: %.4f\n", z_score, prob_z))

# 2. Compare with direct calculation
prob_direct <- pnorm(53, mean = true_mean, sd = true_sd)
cat(sprintf("2. P(X < 53) from Direct Normal:   %.4f\n", prob_direct))

if (abs(prob_z - prob_direct) < 1e-9) {
    cat("   Conclusion: The results match exactly.\n")
} else {
    cat("   Conclusion: The results are different (unexpected).\n")
}

# 3. Reflection
reflection <- "
Reflection on Normal Random Variables:
Through this analysis, I learned that normal random variables are extremely useful for modeling real-world continuous data like manufacturing dimensions. The ability to transform any normal distribution into the Standard Normal Distribution (Z-score) is powerful, as it allows for standardized comparisons across different datasets regardless of their original units or scales. Specifically, the 'middle 95% range' alignment with the 'mean +/- 1.96 SD' rule reinforces the predictable nature of the bell curve, which is essential for quality control in industries like precision engineering.
"
cat("\n3. Reflection:\n")
cat(reflection)
cat("\n")
