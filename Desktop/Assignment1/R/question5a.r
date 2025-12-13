# QUESTION 5A
# Given values
p <- 0.90
n <- 100

# Standard error of the proportion
se <- sqrt(p * (1 - p) / n)

# We want to find the probability that the sample proportion is not within 0.02 of the population proportion.
# This can be written as P(|p_hat - p| >= 0.02), which is equivalent to:
# P(p_hat <= p - 0.02) + P(p_hat >= p + 0.02)

# Calculate the z-scores for p +/- 0.02
z_lower <- ( (p - 0.02) - p) / se
z_upper <- ( (p + 0.02) - p) / se

# Calculate the probabilities
prob_lower <- pnorm(z_lower)
prob_upper <- 1 - pnorm(z_upper)

# Total probability
total_prob <- prob_lower + prob_upper

# Since the normal distribution is symmetrical, we can also calculate it as 2 * P(Z <= z_lower)
total_prob_symmetric <- 2 * pnorm(z_lower)

print(paste("The probability that the sample proportion is not within 0.02 of the population proportion is:", total_prob_symmetric))
