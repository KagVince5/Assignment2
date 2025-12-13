# QUESTION 5C
# Given values
p <- 0.90
n <- 100

# Standard error of the proportion
se <- sqrt(p * (1 - p) / n)

# We want to find the probability that the sample proportion is greater than the population proportion by 0.05 or more.
# This can be written as P(p_hat >= p + 0.05)

# Calculate the z-score
z <- ((p + 0.05) - p) / se

# Calculate the probability
prob <- 1 - pnorm(z)

print(paste("The probability that the sample proportion is greater than the population proportion by 0.05 or more is:", prob))
