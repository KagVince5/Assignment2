# QUESTION 5B
# Given values
p <- 0.90
n <- 100

# Standard error of the proportion
se <- sqrt(p * (1 - p) / n)

# We want to find the probability that the sample proportion is less than the population proportion by 0.06 or more.
# This can be written as P(p_hat <= p - 0.06)

# Calculate the z-score
z <- ((p - 0.06) - p) / se

# Calculate the probability
prob <- pnorm(z)

print(paste("The probability that the sample proportion is less than the population proportion by 0.06 or more is:", prob))
