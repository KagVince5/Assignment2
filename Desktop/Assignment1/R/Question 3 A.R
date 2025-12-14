# Clear workspace
rm(list = ls())

# Set parameters
n_binom <- 6        # number of trials (orders)
p_success <- 0.90   # probability of success (90% on-time)

# A(i): Probability of EXACTLY 4 successes
prob_exactly_4 <- dbinom(4, size = n_binom, prob = p_success)
print(paste("P(X = 4) =", round(prob_exactly_4, 6)))

# A(ii): Probability of AT LEAST 5 successes
# P(X >= 5) = P(X > 4) = 1 - P(X <= 4)
prob_at_least_5 <- pbinom(4, size = n_binom, prob = p_success, lower.tail = FALSE)
print(paste("P(X >= 5) =", round(prob_at_least_5, 6)))

# A(iii): Create the probability distribution table
x_values <- 0:6
probabilities <- dbinom(x_values, size = n_binom, prob = p_success)
binom_table <- data.frame(X = x_values, Probability = round(probabilities, 6))
print(binom_table)

# Plot the distribution
barplot(probabilities, 
        names.arg = x_values,
        main = "Binomial Distribution (n=6, p=0.90)",
        xlab = "Number of Successful Deliveries",
        ylab = "Probability",
        col = "steelblue")


