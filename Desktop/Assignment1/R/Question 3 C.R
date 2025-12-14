# Parameter
lambda <- 15   # Average orders per hour

# C(i): Probability of EXACTLY 10 orders
# dpois() calculates P(X = x)
prob_exactly_10 <- dpois(10, lambda = lambda)
print(paste("P(X = 10) =", round(prob_exactly_10, 6)))

# C(ii): Probability of MORE THAN 18 orders
# P(X > 18) = 1 - P(X <= 18)
prob_more_18 <- ppois(18, lambda = lambda, lower.tail = FALSE)
print(paste("P(X > 18) =", round(prob_more_18, 6)))

# Create probability distribution table
x_pois <- 0:30
prob_pois <- dpois(x_pois, lambda = lambda)
pois_table <- data.frame(X = x_pois, Probability = round(prob_pois, 6))
print(pois_table)

# Plot the distribution
barplot(prob_pois,
        names.arg = x_pois,
        main = "Poisson Distribution (lambda = 15)",
        xlab = "Number of Orders per Hour",
        ylab = "Probability",
        col = "seagreen",
        cex.names = 0.7)
