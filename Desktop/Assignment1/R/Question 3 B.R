# Parameters
N_total <- 20       # Total population (all orders)
K_dinner <- 12      # Number of successes in population (dinner orders)
n_select <- 6       # Number of draws (orders selected)

# B(i): Probability EXACTLY 4 are dinner orders
# dhyper(x, m, n, k) where:
#   x = number of successes we want
#   m = number of success states in population (dinner orders = 12)
#   n = number of failure states in population (non-dinner = 20-12 = 8)
#   k = number of draws (selected orders = 6)
prob_4_dinner <- dhyper(4, m = K_dinner, n = N_total - K_dinner, k = n_select)
print(paste("P(X = 4) =", round(prob_4_dinner, 6)))

# B(ii): Probability AT LEAST 3 are dinner orders
# P(X >= 3) = P(X > 2) = 1 - P(X <= 2)
prob_atleast_3 <- phyper(2, m = K_dinner, n = N_total - K_dinner, k = n_select, 
                         lower.tail = FALSE)
print(paste("P(X >= 3) =", round(prob_atleast_3, 6)))

# Create probability distribution table
x_hyper <- 0:6
prob_hyper <- dhyper(x_hyper, m = K_dinner, n = N_total - K_dinner, k = n_select)
hyper_table <- data.frame(X = x_hyper, Probability = round(prob_hyper, 6))
print(hyper_table)

# Plot the distribution
barplot(prob_hyper,
        names.arg = x_hyper,
        main = "Hypergeometric Distribution (N=20, K=12, n=6)",
        xlab = "Number of Dinner Orders Selected",
        ylab = "Probability",
        col = "coral")
