# ============================================================
# QUESTION 2: Study Location Probability Analysis (20 marks)
# ============================================================
# Study locations 1-4, each representing 25% of study choices
# Analyzing two events:
#   - Criteria A: Getting Distracted (D)
#   - Criteria B: No Seat Available (NS)
# ============================================================

# ------------------------------------------------------------
# Part (i): Generate Random Probability Values
# ------------------------------------------------------------
cat("=== Part (i): Random Probability Values ===\n\n")

# Set seed for reproducibility
set.seed(123)

# Generate random values A1 to A4 (P(No Distraction | Location i))
# These represent probability of NOT getting distracted at each location
A <- round(runif(4, 0, 1), 2)

# Generate random values B1 to B4 (P(Seat Available | Location i))
# These represent probability of having a seat available at each location
B <- round(runif(4, 0, 1), 2)

# Display generated values
cat("Generated Values:\n")
cat("A (P(No Distraction | Location)):", A, "\n")
cat("B (P(Seat Available | Location)):", B, "\n\n")

# Calculate complements: A' and B'
# A' = P(Distraction | Location i) = P(Yes column for Criteria A)
A_prime <- round(1 - A, 2)

# B' = P(No Seat | Location i) = P(Yes column for Criteria B)
B_prime <- round(1 - B, 2)

# Display complement values
cat("Complement Values:\n")
cat("A' (P(Distraction | Location)):", A_prime, "\n")
cat("B' (P(No Seat | Location)):", B_prime, "\n\n")

# Create summary table for Part (i)
cat("Table 1: Probability for Study Location Issues\n")
cat("------------------------------------------------------------------------\n")
cat("Location | Percentage | Criteria A: Distraction | Criteria B: No Seat\n")
cat("         |            | No      | Yes (A')       | No      | Yes (B')\n")
cat("------------------------------------------------------------------------\n")
for (i in 1:4) {
    cat(sprintf(
        "   %d     |    25%%     | %.2f    | %.2f           | %.2f    | %.2f\n",
        i, A[i], A_prime[i], B[i], B_prime[i]
    ))
}
cat("------------------------------------------------------------------------\n\n")


# ------------------------------------------------------------
# Part (ii): Probability of Distraction from Each Location
# ------------------------------------------------------------
cat("=== Part (ii): Probability it Comes from Each Location (Given Distraction) ===\n\n")

# Using Bayes' Theorem:
# P(Location i | Distraction) = P(Distraction | Location i) * P(Location i) / P(Distraction)

# Calculate total probability of distraction P(D)
# P(D) = sum of [P(D|Location i) * P(Location i)] for all locations
# P(D) = sum of [A'[i] * 0.25] for i = 1 to 4
P_D <- sum(A_prime * 0.25)

cat(sprintf("Total Probability of Distraction P(D): %.4f\n\n", P_D))

# Calculate P(Location i | Distraction) using Bayes' theorem
# P(L_i | D) = [P(D | L_i) * P(L_i)] / P(D)
#            = [A'[i] * 0.25] / P_D
P_L_given_D <- round((A_prime * 0.25) / P_D, 3)

cat("Probability that distraction comes from each location:\n")
for (i in 1:4) {
    cat(sprintf(
        "  P(Location %d | Distraction) = %.3f (%.1f%%)\n",
        i, P_L_given_D[i], P_L_given_D[i] * 100
    ))
}
cat("\n")

# Verification: probabilities should sum to 1
cat(sprintf("Verification - Sum of probabilities: %.3f\n\n", sum(P_L_given_D)))


# ------------------------------------------------------------
# Part (iii): Probability of No Seat from Each Location
# ------------------------------------------------------------
cat("=== Part (iii): Probability it Comes from Each Location (Given No Seat) ===\n\n")

# Using Bayes' Theorem:
# P(Location i | No Seat) = P(No Seat | Location i) * P(Location i) / P(No Seat)

# Calculate total probability of no seat P(NS)
# P(NS) = sum of [P(NS|Location i) * P(Location i)] for all locations
# P(NS) = sum of [B'[i] * 0.25] for i = 1 to 4
P_NS <- sum(B_prime * 0.25)

cat(sprintf("Total Probability of No Seat P(NS): %.4f\n\n", P_NS))

# Calculate P(Location i | No Seat) using Bayes' theorem
# P(L_i | NS) = [P(NS | L_i) * P(L_i)] / P_NS
#             = [B'[i] * 0.25] / P_NS
P_L_given_NS <- round((B_prime * 0.25) / P_NS, 3)

cat("Probability that 'no seat' issue comes from each location:\n")
for (i in 1:4) {
    cat(sprintf(
        "  P(Location %d | No Seat) = %.3f (%.1f%%)\n",
        i, P_L_given_NS[i], P_L_given_NS[i] * 100
    ))
}
cat("\n")

# Verification: probabilities should sum to 1
cat(sprintf("Verification - Sum of probabilities: %.3f\n\n", sum(P_L_given_NS)))


# ------------------------------------------------------------
# Part (iv): Conclusion
# ------------------------------------------------------------
cat("=== Part (iv): Conclusion ===\n\n")

# Find which location is most likely for each problem
max_distraction_idx <- which.max(P_L_given_D)
max_no_seat_idx <- which.max(P_L_given_NS)

cat("CONCLUSION:\n")
cat("-----------\n\n")

cat(sprintf("1. DISTRACTION Analysis:\n"))
cat(sprintf(
    "   - Location %d has the HIGHEST probability of being the source of distraction (%.1f%%).\n",
    max_distraction_idx, P_L_given_D[max_distraction_idx] * 100
))
cat(sprintf(
    "   - This is because Location %d has a relatively high conditional probability of distraction (A'%d = %.2f).\n",
    max_distraction_idx, max_distraction_idx, A_prime[max_distraction_idx]
))
cat(sprintf(
    "   - Recommendation: AVOID studying at Location %d if you want to minimize distractions.\n\n",
    max_distraction_idx
))

cat(sprintf("2. NO SEAT Analysis:\n"))
cat(sprintf(
    "   - Location %d has the HIGHEST probability of being the source of 'no seat' issues (%.1f%%).\n",
    max_no_seat_idx, P_L_given_NS[max_no_seat_idx] * 100
))
cat(sprintf(
    "   - This is because Location %d has a relatively high conditional probability of no seat (B'%d = %.2f).\n",
    max_no_seat_idx, max_no_seat_idx, B_prime[max_no_seat_idx]
))
cat(sprintf(
    "   - Recommendation: AVOID studying at Location %d if you want to ensure seat availability.\n\n",
    max_no_seat_idx
))

# Check if same location is problematic for both issues
if (max_distraction_idx == max_no_seat_idx) {
    cat(sprintf("3. OVERALL Assessment:\n"))
    cat(sprintf(
        "   - Location %d is PROBLEMATIC for BOTH issues (distraction AND no seat).\n",
        max_distraction_idx
    ))
    cat(sprintf("   - Strong recommendation: Choose a different location for studying.\n\n"))
} else {
    cat("3. OVERALL Assessment:\n")
    cat("   - Different locations are problematic for different issues.\n")
    cat(sprintf("   - To minimize distractions: Avoid Location %d\n", max_distraction_idx))
    cat(sprintf("   - To ensure seat availability: Avoid Location %d\n", max_no_seat_idx))
    cat("   - Choose the location(s) that best fit your priorities.\n\n")
}

# Identify best location (lowest combined risk)
# Simple approach: find location with lowest sum of conditional probabilities
combined_risk <- A_prime + B_prime
best_location_idx <- which.min(combined_risk)

cat(sprintf("4. BEST LOCATION Recommendation:\n"))
cat(sprintf("   - Location %d appears to be the BEST choice overall.\n", best_location_idx))
cat(sprintf(
    "   - It has the lowest combined risk for both issues (A'%d + B'%d = %.2f).\n\n",
    best_location_idx, best_location_idx, combined_risk[best_location_idx]
))

cat("This analysis demonstrates the practical application of Bayes' Theorem in decision-making,\n")
cat("allowing us to update our beliefs about which location is responsible for issues based on\n")
cat("observed events (distraction or no seat availability).\n")
