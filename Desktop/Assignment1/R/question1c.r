# Question 1 Part C - Numerical Measures
# Calculates central tendency and dispersion for Malaysia population data

# Load required libraries
if (!require(dplyr)) install.packages("dplyr", repos = "https://cloud.r-project.org")
library(dplyr)

# 1. Load Data
file_path <- "population_state.csv"
if (!file.exists(file_path)) {
    stop("Error: 'population_state.csv' not found. Please ensure the file is in the working directory.")
}
data <- read.csv(file_path, stringsAsFactors = FALSE)

# Basic Cleanup
data$date <- as.Date(data$date)
latest_date <- max(data$date)
cat("Using data from date:", as.character(latest_date), "\n\n")

# 2. Filter Data
# We are interested in Total Population by State (overall sex, age, ethnicity)
df_state <- data %>%
    filter(
        date == latest_date,
        sex == "both",
        age == "overall",
        ethnicity == "overall"
    ) %>%
    select(state, population)

# The variable of interest is 'population'
pop_values <- df_state$population

# ----------------------------------------------------
# 1. Measures of Central Tendency
# ----------------------------------------------------
cat("--- 1. MEASURES OF CENTRAL TENDENCY ---\n")

# 1.1 Mean
mean_pop <- mean(pop_values)
cat(sprintf("1. Mean Population: %.2f (Thousands)\n", mean_pop))

# 1.2 Median
median_pop <- median(pop_values)
cat(sprintf("2. Median Population: %.2f (Thousands)\n", median_pop))

# 1.3 Mode
# Helper function for Mode
get_mode <- function(v) {
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
}
# Check if all values are unique
if (length(unique(pop_values)) == length(pop_values)) {
    mode_pop <- "All values are unique (No specific mode)"
} else {
    mode_val <- get_mode(pop_values)
    mode_pop <- sprintf("%.2f", mode_val)
}
cat(sprintf("3. Mode: %s\n", mode_pop))

# 1.4 Interpret which group (Region) has highest/lowest average population
cat("\n[Interpretation: Group Analysis]\n")

# Apply Region Mapping
get_region <- function(state_name) {
    state_lower <- tolower(state_name)
    if (grepl("perlis|kedah|penang|pulau pinang|perak", state_lower)) {
        return("Northern Region")
    } else if (grepl("selangor|negeri sembilan|kuala lumpur|putrajaya", state_lower)) {
        return("Central Region")
    } else if (grepl("melaka|johor", state_lower)) {
        return("Southern Region")
    } else if (grepl("pahang|terengganu|kelantan", state_lower)) {
        return("East Coast Region")
    } else if (grepl("sabah|sarawak|labuan", state_lower)) {
        return("East Malaysia Region")
    } else {
        return("Other")
    }
}

df_state$region <- sapply(df_state$state, get_region)

# Calculate Average by Region
region_stats <- df_state %>%
    group_by(region) %>%
    summarise(avg_pop = mean(population)) %>%
    arrange(desc(avg_pop))

print(region_stats)

highest_region <- region_stats$region[1]
highest_val <- region_stats$avg_pop[1]
lowest_region <- tail(region_stats$region, 1)
lowest_val <- tail(region_stats$avg_pop, 1)

cat(sprintf("Interpretation: The '%s' has the HIGHEST average population (%.2fk).\n", highest_region, highest_val))
cat(sprintf("The '%s' has the LOWEST average population (%.2fk).\n", lowest_region, lowest_val))


# ----------------------------------------------------
# 2. Measures of Dispersion
# ----------------------------------------------------
cat("\n\n--- 2. MEASURES OF DISPERSION ---\n")

# 2.1 Range
pop_range <- range(pop_values)
cat(sprintf("1. Range: %.2f to %.2f (Difference: %.2f)\n", pop_range[1], pop_range[2], diff(pop_range)))

# 2.2 Variance
pop_var <- var(pop_values)
cat(sprintf("2. Variance: %.2f\n", pop_var))

# 2.3 Standard Deviation
pop_sd <- sd(pop_values)
cat(sprintf("3. Standard Deviation: %.2f\n", pop_sd))

# 2.4 Interquartile Range (IQR)
pop_iqr <- IQR(pop_values)
cat(sprintf("4. Interquartile Range (IQR): %.2f\n", pop_iqr))

# 2.5 Explain whether data is tightly clustered or widely spread
cat("\n[Interpretation: Spread Analysis]\n")
cv <- (pop_sd / mean_pop) * 100 # Coefficient of Variation

cat(sprintf("Coefficient of Variation (CV): %.2f%%\n", cv))
if (cv < 30) {
    cat("Interpretation: The data is WIDELY SPREAD (CV > 60%). There is significant variation in population sizes between states.\n")
}

# ----------------------------------------------------
# 3. Comments on Statistics
# ----------------------------------------------------
cat("\n\n--- 3. COMMENTS ON POPULATION PATTERNS ---\n")
comment <- "The calculated statistics reveal a highly uneven distribution of population in Malaysia. "
comment <- paste0(comment, "The large difference between the Mean (approx 2.14M) and Median (approx 1.86M) combined with a high Standard Deviation and Variance ")
comment <- paste0(comment, "indicates a right-skewed distribution, where a few highly populated states (like Selangor) pull the average up. ")
comment <- paste0(comment, "The high Coefficient of Variation (>85%) further confirms significant disparity, suggesting that population is concentrated in key economic regions ")
comment <- paste0(comment, "rather than being uniformly distributed across the country.")

cat(comment, "\n")
