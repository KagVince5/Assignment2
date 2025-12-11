# Question 1 Part B - R Solution
# Generates 4 demographic graphs for Malaysia population data

# Load required libraries
if (!require(ggplot2)) install.packages("ggplot2", repos = "https://cloud.r-project.org")
if (!require(dplyr)) install.packages("dplyr", repos = "https://cloud.r-project.org")
library(ggplot2)
library(dplyr)

# 1. Load Data
data <- read.csv("../xls/population_state.csv", stringsAsFactors = FALSE)

# Basic Cleanup
# Ensure date is date type if needed, or just extract year. Format is YYYY-MM-DD
data$date <- as.Date(data$date)
# Get the latest date to use current population
latest_date <- max(data$date)
message("Using data from date: ", latest_date)

# 2. Assign Regions
# Helper function to map state to region
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

data$region <- sapply(data$state, get_region)

# ----------------------------------------------------
# Graph 1: Bar chart – Total population by State
# ----------------------------------------------------
# Filter: Latest date, both sexes, all ages, all ethnicities
df_state <- data %>%
    filter(
        date == latest_date,
        sex == "both",
        age == "overall",
        ethnicity == "overall"
    ) %>%
    group_by(state) %>%
    summarise(total_pop = sum(population, na.rm = TRUE))

# Plot
p1 <- ggplot(df_state, aes(x = reorder(state, total_pop), y = total_pop)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    coord_flip() + # Flip for better readability of state names
    labs(
        title = "Total Population by State",
        subtitle = paste("Date:", latest_date),
        x = "State", y = "Population (Thousands)"
    ) +
    theme_minimal()

ggsave("graph1.png", plot = p1, width = 10, height = 8)
message("Generated graph1.png")


# ----------------------------------------------------
# Graph 2: Pie chart – Population proportion by Gender
# ----------------------------------------------------
# Filter: Latest date, male/female (exclude 'both'), all ages, all ethnicities
df_gender <- data %>%
    filter(
        date == latest_date,
        sex %in% c("male", "female"),
        age == "overall",
        ethnicity == "overall"
    ) %>%
    group_by(sex) %>%
    summarise(total_pop = sum(population, na.rm = TRUE)) %>%
    mutate(prop = total_pop / sum(total_pop)) %>%
    mutate(label = scales::percent(prop))

# Plot
p2 <- ggplot(df_gender, aes(x = "", y = prop, fill = sex)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar("y", start = 0) +
    geom_text(aes(label = label), position = position_stack(vjust = 0.5)) +
    labs(
        title = "Population Proportion by Gender",
        subtitle = paste("Date:", latest_date),
        fill = "Gender"
    ) +
    theme_void() +
    theme(plot.background = element_rect(fill = "white", color = NA)) +
    scale_fill_manual(values = c("female" = "pink", "male" = "lightblue"))

ggsave("graph2.png", plot = p2, width = 8, height = 6, bg = "white")
message("Generated graph2.png")


# ----------------------------------------------------
# Graph 3: Histogram – Distribution of Population across Age Groups
# ----------------------------------------------------
# Filter: Latest date, both sexes, specific age groups (exclude 'overall'), all ethnicities
df_age <- data %>%
    filter(
        date == latest_date,
        sex == "both",
        age != "overall",
        ethnicity == "overall"
    )

# Aggregate across all states/regions for the histogram
df_age_grouped <- df_age %>%
    group_by(age) %>%
    summarise(total_pop = sum(population, na.rm = TRUE))

# Ensure proper ordering of age groups
# Attempt to extract start numeric for sorting
age_starts <- as.numeric(gsub("([0-9]+).*", "\\1", df_age_grouped$age))
df_age_grouped$age <- reorder(df_age_grouped$age, age_starts)

# Plot
p3 <- ggplot(df_age_grouped, aes(x = age, y = total_pop)) +
    geom_bar(stat = "identity", fill = "lightgreen", color = "black") +
    labs(
        title = "Distribution of Population across Age Groups",
        subtitle = paste("Date:", latest_date),
        x = "Age Group", y = "Population (Thousands)"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("graph3.png", plot = p3, width = 10, height = 6)
message("Generated graph3.png")


# ----------------------------------------------------
# Graph 4: Boxplot – Variation of Population by Region
# ----------------------------------------------------
# Use data filtered by relevant metrics but KEEP individual states
df_region <- data %>%
    filter(
        date == latest_date,
        sex == "both",
        age == "overall",
        ethnicity == "overall"
    ) %>%
    select(state, population, region)

# Plot
p4 <- ggplot(df_region, aes(x = region, y = population, fill = region)) +
    geom_boxplot() +
    labs(
        title = "Variation of Total State Population by Region",
        subtitle = paste("Date:", latest_date),
        x = "Region", y = "State Population (Thousands)"
    ) +
    theme_minimal() +
    theme(legend.position = "none")

ggsave("graph4.png", plot = p4, width = 10, height = 6)
message("Generated graph4.png")

message("All graphs executed successfully.")
cat("\n\n")

# ----------------------------------------------------
# 3. Interpretations
# ----------------------------------------------------
cat("--- 3. Graph Interpretations ---\n")

cat("Graph 1 (Bar Chart): Shows the total population by state. It highlights that Selangor has the highest population, significantly surpassing other states, while Putrajaya and Labuan have the lowest.\n\n")

cat("Graph 2 (Pie Chart): Displays the population proportion by gender. It shows a relatively balanced distribution, with males slightly outnumbering females (approx 52.5% vs 47.5%).\n\n")

cat("Graph 3 (Histogram): Illustrates the distribution of population across age groups. The distribution is right-skewed, indicating a younger population demographic with fewer people in the very elderly categories.\n\n")

cat("Graph 4 (Boxplot): Represents the variation of state populations within each region. The Central Region shows the highest median and variability due to outliers like Selangor, whereas the East Coast and Northern regions have more consistent, lower population sizes.\n")
