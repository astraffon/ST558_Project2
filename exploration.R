# ST558 Project 2 - Exploratory Data Analysis
# Explore historical state-level sports betting data


# Step 1: Load Required Packages

# Load tidyverse for data manipulation and visualization
library(tidyverse)


# Step 2: Import the Clean Dataset

# Import the tidy dataset created in prepare_data.R
sports <- read_csv("data/sports_betting_clean.csv")

# Check the number of rows and columns
dim(sports)

# Inspect the variable names and data types
glimpse(sports)


# Step 3: Create Categorical Variables

# Create categorical variables for year and calendar month
# mutate() adds new columns to our existing dataset
# year() extracts the year from a Date
# as.factor() converts year into a categorical variable
# month() extracts the calendar month
# label = TRUE displays month names rather than numbers
# abbr = TRUE uses abbreviated month names (Jan, Feb, etc.)

sports <- sports |>
  mutate(
    year = as.factor(lubridate::year(month)),
    month_name = lubridate::month(month, label = TRUE, abbr = TRUE)
  )

# Inspect the new variables
sports |>
  select(state, month, year, month_name) |>
  head(10)

# Verify the data types
glimpse(sports)


# Step 4: Create One-Way and Two-Way Contingency Tables

# 4A: One-Way Contingency Table

# table() counts the number of observations in each category
# Count the number of monthly observations for each state
# Each observation represents one state during one month
state_counts <- table(sports$state)

# Display the number of monthly observations by state
state_counts


# 4B: Two-Way Contingency Table

# table() can also count observations across two categorical variables
# Rows represent states and columns represent years
# Each cell contains the number of reported months for that state and year
state_year_counts <- table(sports$state, sports$year)

# Display the complete contingency table
state_year_counts

# Display only Arkansas, New Jersey, and New York
# This makes the table easier to inspect
state_year_counts[c("AR", "NJ", "NY"), ]

# Step 5: Numerical Summaries by State and Year

# Group observations by state and year
# This helps distinguish market growth from historical averages
# Only include observations with reported handle
# Preserve zero and negative handle values
state_year_summary <- sports |>
  filter(!is.na(handle)) |>
  group_by(state, year) |>
  summarise(
    months_reported = n(),
    total_handle = sum(handle),
    mean_handle = mean(handle),
    median_handle = median(handle),
    sd_handle = sd(handle),
    .groups = "drop"
  )

# Inspect the annual summaries for New Jersey
state_year_summary |>
  filter(state == "NJ")

# Step 5B: Mobile vs Retail Handle by State and Year

# Use the mobile_for_split and retail_for_split variables
# created in prepare_data.R
# These account for states that operate only one channel
# Original financial values remain unchanged

# Keep months where total, mobile, and retail handle are available
# Missing values are not automatically treated as zero
# Group observations by state and year
mobile_retail_summary <- sports |>
  filter(
    !is.na(handle),
    !is.na(mobile_for_split),
    !is.na(retail_for_split)
  ) |>
  group_by(state, year) |>
  summarise(
    months_reported = n(),
    total_handle = sum(handle),
    mobile_handle = sum(mobile_for_split),
    retail_handle = sum(retail_for_split),
    .groups = "drop"
  ) |>
  mutate(
    # Calculate mobile and retail handle as percentages of total handle
    # Annual percentages use annual totals, not averages of monthly percentages
    # Return NA if total handle is zero or negative
    mobile_pct = if_else(
      total_handle > 0,
      mobile_handle / total_handle * 100,
      NA_real_
    ),
    retail_pct = if_else(
      total_handle > 0,
      retail_handle / total_handle * 100,
      NA_real_
    )
  )

# Display 2025 channel percentages for four example states
# MS: Retail only
# NC and OR: Mobile only
# NJ: Both mobile and retail

mobile_retail_summary |>
  filter(
    state %in% c("MS", "NC", "NJ", "OR"),
    year == "2025"
  ) |>
  select(
    state, year, months_reported,
    mobile_pct, retail_pct
  )


# Validate that Mobile + Retail equals Total Handle

# Mobile and retail percentages should add to approximately 100%
# Small differences in displayed percentages may be due to rounding
# Compare the combined percentage and underlying dollar difference
mobile_retail_summary |>
  filter(state == "NJ", year == "2025") |>
  mutate(
    combined_pct = round(mobile_pct + retail_pct, 4),
    handle_difference = round(
      total_handle - mobile_handle - retail_handle, 2
    )
  ) |>
  select(
    state,
    year,
    mobile_pct,
    retail_pct,
    combined_pct,
    handle_difference
  )


