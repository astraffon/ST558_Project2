
# ST558 Project 2 - Sports Betting Data Preparation
# Convert state-level monthly sports betting data into tidy format
# The original data has been collected and prepared by Alfonso Straffon

# Step 1 - Load Required Packages

# Load tidyverse for manipulating and tidying data
library(tidyverse)

# Load readxl to import the original Excel workbook
library(readxl)


# Step 2 - Import the Original Excel Data

# Import the Sports worksheet from our Excel database
# col_names = FALSE keeps the first row as data
# col_types = "text" initially imports all columns as character values
sports_raw <- read_excel(
  "data/Sports_Betting_State_Data.xlsx",
  sheet = "Sports",
  col_names = FALSE,
  col_types = "text"
)

# Check dimensions of the original dataset
dim(sports_raw)

# Inspect the structure of the first five columns
glimpse(sports_raw[, 1:5])


# Step 3 - Select Columns Through August 2026

# Columns 1 and 2 contain State and Metric
# Columns 3 through 106 contain monthly observations
# Exclude September-December 2026 and spreadsheet totals
sports_data <- sports_raw |>
  select(1:106)

# Verify dimensions
dim(sports_data)


# Step 4 - Assign Column Names

# The first row contains the actual column names
# unlist() converts the first row from a tibble into a vector
# use.names = FALSE prevents original column names,from being attached to the vector elements
names(sports_data) <- unlist(
  sports_data[1, ],
  use.names = FALSE
)


# Step 5 - Remove Header Row and Rename Variables 

# Remove the first row because it was used for column names
# Rename State and Metric using lowercase names
sports_data <- sports_data |>
  slice(-1) |>
  rename(
    state = State,
    metric = Metric
  )

# Inspect the first 10 rows and first 5 columns
sports_data[1:10, 1:5]


# Step 6 - Convert Excel Dates

# Columns 3 through 106 contain Excel date serial numbers
# as.numeric() converts the column names to numbers
# as.Date() converts these numbers into R dates
# origin specifies the starting date for Excel's date system
month_dates <- as.Date(
  as.numeric(names(sports_data)[3:106]),
  origin = "1899-12-30"
)

# Replace monthly column names with actual dates
# as.character() converts dates into valid column names
names(sports_data)[3:106] <- as.character(month_dates)

# Verify the first six column names
names(sports_data)[1:6]


# Step 7 - Convert Monthly Columns into Rows

# pivot_longer() takes the monthly columns and stacks them into rows
# Keep state and metric as identifying variables
sports_long <- sports_data |>
  pivot_longer(
    cols = -c(state, metric), # Pivot all columns EXCEPT state and metric
    names_to = "month",       # Put original date column names into a new column called month
    values_to = "amount"      # Put original financial values into a new column called amount
  )
# Inspect the first six reported Arkansas Handle observations
sports_long |>
  filter(
    state == "AR",
    metric == "Handle",
    !is.na(amount)
  ) |>
  head(6)


# Step 8 - Convert Data Types

# Convert month from character to Date
# Convert amount from character to numeric (double)
# Preserve missing values, zeros, and negative values
sports_long <- sports_long |>
  mutate(
    month = as.Date(month),
    amount = as.numeric(amount)
  )

# Verify the four columns and their data types
glimpse(sports_long)


# Step 9 - Convert Financial Metrics into Columns

# Remove rows with missing state or metric identifiers
# These correspond to extra blank rows in the Excel worksheet
# Do NOT remove rows simply because financial amounts are missing

# pivot_wider() converts each financial metric into its own column
# The remaining variables, state and month, identify observations
# Each row will represent one state during one month
sports_wide <- sports_long |>
  filter(
    !is.na(state),
    !is.na(metric)
  ) |>
  pivot_wider(
    names_from = metric,
    values_from = amount
  )

# Verify dimensions and data types
dim(sports_wide)
glimpse(sports_wide)


# Step 10 - Remove Months Before Reporting Began

# Keep a month if Handle, Revenue, OR Taxes is not missing
# The | symbol represents the logical OR operator
# is.na() tests whether a value is missing
# !is.na() tests whether a value is NOT missing
#
# Preserve zero and negative financial values
# Preserve New York revenue data before reliable handle was available
# Missing mobile/retail/promotional values remain unchanged
sports_clean <- sports_wide |>
  filter(
    !is.na(Handle) |
      !is.na(Revenue) |
      !is.na(Taxes)
  )

# Check the dimensions of the cleaned dataset
dim(sports_clean)

# Inspect the first ten observations
head(sports_clean, 10)


# Step 11 - Rename Financial Variables

# Use lowercase names and underscores instead of spaces
# Backticks are required for original names containing spaces
# or special characters
# rename() changes column names but not financial values
sports_clean <- sports_clean |>
  rename(
    handle = Handle,
    handle_mobile = `Handle Mobile`,
    handle_retail = `Handle Retail`,
    revenue = Revenue,
    revenue_mobile = `Revenue Mobile`,
    revenue_retail = `Revenue Retail`,
    taxable_revenue = `Taxable Revenue`,
    taxes = Taxes,
    promos_dedux = `Promos/Dedux`
  )

# Verify the updated column names
names(sports_clean)


# Step 12 - Calculate Hold Percentages

# Hold percentage = Revenue / Handle * 100
# Calculate hold separately for total, mobile, and retail
#
# if_else() evaluates a condition and returns one of two results
# Only calculate hold when the corresponding handle is positive
# Return NA_real_ for missing, zero, or negative handle
# Negative revenue remains valid when handle is positive
# Original financial values remain unchanged
sports_clean <- sports_clean |>
  mutate(
    hold_total = if_else(
      handle > 0,
      revenue / handle * 100,
      NA_real_
    ),
    hold_mobile = if_else(
      handle_mobile > 0,
      revenue_mobile / handle_mobile * 100,
      NA_real_
    ),
    hold_retail = if_else(
      handle_retail > 0,
      revenue_retail / handle_retail * 100,
      NA_real_
    )
  )

# Verify dimensions after adding three new variables
dim(sports_clean)

# Inspect the new hold percentage calculations
sports_clean |>
  select(
    state,
    month,
    hold_total,
    hold_mobile,
    hold_retail
  ) |>
  head(10)

# Step 13 - Export the Clean Dataset

# Save the tidied dataset as a CSV file
# write_csv() is part of the readr package in tidyverse
# Missing values (NA) are saved as blank cells
# Zeros and negative values remain unchanged
write_csv(
  sports_clean,
  "data/sports_betting_clean.csv"
)

# Verify the final dimensions of our dataset
dim(sports_clean)
