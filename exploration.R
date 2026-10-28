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

# Step 6: Cumulative U.S. Sports Betting Handle by State

# Create a cumulative stacked area chart showing each state's
# contribution to total U.S. sports betting handle
# Use the full reporting period from January 2018 through August 2026
#
# complete() creates a row for every state and month combination
# This ensures every state shares the same monthly timeline
#
# replace_na() assigns zero to months without reported handle
# This is ONLY for plotting; the original data remains unchanged
# Missing data does not necessarily mean actual handle was zero
# Existing negative handle adjustments are preserved
#
# arrange() orders observations chronologically within each state
# group_by() groups observations by state
# cumsum() calculates the running cumulative total for each state
# ungroup() removes the grouping after calculating the cumulative values

stacked_handle_data <- sports |>
  select(state, month, handle) |>
  complete(
    state,
    month = seq(
      as.Date("2018-01-01"),
      as.Date("2026-08-01"),
      by = "month"
    )
  ) |>
  mutate(
    handle_for_plot = replace_na(handle, 0)
  ) |>
  arrange(state, month) |>
  group_by(state) |>
  mutate(
    cumulative_handle = cumsum(handle_for_plot)
  ) |>
  ungroup()

# Create the cumulative stacked area chart
# aes() maps month to x, cumulative handle to y, and state to fill
# Dividing by 1e9 converts dollars into billions
# geom_area() stacks the cumulative values for individual states
# The combined height represents cumulative reported U.S. handle
# scale_x_date() displays annual labels beginning in 2018
# labs() adds chart and axis labels
# theme_minimal() applies a clean visual style

ggplot(
  data = stacked_handle_data,
  aes(x = month, y = cumulative_handle / 1e9, fill = state)
) +
  geom_area() +
  scale_x_date(
    breaks = seq(
      as.Date("2018-01-01"),
      as.Date("2026-08-01"),
      by = "1 year"
    ),
    date_labels = "%Y",
    limits = as.Date(c("2018-01-01", "2026-08-01")),
    expand = expansion(mult = 0)
  ) +
  labs(
    title = "Cumulative U.S. Sports Betting Handle by State",
    subtitle = "January 2018 - August 2026",
    x = "Year",
    y = "Cumulative Handle ($ Billions)",
    fill = "State",
    caption = "Cumulative reported handle; missing values are not estimated."
  ) +
  theme_minimal()

# Step 7: National Monthly Sports Betting Hold Percentage

# Calculate the total reported U.S. handle and revenue for each month
# Only include state-month observations where BOTH values are available
# This ensures revenue and handle are based on the same observations
# Negative reported values are retained as potential accounting adjustments
#
# group_by(month) groups all states together for each month
# summarise() calculates national monthly totals
# n_distinct() counts the number of states included in each month
# sum() adds handle and revenue across all reporting states
# .groups = "drop" removes the grouping after summarizing
# mutate() creates the national hold percentage
# if_else() prevents division by zero or negative total handle
national_monthly_hold <- sports |>
  filter(
    !is.na(handle),
    !is.na(revenue)
  ) |>
  group_by(month) |>
  summarise(
    states_reporting = n_distinct(state),
    total_handle = sum(handle),
    total_revenue = sum(revenue),
    .groups = "drop"
  ) |>
  mutate(
    national_hold_pct = if_else(
      total_handle > 0,
      total_revenue / total_handle * 100,
      NA_real_
    )
  )

# Create a line chart showing national monthly hold over time
# aes() maps month to the x-axis and national hold to the y-axis
# geom_line() connects the monthly hold percentages
# scale_x_date() displays yearly labels beginning in January 2018
# seq() creates the dates for our x-axis labels
# limits specifies the beginning and end of the chart
# labs() adds chart and axis labels
# theme_minimal() applies a clean visual style

ggplot(
  data = national_monthly_hold,
  aes(x = month, y = national_hold_pct)
) +
  geom_line(linewidth = 1, color = "steelblue") +
  scale_x_date(
    breaks = seq(
      as.Date("2018-01-01"),
      as.Date("2026-08-01"),
      by = "1 year"
    ),
    date_labels = "%Y",
    limits = as.Date(c("2018-01-01", "2026-08-01")),
    expand = expansion(mult = 0)
  ) +
  labs(
    title = "National Monthly Sports Betting Hold Percentage",
    subtitle = "January 2018 - August 2026",
    x = "Year",
    y = "National Hold (%)",
    caption = "Based on states reporting both handle and revenue; coverage varies by month."
  ) +
  theme_minimal()

# Inspect the most recent 12 months of national hold calculations
# select() chooses the variables we want to display
# tail(12) displays the final 12 observations

national_monthly_hold |>
  select(
    month,
    states_reporting,
    total_handle,
    total_revenue,
    national_hold_pct
  ) |>
  tail(12)


# Step 8: Distribution of Monthly Hold Percentages by State

# Examine the frequency distribution of monthly hold percentages
# Each observation represents one state in one month
#
# filter() retains observations with positive handle and reported revenue
# Positive handle is required to calculate a meaningful hold percentage
# Negative revenue and negative hold percentages are retained
# hold_total was already calculated in prepare_data.R

hold_data <- sports |>
  filter(
    handle > 0,
    !is.na(revenue),
    !is.na(hold_total)
  )

# Create a histogram of monthly hold percentages
# ggplot() initializes the graph
# aes() maps hold_total to the x-axis
# geom_histogram() groups hold percentages into intervals (bins)
# binwidth = 2 creates intervals of two percentage points
# boundary = 0 aligns the bins with multiples of two
# The y-axis counts the number of state-month observations
# labs() adds chart and axis labels
# theme_minimal() applies a clean visual style
ggplot(
  data = hold_data,
  aes(x = hold_total)
) +
  geom_histogram(
    binwidth = 2,
    boundary = 0,
    color = "white"
  ) +
  labs(
    title = "Distribution of Monthly Sports Betting Hold Percentages",
    subtitle = "State-Level Observations: January 2018 - August 2026",
    x = "Monthly Hold (%)",
    y = "Frequency (State-Months)"
  ) +
  theme_minimal() +
  coord_cartesian(xlim = c(-10, 35))


# Step 9: Monthly Sports Betting Handle by State

# Select NY, NJ, PA, and MI from January 2022 through August 2026
# %in% checks whether each state is included in our selected states
# as.Date() converts the specified dates into Date objects
# !is.na(handle) removes months without reported handle
handle_data <- sports |>
  filter(
    state %in% c("NY", "NJ", "PA", "MI"),
    month >= as.Date("2022-01-01"),
    month <= as.Date("2026-08-01"),
    !is.na(handle)
  )

# Create a line chart comparing monthly handle across states
# aes() maps month to x, handle to y, and state to line color
# Dividing by 1e9 converts dollars to billions
# geom_line() connects the observations for each state
# scale_x_date() displays dates at three-month intervals
# seq() generates dates starting in January 2022
# limits sets the beginning and end of the x-axis
# expand = expansion(mult = 0) removes extra space at the ends
# labs() adds chart and axis labels
# theme_minimal() applies a clean visual style
ggplot(
  data = handle_data,
  aes(x = month, y = handle / 1e9, color = state)
) +
  geom_line(linewidth = 1) +
  scale_x_date(
    breaks = seq(
      as.Date("2022-01-01"),
      as.Date("2026-08-01"),
      by = "3 months"
    ),
    date_labels = "%b %Y",
    limits = as.Date(c("2022-01-01", "2026-08-01")),
    expand = expansion(mult = 0)
  ) +
  labs(
    title = "Monthly Sports Betting Handle: NY, NJ, PA, and MI",
    subtitle = "January 2022 - August 2026",
    x = "Month",
    y = "Handle ($ Billions)",
    color = "State"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


# Step 10: Monthly Sports Betting Revenue by State

# Select the same states and reporting period
# !is.na(revenue) removes months without reported revenue
# Negative revenue values are retained because they are valid data
revenue_data <- sports |>
  filter(
    state %in% c("NY", "NJ", "PA", "MI"),
    month >= as.Date("2022-01-01"),
    month <= as.Date("2026-08-01"),
    !is.na(revenue)
  )

# Create a line chart comparing monthly revenue across states
# Dividing revenue by 1e6 converts dollars to millions
# Use the same date intervals and graph style as Step 9
# This makes the two charts easier to compare
# seq() generates dates at three-month intervals from January 2022
# limits sets the beginning and end of the x-axis
# expand = expansion(mult = 0) removes extra space at the ends
ggplot(
  data = revenue_data,
  aes(x = month, y = revenue / 1e6, color = state)
) +
  geom_line(linewidth = 1) +
  scale_x_date(
    breaks = seq(
      as.Date("2022-01-01"),
      as.Date("2026-08-01"),
      by = "3 months"
    ),
    date_labels = "%b %Y",
    limits = as.Date(c("2022-01-01", "2026-08-01")),
    expand = expansion(mult = 0)
  ) +
  labs(
    title = "Monthly Sports Betting Revenue: NY, NJ, PA, and MI",
    subtitle = "January 2022 - August 2026",
    x = "Month",
    y = "Revenue ($ Millions)",
    color = "State"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


# Step 11: Monthly Hold Percentage by State and Year

# Compare the distribution of monthly hold percentages
# for NY, NJ, PA, and MI across different years
#
# filter() selects our four states and reporting period
# handle > 0 ensures a meaningful hold percentage
# !is.na(hold_total) removes missing hold percentages
# Negative hold percentages are retained

state_hold_data <- sports |>
  filter(
    state %in% c("NY", "NJ", "PA", "MI"),
    month >= as.Date("2022-01-01"),
    month <= as.Date("2026-08-01"),
    handle > 0,
    !is.na(hold_total)
  )

# Create boxplots of monthly hold percentages by year
# aes() maps year to x and monthly hold percentage to y
# geom_boxplot() displays the median, quartiles, and spread
# geom_jitter() displays individual monthly observations
# width = 0.1 adds slight horizontal spacing between points
# alpha = 0.4 makes individual points partially transparent
# facet_wrap() creates a separate panel for each state
# ncol = 2 arranges the four states in two columns
# labs() adds chart and axis labels
# theme_minimal() applies a clean visual style

ggplot(
  data = state_hold_data,
  aes(x = year, y = hold_total)
) +
  geom_boxplot(fill = "lightblue", outlier.shape = NA) +
  geom_jitter(width = 0.1, alpha = 0.4) +
  facet_wrap(~ state, ncol = 2) +
  labs(
    title = "Distribution of Monthly Sports Betting Hold by State and Year",
    subtitle = "NY, NJ, PA, and MI: January 2022 - August 2026",
    x = "Year",
    y = "Monthly Hold (%)",
    caption = "Each point represents one state-month; 2026 includes January-August only."
  ) +
  theme_minimal()




# Step 12: Sports Betting Taxes per Capita by State

# Import the prepared Census population dataset
# This contains annual state population estimates from 2018 through 2025
# as.character() ensures year can be matched across datasets

population_data <- read_csv("data/state_population_clean.csv") |>
  mutate(
    year = as.character(year)
  )


# Calculate annual sports betting taxes for 2018 through 2025
#
# filter() removes missing tax observations
# Exclude 2026 because we will calculate a separate LTM period
# group_by() groups observations by state and year
# summarise() calculates total taxes collected during each year
# n() counts the number of months reporting taxes
# .groups = "drop" removes the grouping after summarizing

annual_tax_data <- sports |>
  filter(
    !is.na(taxes),
    year != "2026"
  ) |>
  group_by(state, year) |>
  summarise(
    total_taxes = sum(taxes),
    months_reported = n(),
    .groups = "drop"
  ) |>
  mutate(
    year_label = as.character(year)
  ) |>
  select(state, year_label, total_taxes, months_reported)


# Calculate Last Twelve Months (LTM) taxes through August 2026
#
# The LTM period runs from September 2025 through August 2026
# Month dates in our dataset represent the last day of each month
# Use September 1, 2026 as an exclusive upper boundary
# This includes all of August 2026 without including September 2026
#
# Keep only states with all 12 months of reported tax data

ltm_tax_data <- sports |>
  filter(
    month >= as.Date("2025-09-01"),
    month < as.Date("2026-09-01"),
    !is.na(taxes)
  ) |>
  group_by(state) |>
  summarise(
    total_taxes = sum(taxes),
    months_reported = n(),
    .groups = "drop"
  ) |>
  filter(months_reported == 12) |>
  mutate(
    year_label = "2026 LTM"
  ) |>
  select(state, year_label, total_taxes, months_reported)


# Combine the annual and LTM tax summaries
#
# bind_rows() stacks the two datasets together
# Each state can have annual observations plus one LTM observation
#
# For the 2026 LTM period, use the 2025 population estimate
# population_year identifies which Census year to match
#
# left_join() matches tax data with population by state and year
# if_else() prevents division by zero or missing population
# factor() orders the years chronologically on the graph

tax_heatmap_data <- bind_rows(
  annual_tax_data,
  ltm_tax_data
) |>
  mutate(
    population_year = if_else(
      year_label == "2026 LTM",
      "2025",
      year_label
    )
  ) |>
  left_join(
    population_data,
    by = c("state", "population_year" = "year")
  ) |>
  mutate(
    taxes_per_capita = if_else(
      !is.na(population) & population > 0,
      total_taxes / population,
      NA_real_
    ),
    year_label = factor(
      year_label,
      levels = c(as.character(2018:2025), "2026 LTM")
    )
  )


# Sort states by their 2025 taxes per capita
#
# filter() selects the most recent complete calendar year
# arrange() sorts states from lowest to highest
# pull() extracts the ordered state names as a vector
# setdiff() retains states without a 2025 tax observation
# ggplot displays the last factor level at the top of the y-axis

state_order <- tax_heatmap_data |>
  filter(year_label == "2025") |>
  arrange(taxes_per_capita) |>
  pull(state)

state_order <- c(
  setdiff(sort(unique(tax_heatmap_data$state)), state_order),
  state_order
)


# Inspect the LTM results for our four comparison states
# All observations should include 12 months of tax collections

tax_heatmap_data |>
  filter(
    state %in% c("NY", "NJ", "PA", "MI"),
    year_label == "2026 LTM"
  ) |>
  select(
    state,
    year_label,
    months_reported,
    total_taxes,
    population,
    taxes_per_capita
  )


# Create the heatmap
#
# aes() maps year to x, state to y, and taxes per capita to fill
# factor() applies our state ranking based on 2025
# geom_tile() creates a colored rectangle for each state-year
# scale_fill_gradient() creates a continuous color scale
# Darker colors represent greater tax collections per resident
# labs() adds chart and axis labels
# theme_minimal() applies a clean visual style
# panel.grid = element_blank() removes unnecessary gridlines

ggplot(
  data = tax_heatmap_data,
  aes(
    x = year_label,
    y = factor(state, levels = state_order),
    fill = taxes_per_capita
  )
) +
  geom_tile(color = "white") +
  scale_fill_gradient(
    low = "lightyellow",
    high = "darkblue",
    name = "Taxes per\nResident ($)"
  ) +
  labs(
    title = "Sports Betting Taxes per Capita by State",
    subtitle = "2018-2025 Calendar Years | LTM Through August 2026",
    x = "Year",
    y = "State",
    caption = "LTM = Sep 2025-Aug 2026; uses 2025 population and requires 12 reported months. Historical years may have partial reporting."
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank()
  )


# Step 13: Top 10 States by Sports Betting Taxes per Capita

# Use the annual tax and population data created in Step 12
# Select 2025 because it is the most recent complete calendar year
# Only include states with 12 months of reported taxes
# !is.na() excludes missing per-capita tax values
#
# slice_max() selects the observations with the largest values
# order_by specifies the variable used for ranking
# n = 10 selects the 10 highest-ranking states
# with_ties = FALSE ensures exactly 10 states are selected

top_tax_states <- tax_heatmap_data |>
  filter(
    year_label == "2025",
    months_reported == 12,
    !is.na(taxes_per_capita)
  ) |>
  slice_max(
    order_by = taxes_per_capita,
    n = 10,
    with_ties = FALSE
  )

# Inspect the top 10 states and their taxes per capita
top_tax_states |>
  select(state, year_label, total_taxes, population, taxes_per_capita) |>
  arrange(desc(taxes_per_capita))


# Create a horizontal bar chart
#
# ggplot() initializes the graph
# aes() maps state to the x-axis and taxes per capita to the y-axis
# reorder() arranges states according to their taxes per capita
# geom_col() creates bars using the actual numerical values
# coord_flip() switches the axes to make the bars horizontal
# scale_y_continuous() formats the values as dollars
# labs() adds chart and axis labels
# theme_minimal() applies a clean visual style

ggplot(
  data = top_tax_states,
  aes(
    x = reorder(state, taxes_per_capita),
    y = taxes_per_capita
  )
) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  scale_y_continuous(
    labels = scales::label_dollar()
  ) +
  labs(
    title = "Top 10 States by Sports Betting Taxes per Capita",
    subtitle = "Calendar Year 2025",
    x = "State",
    y = "Annual Taxes per Resident ($)",
    caption = "Includes states with 12 reported months of taxes in 2025."
  ) +
  theme_minimal()

