# ST558 Project 2 - Exploratory Data Analysis
# Explore historical state-level sports betting data


# Step 1: Load Required Packages

# Load tidyverse for data manipulation and visualization
library(tidyverse)


# Step 2: Import the Clean Dataset

# Import the tidy dataset created in prepare_data.R
sports <- read_csv("data/sports_betting_clean.csv", show_col_types = FALSE) |>
  mutate(month = as.Date(month))

# Use the same annual Census population estimates and per-capita logic as app.R.
# This keeps the static analysis consistent with the interactive application.
# Both helpers.R and the data/ folder must be in the Project 2 repository.
source("helpers.R", local = TRUE)

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

print(
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
    x = "Reporting Month",
    y = "Cumulative Handle ($ Billions)",
    fill = "State",
    caption = "Cumulative reported handle; missing values are not estimated."
  ) +
  theme_minimal()
)

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

print(
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
    x = "Reporting Month",
    y = "National Hold (%)",
    caption = "Based on states reporting both handle and revenue; coverage varies by month."
  ) +
  theme_minimal()
)

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
print(
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
)


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
print(
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
print(
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

print(
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
)


# Step 12: State-Year Heatmap (A New Plot Type)

# Unlike the line charts, a heatmap uses color to communicate a
# quantitative value for each combination of two categorical variables.
# Here each cell contains total reported sports betting handle for a state-year.
# Blank cells indicate no reported observations, not zero betting activity.
# Annual totals for 2026 are incomplete (only January through August).
state_year_heatmap <- sports |>
  filter(is.finite(handle)) |>
  group_by(state, year) |>
  summarise(
    months_reported = n_distinct(month),
    annual_handle = sum(handle),
    .groups = "drop"
  )

# Inspect several example state-year totals before plotting.
state_year_heatmap |>
  filter(state %in% c("MI", "NJ", "NY", "PA"), year %in% c("2024", "2025")) |>
  arrange(state, year)

# A heatmap uses geom_tile(), which was not one of our introductory charts.
# The fill scale allows a comparison across all reporting states and years.
heatmap_chart <- ggplot(
  state_year_heatmap,
  aes(x = year, y = state, fill = annual_handle)
) +
  geom_tile(color = "white", linewidth = 0.2) +
  scale_fill_viridis_c(
    option = "D", labels = scales::label_dollar(scale_cut = scales::cut_short_scale())
  ) +
  labs(
    title = "U.S. Sports Betting Handle by State and Reporting Year",
    subtitle = "Calendar-year totals; 2026 is partial through August",
    x = "Reporting Year",
    y = "State",
    fill = "Annual Handle ($)",
    caption = "Blank cells indicate missing reports. Partial years are not annualized."
  ) +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(heatmap_chart)


# Step 13: Annual Sports Betting Taxes Per Capita (2025)

# Per-capita taxes = total 2025 reported taxes / 2025 state population.
# Require 12 distinct months with reported taxes for full-year comparability.
# get_per_capita_data() uses Census population data embedded in helpers.R,
# originally from NST-EST2025-ALLDATA.csv (U.S. Census Bureau).
# These are population denominators, not estimates of betting participants.
# The same helper is used by the Shiny Annual Per Capita chart.
taxes_per_capita_2025 <- get_per_capita_data(
  sports, selected_var = "taxes", selected_year = 2025L, min_months = 12L
) |>
  arrange(desc(per_capita))

# Show the exact top-ten amounts in a table.
taxes_per_capita_2025 |>
  select(state, months_reported, annual_amount, population, per_capita) |>
  slice_head(n = 10)

# Compare each state's annual sports betting taxes per resident.
taxes_per_capita_chart <- taxes_per_capita_2025 |>
  slice_head(n = 10) |>
  ggplot(aes(x = per_capita, y = reorder(state, per_capita))) +
  geom_col(fill = "steelblue") +
  scale_x_continuous(labels = scales::label_dollar()) +
  labs(
    title = "Top 10 States by Sports Betting Taxes per Capita",
    subtitle = "Calendar Year 2025 - 12 reported months required",
    x = "Annual Sports Betting Taxes per Resident ($)",
    y = "State",
    caption = "2025 U.S. Census population estimates; partial-year states excluded."
  ) +
  theme_minimal(base_size = 14)
print(taxes_per_capita_chart)


# Step 14: Annual Sports Betting Handle Per Capita (2025)

# Calculate per-resident handle using the same population estimates.
# This is market-level handle divided by population, NOT a measure of
# actual dollars bet by an average individual resident.
handle_per_capita_2025 <- get_per_capita_data(
  sports, selected_var = "handle", selected_year = 2025L, min_months = 12L
) |>
  arrange(desc(per_capita))

# Review exact top-ten dollar values.
handle_per_capita_2025 |>
  select(state, months_reported, annual_amount, population, per_capita) |>
  slice_head(n = 10)

handle_per_capita_chart <- handle_per_capita_2025 |>
  slice_head(n = 10) |>
  ggplot(aes(x = per_capita, y = reorder(state, per_capita))) +
  geom_col(fill = "steelblue") +
  scale_x_continuous(labels = scales::label_dollar()) +
  labs(
    title = "Top 10 States by Sports Betting Handle per Capita",
    subtitle = "Calendar Year 2025 - 12 reported months required",
    x = "Annual Sports Betting Handle per Resident ($)",
    y = "State",
    caption = "2025 U.S. Census population estimates; partial-year states excluded."
  ) +
  theme_minimal(base_size = 14)
print(handle_per_capita_chart)


# Step 15: Grouped Bar Chart - Mobile Versus Retail Handle Share

# Show a multivariate grouped bar chart: state, channel and handle share.
# Use annual dollar totals rather than averaging monthly percentages.
# Only states with complete reported channel splits and 12 reported months
# are included. The adjusted *_for_split columns are for composition only.
channel_mix_2025 <- mobile_retail_summary |>
  filter(
    year == "2025",
    months_reported == 12,
    state %in% c("MI", "NJ", "NY", "PA"),
    is.finite(mobile_pct),
    is.finite(retail_pct)
  ) |>
  select(state, mobile_pct, retail_pct) |>
  pivot_longer(
    cols = c(mobile_pct, retail_pct),
    names_to = "channel",
    values_to = "share_pct"
  ) |>
  mutate(
    channel = recode(
      channel,
      mobile_pct = "Mobile",
      retail_pct = "Retail"
    )
  )

channel_mix_chart <- ggplot(
  channel_mix_2025,
  aes(x = state, y = share_pct, fill = channel)
) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = scales::label_number(suffix = "%")) +
  labs(
    title = "Mobile Versus Retail Share of Sports Betting Handle",
    subtitle = "Selected states, Calendar Year 2025",
    x = "State",
    y = "Share of Annual Handle (%)",
    fill = "Channel",
    caption = "Annual handle shares use adjusted channel-split values; missing values are not assumed zero."
  ) +
  theme_minimal(base_size = 14)
print(channel_mix_chart)


# Step 16: Quick Check of the Static Exploration Requirements

# One-way and two-way tables: Step 4.
# Numerical summaries by state/year: Step 5.
# At least six plots: Steps 6-11, plus heatmap and per-capita charts.
# Multivariate plots: stacked area, colored state lines, faceted boxplots,
# heatmap, and grouped mobile/retail bar chart.
# A less familiar plot type: geom_tile() heatmap in Step 12.
# Faceting: facet_wrap() by state in Step 11.
# Both this exploration.R and helpers.R belong in the GitHub repository.

