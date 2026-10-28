# ST558 Project 2 - Helper functions
# Sourced by app.R after loading packages and defining sports and hold_vars.


# First positive activity for a selected handle channel
first_positive_month <- function(selected_state, selected_var) {
  dates <- sports |>
    filter(state == selected_state,
           is.finite(.data[[selected_var]]),
           .data[[selected_var]] > 0) |>
    pull(month)
  if (length(dates) == 0) return(as.Date(NA))
  min(dates)
}

# Determine when a selected variable first becomes active in the original data
first_active_month <- function(selected_state, selected_var) {
  data <- sports |> filter(state == selected_state) |> arrange(month)
  if (nrow(data) == 0) return(as.Date(NA))

  positive <- function(var) is.finite(data[[var]]) & data[[var]] > 0
  values <- data[[selected_var]]
  nonzero <- is.finite(values) & values != 0

  active <- switch(
    selected_var,
    handle = positive("handle"),
    handle_mobile = positive("handle_mobile"),
    handle_retail = positive("handle_retail"),
    revenue = positive("handle") | nonzero,
    revenue_mobile = positive("handle_mobile") | nonzero,
    revenue_retail = positive("handle_retail") | nonzero,
    hold_total = positive("handle"),
    hold_mobile = positive("handle_mobile"),
    hold_retail = positive("handle_retail"),
    nonzero
  )
  active <- active & is.finite(values)
  if (!any(active)) return(as.Date(NA))
  min(data$month[active])
}

# Exclude prelaunch observations; retain true zeros and negative values thereafter
get_active_series <- function(selected_state, selected_var) {
  start <- first_active_month(selected_state, selected_var)
  if (is.na(start)) {
    return(tibble(month = as.Date(character()), value = numeric()))
  }
  sports |>
    filter(state == selected_state,
           month >= start,
           is.finite(.data[[selected_var]])) |>
    transmute(month, value = .data[[selected_var]]) |>
    arrange(month)
}

# Dynamic numerical slider for the Data & Summaries filters
make_numeric_slider <- function(selected_var, slider_id, data) {
  values <- data[[selected_var]]
  values <- values[is.finite(values)]
  if (length(values) == 0) return(helpText("No values for these selections."))

  is_hold <- selected_var %in% hold_vars
  if (is_hold) {
    lo <- floor(min(values) * 100) / 100
    hi <- ceiling(max(values) * 100) / 100
    step_size <- 0.01
  } else {
    lo <- floor(min(values))
    hi <- ceiling(max(values))
    step_size <- max(1, round((hi - lo) / 500))
  }
  if (lo == hi) hi <- lo + step_size

  sliderInput(
    inputId = slider_id, label = "Value Range",
    min = lo, max = hi, value = c(lo, hi), step = step_size,
    pre = if (is_hold) NULL else "$",
    post = if (is_hold) "%" else NULL,
    sep = ",", ticks = FALSE
  )
}

# Exact values override slider endpoints
get_filter_limits <- function(slider_limits, min_text, max_text) {
  if (length(slider_limits) != 2) return(NULL)
  parse_bound <- function(x) {
    if (is.null(x) || !nzchar(trimws(x))) return(NA_real_)
    suppressWarnings(as.numeric(gsub("[,$%[:space:]]", "", x)))
  }
  lo <- parse_bound(min_text)
  hi <- parse_bound(max_text)
  bad_min <- !is.null(min_text) && nzchar(trimws(min_text)) && !is.finite(lo)
  bad_max <- !is.null(max_text) && nzchar(trimws(max_text)) && !is.finite(hi)
  if (bad_min || bad_max) return(NULL)

  limits <- slider_limits
  if (is.finite(lo)) limits[1] <- lo
  if (is.finite(hi)) limits[2] <- hi
  if (limits[1] > limits[2]) return(NULL)
  limits
}

period_floor <- function(date, mode) {
  unit <- if (mode == "year") "year" else if (mode == "quarter") "quarter" else "month"
  lubridate::floor_date(as.Date(date), unit)
}

format_period <- function(date, mode) {
  date <- as.Date(date, origin = "1970-01-01")
  if (mode == "year") return(format(date, "%Y"))
  if (mode == "quarter") {
    return(paste0(lubridate::year(date), " Q", lubridate::quarter(date)))
  }
  format(date, "%b %Y")
}

# Build a series of reported monthly, quarterly, annual, rolling, or YTD values
# This function intentionally uses sports, not the Data & Summaries filters
get_chart_series <- function(selected_state, selected_var, chart_period) {
  raw <- get_active_series(selected_state, selected_var)
  empty <- tibble(period = as.Date(character()), value = numeric())
  if (nrow(raw) == 0) return(empty)

  # Monthly: show the reported values without aggregating
  if (chart_period == "month") {
    return(raw |> transmute(period = month, value))
  }

  is_hold <- selected_var %in% hold_vars
  if (is_hold) {
    pair <- switch(
      selected_var,
      hold_total = c("revenue", "handle"),
      hold_mobile = c("revenue_mobile", "handle_mobile"),
      hold_retail = c("revenue_retail", "handle_retail")
    )
    components <- sports |>
      filter(state == selected_state) |>
      transmute(month, rev = .data[[pair[1]]], hdl = .data[[pair[2]]])
    raw <- raw |> left_join(components, by = "month")
  }

  # An aggregated observation requires every constituent reporting month
  window_value <- function(anchor, months) {
    observed <- raw |> filter(month %in% months)
    if (nrow(observed) != length(months)) {
      return(tibble(period = as.Date(anchor), value = NA_real_))
    }
    if (is_hold) {
      if (!all(is.finite(observed$rev)) || !all(is.finite(observed$hdl))) {
        amount <- NA_real_
      } else {
        denominator <- sum(observed$hdl)
        amount <- if (denominator > 0) 100 * sum(observed$rev) / denominator else NA_real_
      }
    } else {
      amount <- sum(observed$value)
    }
    tibble(period = as.Date(anchor), value = amount)
  }

  # Full calendar quarters and calendar years only
  if (chart_period %in% c("quarter", "year")) {
    unit <- if (chart_period == "quarter") "quarter" else "year"
    count <- if (chart_period == "quarter") 3 else 12
    starts <- sort(unique(lubridate::floor_date(raw$month, unit)))
    return(purrr::map_dfr(starts, function(start) {
      months <- seq(start, by = "month", length.out = count)
      window_value(start, months)
    }))
  }

  # T3M, TTM, and YTD: each point is labeled by its ending month
  ends <- seq(min(raw$month), max(raw$month), by = "month")
  purrr::map_dfr(ends, function(end) {
    first <- switch(
      chart_period,
      t3m = seq(end, by = "-1 month", length.out = 3)[3],
      ttm = seq(end, by = "-1 month", length.out = 12)[12],
      ytd = lubridate::floor_date(end, "year")
    )
    window_value(end, seq(first, end, by = "month"))
  })
}

# Calculate change versus the matching period from one calendar year earlier
add_yoy <- function(data, selected_var, metric) {
  if (nrow(data) == 0) return(data)
  prior <- data |>
    transmute(period = lubridate::`%m+%`(period, lubridate::years(1)),
              prior_value = value)
  data |>
    left_join(prior, by = "period") |>
    mutate(value = case_when(
      !is.finite(value) | !is.finite(prior_value) ~ NA_real_,
      selected_var %in% hold_vars | metric == "change" ~ value - prior_value,
      prior_value > 0 ~ 100 * (value / prior_value - 1),
      TRUE ~ NA_real_
    )) |>
    select(period, value)
}

comparison_choices <- function(var) {
  if (var %in% hold_vars) {
    c("Reported Value" = "level", "YoY Change (percentage points)" = "change")
  } else {
    c("Reported Value" = "level", "YoY Dollar Change ($)" = "change",
      "YoY Percentage Growth (%)" = "yoy")
  }
}

# Categorical reporting labels for the exploratory tables and charts
# These describe positive reported activity, not official launch status
add_exploration_categories <- function(data) {
  data |>
    mutate(
      reporting_year = as.character(lubridate::year(month)),
      channel_status = case_when(
        is.finite(handle_mobile) & handle_mobile > 0 &
          is.finite(handle_retail) & handle_retail > 0 ~ "Retail and mobile",
        is.finite(handle_mobile) & handle_mobile > 0 ~ "Mobile only",
        is.finite(handle_retail) & handle_retail > 0 ~ "Retail only",
        TRUE ~ "Neither channel positive"
      ),
      channel_status = factor(
        channel_status,
        levels = c(
          "Retail only", "Mobile only", "Retail and mobile",
          "Neither channel positive"
        )
      )
    )
}



# Step 3: Additional exploratory chart calculations

# Build the cumulative state-level handle paths from the original EDA
# Fill unreported months with zero for display only
get_cumulative_handle_data <- function(data) {
  if (nrow(data) == 0) {
    return(tibble(
      state = character(), month = as.Date(character()),
      cumulative_handle = numeric()
    ))
  }

  data |>
    select(state, month, handle) |>
    tidyr::complete(
      state,
      month = seq(min(data$month), max(data$month), by = "month")
    ) |>
    mutate(handle_for_plot = tidyr::replace_na(handle, 0)) |>
    arrange(state, month) |>
    group_by(state) |>
    mutate(cumulative_handle = cumsum(handle_for_plot)) |>
    ungroup()
}


# Aggregate total hold only where both handle and revenue are reported
get_monthly_aggregate_hold <- function(data) {
  data |>
    filter(is.finite(handle), is.finite(revenue)) |>
    group_by(month) |>
    summarise(
      states_reporting = n_distinct(state),
      total_handle = sum(handle),
      total_revenue = sum(revenue),
      .groups = "drop"
    ) |>
    mutate(
      combined_hold_pct = if_else(
        total_handle > 0,
        100 * total_revenue / total_handle,
        NA_real_
      )
    )
}


# Mobile and retail proportions use the adjusted split columns
# The source columns are joined back without changing applied_data()
get_channel_share_data <- function(data, selected_year) {
  original_split <- sports |>
    select(state, month, mobile_for_split, retail_for_split)

  data |>
    left_join(original_split, by = c("state", "month")) |>
    filter(
      lubridate::year(month) == selected_year,
      is.finite(handle),
      is.finite(mobile_for_split),
      is.finite(retail_for_split)
    ) |>
    group_by(state) |>
    summarise(
      total_handle = sum(handle),
      mobile_handle = sum(mobile_for_split),
      retail_handle = sum(retail_for_split),
      months = n(),
      .groups = "drop"
    ) |>
    filter(total_handle > 0) |>
    transmute(
      state,
      Mobile = 100 * mobile_handle / total_handle,
      Retail = 100 * retail_handle / total_handle
    ) |>
    pivot_longer(
      cols = c(Mobile, Retail),
      names_to = "channel",
      values_to = "pct"
    )
}


# Fallback annual state population estimates from U.S. Census Vintage 2025.
# If the prepared merged CSV exists, it replaces these embedded 2020-2025 data.
# The prepared CSV joins Census Vintage 2019 (2018-2019) with
# Census Vintage 2025 (2020-2025), providing 2018-2025 coverage.
# Population estimates reflect July 1 resident population.
population_estimates <- tibble::tibble(
  state = c("AK", "AL", "AR", "AZ", "CA", "CO", "CT", "DC", "DE", "FL", "GA", "HI", "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA", "MD", "ME", "MI", "MN", "MO", "MS", "MT", "NC", "ND", "NE", "NH", "NJ", "NM", "NV", "NY", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VA", "VT", "WA", "WI", "WV", "WY"),
  `2020` = c(732906, 5032962, 3014399, 7186647, 39527808, 5786873, 3579643, 670958, 991890, 21591325, 10732595, 1451130, 3191081, 1849328, 12795348, 6790290, 2938274, 4508271, 4652145, 6985320, 6178387, 1364546, 10072294, 5710562, 6155074, 2958385, 1087156, 10450215, 779612, 1963318, 1378752, 9270476, 2118446, 3116851, 20122262, 11798534, 3965243, 4243544, 12995576, 1096487, 5131992, 887808, 6927736, 29237895, 3283970, 8637303, 642965, 7726812, 5897371, 1791670, 577669),
  `2021` = c(734590, 5050058, 3027127, 7274022, 39152927, 5814368, 3607765, 669637, 1005130, 21836698, 10793038, 1446909, 3199037, 1904855, 12703354, 6814223, 2938966, 4508348, 4628595, 6996721, 6176035, 1379009, 10042362, 5714055, 6172551, 2947603, 1106416, 10565503, 777977, 1964697, 1387907, 9266509, 2117300, 3148472, 19835345, 11761443, 3992760, 4254691, 13016628, 1097381, 5194346, 896303, 6966687, 29572672, 3339874, 8660007, 647238, 7744316, 5882128, 1785921, 579662),
  `2022` = c(733659, 5076868, 3047429, 7370065, 39125347, 5853355, 3618707, 674501, 1020279, 22413989, 10929992, 1437812, 3201890, 1942951, 12609577, 6841491, 2938034, 4521238, 4600892, 7025465, 6183257, 1391585, 10043906, 5727565, 6179285, 2941237, 1121533, 10705768, 780191, 1972595, 1396647, 9298402, 2112859, 3177672, 19713025, 11764950, 4025327, 4245964, 12984655, 1098275, 5288957, 909156, 7063325, 30118002, 3393928, 8681038, 647614, 7786828, 5903472, 1774003, 581742),
  `2023` = c(734654, 5117850, 3069856, 7452073, 39181667, 5912240, 3641369, 682559, 1035354, 22929248, 11063669, 1434950, 3214739, 1970497, 12633389, 6881373, 2949749, 4549953, 4598777, 7073479, 6204927, 1401992, 10062124, 5752998, 6205956, 2942470, 1130612, 10871849, 787071, 1988172, 1401530, 9395315, 2119349, 3211077, 19786543, 11808390, 4062716, 4250392, 13009406, 1101183, 5390798, 917739, 7153029, 30719247, 3449259, 8732034, 647722, 7838655, 5930350, 1770036, 584666),
  `2024` = c(736537, 5163055, 3096080, 7556424, 39364774, 5988502, 3674449, 691310, 1050123, 23265838, 11204208, 1434952, 3230454, 2000872, 12703033, 6934754, 2965252, 4584046, 4614878, 7138560, 6245314, 1408438, 10099962, 5797405, 6243544, 2950172, 1137557, 11052061, 793387, 2005591, 1408518, 9506354, 2126774, 3253543, 20001419, 11860621, 4097758, 4265324, 13045848, 1110415, 5490316, 927110, 7251291, 31318578, 3502983, 8819642, 646521, 7927958, 5957168, 1767402, 586722),
  `2025` = c(737270, 5193088, 3114791, 7623818, 39355309, 6012561, 3688496, 693645, 1059952, 23462518, 11302748, 1432820, 3238387, 2029733, 12719141, 6973333, 2977220, 4606864, 4618189, 7154084, 6265347, 1414874, 10127884, 5830405, 6270541, 2954160, 1144694, 11197968, 799358, 2018006, 1415342, 9548215, 2125498, 3282188, 20002427, 11900510, 4123288, 4273586, 13059432, 1114521, 5570274, 935094, 7315076, 31709821, 3538904, 8880107, 644663, 8001020, 5972787, 1766147, 588753)
) |>
  tidyr::pivot_longer(
    cols = -state, names_to = "year", values_to = "population"
  ) |>
  dplyr::mutate(year = as.integer(year))


# Use the project-prepared Census population file when it is present.
# Expected columns: state (postal abbreviation), year, population.
# We preserve the embedded 2020-2025 estimates as a fallback so that the
# previously working app remains usable if the merged CSV is not yet copied.
population_file <- "data/state_population_clean.csv"
if (file.exists(population_file)) {
  population_merged <- readr::read_csv(population_file, show_col_types = FALSE)

  # Support either state or state_abbr in the user's prepared population file.
  if (!"state" %in% names(population_merged) &&
      "state_abbr" %in% names(population_merged)) {
    population_merged <- dplyr::rename(population_merged, state = state_abbr)
  }

  needed_columns <- c("state", "year", "population")
  if (!all(needed_columns %in% names(population_merged))) {
    stop("data/state_population_clean.csv must have state, year, population columns.")
  }

  population_merged <- population_merged |>
    dplyr::transmute(
      state = trimws(as.character(state)),
      year = as.integer(year),
      population = as.numeric(population)
    )

  if (anyNA(population_merged) ||
      any(population_merged$population <= 0) ||
      anyDuplicated(population_merged[c("state", "year")])) {
    stop("The merged population file has missing, nonpositive, or duplicate state-year values.")
  }

  population_estimates <- population_merged
}

# Used in the About tab, per-capita year selector, and chart help text.
population_coverage <- paste0(
  min(population_estimates$year), "–", max(population_estimates$year)
)


# Summarise eligible annual reported amounts per resident, using matched years
get_per_capita_data <- function(data, selected_var, selected_year,
                                min_months = 12L) {
  data |>
    filter(
      lubridate::year(month) == selected_year,
      is.finite(.data[[selected_var]])
    ) |>
    group_by(state) |>
    summarise(
      months_reported = n_distinct(month),
      annual_amount = sum(.data[[selected_var]]),
      .groups = "drop"
    ) |>
    left_join(
      population_estimates |> filter(year == selected_year),
      by = "state"
    ) |>
    filter(
      months_reported >= min_months,
      is.finite(population), population > 0
    ) |>
    mutate(per_capita = annual_amount / population)
}
