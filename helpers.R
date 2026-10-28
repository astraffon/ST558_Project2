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

