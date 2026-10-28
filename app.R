
# ST558 Project 2 - U.S. Sports Betting Explorer

# Step 1: Load packages and data
library(shiny)
library(tidyverse)
library(DT)
library(plotly)

sports <- readr::read_csv("data/sports_betting_clean.csv", show_col_types = FALSE) |>
  mutate(month = as.Date(month))

sports_download <- sports |> select(-ends_with("_for_split"))
state_choices <- sort(unique(sports$state))

numeric_vars <- c(
  "Total Handle ($)" = "handle",
  "Mobile Handle ($)" = "handle_mobile",
  "Retail Handle ($)" = "handle_retail",
  "Total Revenue ($)" = "revenue",
  "Mobile Revenue ($)" = "revenue_mobile",
  "Retail Revenue ($)" = "revenue_retail",
  "Taxable Revenue ($)" = "taxable_revenue",
  "Taxes ($)" = "taxes",
  "Promotional Deductions ($)" = "promos_dedux",
  "Total Hold (%)" = "hold_total",
  "Mobile Hold (%)" = "hold_mobile",
  "Retail Hold (%)" = "hold_retail"
)

hold_vars <- c("hold_total", "hold_mobile", "hold_retail")

period_choices <- c(
  "Monthly" = "month",
  "Calendar Quarter" = "quarter",
  "Calendar Year" = "year",
  "Trailing 3 Months (T3M)" = "t3m",
  "Trailing 12 Months (TTM)" = "ttm",
  "Year-to-Date (YTD)" = "ytd"
)

period_labels <- c(
  month = "Monthly",
  quarter = "Quarterly",
  year = "Annual",
  t3m = "Trailing 3 Months",
  ttm = "Trailing 12 Months",
  ytd = "YTD"
)


# Step 2: Helper functions

# First positive activity for a selected handle channel
first_positive_month <- function(selected_state, selected_var) {
  
  dates <- sports |>
    filter(
      state == selected_state,
      is.finite(.data[[selected_var]]),
      .data[[selected_var]] > 0
    ) |>
    pull(month)
  
  if (length(dates) == 0) {
    return(as.Date(NA))
  }
  
  min(dates)
}


# Determine when a selected variable first becomes active
first_active_month <- function(selected_state, selected_var) {
  
  data <- sports |>
    filter(state == selected_state) |>
    arrange(month)
  
  if (nrow(data) == 0) {
    return(as.Date(NA))
  }
  
  positive <- function(var) {
    is.finite(data[[var]]) & data[[var]] > 0
  }
  
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
  
  if (!any(active)) {
    return(as.Date(NA))
  }
  
  min(data$month[active])
}


# Exclude prelaunch observations
# Retain true zeros and negative values thereafter
get_active_series <- function(selected_state, selected_var) {
  
  start <- first_active_month(
    selected_state,
    selected_var
  )
  
  if (is.na(start)) {
    return(tibble(
      month = as.Date(character()),
      value = numeric()
    ))
  }
  
  sports |>
    filter(
      state == selected_state,
      month >= start,
      is.finite(.data[[selected_var]])
    ) |>
    transmute(
      month,
      value = .data[[selected_var]]
    ) |>
    arrange(month)
}


# Dynamic numerical slider for the Data & Summaries filters
make_numeric_slider <- function(selected_var, slider_id, data) {
  
  values <- data[[selected_var]]
  values <- values[is.finite(values)]
  
  if (length(values) == 0) {
    return(helpText("No values for these selections."))
  }
  
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
  
  if (lo == hi) {
    hi <- lo + step_size
  }
  
  sliderInput(
    inputId = slider_id,
    label = "Value Range",
    min = lo,
    max = hi,
    value = c(lo, hi),
    step = step_size,
    pre = if (is_hold) NULL else "$",
    post = if (is_hold) "%" else NULL,
    sep = ",",
    ticks = FALSE
  )
}


# Exact values override slider endpoints
get_filter_limits <- function(slider_limits, min_text, max_text) {
  
  if (length(slider_limits) != 2) {
    return(NULL)
  }
  
  parse_bound <- function(x) {
    
    if (is.null(x) || !nzchar(trimws(x))) {
      return(NA_real_)
    }
    
    suppressWarnings(
      as.numeric(gsub("[,$%[:space:]]", "", x))
    )
  }
  
  lo <- parse_bound(min_text)
  hi <- parse_bound(max_text)
  
  bad_min <- !is.null(min_text) &&
    nzchar(trimws(min_text)) &&
    !is.finite(lo)
  
  bad_max <- !is.null(max_text) &&
    nzchar(trimws(max_text)) &&
    !is.finite(hi)
  
  if (bad_min || bad_max) {
    return(NULL)
  }
  
  limits <- slider_limits
  
  if (is.finite(lo)) {
    limits[1] <- lo
  }
  
  if (is.finite(hi)) {
    limits[2] <- hi
  }
  
  if (limits[1] > limits[2]) {
    return(NULL)
  }
  
  limits
}


# Convert date to the beginning of its reporting period
period_floor <- function(date, mode) {
  
  unit <- if (mode == "year") {
    "year"
  } else if (mode == "quarter") {
    "quarter"
  } else {
    "month"
  }
  
  lubridate::floor_date(as.Date(date), unit)
}


# Format reporting periods
format_period <- function(date, mode) {
  
  date <- as.Date(date, origin = "1970-01-01")
  
  if (mode == "year") {
    return(format(date, "%Y"))
  }
  
  if (mode == "quarter") {
    return(paste0(
      lubridate::year(date),
      " Q",
      lubridate::quarter(date)
    ))
  }
  
  format(date, "%b %Y")
}


# Build monthly, quarterly, annual, rolling, or YTD values
# Charts use the original dataset, not Data & Summaries filters
get_chart_series <- function(
    selected_state,
    selected_var,
    chart_period
) {
  
  raw <- get_active_series(
    selected_state,
    selected_var
  )
  
  empty <- tibble(
    period = as.Date(character()),
    value = numeric()
  )
  
  if (nrow(raw) == 0) {
    return(empty)
  }
  
  # Monthly values retain their original reported amounts
  if (chart_period == "month") {
    
    return(raw |>
             transmute(
               period = month,
               value
             ))
  }
  
  is_hold <- selected_var %in% hold_vars
  
  # For aggregate hold, use revenue divided by handle
  if (is_hold) {
    
    pair <- switch(
      selected_var,
      
      hold_total = c("revenue", "handle"),
      hold_mobile = c("revenue_mobile", "handle_mobile"),
      hold_retail = c("revenue_retail", "handle_retail")
    )
    
    components <- sports |>
      filter(state == selected_state) |>
      transmute(
        month,
        rev = .data[[pair[1]]],
        hdl = .data[[pair[2]]]
      )
    
    raw <- raw |>
      left_join(components, by = "month")
  }
  
  
  # Aggregate a full window of reporting months
  window_value <- function(anchor, months) {
    
    observed <- raw |>
      filter(month %in% months)
    
    # Require all constituent months
    if (nrow(observed) != length(months)) {
      
      return(tibble(
        period = as.Date(anchor),
        value = NA_real_
      ))
    }
    
    if (is_hold) {
      
      if (
        !all(is.finite(observed$rev)) ||
        !all(is.finite(observed$hdl))
      ) {
        
        amount <- NA_real_
        
      } else {
        
        denominator <- sum(observed$hdl)
        
        amount <- if (denominator > 0) {
          100 * sum(observed$rev) / denominator
        } else {
          NA_real_
        }
      }
      
    } else {
      
      amount <- sum(observed$value)
    }
    
    tibble(
      period = as.Date(anchor),
      value = amount
    )
  }
  
  
  # Calendar quarters and calendar years
  if (chart_period %in% c("quarter", "year")) {
    
    unit <- if (chart_period == "quarter") {
      "quarter"
    } else {
      "year"
    }
    
    count <- if (chart_period == "quarter") 3 else 12
    
    starts <- sort(unique(
      lubridate::floor_date(raw$month, unit)
    ))
    
    return(
      purrr::map_dfr(starts, function(start) {
        
        months <- seq(
          start,
          by = "month",
          length.out = count
        )
        
        window_value(start, months)
      })
    )
  }
  
  
  # Trailing 3M, Trailing 12M, and YTD
  # Use base R date sequences to avoid lubridate::months()
  ends <- seq(
    min(raw$month),
    max(raw$month),
    by = "month"
  )
  
  purrr::map_dfr(ends, function(end) {
    
    first <- switch(
      chart_period,
      
      # Three months including the current month
      t3m = seq(
        end,
        by = "-1 month",
        length.out = 3
      )[3],
      
      # Twelve months including the current month
      ttm = seq(
        end,
        by = "-1 month",
        length.out = 12
      )[12],
      
      # January through the current month
      ytd = lubridate::floor_date(
        end,
        "year"
      )
    )
    
    window_value(
      end,
      seq(first, end, by = "month")
    )
  })
}


# Calculate change against the equivalent prior-year period
add_yoy <- function(data, selected_var, metric) {
  
  if (nrow(data) == 0) {
    return(data)
  }
  
  prior <- data |>
    transmute(
      period = lubridate::`%m+%`(
        period,
        lubridate::years(1)
      ),
      prior_value = value
    )
  
  data |>
    left_join(prior, by = "period") |>
    mutate(
      value = case_when(
        
        !is.finite(value) |
          !is.finite(prior_value) ~ NA_real_,
        
        selected_var %in% hold_vars |
          metric == "change" ~ value - prior_value,
        
        prior_value > 0 ~
          100 * (value / prior_value - 1),
        
        TRUE ~ NA_real_
      )
    ) |>
    select(period, value)
}


# Select valid YoY calculation methods
comparison_choices <- function(var) {
  
  if (var %in% hold_vars) {
    
    c(
      "Reported Value" = "level",
      "YoY Change (percentage points)" = "change"
    )
    
  } else {
    
    c(
      "Reported Value" = "level",
      "YoY Dollar Change ($)" = "change",
      "YoY Percentage Growth (%)" = "yoy"
    )
  }
}


# Step 3: Define the UI

ui <- fluidPage(
  
  titlePanel("U.S. Sports Betting Explorer"),
  
  tabsetPanel(
    
    # About tab
    tabPanel(
      "About",
      
      h3("About This Application"),
      
      p(
        "Explore historical U.S. sports betting data,",
        "including handle, revenue, taxes, and hold percentages."
      ),
      
      p(
        "The dataset contains monthly state-level observations",
        "from 2018 through August 2026."
      ),
      
      p(
        "Data & Summaries has shared filters for the data",
        "table and numerical summaries."
      ),
      
      p(
        "Correlation and Time Series have independent controls",
        "under Data Exploration."
      )
    ),
    
    
    # Data Table and Numerical Summaries share these filters
    tabPanel(
      "Data & Summaries",
      
      sidebarLayout(
        
        sidebarPanel(
          
          h3("Dataset Filters"),
          
          # Categorical filter 1
          selectizeInput(
            "filter_states",
            "States",
            choices = c(
              "All States" = "ALL",
              state_choices
            ),
            selected = "ALL",
            multiple = TRUE
          ),
          
          # Categorical filter 2
          selectizeInput(
            "filter_years",
            "Reporting Years",
            choices = c(
              "All Years" = "ALL",
              as.character(sort(unique(
                lubridate::year(sports$month)
              )))
            ),
            selected = "ALL",
            multiple = TRUE
          ),
          
          helpText(
            "Remove 'All States' or 'All Years' to select",
            "specific values."
          ),
          
          
          # Numerical Filter 1
          h4("Numerical Filter 1"),
          
          checkboxInput(
            "filter_use_num1",
            "Enable Filter 1",
            FALSE
          ),
          
          conditionalPanel(
            condition = "input.filter_use_num1",
            
            selectInput(
              "filter_num1",
              "Variable",
              numeric_vars,
              selected = "handle"
            ),
            
            uiOutput("filter_slider1"),
            
            fluidRow(
              
              column(
                6,
                textInput(
                  "filter_min1",
                  "Exact minimum (optional)",
                  value = ""
                )
              ),
              
              column(
                6,
                textInput(
                  "filter_max1",
                  "Exact maximum (optional)",
                  value = ""
                )
              )
            )
          ),
          
          
          # Numerical Filter 2
          h4("Numerical Filter 2"),
          
          checkboxInput(
            "filter_use_num2",
            "Enable Filter 2",
            FALSE
          ),
          
          conditionalPanel(
            condition = "input.filter_use_num2",
            
            selectInput(
              "filter_num2",
              "Variable",
              numeric_vars,
              selected = "revenue"
            ),
            
            uiOutput("filter_slider2"),
            
            fluidRow(
              
              column(
                6,
                textInput(
                  "filter_min2",
                  "Exact minimum (optional)",
                  value = ""
                )
              ),
              
              column(
                6,
                textInput(
                  "filter_max2",
                  "Exact maximum (optional)",
                  value = ""
                )
              )
            )
          ),
          
          helpText(
            "Exact values override their corresponding slider endpoints."
          ),
          
          actionButton(
            "apply_filters",
            "Apply Filters"
          ),
          
          helpText(
            "Only the Data Table and Numerical Summaries",
            "use these filters."
          )
        ),
        
        
        mainPanel(
          
          h3("Data and Summary Statistics"),
          
          textOutput("filter_summary"),
          
          textOutput("applied_filter_description"),
          
          tabsetPanel(
            
            # Data Table
            tabPanel(
              "Data Table",
              
              DT::DTOutput("sports_table"),
              
              downloadButton(
                "download_data",
                "Download Data (CSV)"
              )
            ),
            
            
            # Numerical Summaries
            tabPanel(
              "Numerical Summaries",
              
              selectInput(
                "summary_var",
                "Summary Variable",
                numeric_vars,
                selected = "handle"
              ),
              
              radioButtons(
                "summary_mode",
                "Calculation Method",
                choices = c(
                  "Individual state-month observations" = "individual",
                  "Combined monthly totals across states" = "monthly"
                ),
                selected = "individual"
              ),
              
              helpText(
                "Combined monthly totals are for dollar variables only."
              ),
              
              selectInput(
                "summary_group",
                "Group Results By",
                choices = c(
                  "State" = "state",
                  "Reporting Year" = "year",
                  "State and Reporting Year" = "both",
                  "All Selected Data" = "overall"
                ),
                selected = "state"
              ),
              
              textOutput("summary_count"),
              
              DT::DTOutput("summary_table"),
              
              uiOutput("summary_note")
            )
          )
        )
      )
    ),
    
    
    # Data Exploration has independent chart controls
    tabPanel(
      "Data Exploration",
      
      tabsetPanel(
        
        # Correlation
        tabPanel(
          "Correlation",
          
          sidebarLayout(
            
            sidebarPanel(
              
              h3("Correlation Analysis"),
              
              h4("X-Axis"),
              
              selectInput(
                "corr_state_x",
                "State",
                state_choices,
                selected = "NY"
              ),
              
              selectInput(
                "corr_x",
                "Variable",
                numeric_vars,
                selected = "handle"
              ),
              
              h4("Y-Axis"),
              
              selectInput(
                "corr_state_y",
                "State",
                state_choices,
                selected = "NJ"
              ),
              
              selectInput(
                "corr_y",
                "Variable",
                numeric_vars,
                selected = "handle"
              ),
              
              selectInput(
                "corr_period",
                "Reporting Period",
                period_choices,
                selected = "month"
              ),
              
              selectInput(
                "corr_view",
                "Display Metric",
                comparison_choices("handle"),
                selected = "level"
              ),
              
              dateRangeInput(
                "corr_dates",
                "Date Range",
                start = min(sports$month),
                end = max(sports$month),
                min = min(sports$month),
                max = max(sports$month),
                format = "M yyyy",
                separator = " to "
              ),
              
              textOutput("available_period"),
              
              actionButton(
                "run_corr",
                "Run Correlation"
              )
            ),
            
            mainPanel(
              
              h3("Correlation Between Selected Variables"),
              
              plotOutput(
                "corr_plot",
                height = "450px"
              ),
              
              textOutput("corr_result"),
              
              uiOutput("market_note")
            )
          )
        ),
        
        
        # Time Series
        tabPanel(
          "Time Series",
          
          sidebarLayout(
            
            sidebarPanel(
              
              h3("Time Series Analysis"),
              
              selectizeInput(
                "ts_states",
                "States",
                state_choices,
                selected = "NY",
                multiple = TRUE
              ),
              
              helpText(
                "Click inside the box to add more states."
              ),
              
              selectInput(
                "ts_var",
                "Variable",
                numeric_vars,
                selected = "handle"
              ),
              
              selectInput(
                "ts_period",
                "Reporting Period",
                period_choices,
                selected = "month"
              ),
              
              selectInput(
                "ts_view",
                "Display Metric",
                comparison_choices("handle"),
                selected = "level"
              ),
              
              dateRangeInput(
                "ts_dates",
                "Date Range",
                start = min(sports$month),
                end = max(sports$month),
                min = min(sports$month),
                max = max(sports$month),
                format = "M yyyy",
                separator = " to "
              ),
              
              textOutput("ts_available_period"),
              
              selectInput(
                "ts_display",
                "Chart Layout",
                c(
                  "Shared scale",
                  "Separate panels"
                ),
                selected = "Shared scale"
              ),
              
              actionButton(
                "run_ts",
                "Update Chart"
              )
            ),
            
            mainPanel(
              
              h3("Sports Betting Trends"),
              
              plotlyOutput(
                "ts_plot",
                height = "500px"
              ),
              
              uiOutput("ts_context")
            )
          )
        )
      )
    )
  )
)


# Step 4: Define the server

server <- function(input, output, session) {
  
  
  # Data & Summaries Filters
  # These do not affect Correlation or Time Series
  
  state_year_data <- reactive({
    
    filtered <- sports_download
    
    if (
      length(input$filter_states) > 0 &&
      !("ALL" %in% input$filter_states)
    ) {
      
      filtered <- filtered |>
        filter(state %in% input$filter_states)
    }
    
    if (
      length(input$filter_years) > 0 &&
      !("ALL" %in% input$filter_years)
    ) {
      
      filtered <- filtered |>
        filter(
          lubridate::year(month) %in%
            as.integer(input$filter_years)
        )
    }
    
    filtered
  })
  
  
  # Numerical Filter 1 slider
  output$filter_slider1 <- renderUI({
    
    if (!isTRUE(input$filter_use_num1)) {
      return(NULL)
    }
    
    req(input$filter_num1)
    
    make_numeric_slider(
      input$filter_num1,
      "filter_range1",
      state_year_data()
    )
  })
  
  
  # Numerical Filter 2 slider
  output$filter_slider2 <- renderUI({
    
    if (!isTRUE(input$filter_use_num2)) {
      return(NULL)
    }
    
    req(input$filter_num2)
    
    make_numeric_slider(
      input$filter_num2,
      "filter_range2",
      state_year_data()
    )
  })
  
  
  # Store applied filter results
  applied_data <- reactiveVal(sports_download)
  
  applied_description <- reactiveVal(
    "All States | All Years | No numerical filters"
  )
  
  
  # Apply button controls the table and numerical summaries
  observeEvent(input$apply_filters, {
    
    filtered <- state_year_data()
    
    state_label <- if (
      length(input$filter_states) == 0 ||
      "ALL" %in% input$filter_states
    ) {
      "All States"
    } else {
      paste(input$filter_states, collapse = ", ")
    }
    
    year_label <- if (
      length(input$filter_years) == 0 ||
      "ALL" %in% input$filter_years
    ) {
      "All Years"
    } else {
      paste(input$filter_years, collapse = ", ")
    }
    
    notes <- character()
    
    
    # Apply both optional numerical filters
    for (i in 1:2) {
      
      if (!isTRUE(input[[paste0("filter_use_num", i)]])) {
        next
      }
      
      selected_var <- input[[paste0("filter_num", i)]]
      
      limits <- get_filter_limits(
        input[[paste0("filter_range", i)]],
        input[[paste0("filter_min", i)]],
        input[[paste0("filter_max", i)]]
      )
      
      if (is.null(limits)) {
        
        showNotification(
          paste("Check the range for Filter", i),
          type = "warning"
        )
        
        return()
      }
      
      filtered <- filtered |>
        filter(
          is.finite(.data[[selected_var]]),
          .data[[selected_var]] >= limits[1],
          .data[[selected_var]] <= limits[2]
        )
      
      notes <- c(
        notes,
        paste0(
          names(numeric_vars)[numeric_vars == selected_var],
          ": ",
          format(
            limits[1],
            big.mark = ",",
            scientific = FALSE
          ),
          " to ",
          format(
            limits[2],
            big.mark = ",",
            scientific = FALSE
          )
        )
      )
    }
    
    
    # Save the filtered dataset and its description
    applied_data(filtered)
    
    applied_description(
      paste(
        c(
          state_label,
          year_label,
          if (length(notes) == 0) {
            "No numerical filters"
          } else {
            notes
          }
        ),
        collapse = " | "
      )
    )
    
  }, ignoreInit = TRUE)
  
  
  # Display applied filter information
  output$filter_summary <- renderText({
    
    paste0(
      "Showing ",
      format(nrow(applied_data()), big.mark = ","),
      " of ",
      format(nrow(sports_download), big.mark = ","),
      " observations."
    )
  })
  
  
  output$applied_filter_description <- renderText({
    
    paste(
      "Applied filters:",
      applied_description()
    )
  })
  
  
  # Data Table
  output$sports_table <- DT::renderDT({
    
    numeric_cols <- names(applied_data())[
      vapply(
        applied_data(),
        is.numeric,
        logical(1)
      )
    ]
    
    DT::datatable(
      applied_data(),
      rownames = FALSE,
      options = list(
        pageLength = 10,
        scrollX = TRUE
      )
    ) |>
      DT::formatRound(
        columns = setdiff(numeric_cols, hold_vars),
        digits = 2
      ) |>
      DT::formatCurrency(
        columns = hold_vars,
        currency = "%",
        digits = 2,
        before = FALSE
      )
  })
  
  
  # Download the filtered observations
  output$download_data <- downloadHandler(
    
    filename = function() {
      "sports_betting_data.csv"
    },
    
    content = function(file) {
      readr::write_csv(
        applied_data(),
        file
      )
    }
  )
  
  
  # Numerical Summaries
  
  # Hold percentages cannot be combined by summing percentages
  observeEvent(input$summary_var, {
    
    is_hold <- input$summary_var %in% hold_vars
    
    choices <- if (is_hold) {
      
      c(
        "Individual state-month observations" = "individual"
      )
      
    } else {
      
      c(
        "Individual state-month observations" = "individual",
        "Combined monthly totals across states" = "monthly"
      )
    }
    
    current <- isolate(input$summary_mode)
    
    selected <- if (is_hold) {
      "individual"
    } else if (identical(current, "monthly")) {
      "monthly"
    } else {
      "individual"
    }
    
    updateRadioButtons(
      session,
      "summary_mode",
      choices = choices,
      selected = selected
    )
  })
  
  
  # Grouping options depend on the calculation method
  observeEvent(input$summary_mode, {
    
    is_monthly <- identical(
      input$summary_mode,
      "monthly"
    )
    
    choices <- if (is_monthly) {
      
      c(
        "Reporting Year" = "year",
        "All Selected Data" = "overall"
      )
      
    } else {
      
      c(
        "State" = "state",
        "Reporting Year" = "year",
        "State and Reporting Year" = "both",
        "All Selected Data" = "overall"
      )
    }
    
    current <- isolate(input$summary_group)
    
    selected <- if (current %in% unname(choices)) {
      current
    } else if (is_monthly) {
      "year"
    } else {
      "state"
    }
    
    updateSelectInput(
      session,
      "summary_group",
      choices = choices,
      selected = selected
    )
  })
  
  
  # Calculate numerical summaries
  summary_data <- reactive({
    
    req(
      input$summary_var,
      input$summary_mode,
      input$summary_group
    )
    
    filtered <- applied_data()
    
    validate(
      need(
        nrow(filtered) > 0,
        "No observations match the applied filters."
      )
    )
    
    is_hold <- input$summary_var %in% hold_vars
    
    is_monthly <- identical(
      input$summary_mode,
      "monthly"
    )
    
    validate(
      need(
        !(is_hold && is_monthly),
        "Combined monthly totals are only available for dollar variables."
      )
    )
    
    validate(
      need(
        !is_monthly ||
          input$summary_group %in% c("year", "overall"),
        "Choose Reporting Year or All Selected Data."
      )
    )
    
    
    # First active reporting month for each state
    starts <- purrr::map_dfr(
      unique(filtered$state),
      function(s) {
        
        tibble(
          state = s,
          first_month = first_active_month(
            s,
            input$summary_var
          )
        )
      }
    )
    
    
    # Keep observations after activity begins
    selected <- filtered |>
      left_join(starts, by = "state") |>
      filter(
        !is.na(first_month),
        month >= first_month
      ) |>
      mutate(
        reporting_year = lubridate::year(month),
        summary_value = .data[[input$summary_var]]
      )
    
    validate(
      need(
        nrow(selected) > 0,
        "No active reporting observations remain."
      )
    )
    
    active_rows <- nrow(selected)
    
    
    # Combine dollar totals by month when requested
    if (is_monthly) {
      
      analysis <- selected |>
        group_by(month) |>
        summarise(
          summary_value = if (
            any(is.finite(summary_value))
          ) {
            sum(summary_value[is.finite(summary_value)])
          } else {
            NA_real_
          },
          .groups = "drop"
        ) |>
        mutate(
          reporting_year = lubridate::year(month)
        )
      
    } else {
      
      analysis <- selected
    }
    
    analysis_rows <- nrow(analysis)
    
    
    # Group the selected observations
    if (input$summary_group == "state") {
      analysis <- analysis |>
        group_by(state)
    }
    
    if (input$summary_group == "year") {
      analysis <- analysis |>
        group_by(reporting_year)
    }
    
    if (input$summary_group == "both") {
      analysis <- analysis |>
        group_by(state, reporting_year)
    }
    
    
    # Calculate the summary statistics
    stats <- analysis |>
      summarise(
        
        Rows = n(),
        
        Reported = sum(is.finite(summary_value)),
        
        Missing = sum(!is.finite(summary_value)),
        
        Mean = if (Reported > 0) {
          mean(summary_value[is.finite(summary_value)])
        } else {
          NA_real_
        },
        
        Median = if (Reported > 0) {
          median(summary_value[is.finite(summary_value)])
        } else {
          NA_real_
        },
        
        SD = if (Reported > 1) {
          sd(summary_value[is.finite(summary_value)])
        } else {
          NA_real_
        },
        
        Minimum = if (Reported > 0) {
          min(summary_value[is.finite(summary_value)])
        } else {
          NA_real_
        },
        
        Maximum = if (Reported > 0) {
          max(summary_value[is.finite(summary_value)])
        } else {
          NA_real_
        },
        
        Total = if (!is_hold && Reported > 0) {
          sum(summary_value[is.finite(summary_value)])
        } else {
          NA_real_
        },
        
        .groups = "drop"
      )
    
    if (is_hold) {
      stats <- stats |>
        select(-Total)
    }
    
    if (is_monthly) {
      stats <- stats |>
        rename(Months = Rows)
    }
    
    list(
      table = stats,
      is_hold = is_hold,
      is_monthly = is_monthly,
      filtered_rows = nrow(filtered),
      active_rows = active_rows,
      analysis_rows = analysis_rows
    )
  })
  
  
  # Display observation count
  output$summary_count <- renderText({
    
    result <- summary_data()
    
    if (result$is_monthly) {
      
      paste0(
        "Combined ",
        result$active_rows,
        " active state-month observations into ",
        result$analysis_rows,
        " monthly totals."
      )
      
    } else {
      
      paste0(
        "Using ",
        result$active_rows,
        " of ",
        result$filtered_rows,
        " applied observations."
      )
    }
  })
  
  
  # Numerical summary table
  output$summary_table <- DT::renderDT({
    
    result <- summary_data()
    
    cols <- c(
      "Mean",
      "Median",
      "SD",
      "Minimum",
      "Maximum"
    )
    
    tbl <- DT::datatable(
      result$table,
      rownames = FALSE,
      options = list(
        pageLength = 10,
        scrollX = TRUE
      )
    )
    
    if (result$is_hold) {
      
      tbl |>
        DT::formatCurrency(
          columns = cols,
          currency = "%",
          digits = 2,
          before = FALSE
        )
      
    } else {
      
      tbl |>
        DT::formatRound(
          columns = c(cols, "Total"),
          digits = 2
        )
    }
  })
  
  
  # Numerical summary explanation
  output$summary_note <- renderUI({
    
    result <- summary_data()
    
    tags$div(
      style = paste(
        "margin-top:18px;padding:12px;",
        "background-color:#f5f5f5;border-radius:5px;"
      ),
      
      tags$strong("Summary Notes"),
      
      tags$p(
        "These summaries use only the Data & Summaries filters.",
        "Prelaunch observations are excluded; later zeros",
        "and negative values are retained."
      ),
      
      if (result$is_monthly) {
        
        tags$p(
          "Combined monthly totals sum dollar amounts",
          "across states in each month.",
          "The mean and sample SD are then calculated",
          "across monthly totals."
        )
        
      } else {
        
        tags$p(
          "Individual observations treat each state-month",
          "as a separate value."
        )
      },
      
      tags$p(
        "Missing observations are omitted, not converted to zero."
      ),
      
      if (result$is_hold) {
        
        tags$p(
          "Mean hold is the unweighted average of",
          "reported monthly percentages.",
          "No Total is shown for hold rates."
        )
        
      } else {
        
        tags$p(
          "Total is the sum of valid dollar observations."
        )
      }
    )
  })
  
  
  # Correlation Analysis
  # Uses original data and independent controls
  
  observeEvent(
    list(input$corr_x, input$corr_y),
    {
      
      is_hold <- input$corr_x %in% hold_vars ||
        input$corr_y %in% hold_vars
      
      choices <- if (is_hold) {
        
        c(
          "Reported Value" = "level",
          "YoY Change (dollars or percentage points)" = "change"
        )
        
      } else {
        
        comparison_choices("handle")
      }
      
      current <- isolate(input$corr_view)
      
      updateSelectInput(
        session,
        "corr_view",
        choices = choices,
        selected = if (current %in% unname(choices)) {
          current
        } else {
          "level"
        }
      )
    }
  )
  
  
  # Match the two selected state-variable series
  paired_periods <- reactive({
    
    req(
      input$corr_state_x,
      input$corr_state_y,
      input$corr_x,
      input$corr_y,
      input$corr_period,
      input$corr_view
    )
    
    x <- get_chart_series(
      input$corr_state_x,
      input$corr_x,
      input$corr_period
    )
    
    y <- get_chart_series(
      input$corr_state_y,
      input$corr_y,
      input$corr_period
    )
    
    if (input$corr_view != "level") {
      
      x <- add_yoy(
        x,
        input$corr_x,
        input$corr_view
      )
      
      y <- add_yoy(
        y,
        input$corr_y,
        input$corr_view
      )
    }
    
    inner_join(
      x |> rename(x = value),
      y |> rename(y = value),
      by = "period"
    ) |>
      filter(
        is.finite(x),
        is.finite(y)
      ) |>
      arrange(period)
  })
  
  
  # Update the available date range
  observeEvent(paired_periods(), {
    
    data <- paired_periods()
    
    if (nrow(data) > 0) {
      
      updateDateRangeInput(
        session,
        "corr_dates",
        start = min(data$period),
        end = max(data$period),
        min = min(data$period),
        max = max(data$period)
      )
    }
  })
  
  
  output$available_period <- renderText({
    
    data <- paired_periods()
    
    if (nrow(data) == 0) {
      return(
        "No matched periods; YoY requires the previous year."
      )
    }
    
    paste0(
      "Available: ",
      format_period(
        min(data$period),
        input$corr_period
      ),
      " - ",
      format_period(
        max(data$period),
        input$corr_period
      )
    )
  })
  
  
  # Store the last requested correlation
  corr_results <- reactiveVal(NULL)
  
  observeEvent(input$run_corr, {
    
    req(input$corr_dates)
    
    data <- paired_periods()
    
    from <- period_floor(
      input$corr_dates[1],
      input$corr_period
    )
    
    to <- period_floor(
      input$corr_dates[2],
      input$corr_period
    )
    
    data <- data |>
      filter(
        period >= from,
        period <= to
      )
    
    
    # Identify when mobile betting activity began
    x_start <- first_positive_month(
      input$corr_state_x,
      "handle_mobile"
    )
    
    y_start <- first_positive_month(
      input$corr_state_y,
      "handle_mobile"
    )
    
    x_mobile <- if (is.na(x_start)) {
      rep(FALSE, nrow(data))
    } else {
      data$period >= x_start
    }
    
    y_mobile <- if (is.na(y_start)) {
      rep(FALSE, nrow(data))
    } else {
      data$period >= y_start
    }
    
    data <- data |>
      mutate(
        market_period = factor(
          case_when(
            x_mobile & y_mobile ~ "Both states mobile-active",
            x_mobile | y_mobile ~ "One state mobile-active",
            TRUE ~ "Neither state mobile-active"
          ),
          levels = c(
            "Neither state mobile-active",
            "One state mobile-active",
            "Both states mobile-active"
          )
        )
      )
    
    corr_results(
      list(
        data = data,
        state_x = input$corr_state_x,
        state_y = input$corr_state_y,
        var_x = input$corr_x,
        var_y = input$corr_y,
        period = input$corr_period,
        view = input$corr_view
      )
    )
  })
  
  
  # Validate correlation data
  corr_data <- reactive({
    
    result <- corr_results()
    
    validate(
      need(
        !is.null(result),
        "Choose inputs and click Run Correlation."
      )
    )
    
    validate(
      need(
        !(result$state_x == result$state_y &&
            result$var_x == result$var_y),
        "Select different states or variables."
      ),
      
      need(
        nrow(result$data) >= 3,
        "At least three matched periods are required."
      ),
      
      need(
        isTRUE(sd(result$data$x) > 0) &&
          isTRUE(sd(result$data$y) > 0),
        "Correlation cannot be calculated when a variable is constant."
      )
    )
    
    result
  })
  
  
  # Correlation scatterplot
  output$corr_plot <- renderPlot({
    
    result <- corr_data()
    
    axis_label <- function(s, var) {
      
      label <- names(numeric_vars)[
        numeric_vars == var
      ]
      
      if (result$view != "level") {
        
        unit <- if (var %in% hold_vars) {
          "pp"
        } else if (result$view == "change") {
          "$ change"
        } else {
          "% growth"
        }
        
        label <- paste(label, "YoY", unit)
      }
      
      paste(s, label)
    }
    
    ggplot(
      result$data,
      aes(x = x, y = y)
    ) +
      geom_point(
        aes(color = market_period),
        size = 2.5,
        alpha = 0.8
      ) +
      geom_smooth(
        method = "lm",
        se = FALSE,
        color = "black"
      ) +
      scale_color_manual(
        values = c(
          "Neither state mobile-active" = "gray50",
          "One state mobile-active" = "#E69F00",
          "Both states mobile-active" = "#0072B2"
        ),
        name = "Market Period"
      ) +
      scale_x_continuous(
        labels = scales::label_number(
          scale_cut = scales::cut_short_scale()
        )
      ) +
      scale_y_continuous(
        labels = scales::label_number(
          scale_cut = scales::cut_short_scale()
        )
      ) +
      labs(
        title = paste(
          period_labels[[result$period]],
          "Correlation"
        ),
        subtitle = paste(
          result$state_x,
          "vs",
          result$state_y
        ),
        x = axis_label(
          result$state_x,
          result$var_x
        ),
        y = axis_label(
          result$state_y,
          result$var_y
        )
      ) +
      theme_gray()
  })
  
  
  # Correlation coefficient
  output$corr_result <- renderText({
    
    result <- corr_data()
    
    paste0(
      "Pearson correlation (r): ",
      round(
        cor(result$data$x, result$data$y),
        3
      ),
      " | Matched periods: ",
      nrow(result$data),
      " | ",
      format_period(
        min(result$data$period),
        result$period
      ),
      " - ",
      format_period(
        max(result$data$period),
        result$period
      )
    )
  })
  
  
  # Correlation reporting context
  output$market_note <- renderUI({
    
    result <- corr_data()
    
    state_note <- function(s) {
      
      paste0(
        s,
        ": retail first positive ",
        first_positive_month(s, "handle_retail"),
        "; mobile first positive ",
        first_positive_month(s, "handle_mobile"),
        "."
      )
    }
    
    tags$div(
      style = paste(
        "margin-top:18px;padding:12px;",
        "background-color:#f5f5f5;border-radius:5px;"
      ),
      
      tags$strong("Reporting Context"),
      
      tags$p(
        state_note(result$state_x)
      ),
      
      tags$p(
        state_note(result$state_y)
      ),
      
      tags$p(
        "Correlation uses the original dataset,",
        "independently of Data & Summaries filters."
      ),
      
      tags$p(
        "Complete months are required for quarterly,",
        "annual, and rolling calculations.",
        "YoY compares the same period one year earlier."
      ),
      
      tags$p(
        "Adjacent rolling periods overlap",
        "and are not independent."
      )
    )
  })
  
  
  # Time Series Analysis
  # Uses the original data and independent controls
  
  observeEvent(input$ts_var, {
    
    choices <- comparison_choices(input$ts_var)
    
    current <- isolate(input$ts_view)
    
    updateSelectInput(
      session,
      "ts_view",
      choices = choices,
      selected = if (current %in% unname(choices)) {
        current
      } else {
        "level"
      }
    )
  })
  
  
  # Prepare the selected time series
  ts_series <- reactive({
    
    req(
      input$ts_var,
      input$ts_period,
      input$ts_view
    )
    
    if (length(input$ts_states) == 0) {
      
      return(tibble(
        period = as.Date(character()),
        value = numeric(),
        state = character()
      ))
    }
    
    purrr::map_dfr(
      input$ts_states,
      function(s) {
        
        data <- get_chart_series(
          s,
          input$ts_var,
          input$ts_period
        )
        
        if (input$ts_view != "level") {
          
          data <- add_yoy(
            data,
            input$ts_var,
            input$ts_view
          )
        }
        
        data |>
          filter(is.finite(value)) |>
          mutate(state = s)
      }
    ) |>
      arrange(state, period)
  })
  
  
  # Update the available date range
  observeEvent(ts_series(), {
    
    series <- ts_series()
    
    if (nrow(series) > 0) {
      
      updateDateRangeInput(
        session,
        "ts_dates",
        start = min(series$period),
        end = max(series$period),
        min = min(series$period),
        max = max(series$period)
      )
    }
  })
  
  
  output$ts_available_period <- renderText({
    
    series <- ts_series()
    
    if (nrow(series) == 0) {
      return(
        "No available periods; YoY requires the previous year."
      )
    }
    
    paste0(
      "Available: ",
      format_period(
        min(series$period),
        input$ts_period
      ),
      " - ",
      format_period(
        max(series$period),
        input$ts_period
      )
    )
  })
  
  
  # Update the plot only when the button is clicked
  ts_results <- reactiveVal(NULL)
  
  observeEvent(input$run_ts, {
    
    req(input$ts_dates)
    
    from <- period_floor(
      input$ts_dates[1],
      input$ts_period
    )
    
    to <- period_floor(
      input$ts_dates[2],
      input$ts_period
    )
    
    selected <- ts_series() |>
      filter(
        period >= from,
        period <= to
      )
    
    ts_results(
      list(
        data = selected,
        states = input$ts_states,
        variable = input$ts_var,
        period = input$ts_period,
        view = input$ts_view,
        display = input$ts_display,
        start_date = from,
        end_date = to
      )
    )
  })
  
  
  # Validate the time series
  ts_data <- reactive({
    
    result <- ts_results()
    
    validate(
      need(
        !is.null(result),
        "Choose inputs and click Update Chart."
      )
    )
    
    validate(
      need(
        nrow(result$data) > 0,
        "No values for the selected period."
      )
    )
    
    result
  })
  
  
  # Interactive Time Series chart
  output$ts_plot <- renderPlotly({
    
    result <- ts_data()
    
    label <- names(numeric_vars)[
      numeric_vars == result$variable
    ]
    
    is_hold <- result$variable %in% hold_vars
    is_yoy <- result$view != "level"
    is_growth <- result$view == "yoy"
    
    suffix <- if (is_hold && is_yoy) {
      " pp"
    } else if (is_hold || is_growth) {
      "%"
    } else {
      ""
    }
    
    is_dollars <- !is_hold && !is_growth
    
    y_label <- if (is_yoy) {
      
      paste(
        "YoY",
        label,
        if (is_hold) {
          "(percentage points)"
        } else if (is_growth) {
          "(%)"
        } else {
          "($ change)"
        }
      )
      
    } else {
      
      label
    }
    
    
    # Prepare hover text
    chart_data <- result$data |>
      mutate(
        
        period_label = vapply(
          period,
          format_period,
          character(1),
          mode = result$period
        ),
        
        hover_value = if (is_dollars) {
          
          paste0(
            "$",
            formatC(
              value,
              format = "f",
              digits = 2,
              big.mark = ","
            )
          )
          
        } else {
          
          paste0(
            formatC(
              value,
              format = "f",
              digits = 2,
              big.mark = ","
            ),
            suffix
          )
        },
        
        hover_text = paste0(
          "State: ",
          state,
          "<br>Period: ",
          period_label,
          "<br>",
          y_label,
          ": ",
          hover_value
        )
      )
    
    
    # Missing periods remain gaps in the chart
    step <- if (result$period == "year") {
      "year"
    } else if (result$period == "quarter") {
      "quarter"
    } else {
      "month"
    }
    
    chart_data <- chart_data |>
      group_by(state) |>
      complete(
        period = seq(
          min(period),
          max(period),
          by = step
        )
      ) |>
      ungroup()
    
    
    chart <- ggplot(
      chart_data,
      aes(
        x = period,
        y = value,
        color = state,
        group = state,
        text = hover_text
      )
    ) +
      geom_line(
        linewidth = 0.8
      ) +
      geom_point(
        size = 1.5,
        alpha = 0.7,
        na.rm = TRUE
      ) +
      labs(
        title = paste(
          period_labels[[result$period]],
          y_label
        ),
        
        subtitle = paste(
          format_period(
            result$start_date,
            result$period
          ),
          "-",
          format_period(
            result$end_date,
            result$period
          )
        ),
        
        x = "Reporting Period",
        y = y_label,
        color = "State"
      ) +
      theme_gray()
    
    
    # Dollar and percentage formatting
    if (is_dollars) {
      
      chart <- chart +
        scale_y_continuous(
          labels = scales::label_number(
            prefix = "$",
            scale_cut = scales::cut_short_scale()
          )
        )
      
    } else {
      
      chart <- chart +
        scale_y_continuous(
          labels = scales::label_number(
            suffix = suffix
          )
        )
    }
    
    
    # Independent vertical scales when requested
    if (result$display == "Separate panels") {
      
      chart <- chart +
        facet_wrap(
          ~state,
          scales = "free_y"
        ) +
        guides(color = "none")
    }
    
    plotly::ggplotly(
      chart,
      tooltip = "text"
    ) |>
      plotly::layout(
        hovermode = "closest"
      ) |>
      plotly::config(
        displaylogo = FALSE
      )
  })
  
  
  # Time Series reporting context
  output$ts_context <- renderUI({
    
    result <- ts_data()
    
    state_notes <- purrr::map_chr(
      result$states,
      function(s) {
        
        paste0(
          s,
          ": retail first positive ",
          first_positive_month(s, "handle_retail"),
          "; mobile first positive ",
          first_positive_month(s, "handle_mobile"),
          "."
        )
      }
    )
    
    tags$div(
      style = paste(
        "margin-top:18px;padding:12px;",
        "background-color:#f5f5f5;border-radius:5px;"
      ),
      
      tags$strong("Reporting Context"),
      
      lapply(state_notes, tags$p),
      
      tags$p(
        "This chart uses the original dataset",
        "independently of Data & Summaries filters."
      ),
      
      tags$p(
        "Calendar quarters and years require all",
        "3 or 12 months.",
        "Trailing windows require all included months."
      ),
      
      tags$p(
        "YTD totals begin in January and YoY uses",
        "the equivalent prior-year period."
      ),
      
      tags$p(
        "Aggregated hold is revenue divided by handle times 100.",
        "YoY hold changes are measured in percentage points."
      ),
      
      tags$p(
        "First positive observations are not necessarily",
        "legal market launch dates.",
        "Adjacent rolling periods overlap."
      )
    )
  })
}


# Step 5: Run the app

shinyApp(
  ui = ui,
  server = server
)
