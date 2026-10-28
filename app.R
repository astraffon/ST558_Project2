
# ST558 Project 2 - U.S. Sports Betting Explorer
# Interactive application for state-level sports betting data


# Step 1: Load Required Packages and Data

library(shiny)
library(tidyverse)
library(DT)
library(plotly)

# Read in the cleaned sports betting data
sports <- read_csv(
  "data/sports_betting_clean.csv",
  show_col_types = FALSE
)

# Keep all columns for analysis, but omit split-adjustment columns from exports
sports_download <- sports |>
  select(-ends_with("_for_split"))

state_choices <- sort(unique(sports$state))

# Variable names displayed in the dropdowns
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


# Step 2: Identify Reporting Periods

# Find the first month with positive activity for a selected channel
# Return NA if no positive handle was reported
first_positive_month <- function(selected_state, selected_var) {
  
  dates <- sports |>
    filter(
      state == selected_state,
      !is.na(.data[[selected_var]]),
      .data[[selected_var]] > 0
    ) |>
    pull(month)
  
  if (length(dates) == 0) {
    return(as.Date(NA))
  }
  
  min(dates)
}


# Determine the first active month for each state and variable
# Total, mobile, and retail have independent starting dates
# Keep zeros and negative values after the starting month
get_active_series <- function(selected_state, selected_var) {
  
  state_data <- sports |>
    filter(state == selected_state) |>
    arrange(month)
  
  if (nrow(state_data) == 0) {
    return(tibble(month = as.Date(character()), value = numeric()))
  }
  
  total_active <- !is.na(state_data$handle) & state_data$handle > 0
  mobile_active <- !is.na(state_data$handle_mobile) & state_data$handle_mobile > 0
  retail_active <- !is.na(state_data$handle_retail) & state_data$handle_retail > 0
  
  selected_values <- state_data[[selected_var]]
  nonzero_value <- is.finite(selected_values) & selected_values != 0
  
  # Choose the activity rule for the selected variable
  active <- switch(
    selected_var,
    handle = total_active,
    handle_mobile = mobile_active,
    handle_retail = retail_active,
    revenue = total_active | nonzero_value,
    revenue_mobile = mobile_active | nonzero_value,
    revenue_retail = retail_active | nonzero_value,
    hold_total = total_active,
    hold_mobile = mobile_active,
    hold_retail = retail_active,
    nonzero_value
  )
  
  active <- active & is.finite(selected_values)
  
  if (!any(active)) {
    return(tibble(month = as.Date(character()), value = numeric()))
  }
  
  first_month <- min(state_data$month[active])
  
  state_data |>
    filter(
      month >= first_month,
      is.finite(.data[[selected_var]])
    ) |>
    transmute(month, value = .data[[selected_var]])
}


# Create a range slider using the currently selected states and years
make_numeric_slider <- function(selected_var, slider_id, data) {
  
  values <- data[[selected_var]]
  values <- values[is.finite(values)]
  
  if (length(values) == 0) {
    return(helpText("No reported values for this selection."))
  }
  
  is_hold <- selected_var %in% c(
    "hold_total", "hold_mobile", "hold_retail"
  )
  
  if (is_hold) {
    lower <- floor(min(values) * 100) / 100
    upper <- ceiling(max(values) * 100) / 100
    step_size <- 0.01
  } else {
    lower <- floor(min(values))
    upper <- ceiling(max(values))
    step_size <- max(1, round((upper - lower) / 500))
  }
  
  if (lower == upper) {
    upper <- lower + step_size
  }
  
  sliderInput(
    inputId = slider_id,
    label = "Value Range",
    min = lower,
    max = upper,
    value = c(lower, upper),
    step = step_size,
    pre = if (is_hold) NULL else "$",
    post = if (is_hold) "%" else NULL,
    sep = ",",
    ticks = FALSE
  )
}


# Step 3: Define the User Interface (UI)

ui <- fluidPage(
  
  titlePanel("U.S. Sports Betting Explorer"),
  
  # Main tabs for the application
  tabsetPanel(
    
    # About tab
    tabPanel(
      "About",
      h3("About This Application"),
      p(
        "Explore historical U.S. sports betting data, including",
        "handle, revenue, taxes, and hold percentages."
      ),
      p(
        "The dataset contains monthly state-level observations",
        "from 2018 through August 2026."
      ),
      p(
        "Select states, numerical variables, and a reporting",
        "period to examine correlations between monthly observations."
      )
    ),
    
    # Data and summaries share one set of filters
    tabPanel(
      "Data & Summaries",
      
      sidebarLayout(
        sidebarPanel(
          h3("Dataset Filters"),
          
          # Categorical filters: state and reporting year
          selectizeInput(
            inputId = "filter_states",
            label = "States",
            choices = c("All States" = "ALL", state_choices),
            selected = "ALL",
            multiple = TRUE
          ),
          selectizeInput(
            inputId = "filter_years",
            label = "Reporting Years",
            choices = c(
              "All Years" = "ALL",
              as.character(sort(unique(lubridate::year(sports$month))))
            ),
            selected = "ALL",
            multiple = TRUE
          ),
          helpText(
            "Remove 'All States' or 'All Years' to select specific categories."
          ),
          
          # First optional numerical filter
          h4("Numerical Filter 1"),
          checkboxInput("filter_use_num1", "Enable Filter 1", FALSE),
          conditionalPanel(
            condition = "input.filter_use_num1",
            selectInput(
              inputId = "filter_num1", label = "Variable",
              choices = numeric_vars, selected = "handle"
            ),
            uiOutput("filter_slider1"),
            fluidRow(
              column(6, textInput(
                "filter_min1", "Exact minimum (optional)",
                value = "", placeholder = "e.g. 0"
              )),
              column(6, textInput(
                "filter_max1", "Exact maximum (optional)",
                value = "", placeholder = "e.g. 100000"
              ))
            )
          ),
          
          # Second optional numerical filter
          h4("Numerical Filter 2"),
          checkboxInput("filter_use_num2", "Enable Filter 2", FALSE),
          conditionalPanel(
            condition = "input.filter_use_num2",
            selectInput(
              inputId = "filter_num2", label = "Variable",
              choices = numeric_vars, selected = "revenue"
            ),
            uiOutput("filter_slider2"),
            fluidRow(
              column(6, textInput(
                "filter_min2", "Exact minimum (optional)",
                value = "", placeholder = "e.g. 0"
              )),
              column(6, textInput(
                "filter_max2", "Exact maximum (optional)",
                value = "", placeholder = "e.g. 100000"
              ))
            )
          ),
          
          helpText(
            "Slider ranges follow the selected states and years.",
            "Exact entries override the slider limits when supplied."
          ),
          actionButton("apply_filters", "Apply Filters"),
          helpText("Both tables and the CSV update only after Apply Filters.")
        ),
        
        mainPanel(
          h3("Data and Summary Statistics"),
          textOutput("filter_summary"),
          textOutput("applied_filter_description"),
          
          # Switch between the table and summaries without losing the filters
          tabsetPanel(
            tabPanel(
              "Data Table",
              DT::DTOutput("sports_table"),
              downloadButton("download_data", "Download Data (CSV)")
            ),
            tabPanel(
              "Numerical Summaries",
              selectInput(
                inputId = "summary_var",
                label = "Summary Variable",
                choices = numeric_vars,
                selected = "handle"
              ),
              
              # Choose between individual observations and monthly totals
              radioButtons(
                inputId = "summary_mode",
                label = "Calculation Method",
                choices = c(
                  "Individual state-month observations" = "individual",
                  "Combined monthly totals across states" = "monthly"
                ),
                selected = "individual"
              ),
              helpText(
                "Combined monthly totals are available for dollar variables only."
              ),
              
              selectInput(
                inputId = "summary_group",
                label = "Group Results By",
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
    
    # Data Exploration tab
    tabPanel(
      "Data Exploration",
      
      # Each analysis has its own tab and controls
      tabsetPanel(
        
        # Correlation tab
        tabPanel(
          "Correlation",
          
          sidebarLayout(
            
            sidebarPanel(
              h3("Correlation Analysis"),
              
              # X-axis selections
              h4("X-Axis"),
              selectizeInput(
                inputId = "corr_state_x",
                label = "State",
                choices = state_choices,
                selected = "NY"
              ),
              selectizeInput(
                inputId = "corr_x",
                label = "Variable",
                choices = numeric_vars,
                selected = "handle"
              ),
              
              # Y-axis selections
              h4("Y-Axis"),
              selectizeInput(
                inputId = "corr_state_y",
                label = "State",
                choices = state_choices,
                selected = "NJ"
              ),
              selectizeInput(
                inputId = "corr_y",
                label = "Variable",
                choices = numeric_vars,
                selected = "handle"
              ),
              
              # Reporting period adjusts to the selected series
              dateRangeInput(
                inputId = "corr_dates",
                label = "Reporting Period",
                start = min(sports$month),
                end = max(sports$month),
                min = min(sports$month),
                max = max(sports$month),
                format = "M yyyy",
                separator = " to "
              ),
              textOutput("available_period"),
              actionButton("run_corr", "Run Correlation")
            ),
            
            mainPanel(
              h3("Correlation Between Selected Variables"),
              plotOutput("corr_plot", height = "450px"),
              textOutput("corr_result"),
              uiOutput("market_note")
            )
          )
        ),
        
        # Time Series tab
        tabPanel(
          "Time Series",
          
          sidebarLayout(
            sidebarPanel(
              h3("Time Series Analysis"),
              
              selectizeInput(
                inputId = "ts_states",
                label = "States",
                choices = state_choices,
                selected = "NY",
                multiple = TRUE
              ),
              helpText(
                "Click inside the box to add more states.",
                "Multiple states can be selected."
              ),
              
              selectizeInput(
                inputId = "ts_var",
                label = "Variable",
                choices = numeric_vars,
                selected = "handle"
              ),
              
              dateRangeInput(
                inputId = "ts_dates",
                label = "Reporting Period",
                start = min(sports$month),
                end = max(sports$month),
                min = min(sports$month),
                max = max(sports$month),
                format = "M yyyy",
                separator = " to "
              ),
              textOutput("ts_available_period"),
              
              selectInput(
                inputId = "ts_display",
                label = "Chart Layout",
                choices = c("Shared scale", "Separate panels"),
                selected = "Shared scale"
              ),
              actionButton("run_ts", "Update Chart")
            ),
            
            mainPanel(
              h3("Monthly Sports Betting Trends"),
              plotlyOutput("ts_plot", height = "500px"),
              uiOutput("ts_context")
            )
          )
        )
        
        # Additional analysis tabs will go here
      )
    )
  )
)


# Step 4: Define the Server

server <- function(input, output, session) {
  
  # Correlation Analysis
  
  # Match the selected variables by calendar month
  paired_months <- reactive({
    
    x_data <- get_active_series(
      input$corr_state_x,
      input$corr_x
    ) |>
      rename(x = value)
    
    y_data <- get_active_series(
      input$corr_state_y,
      input$corr_y
    ) |>
      rename(y = value)
    
    inner_join(x_data, y_data, by = "month") |>
      arrange(month)
  })
  
  
  # Update the reporting dates when the selected series change
  observeEvent(paired_months(), {
    
    paired_data <- paired_months()
    
    if (nrow(paired_data) > 0) {
      updateDateRangeInput(
        session,
        inputId = "corr_dates",
        start = min(paired_data$month),
        end = max(paired_data$month),
        min = min(paired_data$month),
        max = max(paired_data$month)
      )
    }
  })
  
  
  # Display the common reporting period
  output$available_period <- renderText({
    
    paired_data <- paired_months()
    
    if (nrow(paired_data) == 0) {
      return("No overlapping reported activity.")
    }
    
    paste0(
      "Available: ",
      format(min(paired_data$month), "%b %Y"),
      " - ",
      format(max(paired_data$month), "%b %Y")
    )
  })
  
  
  # Store the results from the last correlation run
  corr_results <- reactiveValues(result = NULL)
  
  
  # Run the correlation only when the button is clicked
  observeEvent(input$run_corr, {
    
    req(input$corr_dates)
    
    start_month <- lubridate::floor_date(
      input$corr_dates[1],
      unit = "month"
    )
    
    end_month <- lubridate::floor_date(
      input$corr_dates[2],
      unit = "month"
    )
    
    paired_data <- paired_months()
    
    # Keep the selected dates within the available period
    if (nrow(paired_data) > 0) {
      start_month <- max(start_month, min(paired_data$month))
      end_month <- min(end_month, max(paired_data$month))
    }
    
    paired_data <- paired_data |>
      filter(month >= start_month, month <= end_month)
    
    # Identify when mobile activity started in each state
    mobile_start_x <- first_positive_month(
      input$corr_state_x,
      "handle_mobile"
    )
    
    mobile_start_y <- first_positive_month(
      input$corr_state_y,
      "handle_mobile"
    )
    
    # Classify each month by mobile-market activity
    x_mobile <- if (is.na(mobile_start_x)) {
      rep(FALSE, nrow(paired_data))
    } else {
      paired_data$month >= mobile_start_x
    }
    
    y_mobile <- if (is.na(mobile_start_y)) {
      rep(FALSE, nrow(paired_data))
    } else {
      paired_data$month >= mobile_start_y
    }
    
    paired_data <- paired_data |>
      mutate(
        market_period = case_when(
          x_mobile & y_mobile ~ "Both states mobile-active",
          x_mobile | y_mobile ~ "One state mobile-active",
          TRUE ~ "Neither state mobile-active"
        ),
        market_period = factor(
          market_period,
          levels = c(
            "Neither state mobile-active",
            "One state mobile-active",
            "Both states mobile-active"
          )
        )
      )
    
    # Save the selections and observations from this run
    corr_results$result <- list(
      data = paired_data,
      state_x = input$corr_state_x,
      state_y = input$corr_state_y,
      var_x = input$corr_x,
      var_y = input$corr_y,
      start_month = start_month,
      end_month = end_month
    )
  })
  
  
  # Check the data before calculating correlation
  corr_data <- reactive({
    
    result <- corr_results$result
    
    validate(
      need(
        !is.null(result),
        "Select your inputs and click Run Correlation."
      )
    )
    
    validate(
      need(
        !(result$state_x == result$state_y &&
            result$var_x == result$var_y),
        "Select different states or different variables."
      ),
      need(
        nrow(result$data) >= 3,
        "Not enough matching months to calculate a correlation."
      )
    )
    
    validate(
      need(
        sd(result$data$x) > 0 &&
          sd(result$data$y) > 0,
        "Correlation cannot be calculated when a variable is constant."
      )
    )
    
    result
  })
  
  
  # Create the correlation scatterplot
  output$corr_plot <- renderPlot({
    
    result <- corr_data()
    x_label <- names(numeric_vars)[numeric_vars == result$var_x]
    y_label <- names(numeric_vars)[numeric_vars == result$var_y]
    
    ggplot(result$data, aes(x = x, y = y)) +
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
        title = "Correlation Analysis",
        subtitle = paste(result$state_x, "vs", result$state_y),
        x = paste(result$state_x, x_label),
        y = paste(result$state_y, y_label)
      ) +
      theme_gray()
  })
  
  
  # Calculate the Pearson correlation coefficient
  output$corr_result <- renderText({
    
    result <- corr_data()
    correlation <- cor(result$data$x, result$data$y)
    
    paste0(
      "Pearson correlation (r): ", round(correlation, 3),
      " | Matched months: ", nrow(result$data),
      " | Period: ", format(result$start_month, "%b %Y"),
      " - ", format(result$end_month, "%b %Y")
    )
  })
  
  
  # Explain the selected states' reporting history
  output$market_note <- renderUI({
    
    result <- corr_data()
    
    date_label <- function(date) {
      if (is.na(date)) return("No positive activity reported")
      format(date, "%b %Y")
    }
    
    series_start <- function(selected_state, selected_var) {
      series <- get_active_series(selected_state, selected_var)
      if (nrow(series) == 0) return("Unavailable")
      format(min(series$month), "%b %Y")
    }
    
    state_note <- function(selected_state, selected_var) {
      
      variable_label <- names(numeric_vars)[numeric_vars == selected_var]
      retail_start <- first_positive_month(
        selected_state, "handle_retail"
      )
      mobile_start <- first_positive_month(
        selected_state, "handle_mobile"
      )
      
      paste0(
        selected_state, " ", variable_label,
        " data available since ",
        series_start(selected_state, selected_var),
        ". Retail activity: ", date_label(retail_start),
        "; mobile activity: ", date_label(mobile_start), "."
      )
    }
    
    x_note <- state_note(result$state_x, result$var_x)
    y_note <- state_note(result$state_y, result$var_y)
    
    total_amount_vars <- c(
      "handle", "revenue", "taxable_revenue",
      "taxes", "promos_dedux"
    )
    
    scale_notes <- character()
    
    for (axis in c("x", "y")) {
      
      selected_state <- result[[paste0("state_", axis)]]
      selected_var <- result[[paste0("var_", axis)]]
      retail_start <- first_positive_month(
        selected_state, "handle_retail"
      )
      mobile_start <- first_positive_month(
        selected_state, "handle_mobile"
      )
      
      if (
        selected_var %in% total_amount_vars &&
        !is.na(retail_start) &&
        !is.na(mobile_start) &&
        retail_start < mobile_start &&
        result$start_month < mobile_start &&
        result$end_month >= mobile_start
      ) {
        scale_notes <- c(
          scale_notes,
          paste0(
            selected_state,
            " reported retail activity before mobile activity began in ",
            format(mobile_start, "%b %Y"),
            ". Earlier total-dollar amounts may appear near zero ",
            "relative to later mobile-era observations."
          )
        )
      }
    }
    
    tags$div(
      style = paste(
        "margin-top: 18px;",
        "padding: 12px;",
        "background-color: #f5f5f5;",
        "border-radius: 5px;"
      ),
      tags$strong("Reporting Context"),
      tags$p(paste0(
        "Comparison: ", result$state_x, " ",
        names(numeric_vars)[numeric_vars == result$var_x],
        " vs ", result$state_y, " ",
        names(numeric_vars)[numeric_vars == result$var_y], "."
      )),
      tags$p(x_note),
      tags$p(y_note),
      if (length(scale_notes) > 0) {
        tags$p(
          paste(unique(scale_notes), collapse = " "),
          "Consider adjusting the reporting period when",
          "comparing different stages of market development."
        )
      },
      tags$p(
        "Point colors indicate whether neither, one, or both",
        "selected states had begun reporting positive mobile handle."
      )
    )
  })
  
  
  # Time Series Analysis
  
  # Collect the selected variable for each state
  ts_series <- reactive({
    
    req(input$ts_var)
    
    if (length(input$ts_states) == 0) {
      return(tibble(
        month = as.Date(character()),
        value = numeric(), state = character()
      ))
    }
    
    purrr::map_dfr(input$ts_states, function(selected_state) {
      get_active_series(selected_state, input$ts_var) |>
        mutate(state = selected_state)
    }) |>
      arrange(state, month)
  })
  
  
  # Display the available reporting dates
  output$ts_available_period <- renderText({
    series <- ts_series()
    if (nrow(series) == 0) {
      return("No reporting data for these selections.")
    }
    paste0(
      "Available: ", format(min(series$month), "%b %Y"),
      " - ", format(max(series$month), "%b %Y")
    )
  })
  
  
  # Reset reporting dates when states or variables change
  observeEvent(ts_series(), {
    series <- ts_series()
    if (nrow(series) > 0) {
      updateDateRangeInput(
        session,
        inputId = "ts_dates",
        start = min(series$month),
        end = max(series$month),
        min = min(series$month),
        max = max(series$month)
      )
    }
  })
  
  
  # Update the chart only when the button is clicked
  ts_results <- reactiveValues(result = NULL)
  
  observeEvent(input$run_ts, {
    req(input$ts_dates)
    req(length(input$ts_states) > 0)
    
    start_month <- lubridate::floor_date(input$ts_dates[1], "month")
    end_month <- lubridate::floor_date(input$ts_dates[2], "month")
    series <- ts_series()
    
    if (nrow(series) > 0) {
      start_month <- max(start_month, min(series$month))
      end_month <- min(end_month, max(series$month))
    }
    
    chart_data <- series |>
      filter(month >= start_month, month <= end_month)
    
    ts_results$result <- list(
      data = chart_data,
      states = input$ts_states,
      variable = input$ts_var,
      display = input$ts_display,
      start_month = start_month,
      end_month = end_month
    )
  })
  
  
  # Check the time series data
  ts_data <- reactive({
    result <- ts_results$result
    validate(
      need(!is.null(result), "Select inputs and click Update Chart.")
    )
    validate(
      need(nrow(result$data) > 0, "No data for this reporting period.")
    )
    result
  })
  
  
  # Interactive time series with exact values on hover
  output$ts_plot <- renderPlotly({
    result <- ts_data()
    variable_label <- names(numeric_vars)[numeric_vars == result$variable]
    is_hold <- result$variable %in% c(
      "hold_total", "hold_mobile", "hold_retail"
    )
    
    # Missing months are gaps, not zero observations
    chart_data <- result$data |>
      group_by(state) |>
      complete(
        month = seq(min(month), max(month), by = "month")
      ) |>
      ungroup() |>
      mutate(
        hover_value = if (is_hold) {
          paste0(
            formatC(value, format = "f", digits = 2, big.mark = ","),
            "%"
          )
        } else {
          paste0(
            "$",
            formatC(value, format = "f", digits = 2, big.mark = ",")
          )
        },
        hover_text = paste0(
          "State: ", state,
          "<br>Month: ", format(month, "%b %Y"),
          "<br>", variable_label, ": ", hover_value
        )
      )
    
    chart <- ggplot(
      chart_data,
      aes(
        x = month, y = value, color = state,
        group = state, text = hover_text
      )
    ) +
      geom_line(linewidth = 0.8) +
      geom_point(size = 1.5, alpha = 0.7, na.rm = TRUE) +
      scale_x_date(
        date_breaks = "1 year",
        date_labels = "%Y"
      ) +
      labs(
        title = paste("Monthly", variable_label),
        subtitle = paste(
          format(result$start_month, "%b %Y"),
          "-",
          format(result$end_month, "%b %Y")
        ),
        x = "Month",
        y = variable_label,
        color = "State"
      ) +
      theme_gray()
    
    if (is_hold) {
      chart <- chart +
        scale_y_continuous(
          labels = scales::label_number(suffix = "%")
        )
    } else {
      chart <- chart +
        scale_y_continuous(
          labels = scales::label_number(
            prefix = "$",
            scale_cut = scales::cut_short_scale()
          )
        )
    }
    
    if (result$display == "Separate panels") {
      chart <- chart +
        facet_wrap(~state, scales = "free_y") +
        guides(color = "none")
    }
    
    plotly::ggplotly(
      chart,
      tooltip = "text"
    ) |>
      plotly::layout(hovermode = "closest") |>
      plotly::config(displaylogo = FALSE)
  })
  
  
  # Explain reporting dates for all selected states
  output$ts_context <- renderUI({
    result <- ts_data()
    variable_label <- names(numeric_vars)[numeric_vars == result$variable]
    
    date_label <- function(date) {
      if (is.na(date)) "Not reported" else format(date, "%b %Y")
    }
    
    state_notes <- purrr::map_chr(
      result$states,
      function(selected_state) {
        
        series <- get_active_series(
          selected_state, result$variable
        )
        
        series_start <- if (nrow(series) > 0) {
          format(min(series$month), "%b %Y")
        } else {
          "Unavailable"
        }
        
        retail_start <- first_positive_month(
          selected_state, "handle_retail"
        )
        mobile_start <- first_positive_month(
          selected_state, "handle_mobile"
        )
        
        paste0(
          selected_state, ": ", variable_label,
          " data available since ", series_start,
          ". First positive retail handle: ",
          date_label(retail_start),
          "; first positive mobile handle: ",
          date_label(mobile_start), "."
        )
      }
    )
    
    total_amount_vars <- c(
      "handle", "revenue", "taxable_revenue",
      "taxes", "promos_dedux"
    )
    scale_states <- character()
    
    if (result$variable %in% total_amount_vars) {
      for (selected_state in result$states) {
        
        retail_start <- first_positive_month(
          selected_state, "handle_retail"
        )
        mobile_start <- first_positive_month(
          selected_state, "handle_mobile"
        )
        
        if (
          !is.na(retail_start) &&
          !is.na(mobile_start) &&
          retail_start != mobile_start &&
          result$start_month < max(retail_start, mobile_start) &&
          result$end_month >= max(retail_start, mobile_start)
        ) {
          scale_states <- c(scale_states, selected_state)
        }
      }
    }
    
    tags$div(
      style = paste(
        "margin-top: 18px; padding: 12px;",
        "background-color: #f5f5f5; border-radius: 5px;"
      ),
      tags$strong("Reporting Context"),
      lapply(state_notes, tags$p),
      if (length(scale_states) > 0) {
        tags$p(
          paste(scale_states, collapse = ", "),
          "have different starting dates for reported retail and mobile",
          "handle. Changes in channel mix may cause material differences",
          "in the scale of total-dollar amounts."
        )
      },
      tags$p(
        "Dates are first positive observations in the dataset,",
        "not necessarily official market launch dates."
      ),
      tags$p(
        "Shared scale compares actual values across states.",
        "Separate panels use independent vertical scales."
      )
    )
  })
  
  
  # Data and Summaries Filters
  
  # Limit the slider data to the selected states and years
  # The tables do not change until Apply Filters is clicked
  state_year_data <- reactive({
    
    filtered <- sports_download
    
    if (length(input$filter_states) > 0 &&
        !("ALL" %in% input$filter_states)) {
      filtered <- filtered |>
        filter(state %in% input$filter_states)
    }
    
    if (length(input$filter_years) > 0 &&
        !("ALL" %in% input$filter_years)) {
      filtered <- filtered |>
        filter(
          lubridate::year(month) %in% as.integer(input$filter_years)
        )
    }
    
    filtered
  })
  
  
  # Show a slider only when its numerical filter is enabled
  output$filter_slider1 <- renderUI({
    if (!isTRUE(input$filter_use_num1)) return(NULL)
    req(input$filter_num1)
    make_numeric_slider(
      input$filter_num1,
      "filter_range1",
      state_year_data()
    )
  })
  
  output$filter_slider2 <- renderUI({
    if (!isTRUE(input$filter_use_num2)) return(NULL)
    req(input$filter_num2)
    make_numeric_slider(
      input$filter_num2,
      "filter_range2",
      state_year_data()
    )
  })
  
  
  # Use exact bounds when entered; otherwise use the slider
  get_filter_limits <- function(slider_limits, min_text, max_text) {
    
    if (length(slider_limits) != 2) return(NULL)
    
    parse_bound <- function(value) {
      if (is.null(value) || !nzchar(trimws(value))) {
        return(NA_real_)
      }
      
      # Allow commas, dollar signs, and percent symbols
      suppressWarnings(
        as.numeric(gsub("[,$%[:space:]]", "", value))
      )
    }
    
    min_exact <- parse_bound(min_text)
    max_exact <- parse_bound(max_text)
    
    # Reject invalid entries instead of silently ignoring them
    if (
      (!is.null(min_text) &&
       nzchar(trimws(min_text)) &&
       !is.finite(min_exact)) ||
      (!is.null(max_text) &&
       nzchar(trimws(max_text)) &&
       !is.finite(max_exact))
    ) {
      return(NULL)
    }
    
    limits <- slider_limits
    if (is.finite(min_exact)) limits[1] <- min_exact
    if (is.finite(max_exact)) limits[2] <- max_exact
    
    if (limits[1] > limits[2]) return(NULL)
    
    limits
  }
  
  
  # Keep the applied observations and a description of the last filters
  applied_data <- reactiveVal(sports_download)
  applied_description <- reactiveVal(
    "All States | All Years | No numerical filters"
  )
  
  output$applied_filter_description <- renderText({
    paste("Applied filters:", applied_description())
  })
  
  
  # Numerical Summaries
  
  # Dollar amounts can be combined by month, but percentages cannot be added
  observeEvent(input$summary_var, {
    is_hold <- input$summary_var %in% c(
      "hold_total", "hold_mobile", "hold_retail"
    )
    
    choices <- if (is_hold) {
      c("Individual state-month observations" = "individual")
    } else {
      c(
        "Individual state-month observations" = "individual",
        "Combined monthly totals across states" = "monthly"
      )
    }
    
    selected_mode <- if (is_hold) {
      "individual"
    } else if (identical(input$summary_mode, "monthly")) {
      "monthly"
    } else {
      "individual"
    }
    
    updateRadioButtons(
      session,
      inputId = "summary_mode",
      choices = choices,
      selected = selected_mode
    )
  })
  
  
  # Combined monthly totals are meaningful across states
  observeEvent(input$summary_mode, {
    
    if (identical(input$summary_mode, "monthly")) {
      updateSelectInput(
        session,
        inputId = "summary_group",
        choices = c(
          "Reporting Year" = "year",
          "All Selected Data" = "overall"
        ),
        selected = if (input$summary_group %in% c("year", "overall")) {
          input$summary_group
        } else {
          "year"
        }
      )
    } else {
      updateSelectInput(
        session,
        inputId = "summary_group",
        choices = c(
          "State" = "state",
          "Reporting Year" = "year",
          "State and Reporting Year" = "both",
          "All Selected Data" = "overall"
        ),
        selected = if (
          input$summary_group %in% c("state", "year", "both", "overall")
        ) {
          input$summary_group
        } else {
          "state"
        }
      )
    }
  })
  
  
  # Begin at each state's first active reporting month
  # Keep zero and negative values after activity begins
  summary_data <- reactive({
    
    req(input$summary_var, input$summary_group, input$summary_mode)
    
    filtered <- applied_data()
    
    validate(
      need(
        nrow(filtered) > 0,
        "No observations match the applied filters."
      )
    )
    
    is_hold <- input$summary_var %in% c(
      "hold_total", "hold_mobile", "hold_retail"
    )
    is_monthly <- identical(input$summary_mode, "monthly")
    
    validate(
      need(
        !(is_hold && is_monthly),
        "Combined monthly totals are only available for dollar variables."
      )
    )
    
    validate(
      need(
        !is_monthly || input$summary_group %in% c("year", "overall"),
        "Choose Reporting Year or All Selected Data for monthly totals."
      )
    )
    
    start_dates <- purrr::map_dfr(
      unique(filtered$state),
      function(selected_state) {
        
        series <- get_active_series(
          selected_state,
          input$summary_var
        )
        
        tibble(
          state = selected_state,
          first_month = if (nrow(series) > 0) {
            min(series$month)
          } else {
            as.Date(NA)
          }
        )
      }
    )
    
    selected_data <- filtered |>
      left_join(start_dates, by = "state") |>
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
        nrow(selected_data) > 0,
        "No active reporting observations remain for this variable."
      )
    )
    
    active_rows <- nrow(selected_data)
    
    if (is_monthly) {
      
      # First sum selected states for each month
      # Missing values are excluded, not replaced with zero
      analysis_data <- selected_data |>
        group_by(month) |>
        summarise(
          summary_value = if (any(is.finite(summary_value))) {
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
      
      # Keep each state-month as a separate observation
      analysis_data <- selected_data
    }
    
    analysis_rows <- nrow(analysis_data)
    
    # Group the observations for the final statistics
    if (input$summary_group == "state") {
      analysis_data <- analysis_data |>
        group_by(state)
      
    } else if (input$summary_group == "year") {
      analysis_data <- analysis_data |>
        group_by(reporting_year)
      
    } else if (input$summary_group == "both") {
      analysis_data <- analysis_data |>
        group_by(state, reporting_year)
    }
    
    # Calculate the statistics
    stats <- analysis_data |>
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
    
    # Percentages cannot be added into dollar totals
    if (is_hold) {
      stats <- stats |>
        select(-Total)
    }
    
    # Monthly mode counts months instead of state-month observations
    if (is_monthly) {
      stats <- stats |>
        rename(Months = Rows)
    }
    
    list(
      table = stats,
      filtered_rows = nrow(filtered),
      active_rows = active_rows,
      analysis_rows = analysis_rows,
      is_hold = is_hold,
      is_monthly = is_monthly,
      variable = input$summary_var
    )
  })
  
  
  # Show the observation count for the chosen calculation method
  output$summary_count <- renderText({
    
    result <- summary_data()
    
    if (result$is_monthly) {
      paste0(
        "Combined ",
        format(result$active_rows, big.mark = ","),
        " active state-month observations into ",
        format(result$analysis_rows, big.mark = ","),
        " monthly totals (from ",
        format(result$filtered_rows, big.mark = ","),
        " applied observations)."
      )
    } else {
      paste0(
        "Using ",
        format(result$active_rows, big.mark = ","),
        " of ",
        format(result$filtered_rows, big.mark = ","),
        " applied observations."
      )
    }
  })
  
  
  # Display the summary statistics
  output$summary_table <- DT::renderDT({
    
    result <- summary_data()
    
    table <- DT::datatable(
      result$table,
      rownames = FALSE,
      options = list(
        pageLength = 10,
        scrollX = TRUE
      )
    )
    
    stat_cols <- c(
      "Mean", "Median", "SD", "Minimum", "Maximum"
    )
    
    if (result$is_hold) {
      table |>
        DT::formatCurrency(
          columns = stat_cols,
          currency = "%",
          digits = 2,
          before = FALSE
        )
    } else {
      table |>
        DT::formatRound(
          columns = c(stat_cols, "Total"),
          digits = 2
        )
    }
  })
  
  
  # Explain the calculation method
  output$summary_note <- renderUI({
    
    result <- summary_data()
    
    tags$div(
      style = paste(
        "margin-top: 18px; padding: 12px;",
        "background-color: #f5f5f5; border-radius: 5px;"
      ),
      
      tags$strong("Summary Notes"),
      
      tags$p(
        "Statistics use the filters last applied in Data & Summaries.",
        "Each state begins at its first active reporting month.",
        "Prelaunch observations are excluded, but later zeros",
        "and negative values are retained."
      ),
      
      if (result$is_monthly) {
        tags$p(
          "Combined monthly totals: first sum available dollar",
          "values across selected states for each month. Then",
          "calculate the mean, median, sample SD, minimum, and",
          "maximum of those monthly totals within each reporting",
          "year or across all selected months. Months counts",
          "monthly totals, not state-months."
        )
      } else {
        tags$p(
          "Individual observations: each state-month contributes",
          "one value to the mean, median, and sample SD."
        )
      },
      
      tags$p(
        "Reported counts valid numerical values; Missing counts",
        "unavailable values. Missing amounts are skipped, not",
        "converted to zeros. With monthly totals, the number of",
        "states contributing can vary if data are unavailable or",
        "excluded by numerical filters."
      ),
      
      if (result$is_hold) {
        tags$p(
          "Hold statistics are percentages and use an unweighted",
          "mean. Hold percentages cannot be summed, so monthly",
          "total mode is unavailable."
        )
      } else {
        tags$p(
          "Total is the sum of reported dollar values. If the same",
          "observations are included, Total is unchanged between",
          "the two calculation methods."
        )
      }
    )
  })
  
  
  # Apply the selected filters
  observeEvent(input$apply_filters, {
    
    filtered <- state_year_data()
    
    # Describe the filters used in the latest run
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
    
    filter_notes <- character()
    
    number_label <- function(number) {
      format(
        number,
        trim = TRUE,
        scientific = FALSE,
        big.mark = ","
      )
    }
    
    # Apply the first enabled numerical filter
    if (isTRUE(input$filter_use_num1)) {
      
      limits <- get_filter_limits(
        input$filter_range1,
        input$filter_min1,
        input$filter_max1
      )
      
      if (is.null(limits)) {
        showNotification(
          "Check the range for Filter 1.",
          type = "warning"
        )
        return()
      }
      
      selected_var <- input$filter_num1
      
      filter_notes <- c(
        filter_notes,
        paste0(
          names(numeric_vars)[numeric_vars == selected_var],
          ": ",
          number_label(limits[1]),
          " to ",
          number_label(limits[2])
        )
      )
      
      filtered <- filtered |>
        filter(
          is.finite(.data[[selected_var]]),
          .data[[selected_var]] >= limits[1],
          .data[[selected_var]] <= limits[2]
        )
    }
    
    
    # Apply the second enabled numerical filter
    if (isTRUE(input$filter_use_num2)) {
      
      limits <- get_filter_limits(
        input$filter_range2,
        input$filter_min2,
        input$filter_max2
      )
      
      if (is.null(limits)) {
        showNotification(
          "Check the range for Filter 2.",
          type = "warning"
        )
        return()
      }
      
      selected_var <- input$filter_num2
      
      filter_notes <- c(
        filter_notes,
        paste0(
          names(numeric_vars)[numeric_vars == selected_var],
          ": ",
          number_label(limits[1]),
          " to ",
          number_label(limits[2])
        )
      )
      
      filtered <- filtered |>
        filter(
          is.finite(.data[[selected_var]]),
          .data[[selected_var]] >= limits[1],
          .data[[selected_var]] <= limits[2]
        )
    }
    
    # Save both the filtered data and its description
    applied_data(filtered)
    
    applied_description(
      paste(
        c(
          state_label,
          year_label,
          if (length(filter_notes) == 0) {
            "No numerical filters"
          } else {
            filter_notes
          }
        ),
        collapse = " | "
      )
    )
  }, ignoreInit = TRUE)
  
  
  # Display the number of filtered observations
  output$filter_summary <- renderText({
    
    paste0(
      "Showing ",
      format(nrow(applied_data()), big.mark = ","),
      " of ",
      format(nrow(sports_download), big.mark = ","),
      " observations."
    )
  })
  
  
  # Format the data table without changing downloaded values
  output$sports_table <- DT::renderDT({
    
    hold_cols <- c(
      "hold_total",
      "hold_mobile",
      "hold_retail"
    )
    
    numeric_cols <- names(applied_data())[
      vapply(
        applied_data(),
        is.numeric,
        logical(1)
      )
    ]
    
    amount_cols <- setdiff(numeric_cols, hold_cols)
    
    DT::datatable(
      applied_data(),
      rownames = FALSE,
      options = list(
        pageLength = 10,
        scrollX = TRUE
      )
    ) |>
      DT::formatRound(
        columns = amount_cols,
        digits = 2
      ) |>
      DT::formatCurrency(
        columns = hold_cols,
        currency = "%",
        digits = 2,
        before = FALSE
      )
  })
  
  
  # Download exactly the rows selected by the applied filters
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
}


# Step 5: Run the Shiny Application

shinyApp(
  ui = ui,
  server = server
)
