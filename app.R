
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
    return(tibble(
      month = as.Date(character()),
      value = numeric()
    ))
  }
  
  total_active <- !is.na(state_data$handle) &
    state_data$handle > 0
  
  mobile_active <- !is.na(state_data$handle_mobile) &
    state_data$handle_mobile > 0
  
  retail_active <- !is.na(state_data$handle_retail) &
    state_data$handle_retail > 0
  
  selected_values <- state_data[[selected_var]]
  
  nonzero_value <- is.finite(selected_values) &
    selected_values != 0
  
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
    return(tibble(
      month = as.Date(character()),
      value = numeric()
    ))
  }
  
  first_month <- min(state_data$month[active])
  
  state_data |>
    filter(
      month >= first_month,
      is.finite(.data[[selected_var]])
    ) |>
    transmute(
      month,
      value = .data[[selected_var]]
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
    
    
    # Data Download tab
    tabPanel(
      "Data Download",
      
      h3("View and Download Data"),
      
      # Show the primary columns only
      DT::DTOutput("sports_table"),
      
      # Download the cleaned data as a CSV
      downloadButton(
        outputId = "download_data",
        label = "Download Data (CSV)"
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
        
        
        # Time Series tab
        tabPanel(
          "Time Series",
          
          sidebarLayout(
            
            sidebarPanel(
              h3("Time Series Analysis"),
              
              # Select one or more states
              selectizeInput(
                inputId = "ts_states",
                label = "States",
                choices = state_choices,
                selected = "NY",
                multiple = TRUE
              ),
              
              # Explain how to select additional states
              helpText(
                "Click inside the box to add more states.",
                "Multiple states can be selected."
              ),
              
              # Select the variable to plot
              selectizeInput(
                inputId = "ts_var",
                label = "Variable",
                choices = numeric_vars,
                selected = "handle"
              ),
              
              # Reporting dates for the selected series
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
              
              # Compare actual sizes or show each state's trend
              selectInput(
                inputId = "ts_display",
                label = "Chart Layout",
                choices = c(
                  "Shared scale",
                  "Separate panels"
                ),
                selected = "Shared scale"
              ),
              
              actionButton(
                inputId = "run_ts",
                label = "Update Chart"
              )
            ),
            
            mainPanel(
              h3("Monthly Sports Betting Trends"),
              
              # Interactive chart with hover values
              plotlyOutput(
                "ts_plot",
                height = "500px"
              ),
              
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
    
    inner_join(
      x_data,
      y_data,
      by = "month"
    ) |>
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
  corr_results <- reactiveValues(
    result = NULL
  )
  
  
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
      
      start_month <- max(
        start_month,
        min(paired_data$month)
      )
      
      end_month <- min(
        end_month,
        max(paired_data$month)
      )
    }
    
    paired_data <- paired_data |>
      filter(
        month >= start_month,
        month <= end_month
      )
    
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
    
    x_label <- names(numeric_vars)[
      numeric_vars == result$var_x
    ]
    
    y_label <- names(numeric_vars)[
      numeric_vars == result$var_y
    ]
    
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
        title = "Correlation Analysis",
        subtitle = paste(
          result$state_x,
          "vs",
          result$state_y
        ),
        x = paste(result$state_x, x_label),
        y = paste(result$state_y, y_label)
      ) +
      theme_gray()
  })
  
  
  # Calculate the Pearson correlation coefficient
  output$corr_result <- renderText({
    
    result <- corr_data()
    
    correlation <- cor(
      result$data$x,
      result$data$y
    )
    
    paste0(
      "Pearson correlation (r): ",
      round(correlation, 3),
      " | Matched months: ",
      nrow(result$data),
      " | Period: ",
      format(result$start_month, "%b %Y"),
      " - ",
      format(result$end_month, "%b %Y")
    )
  })
  
  
  # Explain the selected states' reporting history
  # Use the selections saved when Run Correlation was clicked
  output$market_note <- renderUI({
    
    result <- corr_data()
    
    date_label <- function(date) {
      if (is.na(date)) {
        return("No positive activity reported")
      }
      
      format(date, "%b %Y")
    }
    
    series_start <- function(selected_state, selected_var) {
      
      series <- get_active_series(
        selected_state,
        selected_var
      )
      
      if (nrow(series) == 0) {
        return("Unavailable")
      }
      
      format(min(series$month), "%b %Y")
    }
    
    # Describe each state's selected variable and channel activity
    state_note <- function(selected_state, selected_var) {
      
      variable_label <- names(numeric_vars)[
        numeric_vars == selected_var
      ]
      
      retail_start <- first_positive_month(
        selected_state,
        "handle_retail"
      )
      
      mobile_start <- first_positive_month(
        selected_state,
        "handle_mobile"
      )
      
      paste0(
        selected_state, " ", variable_label,
        " data available since ",
        series_start(selected_state, selected_var),
        ". Retail activity: ",
        date_label(retail_start),
        "; mobile activity: ",
        date_label(mobile_start), "."
      )
    }
    
    x_note <- state_note(
      result$state_x,
      result$var_x
    )
    
    y_note <- state_note(
      result$state_y,
      result$var_y
    )
    
    # Flag total-dollar series that span a retail-only period
    total_amount_vars <- c(
      "handle", "revenue", "taxable_revenue",
      "taxes", "promos_dedux"
    )
    
    scale_notes <- character()
    
    for (axis in c("x", "y")) {
      
      selected_state <- result[[paste0("state_", axis)]]
      selected_var <- result[[paste0("var_", axis)]]
      
      retail_start <- first_positive_month(
        selected_state,
        "handle_retail"
      )
      
      mobile_start <- first_positive_month(
        selected_state,
        "handle_mobile"
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
      
      tags$p(
        paste0(
          "Comparison: ",
          result$state_x, " ",
          names(numeric_vars)[numeric_vars == result$var_x],
          " vs ",
          result$state_y, " ",
          names(numeric_vars)[numeric_vars == result$var_y],
          "."
        )
      ),
      
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
  
  # Collect the selected variable for each selected state
  # Each state retains its own active reporting period
  ts_series <- reactive({
    
    req(input$ts_var)
    
    if (length(input$ts_states) == 0) {
      return(tibble(
        month = as.Date(character()),
        value = numeric(),
        state = character()
      ))
    }
    
    purrr::map_dfr(
      input$ts_states,
      function(selected_state) {
        
        get_active_series(
          selected_state,
          input$ts_var
        ) |>
          mutate(state = selected_state)
        
      }
    ) |>
      arrange(state, month)
  })
  
  
  # Display the available reporting period
  output$ts_available_period <- renderText({
    
    series <- ts_series()
    
    if (nrow(series) == 0) {
      return("No reporting data for these selections.")
    }
    
    paste0(
      "Available: ",
      format(min(series$month), "%b %Y"),
      " - ",
      format(max(series$month), "%b %Y")
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
  
  
  # Store the selected data only when Update Chart is clicked
  ts_results <- reactiveValues(
    result = NULL
  )
  
  
  observeEvent(input$run_ts, {
    
    req(input$ts_dates)
    req(length(input$ts_states) > 0)
    
    start_month <- lubridate::floor_date(
      input$ts_dates[1],
      unit = "month"
    )
    
    end_month <- lubridate::floor_date(
      input$ts_dates[2],
      unit = "month"
    )
    
    series <- ts_series()
    
    if (nrow(series) > 0) {
      
      start_month <- max(
        start_month,
        min(series$month)
      )
      
      end_month <- min(
        end_month,
        max(series$month)
      )
    }
    
    chart_data <- series |>
      filter(
        month >= start_month,
        month <= end_month
      )
    
    ts_results$result <- list(
      data = chart_data,
      states = input$ts_states,
      variable = input$ts_var,
      display = input$ts_display,
      start_month = start_month,
      end_month = end_month
    )
  })
  
  
  # Check the data before creating the chart
  ts_data <- reactive({
    
    result <- ts_results$result
    
    validate(
      need(
        !is.null(result),
        "Select your inputs and click Update Chart."
      )
    )
    
    validate(
      need(
        nrow(result$data) > 0,
        "No observations are available for this reporting period."
      )
    )
    
    result
  })
  
  
  # Create an interactive monthly time series chart
  output$ts_plot <- renderPlotly({
    
    result <- ts_data()
    
    variable_label <- names(numeric_vars)[
      numeric_vars == result$variable
    ]
    
    # Complete the monthly sequence so missing months remain gaps
    chart_data <- result$data |>
      group_by(state) |>
      complete(
        month = seq(min(month), max(month), by = "month")
      ) |>
      ungroup()
    
    # Format hover values as dollars or percentages
    # Hold variables are already stored as percentages
    if (result$variable %in% c(
      "hold_total",
      "hold_mobile",
      "hold_retail"
    )) {
      
      chart_data <- chart_data |>
        mutate(
          hover_value = paste0(
            formatC(
              value,
              format = "f",
              digits = 2,
              big.mark = ","
            ),
            "%"
          )
        )
      
    } else {
      
      chart_data <- chart_data |>
        mutate(
          hover_value = paste0(
            "$",
            formatC(
              value,
              format = "f",
              digits = 2,
              big.mark = ","
            )
          )
        )
    }
    
    # Build the information displayed when hovering
    chart_data <- chart_data |>
      mutate(
        hover_text = paste0(
          "State: ", state,
          "<br>Month: ", format(month, "%b %Y"),
          "<br>", variable_label, ": ", hover_value
        )
      )
    
    # Build the chart with ggplot2
    chart <- ggplot(
      chart_data,
      aes(
        x = month,
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
    
    # Format the vertical axis
    if (result$variable %in% c(
      "hold_total",
      "hold_mobile",
      "hold_retail"
    )) {
      
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
    
    # Separate panels for states with different market sizes
    if (result$display == "Separate panels") {
      
      chart <- chart +
        facet_wrap(
          ~state,
          scales = "free_y"
        ) +
        guides(color = "none")
    }
    
    # Convert the ggplot chart to an interactive Plotly chart
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
  
  
  # Explain reporting history for every selected state
  output$ts_context <- renderUI({
    
    result <- ts_data()
    
    variable_label <- names(numeric_vars)[
      numeric_vars == result$variable
    ]
    
    # Format the first reported activity month
    date_label <- function(date) {
      if (is.na(date)) {
        return("Not reported")
      }
      
      format(date, "%b %Y")
    }
    
    # Build a reporting note for each selected state
    state_notes <- purrr::map_chr(
      result$states,
      function(selected_state) {
        
        state_series <- get_active_series(
          selected_state,
          result$variable
        )
        
        series_start <- if (nrow(state_series) > 0) {
          format(min(state_series$month), "%b %Y")
        } else {
          "Unavailable"
        }
        
        retail_start <- first_positive_month(
          selected_state,
          "handle_retail"
        )
        
        mobile_start <- first_positive_month(
          selected_state,
          "handle_mobile"
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
    
    # Identify states with different retail and mobile reporting dates
    total_amount_vars <- c(
      "handle",
      "revenue",
      "taxable_revenue",
      "taxes",
      "promos_dedux"
    )
    
    scale_states <- character()
    
    if (result$variable %in% total_amount_vars) {
      
      for (selected_state in result$states) {
        
        retail_start <- first_positive_month(
          selected_state,
          "handle_retail"
        )
        
        mobile_start <- first_positive_month(
          selected_state,
          "handle_mobile"
        )
        
        if (
          !is.na(retail_start) &&
          !is.na(mobile_start) &&
          retail_start != mobile_start &&
          result$start_month < max(retail_start, mobile_start) &&
          result$end_month >= max(retail_start, mobile_start)
        ) {
          
          scale_states <- c(
            scale_states,
            selected_state
          )
        }
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
      
      # Show each state's reporting dates
      lapply(state_notes, tags$p),
      
      if (length(scale_states) > 0) {
        tags$p(
          paste(scale_states, collapse = ", "),
          "have different starting dates for reported retail and mobile",
          "handle. Changes in the reported channel mix may cause",
          "substantial differences in the scale of total-dollar amounts.",
          "Consider this when selecting the reporting period."
        )
      },
      
      tags$p(
        "Dates represent the first positive observations in the dataset,",
        "not necessarily official market launch dates."
      ),
      
      tags$p(
        "Shared scale compares actual values across states.",
        "Separate panels use independent vertical scales to make",
        "trends easier to compare."
      )
    )
  })
  
  
  # Data Download
  
  # Display the primary sports betting columns
  # Show financial values with two decimals and hold values with %
  output$sports_table <- DT::renderDT({
    
    hold_cols <- c(
      "hold_total",
      "hold_mobile",
      "hold_retail"
    )
    
    numeric_cols <- names(sports_download)[
      sapply(sports_download, is.numeric)
    ]
    
    amount_cols <- setdiff(numeric_cols, hold_cols)
    
    DT::datatable(
      sports_download,
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
  
  
  # Download the same columns shown in the table
  output$download_data <- downloadHandler(
    
    filename = function() {
      "sports_betting_data.csv"
    },
    
    content = function(file) {
      readr::write_csv(
        sports_download,
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
