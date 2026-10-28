
# ST558 Project 2 - U.S. Sports Betting Explorer
# Interactive application for state-level sports betting data


# Step 1: Load Required Packages and Data

library(shiny)
library(tidyverse)

# Read in the cleaned sports betting data
sports <- read_csv(
  "data/sports_betting_clean.csv",
  show_col_types = FALSE
)

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
  
  # switch() selects the activity rule for the chosen variable
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
        inputId = "run_corr",
        label = "Run Correlation"
      )
      
    ),
    
    mainPanel(
      
      tabsetPanel(
        
        # About tab
        tabPanel(
          "About",
          
          h3("About This Application"),
          
          p(
            "Explore historical U.S. sports betting data,
             including handle, revenue, taxes, and hold percentages."
          ),
          
          p(
            "The dataset contains monthly state-level observations
             from 2018 through August 2026."
          ),
          
          p(
            "Select states, numerical variables, and a reporting
             period to examine correlations between monthly observations."
          )
        ),
        
        # Data Download tab
        tabPanel(
          "Data Download",
          
          h3("View and Download Data"),
          
          p("The data table and download option will go here.")
        ),
        
        # Data Exploration tab
        tabPanel(
          "Data Exploration",
          
          h3("Correlation Between Selected Variables"),
          
          plotOutput(
            outputId = "corr_plot",
            height = "450px"
          ),
          
          textOutput("corr_result"),
          
          # Note explaining reporting periods and market differences
          uiOutput("market_note")
        )
        
      )
      
    )
    
  )
  
)


# Step 4: Define the Server

server <- function(input, output, session) {
  
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
    
    # Classify each month based on mobile-market activity
    # A state remains mobile-active after its first mobile month
    
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
      data = result$data,
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
          result$state_x, "vs", result$state_y
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
    
    # Format a missing or available starting date
    date_label <- function(date) {
      if (is.na(date)) {
        return("No positive activity reported")
      }
      
      format(date, "%b %Y")
    }
    
    # Starting date for each selected variable
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
    
    # Build a reporting description for each state
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
        ". Retail activity: ", date_label(retail_start),
        "; mobile activity: ", date_label(mobile_start), "."
      )
      
    }
    
    x_note <- state_note(result$state_x, result$var_x)
    y_note <- state_note(result$state_y, result$var_y)
    
    # Identify total-dollar series that span a retail-only period
    # These observations may differ substantially in scale
    
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
    
    # Display the note underneath the correlation result
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
  
}


# Step 5: Run the Shiny Application

shinyApp(
  ui = ui,
  server = server
)
