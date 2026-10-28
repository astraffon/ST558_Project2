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
  month = "Monthly", quarter = "Quarterly", year = "Annual",
  t3m = "Trailing 3 Months", ttm = "Trailing 12 Months", ytd = "YTD"
)

# Step 2: Load helper functions
source("helpers.R", local = TRUE)

category_vars <- c(
  "State" = "state",
  "Reporting Year" = "reporting_year",
  "Positive Channel Activity" = "channel_status"
)

plot_choices <- c(
  "Grouped Bar Chart - Channel Activity by Year" = "bar",
  "Histogram - Numerical Distribution" = "histogram",
  "Boxplot - Values by Reporting Year" = "boxplot",
  "Density Curves - Distributions by Year" = "density",
  "Faceted Histogram" = "facets",
  "Heatmap - State by Reporting Year" = "heatmap"
)

# Step 3: Define the UI
ui <- fluidPage(
  titlePanel("U.S. Sports Betting Explorer"),
  tabsetPanel(
    tabPanel(
      "About",
      h3("About This Application"),
      p("Explore historical U.S. sports betting data, including handle, revenue, taxes, and hold percentages."),
      p("The dataset contains monthly state-level observations from 2018 through August 2026."),
      p("Data & Summaries has shared filters for the data table and numerical summaries."),
      p("Correlation and Time Series have independent controls under Data Exploration.")
    ),

    # Data Table and Numerical Summaries share these filters only
    tabPanel(
      "Data & Summaries",
      sidebarLayout(
        sidebarPanel(
          h3("Dataset Filters"),
          selectizeInput(
            "filter_states", "States",
            choices = c("All States" = "ALL", state_choices),
            selected = "ALL", multiple = TRUE
          ),
          selectizeInput(
            "filter_years", "Reporting Years",
            choices = c("All Years" = "ALL",
                        as.character(sort(unique(lubridate::year(sports$month))))),
            selected = "ALL", multiple = TRUE
          ),
          helpText("Remove 'All States' or 'All Years' to select specific values."),
          h4("Numerical Filter 1"),
          checkboxInput("filter_use_num1", "Enable Filter 1", FALSE),
          conditionalPanel(
            condition = "input.filter_use_num1",
            selectInput("filter_num1", "Variable", numeric_vars, selected = "handle"),
            uiOutput("filter_slider1"),
            fluidRow(
              column(6, textInput("filter_min1", "Exact minimum (optional)", value = "")),
              column(6, textInput("filter_max1", "Exact maximum (optional)", value = ""))
            )
          ),
          h4("Numerical Filter 2"),
          checkboxInput("filter_use_num2", "Enable Filter 2", FALSE),
          conditionalPanel(
            condition = "input.filter_use_num2",
            selectInput("filter_num2", "Variable", numeric_vars, selected = "revenue"),
            uiOutput("filter_slider2"),
            fluidRow(
              column(6, textInput("filter_min2", "Exact minimum (optional)", value = "")),
              column(6, textInput("filter_max2", "Exact maximum (optional)", value = ""))
            )
          ),
          helpText("Exact values override their corresponding slider endpoints."),
          actionButton("apply_filters", "Apply Filters"),
          helpText("Only the Data Table and Numerical Summaries use these filters.")
        ),
        mainPanel(
          h3("Data and Summary Statistics"),
          textOutput("filter_summary"),
          textOutput("applied_filter_description"),
          tabsetPanel(
            tabPanel(
              "Data Table",
              DT::DTOutput("sports_table"),
              downloadButton("download_data", "Download Data (CSV)")
            ),
            tabPanel(
              "Numerical Summaries",
              selectInput("summary_var", "Summary Variable", numeric_vars, selected = "handle"),
              radioButtons(
                "summary_mode", "Calculation Method",
                choices = c("Individual state-month observations" = "individual",
                            "Combined monthly totals across states" = "monthly"),
                selected = "individual"
              ),
              helpText("Combined monthly totals are for dollar variables only."),
              selectInput(
                "summary_group", "Group Results By",
                choices = c("State" = "state", "Reporting Year" = "year",
                            "State and Reporting Year" = "both", "All Selected Data" = "overall"),
                selected = "state"
              ),
              textOutput("summary_count"),
              DT::DTOutput("summary_table"),
              uiOutput("summary_note")
            ),
            # Categorical one-way and two-way tables
            tabPanel(
              "Categorical Summaries",
              h4("One-Way Frequency Table"),
              selectInput(
                "cat_one", "Categorical Variable",
                choices = category_vars, selected = "channel_status"
              ),
              DT::DTOutput("cat_one_table"),
              h4("Two-Way Contingency Table"),
              fluidRow(
                column(6, selectInput(
                  "cat_row", "Rows", category_vars, selected = "state"
                )),
                column(6, selectInput(
                  "cat_col", "Columns", category_vars,
                  selected = "reporting_year"
                ))
              ),
              DT::DTOutput("cat_two_table"),
              helpText(
                "Counts represent monthly state observations remaining",
                "after the last Apply Filters click. Channel categories",
                "reflect positive reported handle, not official launch dates."
              )
            ),
            # Additional graphical summaries driven by the applied filters
            tabPanel(
              "Additional Plots",
              selectInput(
                "graph_type", "Plot Type", plot_choices, selected = "bar"
              ),
              conditionalPanel(
                condition = "input.graph_type != 'bar'",
                selectInput(
                  "graph_var", "Numerical Variable", numeric_vars,
                  selected = "handle"
                )
              ),
              conditionalPanel(
                condition = "input.graph_type == 'histogram' || input.graph_type == 'facets'",
                sliderInput("graph_bins", "Histogram Bins", min = 5,
                            max = 80, value = 30, step = 5)
              ),
              conditionalPanel(
                condition = "input.graph_type == 'facets'",
                selectInput(
                  "graph_facet", "Facet By",
                  choices = c("Reporting Year" = "reporting_year",
                              "State" = "state"),
                  selected = "reporting_year"
                )
              ),
              plotOutput("extra_plot", height = "590px"),
              textOutput("extra_note")
            )
          )
        )
      )
    ),

    # Independent chart controls, separate from Data & Summaries
    tabPanel(
      "Data Exploration",
      tabsetPanel(
        tabPanel(
          "Correlation",
          sidebarLayout(
            sidebarPanel(
              h3("Correlation Analysis"),
              h4("X-Axis"),
              selectInput("corr_state_x", "State", state_choices, selected = "NY"),
              selectInput("corr_x", "Variable", numeric_vars, selected = "handle"),
              h4("Y-Axis"),
              selectInput("corr_state_y", "State", state_choices, selected = "NJ"),
              selectInput("corr_y", "Variable", numeric_vars, selected = "handle"),
              selectInput("corr_period", "Reporting Period", period_choices, selected = "month"),
              selectInput("corr_view", "Display Metric", comparison_choices("handle"), selected = "level"),
              dateRangeInput(
                "corr_dates", "Date Range",
                start = min(sports$month), end = max(sports$month),
                min = min(sports$month), max = max(sports$month),
                format = "M yyyy", separator = " to "
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
        tabPanel(
          "Time Series",
          sidebarLayout(
            sidebarPanel(
              h3("Time Series Analysis"),
              selectizeInput("ts_states", "States", state_choices,
                             selected = "NY", multiple = TRUE),
              helpText("Click inside the box to add more states."),
              selectInput("ts_var", "Variable", numeric_vars, selected = "handle"),
              selectInput("ts_period", "Reporting Period", period_choices, selected = "month"),
              selectInput("ts_view", "Display Metric", comparison_choices("handle"), selected = "level"),
              dateRangeInput(
                "ts_dates", "Date Range",
                start = min(sports$month), end = max(sports$month),
                min = min(sports$month), max = max(sports$month),
                format = "M yyyy", separator = " to "
              ),
              textOutput("ts_available_period"),
              selectInput("ts_display", "Chart Layout",
                          c("Shared scale", "Separate panels"), selected = "Shared scale"),
              actionButton("run_ts", "Update Chart")
            ),
            mainPanel(
              h3("Sports Betting Trends"),
              plotlyOutput("ts_plot", height = "500px"),
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

  # Data & Summaries filters (do not affect Correlation or Time Series)
  state_year_data <- reactive({
    filtered <- sports_download
    if (length(input$filter_states) > 0 && !("ALL" %in% input$filter_states)) {
      filtered <- filtered |> filter(state %in% input$filter_states)
    }
    if (length(input$filter_years) > 0 && !("ALL" %in% input$filter_years)) {
      filtered <- filtered |>
        filter(lubridate::year(month) %in% as.integer(input$filter_years))
    }
    filtered
  })

  output$filter_slider1 <- renderUI({
    if (!isTRUE(input$filter_use_num1)) return(NULL)
    req(input$filter_num1)
    make_numeric_slider(input$filter_num1, "filter_range1", state_year_data())
  })
  output$filter_slider2 <- renderUI({
    if (!isTRUE(input$filter_use_num2)) return(NULL)
    req(input$filter_num2)
    make_numeric_slider(input$filter_num2, "filter_range2", state_year_data())
  })

  # Apply button controls both the table and numerical summaries
  applied_data <- reactiveVal(sports_download)
  applied_description <- reactiveVal("All States | All Years | No numerical filters")

  observeEvent(input$apply_filters, {
    filtered <- state_year_data()
    state_label <- if (length(input$filter_states) == 0 || "ALL" %in% input$filter_states) {
      "All States"
    } else paste(input$filter_states, collapse = ", ")
    year_label <- if (length(input$filter_years) == 0 || "ALL" %in% input$filter_years) {
      "All Years"
    } else paste(input$filter_years, collapse = ", ")
    notes <- character()

    for (i in 1:2) {
      if (!isTRUE(input[[paste0("filter_use_num", i)]])) next
      selected_var <- input[[paste0("filter_num", i)]]
      limits <- get_filter_limits(
        input[[paste0("filter_range", i)]],
        input[[paste0("filter_min", i)]],
        input[[paste0("filter_max", i)]]
      )
      if (is.null(limits)) {
        showNotification(paste("Check the range for Filter", i), type = "warning")
        return()
      }
      filtered <- filtered |>
        filter(is.finite(.data[[selected_var]]),
               .data[[selected_var]] >= limits[1],
               .data[[selected_var]] <= limits[2])
      notes <- c(notes, paste0(
        names(numeric_vars)[numeric_vars == selected_var], ": ",
        format(limits[1], big.mark = ",", scientific = FALSE), " to ",
        format(limits[2], big.mark = ",", scientific = FALSE)
      ))
    }
    applied_data(filtered)
    applied_description(paste(
      c(state_label, year_label,
        if (length(notes) == 0) "No numerical filters" else notes),
      collapse = " | "
    ))
  }, ignoreInit = TRUE)

  output$filter_summary <- renderText({
    paste0("Showing ", format(nrow(applied_data()), big.mark = ","),
           " of ", format(nrow(sports_download), big.mark = ","), " observations.")
  })
  output$applied_filter_description <- renderText({
    paste("Applied filters:", applied_description())
  })

  # Data Table: formatting does not alter the CSV values
  output$sports_table <- DT::renderDT({
    numeric_cols <- names(applied_data())[
      vapply(applied_data(), is.numeric, logical(1))
    ]
    DT::datatable(
      applied_data(), rownames = FALSE,
      options = list(pageLength = 10, scrollX = TRUE)
    ) |>
      DT::formatRound(columns = setdiff(numeric_cols, hold_vars), digits = 2) |>
      DT::formatCurrency(columns = hold_vars, currency = "%", digits = 2,
                         before = FALSE)
  })

  output$download_data <- downloadHandler(
    filename = function() "sports_betting_data.csv",
    content = function(file) readr::write_csv(applied_data(), file)
  )

  # Numerical Summaries: switch calculation method and grouping
  observeEvent(input$summary_var, {
    is_hold <- input$summary_var %in% hold_vars
    choices <- if (is_hold) {
      c("Individual state-month observations" = "individual")
    } else {
      c("Individual state-month observations" = "individual",
        "Combined monthly totals across states" = "monthly")
    }
    current <- isolate(input$summary_mode)
    selected <- if (is_hold) "individual" else if (identical(current, "monthly")) "monthly" else "individual"
    updateRadioButtons(session, "summary_mode", choices = choices, selected = selected)
  })

  observeEvent(input$summary_mode, {
    is_monthly <- identical(input$summary_mode, "monthly")
    choices <- if (is_monthly) {
      c("Reporting Year" = "year", "All Selected Data" = "overall")
    } else {
      c("State" = "state", "Reporting Year" = "year",
        "State and Reporting Year" = "both", "All Selected Data" = "overall")
    }
    current <- isolate(input$summary_group)
    selected <- if (current %in% unname(choices)) current else if (is_monthly) "year" else "state"
    updateSelectInput(session, "summary_group", choices = choices, selected = selected)
  })

  summary_data <- reactive({
    req(input$summary_var, input$summary_mode, input$summary_group)
    filtered <- applied_data()
    validate(need(nrow(filtered) > 0, "No observations match the applied filters."))

    is_hold <- input$summary_var %in% hold_vars
    is_monthly <- identical(input$summary_mode, "monthly")
    validate(need(!(is_hold && is_monthly),
                  "Combined monthly totals are only available for dollar variables."))
    validate(need(!is_monthly || input$summary_group %in% c("year", "overall"),
                  "Choose Reporting Year or All Selected Data."))

    starts <- purrr::map_dfr(unique(filtered$state), function(s) {
      tibble(state = s, first_month = first_active_month(s, input$summary_var))
    })
    selected <- filtered |>
      left_join(starts, by = "state") |>
      filter(!is.na(first_month), month >= first_month) |>
      mutate(reporting_year = lubridate::year(month),
             summary_value = .data[[input$summary_var]])
    validate(need(nrow(selected) > 0, "No active reporting observations remain."))
    active_rows <- nrow(selected)

    if (is_monthly) {
      analysis <- selected |>
        group_by(month) |>
        summarise(
          summary_value = if (any(is.finite(summary_value))) {
            sum(summary_value[is.finite(summary_value)])
          } else NA_real_,
          .groups = "drop"
        ) |>
        mutate(reporting_year = lubridate::year(month))
    } else analysis <- selected
    analysis_rows <- nrow(analysis)

    if (input$summary_group == "state") analysis <- analysis |> group_by(state)
    if (input$summary_group == "year") analysis <- analysis |> group_by(reporting_year)
    if (input$summary_group == "both") analysis <- analysis |> group_by(state, reporting_year)

    stats <- analysis |>
      summarise(
        Rows = n(),
        Reported = sum(is.finite(summary_value)),
        Missing = sum(!is.finite(summary_value)),
        Mean = if (Reported > 0) mean(summary_value[is.finite(summary_value)]) else NA_real_,
        Median = if (Reported > 0) median(summary_value[is.finite(summary_value)]) else NA_real_,
        SD = if (Reported > 1) sd(summary_value[is.finite(summary_value)]) else NA_real_,
        Minimum = if (Reported > 0) min(summary_value[is.finite(summary_value)]) else NA_real_,
        Maximum = if (Reported > 0) max(summary_value[is.finite(summary_value)]) else NA_real_,
        Total = if (!is_hold && Reported > 0) sum(summary_value[is.finite(summary_value)]) else NA_real_,
        .groups = "drop"
      )
    if (is_hold) stats <- stats |> select(-Total)
    if (is_monthly) stats <- stats |> rename(Months = Rows)

    list(table = stats, is_hold = is_hold, is_monthly = is_monthly,
         filtered_rows = nrow(filtered), active_rows = active_rows,
         analysis_rows = analysis_rows)
  })

  output$summary_count <- renderText({
    result <- summary_data()
    if (result$is_monthly) {
      paste0("Combined ", result$active_rows, " active state-month observations into ",
             result$analysis_rows, " monthly totals.")
    } else {
      paste0("Using ", result$active_rows, " of ", result$filtered_rows,
             " applied observations.")
    }
  })

  output$summary_table <- DT::renderDT({
    result <- summary_data()
    cols <- c("Mean", "Median", "SD", "Minimum", "Maximum")
    tbl <- DT::datatable(result$table, rownames = FALSE,
                         options = list(pageLength = 10, scrollX = TRUE))
    if (result$is_hold) {
      tbl |> DT::formatCurrency(columns = cols, currency = "%", digits = 2,
                                before = FALSE)
    } else {
      tbl |> DT::formatRound(columns = c(cols, "Total"), digits = 2)
    }
  })

  output$summary_note <- renderUI({
    result <- summary_data()
    tags$div(
      style = "margin-top:18px;padding:12px;background-color:#f5f5f5;border-radius:5px;",
      tags$strong("Summary Notes"),
      tags$p("These summaries use only the Data & Summaries filters.",
             "Prelaunch observations are excluded; later zeros and negative values are retained."),
      if (result$is_monthly) {
        tags$p("Combined monthly totals sum dollar amounts across states in each month.",
               "The mean and sample SD are then calculated across monthly totals.")
      } else {
        tags$p("Individual observations treat each state-month as a separate value.")
      },
      tags$p("Missing observations are omitted, not converted to zero."),
      if (result$is_hold) {
        tags$p("Mean hold is the unweighted average of reported monthly percentages.",
               "No Total is shown for hold rates.")
      } else {
        tags$p("Total is the sum of valid dollar observations.")
      }
    )
  })

  # Categorical Summaries and Additional Plots
  # Use only the shared dataset filters last applied by the button
  exploration_data <- reactive({
    add_exploration_categories(applied_data())
  })

  output$cat_one_table <- DT::renderDT({
    req(input$cat_one)
    data <- exploration_data()
    validate(need(nrow(data) > 0, "No observations match the filters."))

    table_data <- data |>
      transmute(Category = as.character(.data[[input$cat_one]])) |>
      count(Category, name = "Observations") |>
      mutate(Percent = round(100 * Observations / sum(Observations), 1)) |>
      arrange(desc(Observations))

    DT::datatable(
      table_data, rownames = FALSE,
      options = list(pageLength = 10, scrollX = TRUE)
    )
  })

  output$cat_two_table <- DT::renderDT({
    req(input$cat_row, input$cat_col)
    data <- exploration_data()
    validate(
      need(nrow(data) > 0, "No observations match the filters."),
      need(input$cat_row != input$cat_col, "Choose two different categorical variables.")
    )

    table_data <- data |>
      transmute(
        Category = as.character(.data[[input$cat_row]]),
        Column = as.character(.data[[input$cat_col]])
      ) |>
      count(Category, Column, name = "Observations") |>
      pivot_wider(
        names_from = Column,
        values_from = Observations,
        values_fill = 0
      ) |>
      arrange(Category)

    DT::datatable(
      table_data, rownames = FALSE,
      options = list(pageLength = 10, scrollX = TRUE)
    )
  })

  # Create six plot variations with suitable data and labels
  output$extra_plot <- renderPlot({
    req(input$graph_type)
    data <- exploration_data()
    validate(need(nrow(data) > 0, "No observations match the filters."))

    if (input$graph_type == "bar") {
      counts <- data |>
        count(reporting_year, channel_status, name = "Observations")
      return(
        ggplot(counts, aes(x = reporting_year, y = Observations,
                           fill = channel_status)) +
          geom_col(position = "dodge") +
          labs(
            title = "Monthly Observations by Year and Channel Activity",
            x = "Reporting Year", y = "Number of State-Months",
            fill = "Positive Channel Activity"
          ) +
          theme_minimal()
      )
    }

    req(input$graph_var)
    var <- input$graph_var
    label <- names(numeric_vars)[numeric_vars == var]
    finite_data <- data |>
      filter(is.finite(.data[[var]]))

    validate(need(
      nrow(finite_data) > 0,
      "No reported values for the selected numerical variable."
    ))

    if (input$graph_type == "histogram") {
      return(
        ggplot(finite_data, aes(x = .data[[var]])) +
          geom_histogram(bins = input$graph_bins, fill = "#4472B4",
                         color = "white") +
          labs(title = paste("Distribution of", label),
               x = label, y = "Number of State-Months") +
          theme_minimal()
      )
    }

    if (input$graph_type == "boxplot") {
      return(
        ggplot(finite_data,
               aes(x = reporting_year, y = .data[[var]],
                   fill = reporting_year)) +
          geom_boxplot(outlier.alpha = 0.35) +
          guides(fill = "none") +
          labs(title = paste(label, "by Reporting Year"),
               x = "Reporting Year", y = label) +
          theme_minimal()
      )
    }

    if (input$graph_type == "density") {
      density_data <- finite_data |>
        group_by(reporting_year) |>
        filter(n() >= 2, sd(.data[[var]]) > 0) |>
        ungroup()
      validate(need(
        nrow(density_data) > 0,
        "Density requires two or more nonidentical values within a year."
      ))
      return(
        ggplot(density_data,
               aes(x = .data[[var]], color = reporting_year,
                   group = reporting_year)) +
          geom_density(linewidth = 0.9) +
          labs(title = paste("Distribution of", label, "by Year"),
               x = label, y = "Density", color = "Reporting Year") +
          theme_minimal()
      )
    }

    if (input$graph_type == "facets") {
      req(input$graph_facet)
      if (input$graph_facet == "state") {
        validate(need(
          n_distinct(finite_data$state) <= 12,
          "Choose up to 12 states in Dataset Filters for readable facets."
        ))
      }
      return(
        ggplot(finite_data, aes(x = .data[[var]])) +
          geom_histogram(bins = input$graph_bins, fill = "#4472B4",
                         color = "white") +
          facet_wrap(stats::as.formula(paste("~", input$graph_facet)),
                     scales = "free_y", ncol = 3) +
          labs(title = paste("Distribution of", label, "by",
                             if (input$graph_facet == "state") "State" else "Year"),
               x = label, y = "Number of State-Months") +
          theme_minimal()
      )
    }

    # A heatmap is not a conventional bar/scatter/histogram/boxplot
    if (input$graph_type == "heatmap") {
      if (var %in% hold_vars) {
        pair <- switch(
          var,
          hold_total = c("revenue", "handle"),
          hold_mobile = c("revenue_mobile", "handle_mobile"),
          hold_retail = c("revenue_retail", "handle_retail")
        )
        heat_data <- data |>
          mutate(
            numerator = .data[[pair[1]]],
            denominator = .data[[pair[2]]],
            valid_pair = is.finite(.data[[var]]) &
              is.finite(numerator) & is.finite(denominator)
          ) |>
          group_by(state, reporting_year) |>
          summarise(
            amount = if (any(valid_pair) &&
                         sum(denominator[valid_pair]) > 0) {
              100 * sum(numerator[valid_pair]) /
                sum(denominator[valid_pair])
            } else {
              NA_real_
            },
            .groups = "drop"
          )
      } else {
        heat_data <- finite_data |>
          group_by(state, reporting_year) |>
          summarise(amount = sum(.data[[var]]), .groups = "drop")
      }

      return(
        ggplot(heat_data,
               aes(x = reporting_year, y = state, fill = amount)) +
          geom_tile(color = "white", linewidth = 0.25) +
          scale_fill_viridis_c(
            labels = scales::label_number(
              scale_cut = scales::cut_short_scale(),
              suffix = if (var %in% hold_vars) "%" else ""
            ),
            na.value = "grey85"
          ) +
          labs(title = paste("State-Year Heatmap:", label),
               x = "Reporting Year", y = "State", fill = label) +
          theme_minimal() +
          theme(panel.grid = element_blank(),
                axis.text.x = element_text(angle = 45, hjust = 1))
      )
    }
  })

  output$extra_note <- renderText({
    req(input$graph_type)
    if (input$graph_type == "bar") {
      return(paste(
        "Counts show months with positive retail/mobile handle by channel.",
        "Missing channels are not assumed to be zero."
      ))
    }
    if (input$graph_type == "heatmap") {
      return(paste(
        "The heatmap uses the applied state-month observations.",
        "Dollar values are summed within each state-year; partial years",
        "are not annualized. Hold uses revenue divided by handle, times 100."
      ))
    }
    paste(
      "This plot uses only the state-month observations last applied",
      "under Dataset Filters. Missing numerical values are omitted;",
      "reported zeros and negative values are retained."
    )
  })

  # Correlation: original data with independent state/variable/time controls
  observeEvent(list(input$corr_x, input$corr_y), {
    is_hold <- input$corr_x %in% hold_vars || input$corr_y %in% hold_vars
    choices <- if (is_hold) {
      c("Reported Value" = "level",
        "YoY Change (dollars or percentage points)" = "change")
    } else comparison_choices("handle")
    current <- isolate(input$corr_view)
    updateSelectInput(session, "corr_view", choices = choices,
                      selected = if (current %in% unname(choices)) current else "level")
  })

  paired_periods <- reactive({
    req(input$corr_state_x, input$corr_state_y,
        input$corr_x, input$corr_y, input$corr_period, input$corr_view)
    x <- get_chart_series(input$corr_state_x, input$corr_x, input$corr_period)
    y <- get_chart_series(input$corr_state_y, input$corr_y, input$corr_period)
    if (input$corr_view != "level") {
      x <- add_yoy(x, input$corr_x, input$corr_view)
      y <- add_yoy(y, input$corr_y, input$corr_view)
    }
    inner_join(x |> rename(x = value), y |> rename(y = value), by = "period") |>
      filter(is.finite(x), is.finite(y)) |>
      arrange(period)
  })

  observeEvent(paired_periods(), {
    data <- paired_periods()
    if (nrow(data) > 0) {
      updateDateRangeInput(session, "corr_dates",
                           start = min(data$period), end = max(data$period),
                           min = min(data$period), max = max(data$period))
    }
  })

  output$available_period <- renderText({
    data <- paired_periods()
    if (nrow(data) == 0) return("No matched periods; YoY requires the previous year.")
    paste0("Available: ", format_period(min(data$period), input$corr_period),
           " - ", format_period(max(data$period), input$corr_period))
  })

  # Save the last requested correlation, so inputs can be changed before running
  corr_results <- reactiveVal(NULL)
  observeEvent(input$run_corr, {
    req(input$corr_dates)
    data <- paired_periods()
    from <- period_floor(input$corr_dates[1], input$corr_period)
    to <- period_floor(input$corr_dates[2], input$corr_period)
    data <- data |> filter(period >= from, period <= to)

    x_start <- first_positive_month(input$corr_state_x, "handle_mobile")
    y_start <- first_positive_month(input$corr_state_y, "handle_mobile")
    x_mobile <- if (is.na(x_start)) rep(FALSE, nrow(data)) else data$period >= x_start
    y_mobile <- if (is.na(y_start)) rep(FALSE, nrow(data)) else data$period >= y_start
    data <- data |>
      mutate(market_period = factor(case_when(
        x_mobile & y_mobile ~ "Both states mobile-active",
        x_mobile | y_mobile ~ "One state mobile-active",
        TRUE ~ "Neither state mobile-active"
      ), levels = c("Neither state mobile-active", "One state mobile-active",
                    "Both states mobile-active")))

    corr_results(list(data = data, state_x = input$corr_state_x,
                      state_y = input$corr_state_y,
                      var_x = input$corr_x, var_y = input$corr_y,
                      period = input$corr_period, view = input$corr_view))
  })

  corr_data <- reactive({
    result <- corr_results()
    validate(need(!is.null(result), "Choose inputs and click Run Correlation."))
    validate(
      need(!(result$state_x == result$state_y && result$var_x == result$var_y),
           "Select different states or variables."),
      need(nrow(result$data) >= 3, "At least three matched periods are required."),
      need(isTRUE(sd(result$data$x) > 0) && isTRUE(sd(result$data$y) > 0),
           "Correlation cannot be calculated when a variable is constant.")
    )
    result
  })

  output$corr_plot <- renderPlot({
    result <- corr_data()
    axis_label <- function(s, var) {
      label <- names(numeric_vars)[numeric_vars == var]
      if (result$view != "level") {
        unit <- if (var %in% hold_vars) "pp" else if (result$view == "change") "$ change" else "% growth"
        label <- paste(label, "YoY", unit)
      }
      paste(s, label)
    }
    ggplot(result$data, aes(x = x, y = y)) +
      geom_point(aes(color = market_period), size = 2.5, alpha = 0.8) +
      geom_smooth(method = "lm", se = FALSE, color = "black") +
      scale_color_manual(
        values = c("Neither state mobile-active" = "gray50",
                   "One state mobile-active" = "#E69F00",
                   "Both states mobile-active" = "#0072B2"),
        name = "Market Period"
      ) +
      scale_x_continuous(labels = scales::label_number(scale_cut = scales::cut_short_scale())) +
      scale_y_continuous(labels = scales::label_number(scale_cut = scales::cut_short_scale())) +
      labs(title = paste(period_labels[[result$period]], "Correlation"),
           subtitle = paste(result$state_x, "vs", result$state_y),
           x = axis_label(result$state_x, result$var_x),
           y = axis_label(result$state_y, result$var_y)) +
      theme_gray()
  })

  output$corr_result <- renderText({
    result <- corr_data()
    paste0("Pearson correlation (r): ", round(cor(result$data$x, result$data$y), 3),
           " | Matched periods: ", nrow(result$data),
           " | ", format_period(min(result$data$period), result$period),
           " - ", format_period(max(result$data$period), result$period))
  })

  output$market_note <- renderUI({
    result <- corr_data()
    state_note <- function(s) {
      paste0(s, ": retail first positive ", first_positive_month(s, "handle_retail"),
             "; mobile first positive ", first_positive_month(s, "handle_mobile"), ".")
    }
    tags$div(
      style = "margin-top:18px;padding:12px;background-color:#f5f5f5;border-radius:5px;",
      tags$strong("Reporting Context"),
      tags$p(state_note(result$state_x)),
      tags$p(state_note(result$state_y)),
      tags$p("Correlation uses the original dataset, independently of Data & Summaries filters."),
      tags$p("Complete months are required for quarterly, annual, and rolling calculations.",
             "YoY compares the same period one year earlier."),
      tags$p("Adjacent rolling periods overlap and are not independent.")
    )
  })

  # Time Series: original data, independent controls
  observeEvent(input$ts_var, {
    choices <- comparison_choices(input$ts_var)
    current <- isolate(input$ts_view)
    updateSelectInput(session, "ts_view", choices = choices,
                      selected = if (current %in% unname(choices)) current else "level")
  })

  ts_series <- reactive({
    req(input$ts_var, input$ts_period, input$ts_view)
    if (length(input$ts_states) == 0) {
      return(tibble(period = as.Date(character()), value = numeric(), state = character()))
    }
    purrr::map_dfr(input$ts_states, function(s) {
      data <- get_chart_series(s, input$ts_var, input$ts_period)
      if (input$ts_view != "level") data <- add_yoy(data, input$ts_var, input$ts_view)
      data |> filter(is.finite(value)) |> mutate(state = s)
    }) |>
      arrange(state, period)
  })

  observeEvent(ts_series(), {
    series <- ts_series()
    if (nrow(series) > 0) {
      updateDateRangeInput(session, "ts_dates",
                           start = min(series$period), end = max(series$period),
                           min = min(series$period), max = max(series$period))
    }
  })

  output$ts_available_period <- renderText({
    series <- ts_series()
    if (nrow(series) == 0) return("No available periods; YoY requires the previous year.")
    paste0("Available: ", format_period(min(series$period), input$ts_period),
           " - ", format_period(max(series$period), input$ts_period))
  })

  # Update the plot only when Update Chart is clicked
  ts_results <- reactiveVal(NULL)
  observeEvent(input$run_ts, {
    req(input$ts_dates)
    from <- period_floor(input$ts_dates[1], input$ts_period)
    to <- period_floor(input$ts_dates[2], input$ts_period)
    selected <- ts_series() |> filter(period >= from, period <= to)
    ts_results(list(data = selected, states = input$ts_states,
                    variable = input$ts_var, period = input$ts_period,
                    view = input$ts_view, display = input$ts_display,
                    start_date = from, end_date = to))
  })

  ts_data <- reactive({
    result <- ts_results()
    validate(need(!is.null(result), "Choose inputs and click Update Chart."))
    validate(need(nrow(result$data) > 0, "No values for the selected period."))
    result
  })

  output$ts_plot <- renderPlotly({
    result <- ts_data()
    label <- names(numeric_vars)[numeric_vars == result$variable]
    is_hold <- result$variable %in% hold_vars
    is_yoy <- result$view != "level"
    is_growth <- result$view == "yoy"
    suffix <- if (is_hold && is_yoy) " pp" else if (is_hold || is_growth) "%" else ""
    is_dollars <- !is_hold && !is_growth

    y_label <- if (is_yoy) {
      paste("YoY", label,
            if (is_hold) "(percentage points)" else if (is_growth) "(%)" else "($ change)")
    } else label

    chart_data <- result$data |>
      mutate(
        period_label = vapply(period, format_period, character(1), mode = result$period),
        hover_value = if (is_dollars) {
          paste0("$", formatC(value, format = "f", digits = 2, big.mark = ","))
        } else {
          paste0(formatC(value, format = "f", digits = 2, big.mark = ","), suffix)
        },
        hover_text = paste0("State: ", state, "<br>Period: ", period_label,
                            "<br>", y_label, ": ", hover_value)
      )

    step <- if (result$period == "year") "year" else if (result$period == "quarter") "quarter" else "month"
    chart_data <- chart_data |>
      group_by(state) |>
      complete(period = seq(min(period), max(period), by = step)) |>
      ungroup()

    chart <- ggplot(chart_data,
                    aes(x = period, y = value, color = state,
                        group = state, text = hover_text)) +
      geom_line(linewidth = 0.8) +
      geom_point(size = 1.5, alpha = 0.7, na.rm = TRUE) +
      labs(title = paste(period_labels[[result$period]], y_label),
           subtitle = paste(format_period(result$start_date, result$period), "-",
                            format_period(result$end_date, result$period)),
           x = "Reporting Period", y = y_label, color = "State") +
      theme_gray()

    if (is_dollars) {
      chart <- chart + scale_y_continuous(
        labels = scales::label_number(prefix = "$", scale_cut = scales::cut_short_scale()))
    } else {
      chart <- chart + scale_y_continuous(labels = scales::label_number(suffix = suffix))
    }
    if (result$display == "Separate panels") {
      chart <- chart + facet_wrap(~state, scales = "free_y") + guides(color = "none")
    }
    plotly::ggplotly(chart, tooltip = "text") |>
      plotly::layout(hovermode = "closest") |>
      plotly::config(displaylogo = FALSE)
  })

  output$ts_context <- renderUI({
    result <- ts_data()
    state_notes <- purrr::map_chr(result$states, function(s) {
      paste0(s, ": retail first positive ", first_positive_month(s, "handle_retail"),
             "; mobile first positive ", first_positive_month(s, "handle_mobile"), ".")
    })
    tags$div(
      style = "margin-top:18px;padding:12px;background-color:#f5f5f5;border-radius:5px;",
      tags$strong("Reporting Context"),
      lapply(state_notes, tags$p),
      tags$p("This chart uses the original dataset independently of Data & Summaries filters."),
      tags$p("Calendar quarters and years require all 3 or 12 months.",
             "Trailing windows require all included months."),
      tags$p("YTD totals begin in January and YoY uses the equivalent prior-year period."),
      tags$p("Aggregated hold is revenue divided by handle times 100.",
             "YoY hold changes are measured in percentage points."),
      tags$p("First positive observations are not necessarily legal market launch dates.",
             "Adjacent rolling periods overlap.")
    )
  })
}

# Step 5: Run the app
shinyApp(ui = ui, server = server)
