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
  "Cumulative Handle by State - Stacked Area" = "cum_handle",
  "Combined Monthly Hold - Line Chart" = "national_hold",
  "Monthly Handle by State - Line Chart" = "monthly_handle",
  "Monthly Revenue by State - Line Chart" = "monthly_revenue",
  "Monthly Hold Distribution - Histogram" = "hold_hist",
  "Annual Per Capita - State Ranking" = "per_capita",
  "Monthly Hold by State and Year - Faceted Boxplot" = "faceted_hold",
  "Mobile vs Retail Handle Share by State" = "channel_share",
  "Grouped Bar Chart - Channel Activity by Year" = "bar",
  "Histogram - Numerical Distribution" = "histogram",
  "Boxplot - Values by Reporting Year" = "boxplot",
  "Density Curves - Distributions by Year" = "density",
  "Faceted Histogram" = "facets",
  "Heatmap - State by Reporting Year" = "heatmap"
)

# Step 2B: Improve readability of labels across Additional Plots
extra_plot_theme <- theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(size = 20, face = "bold"),
    plot.subtitle = element_text(size = 14),
    axis.title = element_text(size = 17),
    axis.text = element_text(size = 14),
    strip.text = element_text(size = 16, face = "bold"),
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 14),
    plot.caption = element_text(size = 12)
  )

# Keep zero visible on every quantitative axis in the interactive charts.
# The breaks function retains ordinary tick marks and explicitly includes zero.
# Negative observations and existing dollar/percentage units are preserved.
zero_axis_breaks <- function(limits) {
  ordinary <- scales::breaks_extended(n = 5)(limits)
  ordinary <- ordinary[is.finite(ordinary)]
  sort(unique(c(ordinary, 0)))
}

# Add a restrained zero reference to quantitative Y axes. After coord_flip(),
# the same reference appears vertically on horizontal bar charts.
show_zero_reference <- function(chart, numeric_x = FALSE) {
  chart <- chart +
    ggplot2::expand_limits(y = 0) +
    ggplot2::geom_hline(
      yintercept = 0, color = "grey65", linewidth = 0.35,
      inherit.aes = FALSE, show.legend = FALSE
    )

  # Distribution plots have numerical values on X and frequency/density on Y.
  if (isTRUE(numeric_x)) {
    chart <- chart +
      ggplot2::expand_limits(x = 0) +
      ggplot2::geom_vline(
        xintercept = 0, color = "grey75", linewidth = 0.3,
        inherit.aes = FALSE, show.legend = FALSE
      )
  }
  chart
}

# Step 2C: Examples of earlier static reports for the About tab
# Shiny serves these images from the www/images folder.
static_report_examples <- list(
  list(
    title = "LTM Sports Betting Taxes",
    description = "State taxes ranked over the last twelve months",
    src = "images/LTM.Tax.Rankings.10.26.26.jpg"
  ),
  list(
    title = "LTM Sports Betting Handle",
    description = "State handle rankings over the last twelve months",
    src = "images/LTM.Handle.Rankings.10.26.26.jpg"
  ),
  list(
    title = "LTM Gross Revenue",
    description = "State gross revenue rankings over the last twelve months",
    src = "images/LTM.GGR.Rankings.10.26.26.jpg"
  ),
  list(
    title = "Monthly Hold Boxplots",
    description = "Monthly distribution of sportsbook hold",
    src = "images/Monthly.Hold.Boxplot.12.19.24.png"
  ),
  list(
    title = "Annual National Hold",
    description = "National annual and partial-year hold comparison",
    src = "images/Annual.Hold.10.26.26.jpg"
  ),
  list(
    title = "Mobile Handle Per Adult",
    description = "Rolling mobile handle by market maturity",
    src = "images/Ramps.jpg"
  )
)

# Step 2D: Familiar sportsbook brands provide context for the About tab
sportsbook_brand_image <- "images/sportsbook_logos.png"

# Step 3: Define the UI
ui <- fluidPage(
  titlePanel("U.S. Sports Betting Explorer"),
  tabsetPanel(
    # Step 3A: About tab - app overview, instructions, sources, and image
    tabPanel(
      "About",
      fluidRow(
        column(
          width = 7,
          h3("U.S. Sports Betting Explorer"),
          h4("Background and purpose"),
          p(
            "In 2018, the U.S. Supreme Court struck down the Professional and Amateur ",
            "Sports Protection Act of 1992 (PASPA), removing the federal restriction ",
            "that had prevented most states from authorizing sports betting within ",
            "their borders. Before this decision, Nevada was the only state where ",
            "legal, regulated sports betting was broadly available."
          ),
          p(
            "Since the ruling, sports betting has expanded to dozens of states, ",
            "providing legal access to more than half of the U.S. population. ",
            "State gaming regulators generally publish sports betting results ",
            "on a monthly basis, typically reporting activity from a prior ",
            "month. The detail, definitions, and availability of these public ",
            "reports vary by jurisdiction."
          ),
          p(
            "I have personally collected and organized the data presented in ",
            "this app for my personal and professional work as a sports betting ",
            "industry analyst. Using the results generally published by state ",
            "gaming regulators, I have compiled a centralized dataset so users ",
            "can explore trends, compare jurisdictions, and better understand ",
            "the growth of regulated sports betting in the United States."
          ),
          p(
            "Rather than relying on static data tables and charts that must be ",
            "updated manually, I developed this interactive app to enhance ",
            "the experience for visitors to my website, ",
            tags$a(
              "iGaming Analyst",
              href = "https://www.igaminganalyst.com/",
              target = "_blank", rel = "noopener noreferrer"
            ),
            ". It provides a flexible way to explore, filter, and compare ",
            "publicly reported sports betting data without depending on ",
            "separately maintained static charts."
          ),
          h4("What you can explore"),
          p(
            "Compare handle (wagers placed), operator revenue, taxes, hold ",
            "percentages, mobile versus retail activity, and per-capita measures ",
            "using interactive charts and summary tables."
          ),
          h4("Data at a glance"),
          p(
            strong("Coverage: "),
            paste0(
              dplyr::n_distinct(sports$state), " reporting jurisdictions; ",
              format(min(sports$month), "%B %Y"), " through ",
              format(max(sports$month), "%B %Y"), "; ",
              format(nrow(sports), big.mark = ",", scientific = FALSE),
              " state-month observations."
            )
          ),
          p(
            "The underlying sports betting figures generally come from monthly ",
            "public reports issued by state gaming regulators. I collected and ",
            "organized these figures into the analytical dataset used here. ",
            "Coverage differs by jurisdiction and measure; a blank value means ",
            "unreported or unavailable, not necessarily zero. Some reported ",
            "financial adjustments are negative."
          ),
          h4("How to use the app"),
          tags$ol(
            tags$li(
              strong("Choose filters under Data & Summaries. "),
              "Select states, reporting years, and optionally up to two numeric ranges. ",
              "Click Apply Filters to update the data table, summaries, and Additional Plots."
            ),
            tags$li(
              strong("Explore the tabs. "),
              "Data Table lets you browse and download the filtered observations; ",
              "Numerical Summaries calculates descriptive statistics; ",
              "Categorical Summaries creates one-way and two-way contingency tables; ",
              "Additional Plots displays customizable charts, including per-capita rankings."
            ),
            tags$li(
              strong("Use Data Exploration for comparisons over time. "),
              "Correlation compares two selected state-variable series; ",
              "Time Series displays trends, reporting-period aggregations, and year-over-year ",
              "changes. These two tools have their own controls and are independent ",
              "of the Data & Summaries filters."
            ),
            tags$li(
              strong("Inspect chart values. "),
              "Hover over interactive charts to view exact amounts and labels."
            )
          ),
          h4("Definitions and interpretation"),
          tags$ul(
            tags$li(strong("Handle: "), "the dollar amount wagered, not operator revenue."),
            tags$li(strong("Revenue: "), "reported sportsbook revenue; taxes are a separate reported field."),
            tags$li(strong("Hold (%): "), "100 × revenue / handle where the denominator is positive. Aggregated hold is calculated using combined dollars, not an unweighted average of state rates."),
            tags$li(strong("Per capita: "), "an annual financial total divided by the jurisdiction's resident population. Population comparisons cover ", population_coverage, "; partial-year totals are not annualized.")
          )
        ),
        column(
          width = 5,
          # Familiar sportsbook brands provide visual context for visitors.
          h4("Familiar sports betting brands"),
          tags$img(
            src = sportsbook_brand_image,
            alt = "Collage of familiar sportsbook brand logos",
            style = paste0(
              "display:block;width:100%;max-width:640px;height:auto;",
              "border-radius:7px;border:1px solid #ddd;"
            )
          ),
          p(
            "Brands shown as visual context only; no affiliation or endorsement is implied.",
            style = "font-size:11px;color:#666;margin-top:7px;margin-bottom:21px;"
          ),
          h4("Examples of my previous static reports"),
          p(
            "These are examples of the individually maintained charts used in my ",
            "industry analysis. This interactive app is intended to reduce the ",
            "need to recreate similar visualizations manually. ",
            strong("Click any example to enlarge it.")
          ),
          tags$div(
            style = paste0(
              "display:grid;grid-template-columns:repeat(3,minmax(0,1fr));",
              "gap:10px;margin:12px 0 22px 0;"
            ),
            lapply(seq_along(static_report_examples), function(i) {
              report <- static_report_examples[[i]]
              tags$div(
                style = paste0(
                  "border:1px solid #ddd;border-radius:7px;",
                  "padding:6px;background:#fff;min-width:0;"
                ),
                actionLink(
                  inputId = paste0("about_report_", i),
                  label = tags$img(
                    src = report$src,
                    alt = report$title,
                    style = "width:100%;height:auto;display:block;border-radius:3px;"
                  ),
                  style = "display:block;"
                ),
                tags$div(
                  report$title,
                  style = "font-size:12px;font-weight:bold;margin-top:7px;"
                )
              )
            })
          ),
          wellPanel(
            h4("Data sources and background"),
            p(
              strong("Sports betting: "),
              "Underlying data are generally reported monthly by state gaming ",
              "regulators and were personally collected and organized into ",
              "the compiled workbook ",
              tags$code("Sports_Betting_State_Data.xlsx"),
              " (Sports worksheet), then prepared into ",
              tags$code("data/sports_betting_clean.csv"),
              ". The reference below provides public U.S. industry context; ",
              "it is not a direct source for the compiled dataset."
            ),
            p(
              tags$a(
                "American Gaming Association — Commercial Gaming Revenue Tracker",
                href = "https://americangaming.org/resources/commercial-gaming-revenue-tracker/",
                target = "_blank", rel = "noopener noreferrer"
              )
            ),
            p(
              strong("Population: "),
              "U.S. Census Bureau annual state resident population estimates from ",
              "two historical vintages: Vintage 2019 ",
              "(NST-EST2019-ALLDATA) for 2018–2019 and Vintage 2025 ",
              "(NST-EST2025-ALLDATA) for 2020–2025. ",
              "These were prepared as a combined 2018–2025 population dataset ",
              "for state-level per-capita comparisons."
            ),
            p(
              tags$a(
                "U.S. Census Bureau — Vintage 2019 Population Estimates",
                href = "https://www.census.gov/newsroom/press-kits/2019/national-state-estimates.html",
                target = "_blank", rel = "noopener noreferrer"
              ),
              " | ",
              tags$a(
                "Vintage 2025 Population Estimates",
                href = "https://www.census.gov/data/datasets/time-series/demo/popest/2020s-state-total.html",
                target = "_blank", rel = "noopener noreferrer"
              )
            ),
            p(
              style = "font-size:12px; color:#555;",
              "The two periods come from separate Census vintages and may not be ",
              "strictly comparable across the 2019–2020 boundary. "
            ),
            p(
              style = "font-size:12px; color:#555;",
              "Source coverage and definitions vary by jurisdiction. This app is ",
              "for descriptive exploration, not a complete official national census."
            )
          )
        )
      )
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
          helpText("These filters control the Data Table, Numerical Summaries, Categorical Summaries, and Additional Plots only.")
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
                "graph_type", "Plot Type", plot_choices, selected = "bar", width = "420px"
              ),
              conditionalPanel(
                condition = paste0(
                  "input.graph_type != 'bar' && ",
                  "input.graph_type != 'cum_handle' && ",
                  "input.graph_type != 'national_hold' && ",
                  "input.graph_type != 'faceted_hold' && ",
                  "input.graph_type != 'channel_share' && ",
                  "input.graph_type != 'monthly_handle' && ",
                  "input.graph_type != 'monthly_revenue' && ",
                  "input.graph_type != 'hold_hist' && ",
                  "input.graph_type != 'per_capita'"
                ),
                selectInput(
                  "graph_var", "Numerical Variable", numeric_vars,
                  selected = "handle"
                )
              ),
              conditionalPanel(
                condition = "input.graph_type == 'channel_share'",
                selectInput(
                  "graph_channel_year", "Reporting Year",
                  choices = as.character(sort(unique(lubridate::year(sports$month)))),
                  selected = "2025"
                )
              ),
              # Per-capita comparisons use Census annual state population estimates
              conditionalPanel(
                condition = "input.graph_type == 'per_capita'",
                selectInput(
                  "graph_percap_metric", "Annual Measure",
                  choices = c(
                    "Taxes ($)" = "taxes",
                    "Total Handle ($)" = "handle",
                    "Total Revenue ($)" = "revenue",
                    "Taxable Revenue ($)" = "taxable_revenue",
                    "Mobile Handle ($)" = "handle_mobile",
                    "Retail Handle ($)" = "handle_retail"
                  ),
                  selected = "taxes"
                ),
                selectInput(
                  "graph_percap_year", "Reporting Year",
                  choices = as.character(sort(unique(population_estimates$year))),
                  selected = "2025"
                ),
                selectInput(
                  "graph_percap_top", "States to Display",
                  choices = c("Top 10" = "10", "Top 20" = "20",
                              "All Selected States" = "all"),
                  selected = "10"
                ),
                checkboxInput(
                  "graph_percap_full_year", "Require 12 reported months",
                  value = TRUE
                ),
                helpText(
                  "Per-capita values use U.S. Census annual population estimates",
                  paste0("for ", population_coverage, ". Partial-year totals are not annualized.")
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
              uiOutput("extra_plot_ui"),
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
              plotlyOutput("corr_plot", height = "450px"),
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
  # Step 4A: Enlarge an earlier static chart when its thumbnail is clicked
  lapply(seq_along(static_report_examples), function(i) {
    observeEvent(input[[paste0("about_report_", i)]], {
      report <- static_report_examples[[i]]
      showModal(modalDialog(
        title = report$title,
        tags$img(
          src = report$src,
          alt = report$title,
          style = "display:block;width:100%;height:auto;"
        ),
        p(report$description, style = "margin-top:10px;color:#555;"),
        size = "l",
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
    })
  })

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

    # Add an overall total without changing the underlying observations
    table_data <- bind_rows(
      table_data,
      tibble(
        Category = "Total",
        Observations = sum(table_data$Observations),
        Percent = 100
      )
    )

    # Bold the complete Total row, including its numerical values
    DT::datatable(
      table_data, rownames = FALSE,
      options = list(pageLength = 50, scrollX = TRUE, ordering = FALSE)
    ) |>
      DT::formatStyle(
        columns = "Category",
        target = "row",
        fontWeight = DT::styleEqual("Total", "bold")
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

    # Add a total for each category row
    year_or_category_cols <- setdiff(names(table_data), "Category")
    table_data$Total <- rowSums(
      as.matrix(table_data[, year_or_category_cols, drop = FALSE])
    )

    # Add totals for each column and the overall grand total
    total_row <- table_data |>
      summarise(across(-Category, ~ sum(.x, na.rm = TRUE))) |>
      mutate(Category = "Total", .before = 1)

    table_data <- bind_rows(table_data, total_row)

    # Bold the complete Total row, including its numerical values
    DT::datatable(
      table_data, rownames = FALSE,
      options = list(pageLength = 50, scrollX = TRUE, ordering = FALSE)
    ) |>
      DT::formatStyle(
        columns = "Category",
        target = "row",
        fontWeight = DT::styleEqual("Total", "bold")
      )
  })

  # Choose a readable height for plots with many states
  output$extra_plot_ui <- renderUI({
    req(input$graph_type)
    states_n <- dplyr::n_distinct(applied_data()$state)

    height <- if (input$graph_type == "faceted_hold") {
      max(590, ceiling(states_n / 4) * 225)
    } else if (input$graph_type == "channel_share") {
      max(590, states_n * 25 + 140)
    } else if (input$graph_type == "per_capita") {
      n_bars <- if (identical(input$graph_percap_top, "all")) {
        states_n
      } else if (!is.null(input$graph_percap_top)) {
        min(states_n, as.integer(input$graph_percap_top))
      } else {
        10L
      }
      max(590, n_bars * 30 + 160)
    } else {
      590
    }

    plotlyOutput("extra_plot", height = paste0(height, "px"))
  })


  # Choose the reporting year for the channel composition chart
  observeEvent(applied_data(), {
    years <- sort(unique(lubridate::year(applied_data()$month)))
    if (length(years) > 0) {
      current <- isolate(input$graph_channel_year)
      selected <- if (!is.null(current) &&
                      current %in% as.character(years)) {
        current
      } else {
        as.character(max(years))
      }
      updateSelectInput(
        session, "graph_channel_year",
        choices = as.character(years), selected = selected
      )
    }
  })


  # Population-normalized state-year observations from applied filters
  percap_plot_data <- reactive({
    req(input$graph_percap_metric, input$graph_percap_year)
    threshold <- if (isTRUE(input$graph_percap_full_year)) 12L else 1L
    get_per_capita_data(
      exploration_data(),
      input$graph_percap_metric,
      as.integer(input$graph_percap_year),
      min_months = threshold
    )
  })

  # Restrict per-capita year choices to years in the filtered dataset
  # and years for which matching annual population estimates are available
  observeEvent(applied_data(), {
    eligible_years <- intersect(
      sort(unique(lubridate::year(applied_data()$month))),
      sort(unique(population_estimates$year))
    )
    current <- isolate(input$graph_percap_year)
    selected <- if (!is.null(current) &&
                    as.integer(current) %in% eligible_years) {
      current
    } else if (length(eligible_years) > 0) {
      as.character(max(eligible_years))
    } else {
      character(0)
    }
    updateSelectInput(
      session, "graph_percap_year",
      choices = as.character(eligible_years), selected = selected
    )
  })

  # Graphical exploration based on the original static EDA charts
  extra_plot_gg <- reactive({
    req(input$graph_type)
    data <- exploration_data()
    validate(need(nrow(data) > 0, "No observations match the filters."))

    # Original EDA: monthly handle and revenue comparisons by state
    if (input$graph_type %in% c("monthly_handle", "monthly_revenue")) {
      variable <- if (input$graph_type == "monthly_handle") "handle" else "revenue"
      label <- if (variable == "handle") "Handle" else "Revenue"
      scale_divisor <- if (variable == "handle") 1e9 else 1e6
      unit <- if (variable == "handle") "Billions" else "Millions"
      plot_data <- data |>
        filter(is.finite(.data[[variable]])) |>
        mutate(hover_text = paste0(
          "State: ", state,
          "<br>Month: ", format(month, "%b %Y"),
          "<br>", label, ": $",
          formatC(.data[[variable]], format = "f", digits = 2, big.mark = ",")
        ))
      validate(
        need(nrow(plot_data) > 0, "No reported data for these filters."),
        need(n_distinct(plot_data$state) <= 12,
             "For readable state lines, choose up to 12 states in Dataset Filters.")
      )
      return(
        ggplot(plot_data,
               aes(x = month, y = .data[[variable]] / scale_divisor,
                   color = state, group = state, text = hover_text)) +
          geom_line(linewidth = 0.9) +
          scale_y_continuous(breaks = zero_axis_breaks) +
          scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
          labs(
            title = paste("Monthly Sports Betting", label, "by State"),
            subtitle = "Selected states and reporting months",
            x = "Reporting Month", y = paste0(label, " ($ ", unit, ")"),
            color = "State"
          ) +
          extra_plot_theme +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
      )
    }

    # Original EDA: the distribution of monthly hold percentages
    if (input$graph_type == "hold_hist") {
      plot_data <- data |>
        filter(is.finite(handle), handle > 0, is.finite(hold_total))
      validate(need(nrow(plot_data) > 0,
                    "No reported monthly hold values for these filters."))
      return(
        ggplot(plot_data, aes(x = hold_total)) +
          geom_histogram(binwidth = 2, boundary = 0,
                         color = "white", fill = "grey40") +
          scale_x_continuous(breaks = zero_axis_breaks) +
          scale_y_continuous(breaks = zero_axis_breaks) +
          labs(
            title = "Distribution of Monthly Sports Betting Hold Percentages",
            subtitle = "Each observation represents one state-month",
            x = "Monthly Hold (%)", y = "Frequency (State-Months)"
          ) +
          extra_plot_theme
      )
    }

    # Annual state-level financial measures divided by Census population
    if (input$graph_type == "per_capita") {
      req(input$graph_percap_top)
      plot_data <- percap_plot_data() |>
        arrange(desc(per_capita))
      if (input$graph_percap_top != "all") {
        plot_data <- head(plot_data, as.integer(input$graph_percap_top))
      }
      validate(need(nrow(plot_data) > 0,
                    paste(
                      "No eligible state-year data. Population is available for",
                      population_coverage,
                      "; try another year or disable the 12-month requirement."
                    )))
      metric_label <- names(numeric_vars)[numeric_vars == input$graph_percap_metric]
      plot_data <- plot_data |>
        mutate(hover_text = paste0(
          "State: ", state, "<br>",
          metric_label, " per resident: $",
          formatC(per_capita, format = "f", digits = 2, big.mark = ",")
        ))
      return(
        ggplot(plot_data,
               aes(x = reorder(state, per_capita), y = per_capita,
                   text = hover_text)) +
          geom_col(fill = "steelblue") +
          coord_flip() +
          scale_y_continuous(
            breaks = zero_axis_breaks,
            labels = scales::label_dollar(accuracy = 1)
          ) +
          labs(
            title = paste("Sports Betting", metric_label, "Per Capita by State"),
            subtitle = paste(
              "Calendar Year", input$graph_percap_year,
              if (isTRUE(input$graph_percap_full_year))
                "- 12 reported months required" else
                "- partial-year reports may be included"
            ),
            x = "State", y = "Reported Dollars per Resident ($)",
            caption = paste(
              "U.S. Census Bureau annual population estimates;",
              "reported totals are divided by same-year population."
            )
          ) +
          extra_plot_theme
      )
    }

    # Original EDA Step 6: cumulative state contributions to reported handle
    if (input$graph_type == "cum_handle") {
      plot_data <- get_cumulative_handle_data(data) |>
        mutate(hover_text = paste0(
          "State: ", state, "<br>Month: ", format(month, "%b %Y"),
          "<br>Cumulative state handle: $",
          formatC(cumulative_handle, format = "f", digits = 2, big.mark = ",")
        ))
      validate(need(nrow(plot_data) > 0, "No reported handle available."))

      return(
        ggplot(plot_data,
               aes(x = month, y = cumulative_handle / 1e9,
                   fill = state, text = hover_text)) +
          geom_area() +
          scale_y_continuous(breaks = zero_axis_breaks) +
          scale_x_date(
            date_breaks = if (as.numeric(diff(range(plot_data$month))) > 365 * 3)
              "1 year" else "6 months",
            date_labels = "%b %Y",
            limits = range(plot_data$month)
          ) +
          labs(
            title = "Cumulative Reported Sports Betting Handle by State",
            subtitle = "Selected jurisdictions and reporting months",
            x = "Reporting Month", y = "Cumulative Handle ($ Billions)",
            fill = "State",
            caption = paste(
              "Missing or excluded state-months contribute zero to",
              "the plotted cumulative path; original data are unchanged."
            )
          ) +
          extra_plot_theme
      )
    }

    # Original EDA Step 7: combined monthly hold, weighted by handle
    if (input$graph_type == "national_hold") {
      plot_data <- get_monthly_aggregate_hold(data) |>
        mutate(hover_text = paste0(
          "Month: ", format(month, "%b %Y"),
          "<br>Combined hold: ",
          formatC(combined_hold_pct, format = "f", digits = 2, big.mark = ","), "%"
        ))
      validate(need(
        nrow(plot_data) > 0 && any(is.finite(plot_data$combined_hold_pct)),
        "No months have a valid combined handle and revenue."
      ))
      return(
        ggplot(plot_data,
               aes(x = month, y = combined_hold_pct, text = hover_text)) +
          geom_line(color = "steelblue", linewidth = 1, na.rm = TRUE) +
          scale_x_date(
            date_breaks = if (as.numeric(diff(range(plot_data$month))) > 365 * 3)
              "1 year" else "6 months",
            date_labels = "%b %Y",
            limits = range(plot_data$month)
          ) +
          scale_y_continuous(
            breaks = zero_axis_breaks,
            labels = scales::label_number(suffix = "%")
          ) +
          labs(
            title = "Combined Monthly Sports Betting Hold",
            subtitle = "Selected states; revenue divided by handle",
            x = "Reporting Month", y = "Combined Hold (%)",
            caption = "Only state-months with both handle and revenue are included."
          ) +
          extra_plot_theme
      )
    }

    # Original EDA Step 11: monthly hold with state facets and jitter
    if (input$graph_type == "faceted_hold") {
      plot_data <- data |>
        filter(is.finite(handle), handle > 0, is.finite(hold_total))
      validate(need(nrow(plot_data) > 0,
                    "No valid monthly total hold values for these filters."))
      return(
        ggplot(plot_data,
               aes(x = reporting_year, y = hold_total)) +
          geom_boxplot(fill = "lightblue", outlier.shape = NA) +
          geom_jitter(width = 0.12, alpha = 0.4, size = 1) +
          scale_y_continuous(breaks = zero_axis_breaks) +
          facet_wrap(~ state, ncol = 4) +
          labs(
            title = "Monthly Hold Percentage by State and Year",
            x = "Reporting Year", y = "Monthly Hold (%)",
            caption = "Each point is one reported state-month; partial years remain partial."
          ) +
          extra_plot_theme +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
      )
    }

    # Original EDA Step 5B: state-level mobile/retail handle shares
    if (input$graph_type == "channel_share") {
      req(input$graph_channel_year)
      plot_data <- get_channel_share_data(
        data, as.integer(input$graph_channel_year)
      ) |>
        mutate(hover_text = paste0(
          "State: ", state, "<br>Channel: ", channel,
          "<br>Share: ",
          formatC(pct, format = "f", digits = 2, big.mark = ","), "%"
        ))
      validate(need(nrow(plot_data) > 0,
                    "No channel split data for that year and filter selection."))
      return(
        ggplot(plot_data,
               aes(x = state, y = pct, fill = channel,
                   text = hover_text)) +
          geom_col() +
          coord_flip() +
          scale_y_continuous(
            breaks = zero_axis_breaks,
            labels = scales::label_number(suffix = "%")
          ) +
          labs(
            title = paste("Mobile and Retail Handle Share -", input$graph_channel_year),
            subtitle = "Channel totals divided by reported total handle",
            x = "State", y = "Share of Handle (%)", fill = "Channel",
            caption = "Calculated from available months; partial years are not annualized."
          ) +
          extra_plot_theme
      )
    }

    if (input$graph_type == "bar") {
      counts <- data |>
        count(reporting_year, channel_status, name = "Observations") |>
        mutate(hover_text = paste0(
          "Year: ", reporting_year, "<br>Channel status: ", channel_status,
          "<br>Observations: ", Observations
        ))
      return(
        ggplot(counts, aes(x = reporting_year, y = Observations,
                           fill = channel_status, text = hover_text)) +
          geom_col(position = "dodge") +
          scale_y_continuous(breaks = zero_axis_breaks) +
          labs(
            title = "Monthly Observations by Year and Channel Activity",
            x = "Reporting Year", y = "Number of State-Months",
            fill = "Positive Channel Activity"
          ) +
          extra_plot_theme
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
          scale_x_continuous(breaks = zero_axis_breaks) +
          scale_y_continuous(breaks = zero_axis_breaks) +
          labs(title = paste("Distribution of", label),
               x = label, y = "Number of State-Months") +
          extra_plot_theme
      )
    }

    if (input$graph_type == "boxplot") {
      return(
        ggplot(finite_data,
               aes(x = reporting_year, y = .data[[var]],
                   fill = reporting_year)) +
          geom_boxplot(outlier.alpha = 0.35) +
          scale_y_continuous(breaks = zero_axis_breaks) +
          guides(fill = "none") +
          labs(title = paste(label, "by Reporting Year"),
               x = "Reporting Year", y = label) +
          extra_plot_theme
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
          scale_x_continuous(breaks = zero_axis_breaks) +
          scale_y_continuous(breaks = zero_axis_breaks) +
          labs(title = paste("Distribution of", label, "by Year"),
               x = label, y = "Density", color = "Reporting Year") +
          extra_plot_theme
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
          scale_x_continuous(breaks = zero_axis_breaks) +
          scale_y_continuous(breaks = zero_axis_breaks) +
          facet_wrap(stats::as.formula(paste("~", input$graph_facet)),
                     scales = "free_y", ncol = 3) +
          labs(title = paste("Distribution of", label, "by",
                             if (input$graph_facet == "state") "State" else "Year"),
               x = label, y = "Number of State-Months") +
          extra_plot_theme
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

      heat_data <- heat_data |>
        mutate(hover_text = paste0(
          "State: ", state, "<br>Year: ", reporting_year,
          "<br>", label, ": ",
          if (var %in% hold_vars) "" else "$",
          ifelse(is.finite(amount),
                 formatC(amount, format = "f", digits = 2, big.mark = ","),
                 "Not reported"),
          if (var %in% hold_vars) "%" else ""
        ))
      return(
        ggplot(heat_data,
               aes(x = reporting_year, y = state,
                   fill = amount, text = hover_text)) +
          geom_tile(color = "white", linewidth = 0.25) +
          scale_fill_viridis_c(
            limits = range(c(0, heat_data$amount[is.finite(heat_data$amount)])),
            breaks = zero_axis_breaks,
            labels = scales::label_number(
              scale_cut = scales::cut_short_scale(),
              suffix = if (var %in% hold_vars) "%" else ""
            ),
            na.value = "grey85"
          ) +
          labs(title = paste("State-Year Heatmap:", label),
               x = "Reporting Year", y = "State", fill = label) +
          extra_plot_theme +
          theme(panel.grid = element_blank(),
                axis.text.x = element_text(angle = 45, hjust = 1))
      )
    }
  })

  # Step 4A: Convert Additional Plots to interactive charts with hover labels.
  # Keep ggplot calculations in extra_plot_gg() and use Plotly only for display.
  output$extra_plot <- renderPlotly({
    chart <- extra_plot_gg()
    if (input$graph_type != "heatmap") {
      chart <- show_zero_reference(
        chart,
        numeric_x = input$graph_type %in%
          c("hold_hist", "histogram", "density", "facets")
      )
    }
    custom_hover_types <- c(
      "per_capita", "monthly_handle", "monthly_revenue", "cum_handle",
      "national_hold", "channel_share", "bar", "heatmap"
    )
    hover_items <- if (input$graph_type %in% custom_hover_types) {
      "text"
    } else {
      "all"
    }
    plotly::ggplotly(chart, tooltip = hover_items) |>
      plotly::layout(hovermode = "closest") |>
      plotly::config(displaylogo = FALSE, responsive = TRUE)
  })

  output$extra_note <- renderText({
    req(input$graph_type)
    if (input$graph_type == "monthly_handle" ||
        input$graph_type == "monthly_revenue") {
      return(paste(
        "Original state-level monthly time-series view from exploration.R.",
        "All selected months and states come from the last Apply Filters click."
      ))
    }
    if (input$graph_type == "hold_hist") {
      return(paste(
        "Distribution of reported state-month hold percentages where total",
        "handle is positive. Negative reported hold values are retained."
      ))
    }
    if (input$graph_type == "per_capita") {
      return(paste(
        "Annual financial amount divided by same-year state population.",
        "Population estimates derive from Census Vintages 2019 and 2025.",
        paste0("Available years: ", population_coverage, "."),
        "If 12 months are required, partial years are excluded.",
        "Otherwise, partial-year reported totals are not annualized."
      ))
    }
    if (input$graph_type == "cum_handle") {
      return(paste(
        "Based on the cumulative stacked area chart in exploration.R.",
        "Cumulative lines are stacked across states; months excluded",
        "by the applied filters do not contribute to the running total."
      ))
    }
    if (input$graph_type == "national_hold") {
      return(paste(
        "Based on the aggregate hold chart in exploration.R.",
        "This is a handle-weighted hold rate, not the average of state hold percentages.",
        "The contributing states may differ between months."
      ))
    }
    if (input$graph_type == "faceted_hold") {
      return(paste(
        "Based on the faceted hold boxplots in exploration.R.",
        "Each dot represents a monthly observation; negative hold values",
        "remain included when total handle is positive."
      ))
    }
    if (input$graph_type == "channel_share") {
      return(paste(
        "Based on the mobile-retail composition summary in exploration.R.",
        "The adjusted split columns are used only for this composition chart.",
        "A state may have less than 12 months of data in a year."
      ))
    }
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

  output$corr_plot <- renderPlotly({
    result <- corr_data()
    axis_label <- function(s, var) {
      label <- names(numeric_vars)[numeric_vars == var]
      if (result$view != "level") {
        unit <- if (var %in% hold_vars) "pp" else if (result$view == "change") "$ change" else "% growth"
        label <- paste(label, "YoY", unit)
      }
      paste(s, label)
    }
    format_corr_value <- function(z, variable) {
      if (variable %in% hold_vars) {
        suffix <- if (result$view == "change") " pp" else "%"
        paste0(formatC(z, format = "f", digits = 2, big.mark = ","), suffix)
      } else if (result$view == "yoy") {
        paste0(formatC(z, format = "f", digits = 2, big.mark = ","), "%")
      } else {
        paste0("$", formatC(z, format = "f", digits = 2, big.mark = ","))
      }
    }
    chart_data <- result$data |>
      mutate(hover_text = paste0(
        "Period: ", vapply(period, format_period, character(1), mode = result$period),
        "<br>", result$state_x, ": ", format_corr_value(x, result$var_x),
        "<br>", result$state_y, ": ", format_corr_value(y, result$var_y),
        "<br>", market_period
      ))
    chart <- ggplot(chart_data, aes(x = x, y = y)) +
      geom_point(aes(color = market_period, text = hover_text),
                 size = 2.5, alpha = 0.8) +
      geom_smooth(method = "lm", se = FALSE, color = "black") +
      scale_color_manual(
        values = c("Neither state mobile-active" = "gray50",
                   "One state mobile-active" = "#E69F00",
                   "Both states mobile-active" = "#0072B2"),
        name = "Market Period"
      ) +
      scale_x_continuous(
        breaks = zero_axis_breaks,
        labels = scales::label_number(scale_cut = scales::cut_short_scale())
      ) +
      scale_y_continuous(
        breaks = zero_axis_breaks,
        labels = scales::label_number(scale_cut = scales::cut_short_scale())
      ) +
      labs(title = paste(period_labels[[result$period]], "Correlation"),
           subtitle = paste(result$state_x, "vs", result$state_y),
           x = axis_label(result$state_x, result$var_x),
           y = axis_label(result$state_y, result$var_y)) +
      theme_gray()
    chart <- show_zero_reference(chart, numeric_x = TRUE)
    plotly::ggplotly(chart, tooltip = "text") |>
      plotly::layout(hovermode = "closest") |>
      plotly::config(displaylogo = FALSE, responsive = TRUE)
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
        breaks = zero_axis_breaks,
        labels = scales::label_number(prefix = "$", scale_cut = scales::cut_short_scale()))
    } else {
      chart <- chart + scale_y_continuous(
        breaks = zero_axis_breaks,
        labels = scales::label_number(suffix = suffix)
      )
    }
    if (result$display == "Separate panels") {
      chart <- chart + facet_wrap(~state, scales = "free_y") + guides(color = "none")
    }
    chart <- show_zero_reference(chart)
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
