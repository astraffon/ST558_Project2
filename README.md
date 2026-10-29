# U.S. Sports Betting Explorer

**NC State University — ST 558: Data Science for Statisticians, Project 2**  
**Author:** Alfonso Straffon

## Overview

The **U.S. Sports Betting Explorer** is an interactive R Shiny application for investigating publicly reported sports betting activity across U.S. jurisdictions. Users can explore handle (amount wagered), sportsbook revenue, taxes, hold percentages, mobile and retail activity, and per-capita measures using customizable tables and charts.

In 2018, the U.S. Supreme Court struck down the federal Professional and Amateur Sports Protection Act (PASPA), opening the door for states to authorize sports betting. Since then, the market has expanded well beyond Nevada. State gaming regulators generally publish sports betting results monthly, although reporting practices and the level of detail differ among jurisdictions.

I personally collected and organized the sports betting data for my work as a sports betting industry analyst. This project is an effort to make that information easier to explore and to move beyond the manually updated static reports previously published for visitors to [iGaming Analyst](https://www.igaminganalyst.com/), my website.

## Data

The sports betting dataset currently contains **2,377 state-month observations from 35 reporting jurisdictions**, covering **2018 through August 2026**. It was prepared from my compiled state-level reporting data, based primarily on figures released by state gaming regulators.

Annual resident population estimates used for per-capita comparisons come from two **U.S. Census Bureau** datasets:

- **2018–2019:** [Vintage 2019 state population estimates](https://www.census.gov/newsroom/press-kits/2019/national-state-estimates.html) (`NST-EST2019-ALLDATA`).
- **2020–2025:** [Vintage 2025 state population estimates](https://www.census.gov/data/datasets/time-series/demo/popest/2020s-state-total.html) (`NST-EST2025-ALLDATA`).

These two vintages were combined into `data/state_population_clean.csv`. Per-capita amounts are calculated by dividing annual financial totals by each jurisdiction's resident population estimate. Comparisons spanning the 2019–2020 change in Census vintage should be interpreted with care.

**Data considerations:** Availability, definitions, and reporting delays vary by state. Missing entries are not assumed to be zero. Some reported revenue or adjustment amounts can be negative. Partial-year totals are not annualized, and the data are a compiled historical snapshot rather than a live feed.

## Application Features

- **About:** Background, source information, explanations of measures, instructions, and examples of earlier static charts.
- **Data & Summaries → Data Table:** Filter by state, reporting year, and up to two optional numerical ranges; inspect observations and download the filtered data as a CSV.
- **Data & Summaries → Numerical Summaries:** View counts, means, medians, standard deviations, minima, maxima, and totals where applicable, with grouping options.
- **Data & Summaries → Categorical Summaries:** Create one-way frequency tables and two-way contingency tables with totals.
- **Data & Summaries → Additional Plots:** Create interactive charts such as state rankings, per-capita comparisons, stacked areas, histograms, boxplots, heatmaps, and mobile-versus-retail comparisons.
- **Data Exploration → Correlation and Time Series:** Compare series across states, examine monthly and aggregated reporting periods, and evaluate year-over-year changes. These views have their **own controls** and are independent of the Data & Summaries sidebar filters.

In **Data & Summaries**, selections take effect only after clicking **Apply Filters**. Hover over interactive charts to inspect values.

## Run Locally

1. Clone or download this repository and open the project folder in RStudio.
2. Install the necessary packages if they are not already installed:

   ```r
   install.packages(c("shiny", "tidyverse", "DT", "plotly", "lubridate"))
   ```

3. From the folder containing `app.R`, launch the application using RStudio's **Run App** button or:

   ```r
   shiny::runApp()
   ```

Keep the `data/` and `www/images/` folders alongside `app.R`; they provide the datasets and the About-tab graphics.

## Repository Structure

```text
ST558_Project2/
├── app.R                         # Shiny UI and server
├── helpers.R                     # Reusable calculations and data helpers
├── exploration.R                 # Static exploratory analysis for Project 2
├── data/
│   ├── sports_betting_clean.csv  # Prepared state-month observations
│   └── state_population_clean.csv # Combined Census population estimates
├── www/
│   └── images/                   # Historical reporting charts and logo collage
└── README.md
```

The `exploration.R` script contains the preliminary static exploration, including frequency tables, numerical summaries, multiple chart types, faceting, and a heatmap.

## Project and Links

- **Course:** NC State ST 558 — Data Science for Statisticians
- **GitHub repository:** https://github.com/astraffon/ST558_Project2
- **Live Shiny Application:** https://igaminganalyst.shinyapps.io/ST558_Project2/
- **Related website:** https://www.igaminganalyst.com/

This application is intended for descriptive exploration and should not be treated as a substitute for the underlying official state regulatory reports.
