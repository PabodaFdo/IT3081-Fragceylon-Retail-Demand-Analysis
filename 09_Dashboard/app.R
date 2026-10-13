# ============================================================
# IT3081 - Statistical Modelling
# Retail Product Demand Analysis at Fragceylon
# TASK 10 - INDUSTRY INNOVATION PROPOSAL
# R SHINY DEMAND MONITORING & DECISION SUPPORT DASHBOARD
# ============================================================
#
# PURPOSE
# -------
# This dashboard converts the completed statistical analysis into an
# interactive management prototype.
#
# IMPORTANT LIMITATION
# --------------------
# The dashboard does NOT calculate exact reorder quantities because the
# current dataset does not contain complete inventory-on-hand, stock-out,
# supplier lead-time, replenishment and holding-cost information.
#
# The dashboard provides:
# 1. Executive overview
# 2. Product demand analysis
# 3. Seasonal/time-based analysis
# 4. Channel and market analysis
# 5. Predictive demand insights
# 6. Decision-support indicators
#
# ============================================================


# ============================================================
# 1. REQUIRED PACKAGES
# ============================================================

required_packages = c(
  "shiny",
  "shinydashboard",
  "readxl",
  "dplyr",
  "ggplot2",
  "DT",
  "scales"
)

missing_packages =
  required_packages[
    !required_packages %in%
      rownames(
        installed.packages()
      )
  ]

if(length(missing_packages) > 0) {
  stop(
    paste0(
      "Please install the following packages before running the dashboard: ",
      paste(
        missing_packages,
        collapse = ", "
      ),
      "\n\nRun:\ninstall.packages(c(",
      paste0(
        '"',
        missing_packages,
        '"',
        collapse = ", "
      ),
      "))"
    )
  )
}

library(shiny)
library(shinydashboard)
library(readxl)
library(dplyr)
library(ggplot2)
library(DT)
library(scales)


# ============================================================
# 2. PROJECT PATHS
# ============================================================

find_project_root = function(
  start_path = getwd()
) {

  current_path =
    normalizePath(
      start_path,
      winslash = "/",
      mustWork = FALSE
    )

  for(i in 1:10) {

    candidate_file =
      file.path(
        current_path,
        "04_Cleaned_Data",
        "Fragceylon_Regular_Orders.xlsx"
      )

    if(file.exists(candidate_file)) {
      return(current_path)
    }

    parent_path =
      dirname(
        current_path
      )

    if(parent_path == current_path) {
      break
    }

    current_path =
      parent_path
  }

  stop(
    paste0(
      "Project root could not be found. ",
      "Open the Fragceylon project folder in RStudio and run the app again."
    )
  )
}

project_root =
  find_project_root()

regular_file =
  file.path(
    project_root,
    "04_Cleaned_Data",
    "Fragceylon_Regular_Orders.xlsx"
  )

if(!file.exists(regular_file)) {
  stop(
    paste0(
      "Cleaned Regular Order file not found:\n",
      regular_file
    )
  )
}


# ============================================================
# 3. LOAD DATA
# ============================================================

regular_orders =
  read_excel(
    regular_file
  )

regular_orders$Date =
  as.Date(
    regular_orders$Date
  )

regular_orders =
  regular_orders %>%
  mutate(
    Product_Name =
      as.character(
        `Product Name`
      ),
    Product_Code =
      as.character(
        `Product Code`
      ),
    Channel_Type =
      as.character(
        `Channel Type`
      ),
    Export_Market =
      as.character(
        `Export Market`
      ),
    Day =
      as.character(
        Day_of_Week
      ),
    Month_Name =
      as.character(
        Month
      ),
    Month_Start =
      as.Date(
        paste0(
          format(
            Date,
            "%Y-%m"
          ),
          "-01"
        )
      )
  )


# ============================================================
# 4. CURRENT REGIME AND MODEL
# ============================================================

REGIME_CHANGE_DATE =
  as.Date("2025-04-01")

TEST_START_DATE =
  as.Date("2026-04-01")

current_regime =
  regular_orders %>%
  filter(
    Date >=
      REGIME_CHANGE_DATE
  )

model_train =
  current_regime %>%
  filter(
    Date <
      TEST_START_DATE
  )

product_levels =
  c(
    "Dark Matrix",
    "Kennedy",
    "Mesmerose",
    "Secret Ambrosia"
  )

market_levels =
  c(
    "Local",
    "Japan"
  )

day_levels =
  c(
    "Monday",
    "Friday",
    "Saturday",
    "Sunday",
    "Thursday",
    "Tuesday",
    "Wednesday"
  )

model_train$Product_Name =
  factor(
    model_train$Product_Name,
    levels = product_levels
  )

model_train$Export_Market =
  factor(
    model_train$Export_Market,
    levels = market_levels
  )

model_train$Day =
  factor(
    model_train$Day,
    levels = day_levels
  )

reduced_mlr =
  lm(
    Quantity ~
      Product_Name +
      Export_Market +
      Day,
    data =
      model_train
  )

model_test =
  current_regime %>%
  filter(
    Date >=
      TEST_START_DATE
  )

model_test$Product_Name =
  factor(
    model_test$Product_Name,
    levels = product_levels
  )

model_test$Export_Market =
  factor(
    model_test$Export_Market,
    levels = market_levels
  )

model_test$Day =
  factor(
    model_test$Day,
    levels = day_levels
  )

regression_metrics = function(
  actual,
  predicted
) {

  mae =
    mean(
      abs(
        actual - predicted
      )
    )

  rmse =
    sqrt(
      mean(
        (
          actual - predicted
        )^2
      )
    )

  r2 =
    1 -
    sum(
      (
        actual - predicted
      )^2
    ) /
    sum(
      (
        actual - mean(actual)
      )^2
    )

  c(
    MAE = mae,
    RMSE = rmse,
    R2 = r2
  )
}

model_train_prediction =
  predict(
    reduced_mlr,
    newdata = model_train
  )

model_test_prediction =
  predict(
    reduced_mlr,
    newdata = model_test
  )

model_train_metrics =
  regression_metrics(
    model_train$Quantity,
    model_train_prediction
  )

model_test_metrics =
  regression_metrics(
    model_test$Quantity,
    model_test_prediction
  )

baseline_value =
  mean(
    model_train$Quantity
  )

baseline_test_prediction =
  rep(
    baseline_value,
    nrow(model_test)
  )

baseline_test_metrics =
  regression_metrics(
    model_test$Quantity,
    baseline_test_prediction
  )

rmse_improvement_percent =
  (
    baseline_test_metrics["RMSE"] -
    model_test_metrics["RMSE"]
  ) /
  baseline_test_metrics["RMSE"] *
  100

market_holdout_performance =
  data.frame(
    Export_Market =
      market_levels,
    N = NA_integer_,
    MAE = NA_real_,
    RMSE = NA_real_,
    stringsAsFactors = FALSE
  )

for(i in seq_along(market_levels)) {

  market_value =
    market_levels[i]

  market_index =
    as.character(
      model_test$Export_Market
    ) == market_value

  market_holdout_performance$N[i] =
    sum(
      market_index
    )

  market_holdout_performance$MAE[i] =
    mean(
      abs(
        model_test$Quantity[market_index] -
        model_test_prediction[market_index]
      )
    )

  market_holdout_performance$RMSE[i] =
    sqrt(
      mean(
        (
          model_test$Quantity[market_index] -
          model_test_prediction[market_index]
        )^2
      )
    )
}


# ============================================================
# 5. PRE-COMPUTED MANAGEMENT SUMMARIES
# ============================================================

overall_total_demand =
  sum(
    regular_orders$Quantity,
    na.rm = TRUE
  )

overall_total_revenue =
  sum(
    regular_orders$`Total Value (LKR)`,
    na.rm = TRUE
  )

overall_avg_order =
  mean(
    regular_orders$Quantity,
    na.rm = TRUE
  )

current_total_demand =
  sum(
    current_regime$Quantity,
    na.rm = TRUE
  )

overall_regular_orders =
  nrow(
    regular_orders
  )

HIGH_DEMAND_RATIO =
  1.20

LOW_DEMAND_RATIO =
  0.80

product_summary =
  regular_orders %>%
  group_by(
    Product_Name
  ) %>%
  summarise(
    Orders = n(),
    Total_Demand =
      sum(
        Quantity,
        na.rm = TRUE
      ),
    Average_Order =
      mean(
        Quantity,
        na.rm = TRUE
      ),
    Revenue_LKR =
      sum(
        `Total Value (LKR)`,
        na.rm = TRUE
      ),
    .groups = "drop"
  ) %>%
  arrange(
    desc(
      Total_Demand
    )
  )

channel_summary =
  regular_orders %>%
  group_by(
    Channel_Type
  ) %>%
  summarise(
    Orders = n(),
    Total_Demand =
      sum(
        Quantity,
        na.rm = TRUE
      ),
    Average_Order =
      mean(
        Quantity,
        na.rm = TRUE
      ),
    .groups = "drop"
  ) %>%
  arrange(
    desc(
      Total_Demand
    )
  )

market_summary =
  regular_orders %>%
  group_by(
    Export_Market
  ) %>%
  summarise(
    Orders = n(),
    Total_Demand =
      sum(
        Quantity,
        na.rm = TRUE
      ),
    Average_Order =
      mean(
        Quantity,
        na.rm = TRUE
      ),
    .groups = "drop"
  ) %>%
  arrange(
    desc(
      Total_Demand
    )
  )

monthly_summary =
  regular_orders %>%
  group_by(
    Month_Start
  ) %>%
  summarise(
    Total_Demand =
      sum(
        Quantity,
        na.rm = TRUE
      ),
    Orders = n(),
    .groups = "drop"
  ) %>%
  arrange(
    Month_Start
  )

current_monthly =
  current_regime %>%
  group_by(
    Month_Start,
    Product_Name
  ) %>%
  summarise(
    Total_Demand =
      sum(
        Quantity,
        na.rm = TRUE
      ),
    Orders = n(),
    .groups = "drop"
  ) %>%
  arrange(
    Product_Name,
    Month_Start
  )


# ============================================================
# 6. DECISION-SUPPORT FLAGS
# ============================================================
#
# These indicators are descriptive alerts, NOT reorder quantities.
#
# Recent period = latest 3 months in the current regime.
# Historical comparison = all earlier months in current regime.
#
# Above-reference indicator:
#   Recent monthly average > 120% of historical monthly average
#
# Below-reference indicator:
#   Recent monthly average < 80% of historical monthly average
#
# Within-reference indicator:
#   Between 80% and 120%
#
# The thresholds are transparent management indicators only.
# ============================================================

latest_month =
  max(
    current_regime$Month_Start
  )

recent_start =
  seq(
    latest_month,
    by = "-2 months",
    length.out = 2
  )[2]

decision_base =
  current_regime %>%
  group_by(
    Product_Name,
    Month_Start
  ) %>%
  summarise(
    Monthly_Demand =
      sum(
        Quantity,
        na.rm = TRUE
      ),
    .groups = "drop"
  )

decision_support =
  lapply(
    product_levels,
    function(product_value) {

      temp =
        decision_base %>%
        filter(
          Product_Name ==
            product_value
        )

      recent =
        temp %>%
        filter(
          Month_Start >=
            recent_start
        )

      historical =
        temp %>%
        filter(
          Month_Start <
            recent_start
        )

      recent_avg =
        mean(
          recent$Monthly_Demand,
          na.rm = TRUE
        )

      historical_avg =
        mean(
          historical$Monthly_Demand,
          na.rm = TRUE
        )

      ratio =
        ifelse(
          is.finite(
            historical_avg
          ) &&
          historical_avg > 0,
          recent_avg /
            historical_avg,
          NA_real_
        )

      alert =
        ifelse(
          is.na(ratio),
          "Insufficient History",
          ifelse(
            ratio > HIGH_DEMAND_RATIO,
            "Above Reference Range",
            ifelse(
              ratio < LOW_DEMAND_RATIO,
              "Below Reference Range",
              "Within Reference Range"
            )
          )
        )

      data.frame(
        Product =
          product_value,
        Recent_3_Month_Avg =
          recent_avg,
        Earlier_Current_Regime_Avg =
          historical_avg,
        Recent_to_Historical_Ratio =
          ratio,
        Indicator =
          alert,
        stringsAsFactors = FALSE
      )
    }
  ) %>%
  bind_rows()


# ============================================================
# 7. UI
# ============================================================

ui =
  dashboardPage(

    skin = "blue",

    dashboardHeader(
      title =
        "Fragceylon Demand Monitoring",
      titleWidth = 300
    ),

    dashboardSidebar(
      width = 250,

      sidebarMenu(
        id = "tabs",

        menuItem(
          "Overview",
          tabName = "overview",
          icon = icon("chart-line")
        ),

        menuItem(
          "Product Demand",
          tabName = "products",
          icon = icon("box")
        ),

        menuItem(
          "Monthly Demand",
          tabName = "seasonal",
          icon = icon("calendar")
        ),

        menuItem(
          "Market & Channel",
          tabName = "channel_market",
          icon = icon("store")
        ),

        menuItem(
          "Prediction",
          tabName = "predictive",
          icon = icon("calculator")
        ),

        menuItem(
          "Demand Monitoring",
          tabName = "decision",
          icon = icon("lightbulb")
        ),

        menuItem(
          "Data & Model Notes",
          tabName = "about",
          icon = icon("circle-info")
        )
      )
    ),

    dashboardBody(

      tags$head(
        tags$style(
          HTML(
            "
            .content-wrapper, .right-side {
              background-color: #f4f6f9;
            }

            .box {
              border-radius: 4px;
            }

            .small-box {
              border-radius: 4px;
            }

            .dashboard-note {
              padding: 12px;
              border-left: 4px solid #3c8dbc;
              background: #ffffff;
              margin-bottom: 15px;
            }

            .warning-note {
              padding: 12px;
              border-left: 4px solid #f39c12;
              background: #fffdf5;
              margin-bottom: 15px;
            }

            .success-note {
              padding: 12px;
              border-left: 4px solid #00a65a;
              background: #f7fff9;
              margin-bottom: 15px;
            }
            "
          )
        )
      ),

      tabItems(

        # ====================================================
        # EXECUTIVE OVERVIEW
        # ====================================================

        tabItem(
          tabName = "overview",

          fluidRow(
            valueBoxOutput(
              "totalDemandBox",
              width = 3
            ),
            valueBoxOutput(
              "revenueBox",
              width = 3
            ),
            valueBoxOutput(
              "avgOrderBox",
              width = 3
            ),
            valueBoxOutput(
              "regularOrderBox",
              width = 3
            )
          ),

          fluidRow(
            box(
              title =
                "Monthly Regular-Order Demand",
              status = "primary",
              solidHeader = TRUE,
              width = 8,
              plotOutput(
                "overviewMonthlyPlot",
                height = 330
              )
            ),

            box(
              title =
                "Demand by Product",
              status = "primary",
              solidHeader = TRUE,
              width = 4,
              plotOutput(
                "overviewProductPlot",
                height = 330
              )
            )
          ),

          fluidRow(
            box(
              title =
                "Management Notes",
              status = "info",
              solidHeader = TRUE,
              width = 12,

              tags$ul(
                tags$li(
                  "Use the dashboard as a weekly review of Regular Order demand."
                ),
                tags$li(
                  "The selected model predicts transaction-level order quantity, not monthly total demand."
                ),
                tags$li(
                  "Market and channel patterns are historical associations and should not be interpreted as causal effects."
                ),
                tags$li(
                  "Demand-monitoring thresholds are prototype rules for management review, not automatic reorder quantities."
                )
              )
            )
          )
        ),


        # ====================================================
        # PRODUCT DEMAND
        # ====================================================

        tabItem(
          tabName = "products",

          fluidRow(
            box(
              title = "Filters",
              status = "primary",
              solidHeader = TRUE,
              width = 3,

              selectInput(
                "productFilter",
                "Product",
                choices =
                  c(
                    "All Products",
                    product_levels
                  ),
                selected =
                  "All Products"
              ),

              dateRangeInput(
                "productDateRange",
                "Date Range",
                start =
                  min(
                    regular_orders$Date
                  ),
                end =
                  max(
                    regular_orders$Date
                  ),
                min =
                  min(
                    regular_orders$Date
                  ),
                max =
                  max(
                    regular_orders$Date
                  )
              )
            ),

            box(
              title =
                "Demand Trend",
              status = "primary",
              solidHeader = TRUE,
              width = 9,

              plotOutput(
                "productTrendPlot",
                height = 350
              )
            )
          ),

          fluidRow(
            box(
              title =
                "Product Summary",
              status = "info",
              solidHeader = TRUE,
              width = 12,

              DTOutput(
                "productTable"
              )
            )
          )
        ),


        # ====================================================
        # SEASONAL ANALYSIS
        # ====================================================

        tabItem(
          tabName = "seasonal",

          fluidRow(
            box(
              title =
                "Monthly Demand - Current Business Regime",
              status = "primary",
              solidHeader = TRUE,
              width = 8,

              plotOutput(
                "seasonalMonthlyPlot",
                height = 350
              )
            ),

            box(
              title =
                "Time-Series Note",
              status = "warning",
              solidHeader = TRUE,
              width = 4,

              div(
                class = "warning-note",
                tags$b(
                  "Important limitation"
                ),
                tags$br(),
                "The current Main Dealer regime contains only 15 monthly observations (April 2025 to June 2026)."
              ),

              tags$ul(
                tags$li(
                  "Only about 1.25 comparable annual cycles."
                ),
                tags$li(
                  "Annual seasonality is not established."
                ),
                tags$li(
                  "ARIMA/SARIMA should not be treated as production models yet."
                )
              )
            )
          ),

          fluidRow(
            box(
              title =
                "Month-of-Year View",
              status = "info",
              solidHeader = TRUE,
              width = 12,

              plotOutput(
                "monthOfYearPlot",
                height = 350
              )
            )
          )
        ),


        # ====================================================
        # CHANNEL & MARKET
        # ====================================================

        tabItem(
          tabName = "channel_market",

          fluidRow(
            box(
              title =
                "Historical Demand by Channel",
              status = "primary",
              solidHeader = TRUE,
              width = 6,

              plotOutput(
                "channelPlot",
                height = 330
              )
            ),

            box(
              title =
                "Historical Demand by Market",
              status = "primary",
              solidHeader = TRUE,
              width = 6,

              plotOutput(
                "marketPlot",
                height = 330
              )
            )
          ),

          fluidRow(
            box(
              title =
                "Historical Channel Summary",
              status = "info",
              solidHeader = TRUE,
              width = 6,

              DTOutput(
                "channelTable"
              )
            ),

            box(
              title =
                "Historical Market Summary",
              status = "info",
              solidHeader = TRUE,
              width = 6,

              DTOutput(
                "marketTable"
              )
            )
          ),

          fluidRow(
            box(
              title =
                "Structural Change Warning",
              status = "warning",
              solidHeader = TRUE,
              width = 12,

              div(
                class = "warning-note",
                "Before 1 April 2025, Regular Orders came through Shop and Small Dealer channels. "
                ,
                "From 1 April 2025 onward, all Regular Orders are Main Dealer records. "
                ,
                "Therefore, channel comparisons across periods should not be interpreted as causal effects."
              )
            )
          )
        ),


        # ====================================================
        # PREDICTIVE INSIGHTS
        # ====================================================

        tabItem(
          tabName = "predictive",

          fluidRow(
            box(
              title =
                "Demand Scenario",
              status = "primary",
              solidHeader = TRUE,
              width = 4,

              selectInput(
                "predProduct",
                "Product",
                choices =
                  product_levels,
                selected =
                  "Dark Matrix"
              ),

              selectInput(
                "predMarket",
                "Market",
                choices =
                  market_levels,
                selected =
                  "Local"
              ),

              selectInput(
                "predDay",
                "Day of Week",
                choices =
                  day_levels,
                selected =
                  "Monday"
              ),

              actionButton(
                "runPrediction",
                "Estimate Demand",
                icon =
                  icon(
                    "calculator"
                  ),
                class =
                  "btn-primary"
              )
            ),

            box(
              title =
                "Expected Order Quantity",
              status = "success",
              solidHeader = TRUE,
              width = 4,

              h2(
                textOutput(
                  "predictionValue"
                )
              ),

              tags$b(
                "95% model-based prediction interval (nonnegative display)"
              ),

              p(
                textOutput(
                  "predictionInterval"
                )
              ),

              p(
                tags$small(
                  textOutput(
                    "predictionIntervalNote"
                  )
                )
              ),

              p(
                "This is a model estimate for one future transaction, not a guaranteed sale."
              )
            ),

            box(
              title =
                "Model Summary",
              status = "info",
              solidHeader = TRUE,
              width = 4,

              uiOutput(
                "modelContext"
              )
            )
          ),

          fluidRow(
            box(
              title =
                "Holdout Performance by Market",
              status = "info",
              solidHeader = TRUE,
              width = 6,

              DTOutput(
                "marketHoldoutTable"
              )
            ),

            box(
              title =
                "Prediction Limits",
              status = "warning",
              solidHeader = TRUE,
              width = 6,

              tags$ul(
                tags$li(
                  "The model predicts transaction-level Quantity only."
                ),
                tags$li(
                  "The Japan holdout subgroup is smaller and has higher prediction error."
                ),
                tags$li(
                  "Inventory, promotion, stock-out, supplier lead-time and marketing variables are not available."
                ),
                tags$li(
                  "Predictions should support management review rather than replace judgment."
                )
              )
            )
          )
        ),


        # ====================================================
        # DEMAND MONITORING
        # ====================================================

        tabItem(
          tabName = "decision",

          fluidRow(
            box(
              title =
                "Product Monitoring Status",
              status = "primary",
              solidHeader = TRUE,
              width = 12,

              DTOutput(
                "decisionTable"
              )
            )
          ),

          fluidRow(
            box(
              title =
                "Monitoring Rule",
              status = "info",
              solidHeader = TRUE,
              width = 6,

              tags$ul(
                tags$li(
                  "Recent demand = average monthly demand across the latest 3 months."
                ),
                tags$li(
                  "Historical comparison = earlier months within the current Main Dealer regime."
                ),
                tags$li(
                  "Above Reference Range = recent average > 120% of earlier average."
                ),
                tags$li(
                  "Below Reference Range = recent average < 80% of earlier average."
                ),
                tags$li(
                  "Otherwise = Within Reference Range."
                )
              )
            ),

            box(
              title =
                "Use Restriction",
              status = "warning",
              solidHeader = TRUE,
              width = 6,

              div(
                class = "warning-note",
                tags$b(
                  "Do not use these indicators as automatic reorder quantities."
                ),
                tags$br(),
                "They are prototype monitoring rules designed to direct management attention. "
                ,
                "Exact replenishment decisions require inventory on hand, stock-out history, supplier lead time, order costs and service-level targets."
              )
            )
          )
        ),


        # ====================================================
        # ABOUT & LIMITATIONS
        # ====================================================

        tabItem(
          tabName = "about",

          fluidRow(
            box(
              title =
                "What This Dashboard Does",
              status = "primary",
              solidHeader = TRUE,
              width = 12,

              h4(
                "Fragceylon Demand Monitoring & Decision Support Dashboard"
              ),

              p(
                "The dashboard brings the project results into one place for weekly review of product demand, market/channel patterns, model-based order estimates and unusual recent demand changes."
              )
            )
          ),

          fluidRow(
            box(
              title =
                "Data Flow",
              status = "info",
              solidHeader = TRUE,
              width = 6,

              tags$pre(
                "
Historical Sales Data
        ↓
Data Cleaning
        ↓
Statistical Analysis
        ↓
Demand Model
        ↓
Demand Insights
        ↓
R Shiny Dashboard
        ↓
Management Decisions
        ↓
New Data / Feedback
                "
              )
            ),

            box(
              title =
                "Current Data Limits",
              status = "warning",
              solidHeader = TRUE,
              width = 6,

              tags$ul(
                tags$li(
                  "Historical period is short."
                ),
                tags$li(
                  "Major channel/process change occurred in April 2025."
                ),
                tags$li(
                  "No complete inventory-on-hand history."
                ),
                tags$li(
                  "No supplier lead-time history."
                ),
                tags$li(
                  "No complete stock-out records."
                ),
                tags$li(
                  "No full promotion/marketing history."
                ),
                tags$li(
                  "Predictions contain uncertainty."
                ),
                tags$li(
                  "Model associations should not be interpreted as causal effects."
                )
              )
            )
          ),

          fluidRow(
            box(
              title =
                "Future Data to Collect",
              status = "success",
              solidHeader = TRUE,
              width = 12,

              tags$ul(
                tags$li(
                  "Stock on hand"
                ),
                tags$li(
                  "Reorder date"
                ),
                tags$li(
                  "Stock-out dates"
                ),
                tags$li(
                  "Supplier lead time"
                ),
                tags$li(
                  "Purchase/restock quantity"
                ),
                tags$li(
                  "Promotion indicator and discount"
                ),
                tags$li(
                  "Marketing campaign information"
                ),
                tags$li(
                  "Longer stable historical sales period"
                )
              )
            )
          )
        )
      )
    )
  )


# ============================================================
# 8. SERVER
# ============================================================

server =
  function(
    input,
    output,
    session
  ) {

    # --------------------------------------------------------
    # EXECUTIVE VALUE BOXES
    # --------------------------------------------------------

    output$totalDemandBox =
      renderValueBox({
        valueBox(
          value =
            comma(
              overall_total_demand
            ),
          subtitle =
            "Total Regular-Order Demand",
          icon =
            icon(
              "boxes-stacked"
            ),
          color =
            "aqua"
        )
      })

    output$revenueBox =
      renderValueBox({
        valueBox(
          value =
            paste0(
              "LKR ",
              comma(
                round(
                  overall_total_revenue,
                  0
                )
              )
            ),
          subtitle =
            "Regular-Order Revenue",
          icon =
            icon(
              "money-bill-wave"
            ),
          color =
            "green"
        )
      })

    output$avgOrderBox =
      renderValueBox({
        valueBox(
          value =
            comma(
              round(
                overall_avg_order,
                1
              )
            ),
          subtitle =
            "Average Quantity per Order",
          icon =
            icon(
              "cart-shopping"
            ),
          color =
            "yellow"
        )
      })

    output$regularOrderBox =
      renderValueBox({
        valueBox(
          value =
            comma(
              overall_regular_orders
            ),
          subtitle =
            "Regular Orders",
          icon =
            icon(
              "receipt"
            ),
          color =
            "purple"
        )
      })


    # --------------------------------------------------------
    # EXECUTIVE PLOTS
    # --------------------------------------------------------

    output$overviewMonthlyPlot =
      renderPlot({

        ggplot(
          monthly_summary,
          aes(
            x =
              Month_Start,
            y =
              Total_Demand
          )
        ) +
          geom_line(
            linewidth = 0.8
          ) +
          geom_point(
            size = 2
          ) +
          geom_vline(
            xintercept =
              as.numeric(
                REGIME_CHANGE_DATE
              ),
            linetype =
              "dashed"
          ) +
          scale_y_continuous(
            labels =
              comma
          ) +
          labs(
            x = NULL,
            y = "Quantity",
            caption =
              "Dashed line = start of current Main Dealer regime"
          ) +
          theme_minimal(
            base_size = 12
          )
      })

    output$overviewProductPlot =
      renderPlot({

        ggplot(
          product_summary,
          aes(
            x =
              reorder(
                Product_Name,
                Total_Demand
              ),
            y =
              Total_Demand
          )
        ) +
          geom_col() +
          coord_flip() +
          scale_y_continuous(
            labels =
              comma
          ) +
          labs(
            x = NULL,
            y = "Total Demand"
          ) +
          theme_minimal(
            base_size = 12
          )
      })


    # --------------------------------------------------------
    # PRODUCT FILTERS
    # --------------------------------------------------------

    filtered_product_data =
      reactive({

        data =
          regular_orders %>%
          filter(
            Date >=
              input$productDateRange[1],
            Date <=
              input$productDateRange[2]
          )

        if(
          input$productFilter !=
            "All Products"
        ) {
          data =
            data %>%
            filter(
              Product_Name ==
                input$productFilter
            )
        }

        data
      })


    output$productTrendPlot =
      renderPlot({

        plot_data =
          filtered_product_data() %>%
          group_by(
            Month_Start,
            Product_Name
          ) %>%
          summarise(
            Total_Demand =
              sum(
                Quantity,
                na.rm = TRUE
              ),
            .groups = "drop"
          )

        ggplot(
          plot_data,
          aes(
            x =
              Month_Start,
            y =
              Total_Demand,
            group =
              Product_Name,
            linetype =
              Product_Name,
            shape =
              Product_Name
          )
        ) +
          geom_line(
            linewidth = 0.8
          ) +
          geom_point(
            size = 2
          ) +
          scale_y_continuous(
            labels =
              comma
          ) +
          labs(
            x = NULL,
            y = "Monthly Demand",
            linetype = "Product",
            shape = "Product"
          ) +
          theme_minimal(
            base_size = 12
          )
      })


    output$productTable =
      renderDT({

        data =
          filtered_product_data() %>%
          group_by(
            Product_Name
          ) %>%
          summarise(
            Orders = n(),
            Total_Demand =
              sum(
                Quantity,
                na.rm = TRUE
              ),
            Average_Order =
              mean(
                Quantity,
                na.rm = TRUE
              ),
            Revenue_LKR =
              sum(
                `Total Value (LKR)`,
                na.rm = TRUE
              ),
            .groups = "drop"
          ) %>%
          arrange(
            desc(
              Total_Demand
            )
          )

        datatable(
          data,
          rownames = FALSE,
          options =
            list(
              pageLength = 10,
              scrollX = TRUE
            )
        ) %>%
          formatRound(
            columns =
              c(
                "Average_Order"
              ),
            digits = 2
          ) %>%
          formatCurrency(
            columns =
              c(
                "Revenue_LKR"
              ),
            currency = "LKR ",
            digits = 0
          )
      })


    # --------------------------------------------------------
    # SEASONAL ANALYSIS
    # --------------------------------------------------------

    output$seasonalMonthlyPlot =
      renderPlot({

        current_total_monthly =
          current_regime %>%
          group_by(
            Month_Start
          ) %>%
          summarise(
            Total_Demand =
              sum(
                Quantity,
                na.rm = TRUE
              ),
            .groups = "drop"
          )

        ggplot(
          current_total_monthly,
          aes(
            x =
              Month_Start,
            y =
              Total_Demand
          )
        ) +
          geom_line(
            linewidth = 0.9
          ) +
          geom_point(
            size = 2.5
          ) +
          scale_y_continuous(
            labels =
              comma
          ) +
          labs(
            x = NULL,
            y = "Monthly Demand"
          ) +
          theme_minimal(
            base_size = 12
          )
      })


    output$monthOfYearPlot =
      renderPlot({

        month_data =
          current_regime %>%
          group_by(
            Year,
            Month_Number,
            Month_Name
          ) %>%
          summarise(
            Total_Demand =
              sum(
                Quantity,
                na.rm = TRUE
              ),
            .groups = "drop"
          )

        month_data$Month_Name =
          factor(
            month_data$Month_Name,
            levels =
              month.name
          )

        ggplot(
          month_data,
          aes(
            x =
              Month_Name,
            y =
              Total_Demand,
            group =
              factor(
                Year
              ),
            linetype =
              factor(
                Year
              ),
            shape =
              factor(
                Year
              )
          )
        ) +
          geom_line(
            linewidth = 0.8
          ) +
          geom_point(
            size = 2
          ) +
          scale_y_continuous(
            labels =
              comma
          ) +
          labs(
            x = "Month",
            y = "Demand",
            linetype = "Year",
            shape = "Year",
            caption =
              "Current history is too short to establish dependable annual seasonality."
          ) +
          theme_minimal(
            base_size = 12
          ) +
          theme(
            axis.text.x =
              element_text(
                angle = 45,
                hjust = 1
              )
          )
      })


    # --------------------------------------------------------
    # CHANNEL AND MARKET
    # --------------------------------------------------------

    output$channelPlot =
      renderPlot({

        ggplot(
          channel_summary,
          aes(
            x =
              reorder(
                Channel_Type,
                Total_Demand
              ),
            y =
              Total_Demand
          )
        ) +
          geom_col() +
          coord_flip() +
          scale_y_continuous(
            labels =
              comma
          ) +
          labs(
            x = NULL,
            y = "Total Demand"
          ) +
          theme_minimal(
            base_size = 12
          )
      })

    output$marketPlot =
      renderPlot({

        ggplot(
          market_summary,
          aes(
            x =
              Export_Market,
            y =
              Total_Demand
          )
        ) +
          geom_col() +
          scale_y_continuous(
            labels =
              comma
          ) +
          labs(
            x = "Market",
            y = "Total Demand"
          ) +
          theme_minimal(
            base_size = 12
          )
      })

    output$channelTable =
      renderDT({

        datatable(
          channel_summary,
          rownames = FALSE,
          options =
            list(
              pageLength = 5,
              dom = "t",
              scrollX = TRUE
            )
        ) %>%
          formatRound(
            "Average_Order",
            2
          )
      })

    output$marketTable =
      renderDT({

        datatable(
          market_summary,
          rownames = FALSE,
          options =
            list(
              pageLength = 5,
              dom = "t",
              scrollX = TRUE
            )
        ) %>%
          formatRound(
            "Average_Order",
            2
          )
      })


    # --------------------------------------------------------
    # PREDICTION
    # --------------------------------------------------------

    output$modelContext =
      renderUI({

        tags$ul(
          tags$li(
            "Model: Reduced Multiple Linear Regression"
          ),
          tags$li(
            "Predictors: Product, Export Market, Day of Week"
          ),
          tags$li(
            paste0(
              "Training RMSE: ",
              round(
                model_train_metrics["RMSE"],
                2
              ),
              " units"
            )
          ),
          tags$li(
            paste0(
              "Test RMSE: ",
              round(
                model_test_metrics["RMSE"],
                2
              ),
              " units"
            )
          ),
          tags$li(
            paste0(
              "Test R-squared: ",
              round(
                model_test_metrics["R2"],
                3
              )
            )
          ),
          tags$li(
            paste0(
              "RMSE improvement over mean baseline: ",
              round(
                rmse_improvement_percent,
                2
              ),
              "%"
            )
          )
        )
      })

    output$marketHoldoutTable =
      renderDT({

        display_data =
          market_holdout_performance %>%
          mutate(
            MAE =
              round(
                MAE,
                2
              ),
            RMSE =
              round(
                RMSE,
                2
              )
          )

        datatable(
          display_data,
          rownames = FALSE,
          options =
            list(
              dom = "t",
              pageLength = 5
            )
        )
      })

    prediction_result =
      eventReactive(
        input$runPrediction,
        {

          new_data =
            data.frame(
              Product_Name =
                factor(
                  input$predProduct,
                  levels =
                    product_levels
                ),
              Export_Market =
                factor(
                  input$predMarket,
                  levels =
                    market_levels
                ),
              Day =
                factor(
                  input$predDay,
                  levels =
                    day_levels
                )
            )

          prediction =
            predict(
              reduced_mlr,
              newdata =
                new_data,
              interval =
                "prediction",
              level =
                0.95
            )

          prediction
        },
        ignoreInit = FALSE
      )


    output$predictionValue =
      renderText({

        prediction =
          prediction_result()

        paste0(
          round(
            prediction[
              1,
              "fit"
            ],
            1
          ),
          " units"
        )
      })

    # Only the displayed range is bounded below by zero.
    # The original model prediction interval remains unchanged.
    output$predictionInterval =
      renderText({

        prediction =
          prediction_result()

        displayed_lower =
          pmax(
            0,
            prediction[1, "lwr"]
          )

        displayed_upper =
          pmax(
            0,
            prediction[1, "upr"]
          )

        paste0(
          sprintf("%.1f", displayed_lower),
          " to ",
          sprintf("%.1f", displayed_upper),
          " units"
        )
      })

    output$predictionIntervalNote =
      renderText({

        prediction =
          prediction_result()

        original_lower =
          as.numeric(prediction[1, "lwr"])

        original_upper =
          as.numeric(prediction[1, "upr"])

        if(original_lower < 0 || original_upper < 0) {
          paste0(
            "Original unmodified interval: ",
            sprintf("%.1f", original_lower),
            " to ",
            sprintf("%.1f", original_upper),
            " units. Negative bounds are displayed as zero because ",
            "order quantities cannot be negative. This is a ",
            "display adjustment, not a recalibrated statistical interval."
          )
        } else {
          "This model-based interval describes uncertainty for one future transaction; its reliability depends on the regression assumptions."
        }
      })


    # --------------------------------------------------------
    # DEMAND MONITORING
    # --------------------------------------------------------

    output$decisionTable =
      renderDT({

        display_data =
          decision_support %>%
          mutate(
            Recent_3_Month_Avg =
              round(
                Recent_3_Month_Avg,
                1
              ),
            Earlier_Current_Regime_Avg =
              round(
                Earlier_Current_Regime_Avg,
                1
              ),
            Recent_to_Historical_Ratio =
              paste0(
                round(
                  Recent_to_Historical_Ratio *
                  100,
                  1
                ),
                "%"
              )
          )

        names(display_data) =
          c(
            "Product",
            "Recent 3-Month Avg",
            "Earlier Regime Avg",
            "Recent / Earlier",
            "Monitoring Status"
          )

        datatable(
          display_data,
          rownames = FALSE,
          options =
            list(
              pageLength = 10,
              dom = "t",
              scrollX = TRUE
            )
        )
      })
  }


# ============================================================
# 9. RUN APPLICATION
# ============================================================

shinyApp(
  ui = ui,
  server = server
)
