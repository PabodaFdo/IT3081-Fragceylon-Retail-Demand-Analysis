# ============================================================
# IT3081 - Statistical Modelling
# Retail Product Demand Analysis at Fragceylon
# TASK 9 - CRITICAL EVALUATION OF TIME SERIES ANALYSIS
# R SCRIPT - TIME-SERIES READINESS + EXPLORATORY DIAGNOSTICS
# ============================================================
#
# IMPORTANT:
# The assignment does NOT require a full time-series model.
#
# This script therefore does NOT force ARIMA/SARIMA onto the data.
#
# The purpose is to:
# 1. Evaluate whether the current Fragceylon history is long and stable
#    enough for dependable time-series forecasting.
# 2. Quantify the available historical span and structural regime change.
# 3. Aggregate Regular Order demand by month and by product.
# 4. Assess trend evidence in the current stable Main Dealer regime.
# 5. Assess whether annual seasonality can be supported by the current data.
# 6. Compare monthly and weekly forecasting data availability.
# 7. Create report-ready tables and charts.
# 8. Provide future ARIMA/SARIMA implementation templates without
#    pretending that the current history is sufficient.
#
# No full production ARIMA/SARIMA model is fitted in this task.
# ============================================================


# ============================================================
# 1. SET WORKING DIRECTORY
# ============================================================

setwd(
  "C:/Users/saths/Desktop/Sliit/3rd year/3rd Year 1st sem/IT3081-SM/Project/IT3081-Fragceylon-Retail-Demand-Analysis"
)

getwd()


# ============================================================
# 2. LOAD REQUIRED LIBRARY
# ============================================================

library(readxl)


# ============================================================
# 3. CREATE OUTPUT FOLDERS
# ============================================================

dir.create(
  "06_Charts/R",
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  "08_Model_Results/R",
  recursive = TRUE,
  showWarnings = FALSE
)


# ============================================================
# 4. LOAD CLEANED REGULAR-ORDER DATA
# ============================================================

regular_file =
  "04_Cleaned_Data/Fragceylon_Regular_Orders.xlsx"

if(!file.exists(regular_file)) {
  stop(
    "Fragceylon_Regular_Orders.xlsx was not found. Run Task 3A first."
  )
}

df =
  read_excel(
    regular_file
  )

df$Date =
  as.Date(
    df$Date
  )

cat(
  "\n============================================================\n"
)

cat(
  "TASK 9 - TIME SERIES READINESS AUDIT\n"
)

cat(
  "============================================================\n"
)

cat(
  "Regular Order rows:",
  nrow(df),
  "\n"
)

cat(
  "Date range:",
  format(min(df$Date)),
  "to",
  format(max(df$Date)),
  "\n"
)

cat(
  "Number of products:",
  length(unique(df$`Product Name`)),
  "\n"
)


# ============================================================
# 5. HISTORICAL SPAN AND REGIME STRUCTURE
# ============================================================

overall_start =
  min(
    df$Date
  )

overall_end =
  max(
    df$Date
  )

overall_days =
  as.integer(
    overall_end -
    overall_start
  ) + 1

overall_years =
  overall_days /
  365.25

REGIME_CHANGE_DATE =
  as.Date("2025-04-01")

pre_regime =
  df[
    df$Date <
      REGIME_CHANGE_DATE,
  ]

current_regime =
  df[
    df$Date >=
      REGIME_CHANGE_DATE,
  ]

current_start =
  min(
    current_regime$Date
  )

current_end =
  max(
    current_regime$Date
  )

current_days =
  as.integer(
    current_end -
    current_start
  ) + 1

current_years =
  current_days /
  365.25

history_summary = data.frame(
  Measure = c(
    "Overall start date",
    "Overall end date",
    "Overall calendar days",
    "Overall approximate years",
    "Pre-April-2025 rows",
    "Current-regime rows",
    "Current-regime start date",
    "Current-regime end date",
    "Current-regime calendar days",
    "Current-regime approximate years"
  ),
  Value = c(
    as.character(overall_start),
    as.character(overall_end),
    overall_days,
    round(overall_years, 4),
    nrow(pre_regime),
    nrow(current_regime),
    as.character(current_start),
    as.character(current_end),
    current_days,
    round(current_years, 4)
  ),
  stringsAsFactors = FALSE
)

cat(
  "\nHistorical span summary:\n"
)

print(
  history_summary
)

write.csv(
  history_summary,
  "08_Model_Results/R/Task9_Historical_Span_Summary.csv",
  row.names = FALSE
)


# ============================================================
# 6. VERIFY STRUCTURAL CHANNEL CHANGE
# ============================================================

regime_channel_table =
  table(
    Period = ifelse(
      df$Date <
        REGIME_CHANGE_DATE,
      "Before 2025-04-01",
      "2025-04-01 onward"
    ),
    Channel = df$`Channel Type`
  )

cat(
  "\nChannel structure by period:\n"
)

print(
  regime_channel_table
)

write.csv(
  as.data.frame.matrix(
    regime_channel_table
  ),
  "08_Model_Results/R/Task9_Regime_Channel_Structure.csv",
  row.names = TRUE
)


# ============================================================
# 7. CREATE MONTHLY TOTAL DEMAND SERIES
# ============================================================

df$Month_Start =
  as.Date(
    paste0(
      format(
        df$Date,
        "%Y-%m"
      ),
      "-01"
    )
  )

monthly_total =
  aggregate(
    Quantity ~ Month_Start,
    data = df,
    FUN = sum
  )

monthly_orders =
  aggregate(
    `Order ID` ~ Month_Start,
    data = df,
    FUN = length
  )

names(
  monthly_orders
)[2] =
  "Order_Lines"

monthly_total =
  merge(
    monthly_total,
    monthly_orders,
    by = "Month_Start",
    all = TRUE
  )

monthly_total$Period =
  ifelse(
    monthly_total$Month_Start <
      REGIME_CHANGE_DATE,
    "Pre-April-2025 structure",
    "Current Main Dealer regime"
  )

monthly_total =
  monthly_total[
    order(
      monthly_total$Month_Start
    ),
  ]

rownames(
  monthly_total
) = NULL

cat(
  "\nMonthly total demand:\n"
)

print(
  monthly_total
)

write.csv(
  monthly_total,
  "08_Model_Results/R/Task9_Monthly_Total_Demand.csv",
  row.names = FALSE
)


# ============================================================
# 8. FLAG PARTIAL CALENDAR MONTHS
# ============================================================
#
# October 2024 begins on the 18th and is therefore a partial month.
# June 2026 is complete through 30 June.
# ============================================================

month_days = function(date_value) {
  start =
    as.Date(
      format(
        date_value,
        "%Y-%m-01"
      )
    )

  next_month =
    seq(
      start,
      by = "month",
      length.out = 2
    )[2]

  as.integer(
    next_month -
    start
  )
}

monthly_coverage =
  do.call(
    rbind,
    lapply(
      monthly_total$Month_Start,
      function(month_start) {
        month_end =
          seq(
            month_start,
            by = "month",
            length.out = 2
          )[2] - 1

        observed_start =
          max(
            month_start,
            overall_start
          )

        observed_end =
          min(
            month_end,
            overall_end
          )

        days_observed =
          as.integer(
            observed_end -
            observed_start
          ) + 1

        total_days =
          month_days(
            month_start
          )

        data.frame(
          Month_Start =
            month_start,
          Calendar_Days =
            total_days,
          Observed_Days =
            days_observed,
          Coverage_Percent =
            100 *
            days_observed /
            total_days,
          Full_Calendar_Month =
            days_observed ==
            total_days,
          stringsAsFactors = FALSE
        )
      }
    )
  )

monthly_coverage$Month_Start =
  as.Date(
    monthly_coverage$Month_Start,
    origin = "1970-01-01"
  )

cat(
  "\nMonthly calendar coverage:\n"
)

print(
  monthly_coverage
)

write.csv(
  monthly_coverage,
  "08_Model_Results/R/Task9_Monthly_Calendar_Coverage.csv",
  row.names = FALSE
)


# ============================================================
# 9. CURRENT-REGIME MONTHLY SERIES
# ============================================================

current_monthly =
  monthly_total[
    monthly_total$Month_Start >=
      REGIME_CHANGE_DATE,
  ]

current_monthly$Time_Index =
  seq_len(
    nrow(
      current_monthly
    )
  )

cat(
  "\nCurrent-regime monthly observations:",
  nrow(current_monthly),
  "\n"
)

cat(
  "Current-regime monthly range:",
  format(min(current_monthly$Month_Start)),
  "to",
  format(max(current_monthly$Month_Start)),
  "\n"
)


# ============================================================
# 10. MONTH-OF-YEAR COVERAGE FOR SEASONALITY
# ============================================================
#
# Reliable annual seasonality typically requires repeated observations
# of the same calendar months across multiple comparable years.
#
# Because the current stable regime runs only Apr-2025 to Jun-2026,
# most month-of-year values appear only once in the current regime.
# ============================================================

current_monthly$Month_Name =
  format(
    current_monthly$Month_Start,
    "%B"
  )

month_of_year_counts =
  as.data.frame(
    table(
      current_monthly$Month_Name
    ),
    stringsAsFactors = FALSE
  )

names(
  month_of_year_counts
) =
  c(
    "Month",
    "Number_of_Current_Regime_Observations"
  )

month_order =
  month.name

month_of_year_counts$Month =
  factor(
    month_of_year_counts$Month,
    levels = month_order
  )

month_of_year_counts =
  month_of_year_counts[
    order(
      month_of_year_counts$Month
    ),
  ]

month_of_year_counts$Month =
  as.character(
    month_of_year_counts$Month
  )

cat(
  "\nCurrent-regime month-of-year replication:\n"
)

print(
  month_of_year_counts
)

write.csv(
  month_of_year_counts,
  "08_Model_Results/R/Task9_Current_Regime_Month_Replication.csv",
  row.names = FALSE
)


# ============================================================
# 11. COUNT DIRECT 12-MONTH COMPARISONS
# ============================================================

lag12_pairs =
  merge(
    current_monthly[
      ,
      c(
        "Month_Start",
        "Quantity"
      )
    ],
    transform(
      current_monthly[
        ,
        c(
          "Month_Start",
          "Quantity"
        )
      ],
      Month_Start =
        seq(
          min(
            current_monthly$Month_Start
          ),
          by = "month",
          length.out =
            nrow(
              current_monthly
            )
        ) + 365
    ),
    by = "Month_Start"
  )

# More robust explicit matching by year-month difference:
lag12_matches =
  data.frame()

for(i in 1:nrow(current_monthly)) {
  target =
    seq(
      current_monthly$Month_Start[i],
      by = "month",
      length.out = 13
    )[13]

  j =
    which(
      current_monthly$Month_Start ==
        target
    )

  if(length(j) == 1) {
    lag12_matches =
      rbind(
        lag12_matches,
        data.frame(
          Earlier_Month =
            current_monthly$Month_Start[i],
          Later_Month =
            current_monthly$Month_Start[j],
          Earlier_Quantity =
            current_monthly$Quantity[i],
          Later_Quantity =
            current_monthly$Quantity[j],
          Difference =
            current_monthly$Quantity[j] -
            current_monthly$Quantity[i],
          stringsAsFactors = FALSE
        )
      )
  }
}

cat(
  "\nNumber of direct 12-month comparisons in current regime:",
  nrow(lag12_matches),
  "\n"
)

print(
  lag12_matches
)

write.csv(
  lag12_matches,
  "08_Model_Results/R/Task9_Current_Regime_Lag12_Comparisons.csv",
  row.names = FALSE
)


# ============================================================
# 12. EXPLORATORY CURRENT-REGIME TREND
# ============================================================
#
# This simple trend regression is descriptive only.
# It is NOT a production forecasting model.
# ============================================================

trend_model =
  lm(
    Quantity ~ Time_Index,
    data = current_monthly
  )

trend_summary =
  summary(
    trend_model
  )

trend_ci =
  confint(
    trend_model,
    "Time_Index",
    level = 0.95
  )

trend_table = data.frame(
  Metric = c(
    "Monthly trend coefficient",
    "Trend standard error",
    "Trend t statistic",
    "Trend p value",
    "Trend 95% CI lower",
    "Trend 95% CI upper",
    "Model R-squared",
    "Adjusted R-squared",
    "Current-regime monthly observations"
  ),
  Value = c(
    coef(
      trend_model
    )["Time_Index"],
    coef(
      trend_summary
    )["Time_Index", "Std. Error"],
    coef(
      trend_summary
    )["Time_Index", "t value"],
    coef(
      trend_summary
    )["Time_Index", "Pr(>|t|)"],
    trend_ci[1],
    trend_ci[2],
    trend_summary$r.squared,
    trend_summary$adj.r.squared,
    nrow(current_monthly)
  ),
  stringsAsFactors = FALSE
)

cat(
  "\nExploratory current-regime monthly trend:\n"
)

print(
  trend_table
)

write.csv(
  trend_table,
  "08_Model_Results/R/Task9_Current_Regime_Trend_Assessment.csv",
  row.names = FALSE
)


# ============================================================
# 13. AUTOCORRELATION READINESS AUDIT
# ============================================================
#
# With only 15 monthly observations in the current stable regime,
# ACF values are highly uncertain. We calculate them only as an
# exploratory diagnostic and do NOT use them to justify a production
# ARIMA/SARIMA model.
# ============================================================

max_lag =
  min(
    12,
    nrow(current_monthly) - 1
  )

acf_result =
  acf(
    current_monthly$Quantity,
    lag.max = max_lag,
    plot = FALSE
  )

acf_table = data.frame(
  Lag =
    as.numeric(
      acf_result$lag
    ),
  Autocorrelation =
    as.numeric(
      acf_result$acf
    ),
  stringsAsFactors = FALSE
)

cat(
  "\nExploratory monthly autocorrelation table:\n"
)

print(
  acf_table
)

write.csv(
  acf_table,
  "08_Model_Results/R/Task9_Current_Regime_ACF.csv",
  row.names = FALSE
)


# ============================================================
# 14. PRODUCT-LEVEL MONTHLY DEMAND
# ============================================================

product_monthly =
  aggregate(
    Quantity ~
      Month_Start +
      `Product Name`,
    data = df,
    FUN = sum
  )

product_monthly =
  product_monthly[
    order(
      product_monthly$`Product Name`,
      product_monthly$Month_Start
    ),
  ]

rownames(
  product_monthly
) = NULL

write.csv(
  product_monthly,
  "08_Model_Results/R/Task9_Product_Monthly_Demand.csv",
  row.names = FALSE
)


# ============================================================
# 15. CURRENT-REGIME PRODUCT MONTH COVERAGE
# ============================================================

current_product_monthly =
  product_monthly[
    product_monthly$Month_Start >=
      REGIME_CHANGE_DATE,
  ]

product_coverage =
  aggregate(
    Month_Start ~
      `Product Name`,
    data =
      current_product_monthly,
    FUN = length
  )

names(
  product_coverage
)[2] =
  "Observed_Current_Regime_Months"

cat(
  "\nProduct-level current-regime monthly coverage:\n"
)

print(
  product_coverage
)

write.csv(
  product_coverage,
  "08_Model_Results/R/Task9_Product_Monthly_Coverage.csv",
  row.names = FALSE
)


# ============================================================
# 16. WEEKLY AGGREGATION READINESS
# ============================================================
#
# Weekly aggregation provides more observations than monthly
# aggregation, but may be noisier and may contain irregular order timing.
#
# Week starts on Monday.
# ============================================================

weekday_num =
  as.integer(
    format(
      df$Date,
      "%u"
    )
  )

df$Week_Start =
  df$Date -
  (
    weekday_num -
      1
  )

weekly_total =
  aggregate(
    Quantity ~ Week_Start,
    data = df,
    FUN = sum
  )

current_weekly =
  weekly_total[
    weekly_total$Week_Start >=
      REGIME_CHANGE_DATE,
  ]

frequency_comparison = data.frame(
  Aggregation = c(
    "Monthly - full history",
    "Monthly - current regime",
    "Weekly - full history",
    "Weekly - current regime"
  ),
  Observations = c(
    nrow(monthly_total),
    nrow(current_monthly),
    nrow(weekly_total),
    nrow(current_weekly)
  ),
  Approx_Seasonal_Period = c(
    12,
    12,
    52,
    52
  ),
  Approx_Number_of_Seasonal_Cycles = c(
    nrow(monthly_total) / 12,
    nrow(current_monthly) / 12,
    nrow(weekly_total) / 52,
    nrow(current_weekly) / 52
  ),
  stringsAsFactors = FALSE
)

cat(
  "\nAggregation-frequency comparison:\n"
)

print(
  frequency_comparison
)

write.csv(
  frequency_comparison,
  "08_Model_Results/R/Task9_Aggregation_Frequency_Comparison.csv",
  row.names = FALSE
)


# ============================================================
# 17. TIME-SERIES METHOD APPLICABILITY ASSESSMENT
# ============================================================

time_series_applicability = data.frame(
  Area = c(
    "Trend analysis",
    "Seasonal analysis",
    "Forecasting",
    "ARIMA",
    "SARIMA",
    "Product-level forecasting",
    "Weekly aggregation",
    "Monthly aggregation"
  ),
  Current_Assessment = c(
    "Possible descriptively, but trend estimates should be treated cautiously because the current stable regime contains only 15 monthly observations.",
    "Weakly supported for annual seasonality because the current stable regime covers only about 1.25 years and has very few repeated calendar months.",
    "Short-horizon exploratory forecasting may be possible, but dependable operational forecasting requires a longer stable history.",
    "Conceptually applicable to autocorrelated demand after sufficient stable history is collected and stationarity is assessed.",
    "Potentially useful if clear annual seasonality emerges, but current history is too short for dependable seasonal ARIMA estimation.",
    "Potentially valuable because each perfume may have different demand dynamics, but each product still has limited stable historical coverage.",
    "Provides more observations, but order timing may be noisy and a 52-week seasonal cycle still requires multiple comparable years.",
    "Easy to communicate and useful for management planning, but only 15 stable-regime monthly observations are currently available."
  ),
  Key_Limitation = c(
    "Short stable time span and structural business change.",
    "Insufficient repeated annual cycles.",
    "Forecast uncertainty would be high and structural change may not persist.",
    "Model identification and diagnostics are unreliable with very short series.",
    "Seasonal parameters need repeated comparable seasonal cycles.",
    "Smaller effective sample within each individual product series.",
    "Higher short-term variability and irregular ordering behaviour.",
    "Very small sample size for time-series model estimation."
  ),
  stringsAsFactors = FALSE
)

cat(
  "\n============================================================\n"
)

cat(
  "TIME SERIES METHOD APPLICABILITY\n"
)

cat(
  "============================================================\n"
)

print(
  time_series_applicability
)

write.csv(
  time_series_applicability,
  "08_Model_Results/R/Task9_Time_Series_Applicability.csv",
  row.names = FALSE
)


# ============================================================
# 18. FUTURE DATA REQUIREMENTS
# ============================================================

future_requirements = data.frame(
  Requirement = c(
    "Longer stable history",
    "Consistent channel structure",
    "Complete calendar coverage",
    "Stock-out indicator",
    "Inventory on hand",
    "Promotion information",
    "Price history",
    "Product-level series",
    "Market-level series",
    "Forecast evaluation protocol"
  ),
  Why_Important = c(
    "Multiple comparable annual cycles are needed to estimate trend and seasonality more reliably.",
    "Structural changes can be mistaken for trend or seasonality.",
    "Missing or partial periods can distort time-dependent patterns.",
    "Observed sales may understate true demand when products are unavailable.",
    "Allows sales forecasts to be linked to inventory decisions.",
    "Promotions can create temporary demand spikes that should not be mistaken for recurring seasonality.",
    "Price changes may shift demand and should be modelled when relevant.",
    "Different perfumes may have different trends and seasonal patterns.",
    "Local and export demand may behave differently.",
    "Future forecasts should be tested using rolling-origin or future holdout evaluation rather than only in-sample fit."
  ),
  stringsAsFactors = FALSE
)

print(
  future_requirements
)

write.csv(
  future_requirements,
  "08_Model_Results/R/Task9_Future_Time_Series_Data_Requirements.csv",
  row.names = FALSE
)


# ============================================================
# 19. CHART 1 - FULL MONTHLY DEMAND WITH REGIME CHANGE
# ============================================================

png(
  "06_Charts/R/Task9_01_Monthly_Demand_Regime_Change.png",
  width = 1100,
  height = 700
)

plot(
  monthly_total$Month_Start,
  monthly_total$Quantity,
  type = "b",
  pch = 16,
  main = "Monthly Regular-Order Demand and Structural Regime Change",
  xlab = "Month",
  ylab = "Quantity"
)

abline(
  v =
    as.numeric(
      REGIME_CHANGE_DATE
    ),
  lty = 2
)

dev.off()


# ============================================================
# 20. CHART 2 - CURRENT-REGIME MONTHLY TREND
# ============================================================

png(
  "06_Charts/R/Task9_02_Current_Regime_Monthly_Trend.png",
  width = 1100,
  height = 700
)

plot(
  current_monthly$Month_Start,
  current_monthly$Quantity,
  type = "b",
  pch = 16,
  main = "Current-Regime Monthly Demand",
  xlab = "Month",
  ylab = "Quantity"
)

lines(
  current_monthly$Month_Start,
  fitted(
    trend_model
  ),
  lty = 2
)

dev.off()


# ============================================================
# 21. CHART 3 - PRODUCT MONTHLY DEMAND
# ============================================================

products =
  unique(
    product_monthly$`Product Name`
  )

png(
  "06_Charts/R/Task9_03_Product_Monthly_Demand.png",
  width = 1200,
  height = 800
)

plot(
  range(
    product_monthly$Month_Start
  ),
  range(
    product_monthly$Quantity
  ),
  type = "n",
  main = "Monthly Demand by Perfume",
  xlab = "Month",
  ylab = "Quantity"
)

pch_values =
  c(
    16,
    17,
    15,
    18
  )

lty_values =
  c(
    1,
    2,
    3,
    4
  )

for(i in seq_along(products)) {
  temp =
    product_monthly[
      product_monthly$`Product Name` ==
        products[i],
    ]

  lines(
    temp$Month_Start,
    temp$Quantity,
    type = "b",
    pch =
      pch_values[i],
    lty =
      lty_values[i]
  )
}

legend(
  "topleft",
  legend = products,
  pch = pch_values[
    seq_along(products)
  ],
  lty = lty_values[
    seq_along(products)
  ],
  cex = 0.8
)

dev.off()


# ============================================================
# 22. CHART 4 - EXPLORATORY CURRENT-REGIME ACF
# ============================================================

png(
  "06_Charts/R/Task9_04_Current_Regime_ACF.png",
  width = 1000,
  height = 700
)

acf(
  current_monthly$Quantity,
  lag.max = max_lag,
  main = "Exploratory ACF of Current-Regime Monthly Demand"
)

dev.off()


# ============================================================
# 23. FUTURE ARIMA / SARIMA CODE TEMPLATES
# ============================================================
#
# DO NOT RUN THESE UNTIL A LONGER, STABLE TIME SERIES EXISTS.
#
# Example future monthly series:
#
# monthly_ts = ts(
#   future_monthly_data$Quantity,
#   frequency = 12,
#   start = c(2027, 1)
# )
#
# Basic ARIMA example:
#
# fit_arima = arima(
#   monthly_ts,
#   order = c(p, d, q)
# )
#
# Seasonal ARIMA example:
#
# fit_sarima = arima(
#   monthly_ts,
#   order = c(p, d, q),
#   seasonal = list(
#     order = c(P, D, Q),
#     period = 12
#   )
# )
#
# Diagnostics should include:
#
# acf(residuals(fit_arima))
# Box.test(
#   residuals(fit_arima),
#   lag = appropriate_lag,
#   type = "Ljung-Box"
# )
#
# Forecasts must be evaluated using future observations / rolling origin.
# ============================================================


# ============================================================
# 24. REPORT-READY SUMMARY
# ============================================================

sink(
  "08_Model_Results/R/Task9_Report_Ready_Summary.txt"
)

cat(
  "TASK 9 - CRITICAL EVALUATION OF TIME SERIES ANALYSIS\n\n"
)

cat(
  "CURRENT DATA HISTORY\n"
)

cat(
  "The full Regular Order history runs from ",
  format(overall_start),
  " to ",
  format(overall_end),
  ", approximately ",
  round(overall_years, 2),
  " years. However, a major structural channel change occurred on 2025-04-01. The current stable Main Dealer regime therefore contains only ",
  nrow(current_monthly),
  " monthly observations from ",
  format(min(current_monthly$Month_Start)),
  " to ",
  format(max(current_monthly$Month_Start)),
  ".\n\n",
  sep = ""
)

cat(
  "TREND\n"
)

cat(
  "A simple current-regime monthly linear trend was estimated only as a descriptive diagnostic. It should not be treated as a production forecasting model because the series is short.\n\n"
)

cat(
  "Monthly trend coefficient: ",
  round(
    coef(
      trend_model
    )["Time_Index"],
    4
  ),
  "\n",
  sep = ""
)

cat(
  "Trend p-value: ",
  format(
    coef(
      trend_summary
    )["Time_Index", "Pr(>|t|)"],
    digits = 4
  ),
  "\n\n",
  sep = ""
)

cat(
  "SEASONALITY\n"
)

cat(
  "The current stable regime covers only about ",
  round(current_years, 2),
  " years. This provides too few repeated annual cycles to support dependable conclusions about yearly seasonality. There are only ",
  nrow(lag12_matches),
  " direct same-month comparisons separated by 12 months in the current regime.\n\n",
  sep = ""
)

cat(
  "ARIMA / SARIMA\n"
)

cat(
  "ARIMA and SARIMA are conceptually relevant for future demand forecasting, but a dependable model should not be forced onto the current short, structurally changing history. SARIMA is especially difficult to justify because annual seasonal parameters require repeated comparable seasonal cycles.\n\n"
)

cat(
  "BUSINESS APPLICATION\n"
)

cat(
  "With a longer stable history, time-series forecasting could support product-level demand planning, seasonal preparation, purchasing schedules, dealer planning and short-horizon inventory decisions. Forecasts should be generated separately by perfume where sufficient data exist.\n\n"
)

cat(
  "DATA COLLECTION RECOMMENDATION\n"
)

cat(
  "Fragceylon should continue collecting consistent product-level sales data and add stock availability, stock-out indicators, promotion data, price history and inventory information. Future forecast validation should use rolling-origin or future holdout evaluation.\n\n"
)

cat(
  "LITERATURE REQUIREMENT\n"
)

cat(
  "The final Task 9 report section must support the discussion using recent verified literature. No references are fabricated by this R script.\n"
)

sink()


# ============================================================
# 25. FINAL VALIDATION
# ============================================================

stopifnot(
  nrow(df) == 3116,
  nrow(current_regime) == 2014,
  nrow(current_monthly) == 15,
  min(current_monthly$Month_Start) ==
    as.Date("2025-04-01"),
  max(current_monthly$Month_Start) ==
    as.Date("2026-06-01"),
  all(
    current_regime$`Channel Type` ==
      "Main Dealer"
  ),
  nrow(product_coverage) == 4,
  all(
    product_coverage$Observed_Current_Regime_Months ==
      15
  ),
  nrow(lag12_matches) == 3,
  is.finite(
    coef(
      trend_model
    )["Time_Index"]
  ),
  all(
    is.finite(
      acf_table$Autocorrelation
    )
  )
)

cat(
  "\n============================================================\n"
)

print(
  "All Task 9 time-series evaluation validation checks passed."
)

cat(
  "============================================================\n"
)


# ============================================================
# 26. LIST CREATED TASK 9 FILES
# ============================================================

cat(
  "\nTask 9 result files:\n"
)

print(
  list.files(
    "08_Model_Results/R",
    pattern = "^Task9",
    full.names = TRUE
  )
)

cat(
  "\nTask 9 chart files:\n"
)

print(
  list.files(
    "06_Charts/R",
    pattern = "^Task9",
    full.names = TRUE
  )
)


# ============================================================
# 27. COMPLETION MESSAGE
# ============================================================

cat(
  "\n============================================================\n"
)

cat(
  "TASK 9 R SCRIPT COMPLETE\n"
)

cat(
  "============================================================\n"
)

cat(
  "Time-series readiness, trend, seasonality and ARIMA/SARIMA applicability were evaluated.\n"
)

cat(
  "No production ARIMA/SARIMA model was forced onto the short and structurally changing history.\n"
)

cat(
  "Next step: check the outputs, then create Task9_Time_Series_Evaluation.Rmd with critical interpretation and recent literature support.\n"
)
