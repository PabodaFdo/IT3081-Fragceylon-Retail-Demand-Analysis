# ============================================================================
# IT3081 - Statistical Modelling
# Fragceylon Retail Product Demand Analysis
# TASK 5 - Predictive Statistical Modelling
# LAB-SHEET STYLE FINAL R VERSION
#
# This R version follows the completed Task5_Predictive_Modelling.ipynb.
# It uses the cleaned Regular Orders dataset created in Task 3A.
#
# Main modelling approach:
#   1. Predictor audit
#   2. Current-regime modelling population
#   3. Chronological train/test split
#   4. Training-mean baseline
#   5. Initial Multiple Linear Regression
#   6. Regression diagnostics
#   7. Training-only chronological specification comparison
#   8. Final reduced MLR
#   9. HC3 robust standard errors
#   10. Final unseen-test evaluation
#
# If required, install packages once:
# install.packages(c("readxl", "car", "lmtest"))
# ============================================================================


# ----------------------------------------------------------------------------
# Step 1: Set the Working Directory
# ----------------------------------------------------------------------------

setwd("C:/Users/saths/Desktop/Sliit/3rd year/3rd Year 1st sem/IT3081-SM/Project/IT3081-Fragceylon-Retail-Demand-Analysis")

getwd()


# ----------------------------------------------------------------------------
# Step 2: Load Required Libraries
# ----------------------------------------------------------------------------

library(readxl)
library(car)
library(lmtest)


# ----------------------------------------------------------------------------
# Step 3: Create Output Folders
# ----------------------------------------------------------------------------

dir.create("06_Charts", showWarnings = FALSE)
dir.create("08_Model_Results", showWarnings = FALSE)
dir.create("08_Model_Results/R", showWarnings = FALSE)


# ----------------------------------------------------------------------------
# Step 4: Load the Cleaned Regular Orders Dataset
# ----------------------------------------------------------------------------

regular_file = "04_Cleaned_Data/Fragceylon_Regular_Orders.xlsx"

if(!file.exists(regular_file)) {
  stop("Fragceylon_Regular_Orders.xlsx was not found. Run Task 3A first.")
}

df = read_excel(regular_file)

df$Date = as.Date(df$Date)


# ----------------------------------------------------------------------------
# Step 5: Examine and Verify the Modelling Dataset
# ----------------------------------------------------------------------------

head(df)
str(df)
dim(df)
nrow(df)
ncol(df)
colnames(df)
summary(df$Quantity)

print(table(df$`Product Name`))
print(table(df$`Export Market`))
print(table(df$`Channel Type`))
print(table(df$Sales_Regime))

stopifnot(
  nrow(df) == 3116,
  ncol(df) == 22,
  sum(is.na(df)) == 0,
  all(df$`Line Type` == "Regular Order"),
  sum(df$Quantity) == 705810
)

print("Task 5 modelling-data checks passed.")


# ----------------------------------------------------------------------------
# Step 6: Predictor Audit
# ----------------------------------------------------------------------------
# Quantity is the response variable.
# Total Value is excluded because it is directly calculated from Quantity.
# Product Code is redundant with Product Name.
# Buyer is excluded from the primary model to reduce customer memorization.
# Channel Type and Sales Regime become constant in the current regime.

predictor_audit = data.frame(
  Variable = c(
    "Quantity",
    "Order ID",
    "Date",
    "Product Name",
    "Product Code",
    "Channel Type",
    "Buyer",
    "Line Type",
    "Unit Price (LKR)",
    "Total Value (LKR)",
    "Export Market",
    "Outlier_IQR_Flag",
    "Year",
    "Month",
    "Month_Number",
    "Quarter",
    "Day_of_Week",
    "Year_Month",
    "Time_Trend",
    "Sales_Regime",
    "Market_Group",
    "Is_Regular_Order"
  ),
  Decision = c(
    "TARGET",
    "EXCLUDE",
    "EXCLUDE DIRECTLY",
    "PRIMARY CANDIDATE",
    "EXCLUDE",
    "CONDITIONAL",
    "EXCLUDE FROM PRIMARY",
    "EXCLUDE",
    "EXCLUDE FROM PRIMARY",
    "EXCLUDE",
    "PRIMARY CANDIDATE",
    "EXCLUDE",
    "EXCLUDE FROM PRIMARY",
    "PRIMARY CANDIDATE",
    "EXCLUDE",
    "EXCLUDE FROM PRIMARY",
    "PRIMARY CANDIDATE",
    "EXCLUDE FROM PRIMARY",
    "PRIMARY CANDIDATE",
    "CONDITIONAL",
    "EXCLUDE",
    "EXCLUDE"
  ),
  Reason = c(
    "Response variable representing Regular Order demand.",
    "Unique transaction identifier with no substantive predictive meaning.",
    "Use selected derived time variables rather than raw Date.",
    "Known at prediction time and directly interpretable.",
    "Redundant with Product Name.",
    "Structurally linked to the April 2025 business change.",
    "High-cardinality customer identifier; may reduce generalizability.",
    "Constant after restricting analysis to Regular Orders.",
    "Commercial variable partly determined by quantity, channel and month; excluded from primary model.",
    "Directly derived from Quantity and Unit Price; target leakage.",
    "Potentially known at prediction time but structurally linked to the Japan buyer pattern.",
    "Derived from Quantity; not a valid predictor.",
    "Overlaps with other calendar variables.",
    "Calendar candidate known at prediction time.",
    "Redundant representation of Month.",
    "Overlaps with Month.",
    "Calendar candidate known at prediction time.",
    "High-detail time category; excluded from primary specification.",
    "Numeric temporal candidate.",
    "Structurally linked to Channel Type and constant in current regime.",
    "Redundant with Export Market.",
    "Constant in Regular Orders dataset."
  ),
  stringsAsFactors = FALSE
)

print(predictor_audit)

write.csv(
  predictor_audit,
  "08_Model_Results/R/Task5_Predictor_Audit.csv",
  row.names = FALSE
)


# ----------------------------------------------------------------------------
# Step 7: Define the Current-Regime Modelling Population
# ----------------------------------------------------------------------------
# Prediction objective:
# Estimate future Regular Order Quantity under Fragceylon's current business
# structure. Therefore the primary modelling population begins on 2025-04-01.

current_regime_start = as.Date("2025-04-01")

current_df = df[df$Date >= current_regime_start, ]
current_df = current_df[order(current_df$Date, current_df$`Order ID`), ]
rownames(current_df) = NULL

cat("\nCURRENT-REGIME MODELLING POPULATION\n")
cat("Rows:", nrow(current_df), "\n")
cat("Columns:", ncol(current_df), "\n")
cat("Start date:", format(min(current_df$Date)), "\n")
cat("End date:", format(max(current_df$Date)), "\n")

print(table(current_df$`Channel Type`))
print(table(current_df$Sales_Regime))

stopifnot(
  nrow(current_df) == 2014,
  length(unique(current_df$`Channel Type`)) == 1,
  unique(current_df$`Channel Type`) == "Main Dealer",
  length(unique(current_df$Sales_Regime)) == 1,
  unique(current_df$Sales_Regime) == "Post-April-2025"
)


# ----------------------------------------------------------------------------
# Step 8: Set Factor Levels for Reproducible Reference Categories
# ----------------------------------------------------------------------------

product_levels = c(
  "Dark Matrix",
  "Kennedy",
  "Mesmerose",
  "Secret Ambrosia"
)

market_levels = c(
  "Local",
  "Japan"
)

month_levels = c(
  "April",
  "August",
  "December",
  "February",
  "January",
  "July",
  "June",
  "March",
  "May",
  "November",
  "October",
  "September"
)

day_levels = c(
  "Monday",
  "Friday",
  "Saturday",
  "Sunday",
  "Thursday",
  "Tuesday",
  "Wednesday"
)

current_df$`Product Name` = factor(
  current_df$`Product Name`,
  levels = product_levels
)

current_df$`Export Market` = factor(
  current_df$`Export Market`,
  levels = market_levels
)

current_df$Month = factor(
  current_df$Month,
  levels = month_levels
)

current_df$Day_of_Week = factor(
  current_df$Day_of_Week,
  levels = day_levels
)


# ----------------------------------------------------------------------------
# Step 9: Monthly Audit of the Current-Regime Data
# ----------------------------------------------------------------------------

current_months = sort(unique(current_df$Year_Month))

monthly_audit = data.frame()

for(month_value in current_months) {
  temp = current_df[current_df$Year_Month == month_value, ]
  monthly_audit = rbind(
    monthly_audit,
    data.frame(
      Year_Month = month_value,
      Orders = nrow(temp),
      Mean_Quantity = mean(temp$Quantity),
      Median_Quantity = median(temp$Quantity),
      SD_Quantity = sd(temp$Quantity),
      Min_Quantity = min(temp$Quantity),
      Max_Quantity = max(temp$Quantity),
      Products = length(unique(temp$`Product Name`)),
      Markets = length(unique(temp$`Export Market`)),
      Buyers = length(unique(temp$Buyer))
    )
  )
}

print(monthly_audit)

write.csv(
  monthly_audit,
  "08_Model_Results/R/Task5_Current_Regime_Monthly_Audit.csv",
  row.names = FALSE
)


# ----------------------------------------------------------------------------
# Step 10: Final Chronological Train-Test Split
# ----------------------------------------------------------------------------
# Training: 2025-04-01 to 2026-03-31
# Testing : 2026-04-01 to 2026-06-30
# The final test set remains chronologically later than all training records.

TEST_START_DATE = as.Date("2026-04-01")

train_df = current_df[current_df$Date < TEST_START_DATE, ]
test_df = current_df[current_df$Date >= TEST_START_DATE, ]

train_df = train_df[order(train_df$Date, train_df$`Order ID`), ]
test_df = test_df[order(test_df$Date, test_df$`Order ID`), ]

rownames(train_df) = NULL
rownames(test_df) = NULL

cat("\nFINAL CHRONOLOGICAL TRAIN-TEST SPLIT\n")
cat("Training rows:", nrow(train_df), "\n")
cat("Training dates:", format(min(train_df$Date)), "to", format(max(train_df$Date)), "\n")
cat("Testing rows:", nrow(test_df), "\n")
cat("Testing dates:", format(min(test_df$Date)), "to", format(max(test_df$Date)), "\n")
cat("Test proportion:", round(nrow(test_df) / nrow(current_df) * 100, 2), "%\n")

print(summary(train_df$Quantity))
print(summary(test_df$Quantity))

print(table(test_df$`Product Name`))
print(table(test_df$`Export Market`))

stopifnot(
  nrow(train_df) == 1599,
  nrow(test_df) == 415,
  min(train_df$Date) == as.Date("2025-04-01"),
  max(train_df$Date) == as.Date("2026-03-31"),
  min(test_df$Date) == as.Date("2026-04-01"),
  max(test_df$Date) == as.Date("2026-06-30"),
  max(train_df$Date) < min(test_df$Date),
  sum(is.na(train_df)) == 0,
  sum(is.na(test_df)) == 0
)

split_summary = data.frame(
  Dataset = c("Training", "Testing"),
  Rows = c(nrow(train_df), nrow(test_df)),
  Start_Date = c(as.character(min(train_df$Date)), as.character(min(test_df$Date))),
  End_Date = c(as.character(max(train_df$Date)), as.character(max(test_df$Date))),
  Mean_Quantity = c(mean(train_df$Quantity), mean(test_df$Quantity)),
  Median_Quantity = c(median(train_df$Quantity), median(test_df$Quantity)),
  SD_Quantity = c(sd(train_df$Quantity), sd(test_df$Quantity)),
  Min_Quantity = c(min(train_df$Quantity), min(test_df$Quantity)),
  Max_Quantity = c(max(train_df$Quantity), max(test_df$Quantity))
)

print(split_summary)

write.csv(
  split_summary,
  "08_Model_Results/R/Task5_Train_Test_Summary.csv",
  row.names = FALSE
)


# ----------------------------------------------------------------------------
# Step 11: Regression Performance Function
# ----------------------------------------------------------------------------

regression_metrics = function(actual, predicted) {
  mae = mean(abs(actual - predicted))
  rmse = sqrt(mean((actual - predicted)^2))
  r2 = 1 - sum((actual - predicted)^2) / sum((actual - mean(actual))^2)
  return(c(MAE = mae, RMSE = rmse, R2 = r2))
}


# ----------------------------------------------------------------------------
# Step 12: Training-Mean Baseline Model
# ----------------------------------------------------------------------------

baseline_value = mean(train_df$Quantity)

baseline_train_pred = rep(baseline_value, nrow(train_df))
baseline_test_pred = rep(baseline_value, nrow(test_df))

baseline_train_metrics = regression_metrics(
  train_df$Quantity,
  baseline_train_pred
)

baseline_test_metrics = regression_metrics(
  test_df$Quantity,
  baseline_test_pred
)

cat("\nBASELINE MODEL\n")
cat("Training-set mean Quantity:", baseline_value, "\n")
print(baseline_train_metrics)
print(baseline_test_metrics)

baseline_performance = data.frame(
  Dataset = c("Training", "Testing"),
  MAE = c(baseline_train_metrics["MAE"], baseline_test_metrics["MAE"]),
  RMSE = c(baseline_train_metrics["RMSE"], baseline_test_metrics["RMSE"]),
  R2 = c(baseline_train_metrics["R2"], baseline_test_metrics["R2"])
)

print(baseline_performance)

write.csv(
  baseline_performance,
  "08_Model_Results/R/Task5_Baseline_Performance.csv",
  row.names = FALSE
)


# ----------------------------------------------------------------------------
# Step 13: Initial Full Multiple Linear Regression
# ----------------------------------------------------------------------------
# Predictors:
# Product Name + Export Market + Month + Day of Week + Time Trend

training_time_mean = mean(train_df$Time_Trend)

train_df$Time_Trend_Centered = train_df$Time_Trend - training_time_mean
test_df$Time_Trend_Centered = test_df$Time_Trend - training_time_mean

cat("\nTraining Time_Trend mean:", training_time_mean, "\n")
cat(
  "Centered training range:",
  min(train_df$Time_Trend_Centered),
  "to",
  max(train_df$Time_Trend_Centered),
  "\n"
)
cat(
  "Centered testing range:",
  min(test_df$Time_Trend_Centered),
  "to",
  max(test_df$Time_Trend_Centered),
  "\n"
)

initial_mlr = lm(
  Quantity ~ `Product Name` + `Export Market` + Month + Day_of_Week + Time_Trend_Centered,
  data = train_df
)

print(summary(initial_mlr))

# Statsmodels-style AIC/BIC used in the original notebook.
initial_loglik = as.numeric(logLik(initial_mlr))
initial_k = length(coef(initial_mlr))
initial_n = nobs(initial_mlr)

initial_aic = -2 * initial_loglik + 2 * initial_k
initial_bic = -2 * initial_loglik + log(initial_n) * initial_k

cat("Initial MLR R-squared:", summary(initial_mlr)$r.squared, "\n")
cat("Initial MLR Adjusted R-squared:", summary(initial_mlr)$adj.r.squared, "\n")
cat("Initial MLR AIC:", initial_aic, "\n")
cat("Initial MLR BIC:", initial_bic, "\n")

initial_ci = confint(initial_mlr)
initial_coef_summary = summary(initial_mlr)$coefficients

initial_coef_table = data.frame(
  Variable = rownames(initial_coef_summary),
  Coefficient = initial_coef_summary[, "Estimate"],
  Std_Error = initial_coef_summary[, "Std. Error"],
  t_value = initial_coef_summary[, "t value"],
  p_value = initial_coef_summary[, "Pr(>|t|)"],
  CI_2.5 = initial_ci[, 1],
  CI_97.5 = initial_ci[, 2],
  row.names = NULL
)

print(initial_coef_table)

write.csv(
  initial_coef_table,
  "08_Model_Results/R/Task5_Initial_Full_MLR_Coefficients.csv",
  row.names = FALSE
)


# ----------------------------------------------------------------------------
# Step 14: Initial Full MLR Predictions and Performance
# ----------------------------------------------------------------------------

initial_train_pred = predict(initial_mlr, newdata = train_df)
initial_test_pred = predict(initial_mlr, newdata = test_df)

initial_train_metrics = regression_metrics(
  train_df$Quantity,
  initial_train_pred
)

initial_test_metrics = regression_metrics(
  test_df$Quantity,
  initial_test_pred
)

initial_performance = data.frame(
  Dataset = c("Training", "Testing"),
  MAE = c(initial_train_metrics["MAE"], initial_test_metrics["MAE"]),
  RMSE = c(initial_train_metrics["RMSE"], initial_test_metrics["RMSE"]),
  R2 = c(initial_train_metrics["R2"], initial_test_metrics["R2"])
)

print(initial_performance)

write.csv(
  initial_performance,
  "08_Model_Results/R/Task5_Initial_Full_MLR_Performance.csv",
  row.names = FALSE
)

cat(
  "Initial test MAE improvement over baseline (%):",
  (baseline_test_metrics["MAE"] - initial_test_metrics["MAE"]) /
    baseline_test_metrics["MAE"] * 100,
  "\n"
)

cat(
  "Initial test RMSE improvement over baseline (%):",
  (baseline_test_metrics["RMSE"] - initial_test_metrics["RMSE"]) /
    baseline_test_metrics["RMSE"] * 100,
  "\n"
)


# ----------------------------------------------------------------------------
# Step 15: Helper Function for Individual-Dummy VIF
# ----------------------------------------------------------------------------
# car::vif() reports GVIF for multi-level factors.
# The original notebook checked VIF for each encoded design-matrix column,
# so this function reproduces that style.

calculate_column_vif = function(model) {
  X = model.matrix(model)
  X = X[, colnames(X) != "(Intercept)", drop = FALSE]

  vif_values = numeric(ncol(X))

  for(i in 1:ncol(X)) {
    y_col = X[, i]
    x_other = X[, -i, drop = FALSE]

    temp_model = lm(y_col ~ x_other)
    r2_temp = summary(temp_model)$r.squared
    vif_values[i] = 1 / (1 - r2_temp)
  }

  result = data.frame(
    Variable = colnames(X),
    VIF = vif_values,
    stringsAsFactors = FALSE
  )

  result = result[order(result$VIF, decreasing = TRUE), ]
  rownames(result) = NULL

  return(result)
}


# ----------------------------------------------------------------------------
# Step 16: Initial Full MLR Diagnostics
# ----------------------------------------------------------------------------

initial_residuals = residuals(initial_mlr)
initial_fitted = fitted(initial_mlr)

# Residual skewness and kurtosis
res_mean = mean(initial_residuals)
res_sd = sd(initial_residuals)
res_n = length(initial_residuals)

res_skew =
  res_n / ((res_n - 1) * (res_n - 2)) *
  sum(((initial_residuals - res_mean) / res_sd)^3)

res_excess_kurt =
  (
    res_n * (res_n + 1) /
      ((res_n - 1) * (res_n - 2) * (res_n - 3))
  ) *
  sum(((initial_residuals - res_mean) / res_sd)^4) -
  3 * (res_n - 1)^2 /
  ((res_n - 2) * (res_n - 3))

# Jarque-Bera statistic using population-style moments
jb_skew = mean((initial_residuals - mean(initial_residuals))^3) /
  mean((initial_residuals - mean(initial_residuals))^2)^(3/2)

jb_kurt = mean((initial_residuals - mean(initial_residuals))^4) /
  mean((initial_residuals - mean(initial_residuals))^2)^2

jb_stat = res_n / 6 * (jb_skew^2 + ((jb_kurt - 3)^2) / 4)
jb_p = pchisq(jb_stat, df = 2, lower.tail = FALSE)

# Breusch-Pagan and Durbin-Watson
bp_initial = bptest(initial_mlr)
dw_initial = dwtest(initial_mlr)

cat("\nINITIAL MLR RESIDUAL DIAGNOSTICS\n")
cat("Residual mean:", mean(initial_residuals), "\n")
cat("Residual skewness:", res_skew, "\n")
cat("Residual excess kurtosis:", res_excess_kurt, "\n")
cat("Jarque-Bera statistic:", jb_stat, "\n")
cat("Jarque-Bera p-value:", jb_p, "\n")
print(bp_initial)
print(dw_initial)

initial_vif = calculate_column_vif(initial_mlr)
print(initial_vif)

write.csv(
  initial_vif,
  "08_Model_Results/R/Task5_Initial_Full_MLR_VIF.csv",
  row.names = FALSE
)


# ----------------------------------------------------------------------------
# Step 17: Influence Diagnostics
# ----------------------------------------------------------------------------

cook_values = cooks.distance(initial_mlr)
leverage_values = hatvalues(initial_mlr)
studentized_values = rstudent(initial_mlr)

n_initial = nobs(initial_mlr)
p_initial = length(coef(initial_mlr))

cook_threshold = 4 / n_initial
leverage_threshold = 2 * p_initial / n_initial

high_cook = cook_values > cook_threshold
high_leverage = leverage_values > leverage_threshold
high_studentized = abs(studentized_values) > 3

cat("\nCook threshold (4/n):", cook_threshold, "\n")
cat("Leverage threshold (2p/n):", leverage_threshold, "\n")
cat("Observations above Cook threshold:", sum(high_cook), "\n")
cat("Observations above leverage threshold:", sum(high_leverage), "\n")
cat("|Studentized residual| > 3:", sum(high_studentized), "\n")

influence_table = data.frame(
  `Order ID` = train_df$`Order ID`,
  Date = train_df$Date,
  `Product Name` = train_df$`Product Name`,
  `Export Market` = train_df$`Export Market`,
  Buyer = train_df$Buyer,
  Quantity = train_df$Quantity,
  Outlier_IQR_Flag = train_df$Outlier_IQR_Flag,
  Fitted_Quantity = initial_fitted,
  Residual = initial_residuals,
  Studentized_Residual = studentized_values,
  Leverage = leverage_values,
  Cooks_Distance = cook_values,
  High_Cook = high_cook,
  High_Leverage = high_leverage,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

influence_table = influence_table[
  order(influence_table$Cooks_Distance, decreasing = TRUE),
]

print(head(influence_table, 15))

write.csv(
  influence_table,
  "08_Model_Results/R/Task5_Initial_MLR_Influence_Diagnostics.csv",
  row.names = FALSE
)


# ----------------------------------------------------------------------------
# Step 18: Initial MLR Diagnostic Charts
# ----------------------------------------------------------------------------

png(
  "06_Charts/Task5_01_Initial_MLR_Residuals_vs_Fitted.png",
  width = 1000,
  height = 700
)

plot(
  initial_fitted,
  initial_residuals,
  main = "Residuals vs Fitted Values - Initial MLR",
  xlab = "Fitted Quantity",
  ylab = "Residual"
)

abline(h = 0, col = "red", lty = 2)

dev.off()


png(
  "06_Charts/Task5_02_Initial_MLR_QQ_Plot.png",
  width = 900,
  height = 700
)

qqnorm(
  initial_residuals,
  main = "Q-Q Plot of Initial MLR Residuals"
)
qqline(initial_residuals, col = "red")

dev.off()


png(
  "06_Charts/Task5_03_Initial_MLR_Residual_Histogram.png",
  width = 1000,
  height = 700
)

hist(
  initial_residuals,
  breaks = 30,
  main = "Distribution of Initial MLR Residuals",
  xlab = "Residual",
  col = "steelblue",
  border = "white"
)

dev.off()


png(
  "06_Charts/Task5_04_Initial_MLR_Cooks_Distance.png",
  width = 1100,
  height = 700
)

plot(
  cook_values,
  type = "h",
  main = "Cook's Distance - Initial MLR",
  xlab = "Training Observation Index",
  ylab = "Cook's Distance"
)

abline(h = cook_threshold, col = "red", lty = 2)

dev.off()


# ----------------------------------------------------------------------------
# Step 19: Create Training-Only Chronological Validation Folds
# ----------------------------------------------------------------------------
# These date boundaries reproduce the expanding-window folds used in the
# completed Python notebook. The final Apr-Jun 2026 test set is not used here.

validation_folds = data.frame(
  Fold = 1:5,
  Train_Start = as.Date(rep("2025-04-01", 5)),
  Train_End = as.Date(c(
    "2025-05-31",
    "2025-09-09",
    "2025-11-12",
    "2025-12-25",
    "2026-02-12"
  )),
  Validation_Start = as.Date(c(
    "2025-06-02",
    "2025-09-10",
    "2025-11-13",
    "2025-12-26",
    "2026-02-13"
  )),
  Validation_End = as.Date(c(
    "2025-09-09",
    "2025-11-12",
    "2025-12-25",
    "2026-02-12",
    "2026-03-31"
  ))
)

for(i in 1:nrow(validation_folds)) {
  validation_folds$Train_Rows[i] = sum(
    train_df$Date >= validation_folds$Train_Start[i] &
    train_df$Date <= validation_folds$Train_End[i]
  )

  validation_folds$Validation_Rows[i] = sum(
    train_df$Date >= validation_folds$Validation_Start[i] &
    train_df$Date <= validation_folds$Validation_End[i]
  )
}

print(validation_folds)

write.csv(
  validation_folds,
  "08_Model_Results/R/Task5_Chronological_Validation_Folds.csv",
  row.names = FALSE
)


# ----------------------------------------------------------------------------
# Step 20: Helper Functions for sklearn-Style Dummy Encoding
# ----------------------------------------------------------------------------
# For each categorical variable, the alphabetically first category observed
# in that fold is dropped. Categories not seen in fold training are encoded
# as all zeros in validation, matching the notebook's OneHotEncoder setup.

make_dummy_matrices = function(train_data, valid_data, categorical_vars, numeric_vars) {

  train_matrix = matrix(nrow = nrow(train_data), ncol = 0)
  valid_matrix = matrix(nrow = nrow(valid_data), ncol = 0)

  train_names = character(0)

  for(var_name in categorical_vars) {

    train_values = as.character(train_data[[var_name]])
    valid_values = as.character(valid_data[[var_name]])

    categories = sort(unique(train_values))

    if(length(categories) > 1) {
      kept_categories = categories[-1]

      for(category_value in kept_categories) {
        train_col = as.numeric(train_values == category_value)
        valid_col = as.numeric(valid_values == category_value)

        train_matrix = cbind(train_matrix, train_col)
        valid_matrix = cbind(valid_matrix, valid_col)

        train_names = c(
          train_names,
          paste(var_name, category_value, sep = "_")
        )
      }
    }
  }

  if(length(numeric_vars) > 0) {
    for(var_name in numeric_vars) {
      train_matrix = cbind(
        train_matrix,
        as.numeric(train_data[[var_name]])
      )

      valid_matrix = cbind(
        valid_matrix,
        as.numeric(valid_data[[var_name]])
      )

      train_names = c(train_names, var_name)
    }
  }

  train_matrix = cbind(Intercept = 1, train_matrix)
  valid_matrix = cbind(Intercept = 1, valid_matrix)

  colnames(train_matrix) = c("Intercept", train_names)
  colnames(valid_matrix) = c("Intercept", train_names)

  return(list(train = train_matrix, valid = valid_matrix))
}


fit_predict_matrix = function(X_train, y_train, X_valid) {
  fit = lm.fit(x = X_train, y = y_train)
  beta = fit$coefficients

  # lm.fit can return NA for an exactly redundant column.
  # Replace such coefficients with zero, equivalent to an unused dummy column.
  beta[is.na(beta)] = 0

  predictions = as.numeric(X_valid %*% beta)
  return(predictions)
}


# ----------------------------------------------------------------------------
# Step 21: Candidate Linear Specifications
# ----------------------------------------------------------------------------

candidate_models = list(
  "Full: Product + Market + Month + Day + Trend" = list(
    categorical = c("Product Name", "Export Market", "Month", "Day_of_Week"),
    numeric = c("Time_Trend")
  ),

  "No Trend: Product + Market + Month + Day" = list(
    categorical = c("Product Name", "Export Market", "Month", "Day_of_Week"),
    numeric = character(0)
  ),

  "No Month: Product + Market + Day + Trend" = list(
    categorical = c("Product Name", "Export Market", "Day_of_Week"),
    numeric = c("Time_Trend")
  ),

  "Reduced: Product + Market + Day" = list(
    categorical = c("Product Name", "Export Market", "Day_of_Week"),
    numeric = character(0)
  )
)


# ----------------------------------------------------------------------------
# Step 22: Training-Only Chronological Specification Comparison
# ----------------------------------------------------------------------------

cv_results = data.frame()

for(model_name in names(candidate_models)) {

  specification = candidate_models[[model_name]]

  for(i in 1:nrow(validation_folds)) {

    fold_train = train_df[
      train_df$Date >= validation_folds$Train_Start[i] &
      train_df$Date <= validation_folds$Train_End[i],
    ]

    fold_valid = train_df[
      train_df$Date >= validation_folds$Validation_Start[i] &
      train_df$Date <= validation_folds$Validation_End[i],
    ]

    matrices = make_dummy_matrices(
      fold_train,
      fold_valid,
      specification$categorical,
      specification$numeric
    )

    valid_pred = fit_predict_matrix(
      matrices$train,
      fold_train$Quantity,
      matrices$valid
    )

    metrics = regression_metrics(
      fold_valid$Quantity,
      valid_pred
    )

    cv_results = rbind(
      cv_results,
      data.frame(
        Model = model_name,
        Fold = i,
        Train_Rows = nrow(fold_train),
        Validation_Rows = nrow(fold_valid),
        MAE = metrics["MAE"],
        RMSE = metrics["RMSE"],
        R2 = metrics["R2"],
        row.names = NULL
      )
    )
  }
}

print(cv_results)

write.csv(
  cv_results,
  "08_Model_Results/R/Task5_Chronological_Validation_Results.csv",
  row.names = FALSE
)


# ----------------------------------------------------------------------------
# Step 23: Summarize Chronological Validation Performance
# ----------------------------------------------------------------------------

model_names = names(candidate_models)
cv_summary = data.frame()

for(model_name in model_names) {
  temp = cv_results[cv_results$Model == model_name, ]

  cv_summary = rbind(
    cv_summary,
    data.frame(
      Model = model_name,
      Mean_MAE = mean(temp$MAE),
      SD_MAE = sd(temp$MAE),
      Mean_RMSE = mean(temp$RMSE),
      SD_RMSE = sd(temp$RMSE),
      Mean_R2 = mean(temp$R2),
      SD_R2 = sd(temp$R2),
      Best_MAE = min(temp$MAE),
      Worst_MAE = max(temp$MAE),
      Best_RMSE = min(temp$RMSE),
      Worst_RMSE = max(temp$RMSE),
      Min_R2 = min(temp$R2),
      Max_R2 = max(temp$R2)
    )
  )
}

cv_summary = cv_summary[
  order(cv_summary$Mean_RMSE),
]

rownames(cv_summary) = NULL

print(cv_summary)

write.csv(
  cv_summary,
  "08_Model_Results/R/Task5_Chronological_Validation_Summary.csv",
  row.names = FALSE
)


# ----------------------------------------------------------------------------
# Step 24: Select Final Interpretable MLR Specification
# ----------------------------------------------------------------------------
# Training-only validation selected:
# Product Name + Export Market + Day of Week

final_train = train_df
final_test = test_df

final_mlr = lm(
  Quantity ~ `Product Name` + `Export Market` + Day_of_Week,
  data = final_train
)

print(summary(final_mlr))

final_loglik = as.numeric(logLik(final_mlr))
final_k = length(coef(final_mlr))
final_n = nobs(final_mlr)

final_aic = -2 * final_loglik + 2 * final_k
final_bic = -2 * final_loglik + log(final_n) * final_k

cat("\nFINAL REDUCED MLR\n")
cat("Observations:", final_n, "\n")
cat("R-squared:", summary(final_mlr)$r.squared, "\n")
cat("Adjusted R-squared:", summary(final_mlr)$adj.r.squared, "\n")
cat("AIC:", final_aic, "\n")
cat("BIC:", final_bic, "\n")


# ----------------------------------------------------------------------------
# Step 25: HC3 Robust Standard Errors
# ----------------------------------------------------------------------------
# HC3 changes coefficient uncertainty, not fitted coefficients or predictions.

hc3_cov = hccm(
  final_mlr,
  type = "hc3"
)

final_beta = coef(final_mlr)
hc3_se = sqrt(diag(hc3_cov))
hc3_t = final_beta / hc3_se
hc3_p = 2 * pt(
  abs(hc3_t),
  df = df.residual(final_mlr),
  lower.tail = FALSE
)

hc3_t_critical = qt(
  0.975,
  df = df.residual(final_mlr)
)

hc3_lower = final_beta - hc3_t_critical * hc3_se
hc3_upper = final_beta + hc3_t_critical * hc3_se

final_hc3_table = data.frame(
  Variable = names(final_beta),
  Coefficient = as.numeric(final_beta),
  HC3_SE = as.numeric(hc3_se),
  t_value = as.numeric(hc3_t),
  p_value = as.numeric(hc3_p),
  CI_2.5 = as.numeric(hc3_lower),
  CI_97.5 = as.numeric(hc3_upper),
  row.names = NULL
)

print(final_hc3_table)

write.csv(
  final_hc3_table,
  "08_Model_Results/R/Task5_Final_MLR_HC3_Coefficients.csv",
  row.names = FALSE
)


# ----------------------------------------------------------------------------
# Step 26: Final Model VIF
# ----------------------------------------------------------------------------

final_vif = calculate_column_vif(final_mlr)

print(final_vif)

write.csv(
  final_vif,
  "08_Model_Results/R/Task5_Final_MLR_VIF.csv",
  row.names = FALSE
)


# ----------------------------------------------------------------------------
# Step 27: Final Residual Diagnostic Recheck
# ----------------------------------------------------------------------------

final_residuals = residuals(final_mlr)
final_residual_n = length(final_residuals)

final_jb_skew = mean((final_residuals - mean(final_residuals))^3) /
  mean((final_residuals - mean(final_residuals))^2)^(3/2)

final_jb_kurt = mean((final_residuals - mean(final_residuals))^4) /
  mean((final_residuals - mean(final_residuals))^2)^2

final_jb_stat = final_residual_n / 6 *
  (final_jb_skew^2 + ((final_jb_kurt - 3)^2) / 4)

final_jb_p = pchisq(
  final_jb_stat,
  df = 2,
  lower.tail = FALSE
)

bp_final = bptest(final_mlr)

cat("\nFINAL MLR RESIDUAL DIAGNOSTICS\n")
cat("Jarque-Bera statistic:", final_jb_stat, "\n")
cat("Jarque-Bera p-value:", final_jb_p, "\n")
cat("Skewness:", final_jb_skew, "\n")
cat("Kurtosis:", final_jb_kurt, "\n")
print(bp_final)


# ----------------------------------------------------------------------------
# Step 28: Final MLR Predictions and Performance
# ----------------------------------------------------------------------------

final_train_pred = predict(
  final_mlr,
  newdata = final_train
)

final_test_pred = predict(
  final_mlr,
  newdata = final_test
)

final_train_metrics = regression_metrics(
  final_train$Quantity,
  final_train_pred
)

final_test_metrics = regression_metrics(
  final_test$Quantity,
  final_test_pred
)

final_performance = data.frame(
  Dataset = c("Training", "Testing"),
  MAE = c(final_train_metrics["MAE"], final_test_metrics["MAE"]),
  RMSE = c(final_train_metrics["RMSE"], final_test_metrics["RMSE"]),
  R2 = c(final_train_metrics["R2"], final_test_metrics["R2"])
)

print(final_performance)

write.csv(
  final_performance,
  "08_Model_Results/R/Task5_Final_MLR_Performance.csv",
  row.names = FALSE
)


# ----------------------------------------------------------------------------
# Step 29: Final Model Comparison
# ----------------------------------------------------------------------------

model_comparison = data.frame(
  Model = c(
    "Training Mean Baseline",
    "Initial Full MLR",
    "Selected Reduced MLR"
  ),
  Train_MAE = c(
    baseline_train_metrics["MAE"],
    initial_train_metrics["MAE"],
    final_train_metrics["MAE"]
  ),
  Train_RMSE = c(
    baseline_train_metrics["RMSE"],
    initial_train_metrics["RMSE"],
    final_train_metrics["RMSE"]
  ),
  Train_R2 = c(
    baseline_train_metrics["R2"],
    initial_train_metrics["R2"],
    final_train_metrics["R2"]
  ),
  Test_MAE = c(
    baseline_test_metrics["MAE"],
    initial_test_metrics["MAE"],
    final_test_metrics["MAE"]
  ),
  Test_RMSE = c(
    baseline_test_metrics["RMSE"],
    initial_test_metrics["RMSE"],
    final_test_metrics["RMSE"]
  ),
  Test_R2 = c(
    baseline_test_metrics["R2"],
    initial_test_metrics["R2"],
    final_test_metrics["R2"]
  ),
  row.names = NULL
)

print(model_comparison)

write.csv(
  model_comparison,
  "08_Model_Results/R/Task5_Model_Comparison.csv",
  row.names = FALSE
)

final_mae_improvement =
  (baseline_test_metrics["MAE"] - final_test_metrics["MAE"]) /
  baseline_test_metrics["MAE"] * 100

final_rmse_improvement =
  (baseline_test_metrics["RMSE"] - final_test_metrics["RMSE"]) /
  baseline_test_metrics["RMSE"] * 100

cat("\nSelected MLR Test MAE improvement (%):", final_mae_improvement, "\n")
cat("Selected MLR Test RMSE improvement (%):", final_rmse_improvement, "\n")
cat("Training prediction range:", min(final_train_pred), "to", max(final_train_pred), "\n")
cat("Testing prediction range:", min(final_test_pred), "to", max(final_test_pred), "\n")
cat("Negative test predictions:", sum(final_test_pred < 0), "\n")


# ----------------------------------------------------------------------------
# Step 30: Final Prediction Output File
# ----------------------------------------------------------------------------

prediction_output = data.frame(
  `Order ID` = final_test$`Order ID`,
  Date = final_test$Date,
  `Product Name` = final_test$`Product Name`,
  `Export Market` = final_test$`Export Market`,
  Day_of_Week = final_test$Day_of_Week,
  Actual_Quantity = final_test$Quantity,
  Predicted_Quantity = as.numeric(final_test_pred),
  Error = final_test$Quantity - as.numeric(final_test_pred),
  Absolute_Error = abs(final_test$Quantity - as.numeric(final_test_pred)),
  check.names = FALSE
)

print(head(prediction_output))

write.csv(
  prediction_output,
  "08_Model_Results/R/Task5_Final_Test_Predictions.csv",
  row.names = FALSE
)


# ----------------------------------------------------------------------------
# Step 31: Final Model Charts
# ----------------------------------------------------------------------------

png(
  "06_Charts/Task5_05_Final_MLR_Actual_vs_Predicted.png",
  width = 1000,
  height = 700
)

plot(
  final_test$Quantity,
  final_test_pred,
  main = "Final MLR: Actual vs Predicted Quantity",
  xlab = "Actual Quantity",
  ylab = "Predicted Quantity"
)

abline(0, 1, col = "red", lty = 2)

dev.off()


png(
  "06_Charts/Task5_06_Final_MLR_Residuals_vs_Fitted.png",
  width = 1000,
  height = 700
)

plot(
  fitted(final_mlr),
  residuals(final_mlr),
  main = "Final MLR Residuals vs Fitted",
  xlab = "Fitted Quantity",
  ylab = "Residual"
)

abline(h = 0, col = "red", lty = 2)

dev.off()


png(
  "06_Charts/Task5_07_Final_MLR_QQ_Plot.png",
  width = 900,
  height = 700
)

qqnorm(
  residuals(final_mlr),
  main = "Q-Q Plot of Final MLR Residuals"
)
qqline(residuals(final_mlr), col = "red")

dev.off()


# ----------------------------------------------------------------------------
# Step 32: Save Report-Ready Task 5 Summary
# ----------------------------------------------------------------------------

sink(
  "08_Model_Results/R/Task5_Report_Ready_Summary.txt"
)

cat("TASK 5 - PREDICTIVE STATISTICAL MODELLING\n\n")

cat("Prediction objective:\n")
cat(
  "Estimate future Regular Order Quantity under Fragceylon's current post-April-2025 business structure.\n\n"
)

cat("MODELLING POPULATION\n")
cat("Current-regime rows:", nrow(current_df), "\n")
cat("Current-regime dates: 2025-04-01 to 2026-06-30\n")
cat("Response: Quantity\n\n")

cat("TRAIN-TEST STRATEGY\n")
cat("Training: 2025-04-01 to 2026-03-31; n =", nrow(train_df), "\n")
cat("Testing : 2026-04-01 to 2026-06-30; n =", nrow(test_df), "\n")
cat(
  "A chronological split was used because the practical objective is prediction of future transactions.\n\n"
)

cat("PREDICTOR DECISIONS\n")
cat(
  "Initial candidates: Product Name, Export Market, Month, Day of Week and Time Trend.\n"
)
cat(
  "Channel Type and Sales Regime were excluded because both are constant in the current-regime modelling population.\n"
)
cat(
  "Buyer was excluded from the primary model to reduce customer-specific memorization.\n"
)
cat(
  "Total Value was excluded because it is directly derived from Quantity and would cause target leakage.\n\n"
)

cat("BASELINE\n")
cat("Training mean prediction:", round(baseline_value, 4), "\n")
cat("Test MAE:", round(baseline_test_metrics["MAE"], 4), "\n")
cat("Test RMSE:", round(baseline_test_metrics["RMSE"], 4), "\n")
cat("Test R-squared:", round(baseline_test_metrics["R2"], 4), "\n\n")

cat("INITIAL FULL MLR\n")
cat(
  "Formula: Quantity ~ Product Name + Export Market + Month + Day of Week + Time Trend\n"
)
cat("Training R-squared:", round(summary(initial_mlr)$r.squared, 6), "\n")
cat("Adjusted R-squared:", round(summary(initial_mlr)$adj.r.squared, 6), "\n")
cat("Test MAE:", round(initial_test_metrics["MAE"], 4), "\n")
cat("Test RMSE:", round(initial_test_metrics["RMSE"], 4), "\n")
cat("Test R-squared:", round(initial_test_metrics["R2"], 4), "\n")
cat(
  "Diagnostics identified severe multicollinearity between Month indicators and Time Trend and statistically significant heteroscedasticity.\n\n"
)

cat("TRAINING-ONLY SPECIFICATION COMPARISON\n")
cat(
  "The reduced specification using Product Name, Export Market and Day of Week achieved the strongest balance of average chronological validation error, stability, parsimony and interpretability.\n\n"
)

cat("FINAL SELECTED MLR\n")
cat(
  "Formula: Quantity ~ Product Name + Export Market + Day of Week\n"
)
cat("Training R-squared:", round(summary(final_mlr)$r.squared, 6), "\n")
cat("Adjusted R-squared:", round(summary(final_mlr)$adj.r.squared, 6), "\n")
cat("Test MAE:", round(final_test_metrics["MAE"], 4), "\n")
cat("Test RMSE:", round(final_test_metrics["RMSE"], 4), "\n")
cat("Test R-squared:", round(final_test_metrics["R2"], 4), "\n")
cat("Test MAE improvement over baseline:", round(final_mae_improvement, 2), "%\n")
cat("Test RMSE improvement over baseline:", round(final_rmse_improvement, 2), "%\n\n")

cat("ROBUST INFERENCE\n")
cat(
  "HC3 heteroscedasticity-robust standard errors were used for coefficient uncertainty because the Breusch-Pagan diagnostic indicated unequal residual variance.\n"
)
cat(
  "The Japan coefficient is large and positive in the selected model, but it must be interpreted as an association rather than a causal market effect because Japan transactions are structurally concentrated within one buyer and the Main Dealer/post-April regime.\n\n"
)

cat("LIMITATIONS\n")
cat("- Historical observational data do not establish causality.\n")
cat("- Repeated transactions from the same buyers may violate complete observation-level independence.\n")
cat("- Japan transactions are concentrated within one buyer structure.\n")
cat("- The model predicts transaction-level Quantity, not aggregate monthly demand.\n")
cat("- Test results describe the Apr-Jun 2026 holdout period and should not be treated as guaranteed future accuracy.\n")
cat("- The 41 valid high-volume orders were retained; no observations were deleted simply because they were influential.\n")

sink()


# ----------------------------------------------------------------------------
# Step 33: Final Validation Against the Completed Python Notebook
# ----------------------------------------------------------------------------
# Small tolerances are used because R and Python can differ slightly in
# floating-point calculations and statistical implementation details.

reduced_cv = cv_summary[
  cv_summary$Model == "Reduced: Product + Market + Day",
]

stopifnot(
  # Current regime and split
  nrow(current_df) == 2014,
  nrow(train_df) == 1599,
  nrow(test_df) == 415,
  length(unique(train_df$Date)) == 257,

  # Baseline
  abs(baseline_value - 306.5503) < 0.001,
  abs(baseline_train_metrics["MAE"] - 133.9660) < 0.01,
  abs(baseline_train_metrics["RMSE"] - 170.5427) < 0.01,
  abs(baseline_test_metrics["MAE"] - 137.0608) < 0.01,
  abs(baseline_test_metrics["RMSE"] - 178.3421) < 0.01,
  abs(baseline_test_metrics["R2"] - (-0.0050)) < 0.001,

  # Initial full MLR
  abs(summary(initial_mlr)$r.squared - 0.326319) < 0.0001,
  abs(summary(initial_mlr)$adj.r.squared - 0.316915) < 0.0001,
  abs(initial_train_metrics["MAE"] - 118.8001) < 0.02,
  abs(initial_train_metrics["RMSE"] - 139.9781) < 0.02,
  abs(initial_test_metrics["MAE"] - 118.1498) < 0.02,
  abs(initial_test_metrics["RMSE"] - 140.1878) < 0.02,
  abs(initial_test_metrics["R2"] - 0.3790) < 0.001,

  # Chronological validation selected reduced model
  abs(reduced_cv$Mean_MAE - 114.8169) < 0.1,
  abs(reduced_cv$Mean_RMSE - 136.9192) < 0.1,
  abs(reduced_cv$Mean_R2 - 0.2906) < 0.01,

  # Final selected MLR
  abs(summary(final_mlr)$r.squared - 0.318380) < 0.0001,
  abs(summary(final_mlr)$adj.r.squared - 0.314088) < 0.0001,
  abs(coef(final_mlr)["`Export Market`Japan"] - 333.9565) < 0.05,
  abs(final_train_metrics["MAE"] - 119.4688) < 0.02,
  abs(final_train_metrics["RMSE"] - 140.8005) < 0.02,
  abs(final_test_metrics["MAE"] - 116.8403) < 0.02,
  abs(final_test_metrics["RMSE"] - 137.9482) < 0.02,
  abs(final_test_metrics["R2"] - 0.3987) < 0.001,
  abs(final_mae_improvement - 14.75) < 0.05,
  abs(final_rmse_improvement - 22.65) < 0.05,
  sum(final_test_pred < 0) == 0
)

print("All Task 5 validation checks passed.")


# ----------------------------------------------------------------------------
# Step 34: Show Created Output Files
# ----------------------------------------------------------------------------

cat("\nTask 5 result files:\n")
print(
  list.files(
    "08_Model_Results/R",
    pattern = "^Task5",
    full.names = TRUE
  )
)

cat("\nTask 5 chart files:\n")
print(
  list.files(
    "06_Charts",
    pattern = "^Task5",
    full.names = TRUE
  )
)


# ----------------------------------------------------------------------------
# Step 35: Final Message
# ----------------------------------------------------------------------------

cat("\n============================================================\n")
cat("TASK 5 COMPLETE\n")
cat("============================================================\n")

cat("Current-regime observations:", nrow(current_df), "\n")
cat("Training observations:", nrow(train_df), "\n")
cat("Testing observations:", nrow(test_df), "\n")

cat(
  "Baseline test MAE / RMSE / R2:",
  round(baseline_test_metrics["MAE"], 4), "/",
  round(baseline_test_metrics["RMSE"], 4), "/",
  round(baseline_test_metrics["R2"], 4),
  "\n"
)

cat(
  "Initial Full MLR test MAE / RMSE / R2:",
  round(initial_test_metrics["MAE"], 4), "/",
  round(initial_test_metrics["RMSE"], 4), "/",
  round(initial_test_metrics["R2"], 4),
  "\n"
)

cat(
  "Selected Reduced MLR test MAE / RMSE / R2:",
  round(final_test_metrics["MAE"], 4), "/",
  round(final_test_metrics["RMSE"], 4), "/",
  round(final_test_metrics["R2"], 4),
  "\n"
)

cat("Test MAE improvement over baseline:", round(final_mae_improvement, 2), "%\n")
cat("Test RMSE improvement over baseline:", round(final_rmse_improvement, 2), "%\n")

cat(
  "Final model: Quantity ~ Product Name + Export Market + Day of Week\n"
)

cat(
  "HC3 robust standard errors were applied for coefficient inference.\n"
)

cat("Task 5 predictive modelling completed successfully.\n")
