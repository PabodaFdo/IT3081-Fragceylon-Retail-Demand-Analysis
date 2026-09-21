# ============================================================================
# IT3081 - Statistical Modelling
# Fragceylon Retail Product Demand Analysis
# TASK 5B - Alternative Predictive Models and Final Model Comparison
# LAB-SHEET STYLE FINAL R VERSION
#
# This file continues Task 5A.
# It investigates the additional models planned for the project:
#   1. Poisson GLM
#   2. Negative Binomial GLM
#   3. Ridge Regression
#   4. LASSO Regression
#   5. Elastic Net Regression
#   6. Final comparison with Baseline and Reduced MLR
#
# IMPORTANT:
# Run Task 3A first so this file exists:
# 04_Cleaned_Data/Fragceylon_Regular_Orders.xlsx
#
# If required, install packages once:
# install.packages(c("readxl", "MASS", "glmnet"))
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
library(MASS)
library(glmnet)


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

head(df)
str(df)
dim(df)
summary(df$Quantity)

stopifnot(
  nrow(df) == 3116,
  ncol(df) == 22,
  sum(is.na(df)) == 0,
  all(df$`Line Type` == "Regular Order"),
  sum(df$Quantity) == 705810
)

print("Task 5B input-data checks passed.")


# ----------------------------------------------------------------------------
# Step 5: Use the Same Current-Regime Modelling Population as Task 5A
# ----------------------------------------------------------------------------

current_regime_start = as.Date("2025-04-01")

current_df = df[df$Date >= current_regime_start, ]
current_df = current_df[order(current_df$Date, current_df$`Order ID`), ]
rownames(current_df) = NULL

cat("\nCURRENT-REGIME MODELLING POPULATION\n")
cat("Rows:", nrow(current_df), "\n")
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
# Step 6: Set the Same Factor Reference Levels as Task 5A
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
# Step 7: Use the Same Chronological Train-Test Split as Task 5A
# ----------------------------------------------------------------------------

TEST_START_DATE = as.Date("2026-04-01")

train_df = current_df[current_df$Date < TEST_START_DATE, ]
test_df = current_df[current_df$Date >= TEST_START_DATE, ]

train_df = train_df[order(train_df$Date, train_df$`Order ID`), ]
test_df = test_df[order(test_df$Date, test_df$`Order ID`), ]

rownames(train_df) = NULL
rownames(test_df) = NULL

cat("\nCHRONOLOGICAL TRAIN-TEST SPLIT\n")
cat("Training rows:", nrow(train_df), "\n")
cat("Training dates:", format(min(train_df$Date)), "to", format(max(train_df$Date)), "\n")
cat("Testing rows:", nrow(test_df), "\n")
cat("Testing dates:", format(min(test_df$Date)), "to", format(max(test_df$Date)), "\n")

stopifnot(
  nrow(train_df) == 1599,
  nrow(test_df) == 415,
  min(train_df$Date) == as.Date("2025-04-01"),
  max(train_df$Date) == as.Date("2026-03-31"),
  min(test_df$Date) == as.Date("2026-04-01"),
  max(test_df$Date) == as.Date("2026-06-30"),
  max(train_df$Date) < min(test_df$Date)
)


# ----------------------------------------------------------------------------
# Step 8: Regression Performance Function
# ----------------------------------------------------------------------------

regression_metrics = function(actual, predicted) {
  mae = mean(abs(actual - predicted))
  rmse = sqrt(mean((actual - predicted)^2))
  r2 = 1 - sum((actual - predicted)^2) / sum((actual - mean(actual))^2)

  return(c(
    MAE = mae,
    RMSE = rmse,
    R2 = r2
  ))
}


# ----------------------------------------------------------------------------
# Step 9: Recreate Baseline and Final Reduced MLR for Fair Comparison
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


reduced_mlr = lm(
  Quantity ~ `Product Name` + `Export Market` + Day_of_Week,
  data = train_df
)

mlr_train_pred = predict(
  reduced_mlr,
  newdata = train_df
)

mlr_test_pred = predict(
  reduced_mlr,
  newdata = test_df
)

mlr_train_metrics = regression_metrics(
  train_df$Quantity,
  mlr_train_pred
)

mlr_test_metrics = regression_metrics(
  test_df$Quantity,
  mlr_test_pred
)

cat("\nREFERENCE MODELS\n")
cat("Baseline test metrics:\n")
print(baseline_test_metrics)
cat("Reduced MLR test metrics:\n")
print(mlr_test_metrics)


# ============================================================================
# PART A - POISSON GLM
# ============================================================================


# ----------------------------------------------------------------------------
# Step 10: Fit Poisson Regression
# ----------------------------------------------------------------------------
# Quantity is integer-valued and count-like, so a Poisson GLM is investigated.
# The same reduced predictors are used so that the distributional assumption
# can be compared with the selected MLR without changing the predictor set.

cat("\n============================================================\n")
cat("POISSON GLM\n")
cat("============================================================\n")

poisson_model = glm(
  Quantity ~ `Product Name` + `Export Market` + Day_of_Week,
  data = train_df,
  family = poisson(link = "log")
)

print(summary(poisson_model))

poisson_train_pred = predict(
  poisson_model,
  newdata = train_df,
  type = "response"
)

poisson_test_pred = predict(
  poisson_model,
  newdata = test_df,
  type = "response"
)

poisson_train_metrics = regression_metrics(
  train_df$Quantity,
  poisson_train_pred
)

poisson_test_metrics = regression_metrics(
  test_df$Quantity,
  poisson_test_pred
)

cat("Poisson training metrics:\n")
print(poisson_train_metrics)

cat("Poisson testing metrics:\n")
print(poisson_test_metrics)

cat("Poisson AIC:", AIC(poisson_model), "\n")
cat("Poisson residual deviance:", deviance(poisson_model), "\n")
cat("Poisson residual df:", df.residual(poisson_model), "\n")


# ----------------------------------------------------------------------------
# Step 11: Poisson Overdispersion Check
# ----------------------------------------------------------------------------
# Under a well-fitting Poisson model, conditional variance should be close to
# conditional mean. A Pearson dispersion ratio much larger than 1 indicates
# overdispersion.

poisson_pearson_chi2 = sum(
  residuals(
    poisson_model,
    type = "pearson"
  )^2
)

poisson_dispersion =
  poisson_pearson_chi2 /
  df.residual(poisson_model)

poisson_deviance_ratio =
  deviance(poisson_model) /
  df.residual(poisson_model)

cat("\nPOISSON OVERDISPERSION CHECK\n")
cat("Pearson Chi-square:", poisson_pearson_chi2, "\n")
cat("Pearson dispersion ratio:", poisson_dispersion, "\n")
cat("Residual deviance / df:", poisson_deviance_ratio, "\n")

if(poisson_dispersion > 1.5) {
  print("Strong overdispersion is present. Poisson variance assumption is not adequate.")
} else {
  print("Strong overdispersion was not detected by the Pearson dispersion ratio.")
}


# ----------------------------------------------------------------------------
# Step 12: Poisson Coefficients and Incidence Rate Ratios
# ----------------------------------------------------------------------------

poisson_summary = summary(poisson_model)$coefficients

poisson_coef_table = data.frame(
  Variable = rownames(poisson_summary),
  Coefficient = poisson_summary[, "Estimate"],
  Std_Error = poisson_summary[, "Std. Error"],
  z_value = poisson_summary[, "z value"],
  p_value = poisson_summary[, "Pr(>|z|)"],
  IRR = exp(poisson_summary[, "Estimate"]),
  row.names = NULL
)

print(poisson_coef_table)

write.csv(
  poisson_coef_table,
  "08_Model_Results/R/Task5B_Poisson_Coefficients.csv",
  row.names = FALSE
)

poisson_diagnostics = data.frame(
  Measure = c(
    "AIC",
    "Residual Deviance",
    "Residual DF",
    "Pearson Chi-square",
    "Pearson Dispersion Ratio",
    "Deviance / DF"
  ),
  Value = c(
    AIC(poisson_model),
    deviance(poisson_model),
    df.residual(poisson_model),
    poisson_pearson_chi2,
    poisson_dispersion,
    poisson_deviance_ratio
  )
)

write.csv(
  poisson_diagnostics,
  "08_Model_Results/R/Task5B_Poisson_Diagnostics.csv",
  row.names = FALSE
)


# ============================================================================
# PART B - NEGATIVE BINOMIAL GLM
# ============================================================================


# ----------------------------------------------------------------------------
# Step 13: Fit Negative Binomial Regression
# ----------------------------------------------------------------------------
# Negative Binomial regression is investigated because it allows variance to
# exceed the mean and is therefore more flexible for overdispersed count data.

cat("\n============================================================\n")
cat("NEGATIVE BINOMIAL GLM\n")
cat("============================================================\n")

nb_model = glm.nb(
  Quantity ~ `Product Name` + `Export Market` + Day_of_Week,
  data = train_df,
  link = log,
  control = glm.control(maxit = 100)
)

print(summary(nb_model))

nb_train_pred = predict(
  nb_model,
  newdata = train_df,
  type = "response"
)

nb_test_pred = predict(
  nb_model,
  newdata = test_df,
  type = "response"
)

nb_train_metrics = regression_metrics(
  train_df$Quantity,
  nb_train_pred
)

nb_test_metrics = regression_metrics(
  test_df$Quantity,
  nb_test_pred
)

cat("Negative Binomial theta:", nb_model$theta, "\n")
cat("Negative Binomial AIC:", AIC(nb_model), "\n")
cat("Negative Binomial residual deviance:", deviance(nb_model), "\n")

cat("Negative Binomial training metrics:\n")
print(nb_train_metrics)

cat("Negative Binomial testing metrics:\n")
print(nb_test_metrics)


# ----------------------------------------------------------------------------
# Step 14: Negative Binomial Coefficients and Multiplicative Effects
# ----------------------------------------------------------------------------

nb_summary = summary(nb_model)$coefficients

nb_coef_table = data.frame(
  Variable = rownames(nb_summary),
  Coefficient = nb_summary[, "Estimate"],
  Std_Error = nb_summary[, "Std. Error"],
  z_value = nb_summary[, "z value"],
  p_value = nb_summary[, "Pr(>|z|)"],
  Multiplicative_Effect = exp(nb_summary[, "Estimate"]),
  row.names = NULL
)

print(nb_coef_table)

write.csv(
  nb_coef_table,
  "08_Model_Results/R/Task5B_Negative_Binomial_Coefficients.csv",
  row.names = FALSE
)

nb_diagnostics = data.frame(
  Measure = c(
    "AIC",
    "Residual Deviance",
    "Residual DF",
    "Theta"
  ),
  Value = c(
    AIC(nb_model),
    deviance(nb_model),
    df.residual(nb_model),
    nb_model$theta
  )
)

write.csv(
  nb_diagnostics,
  "08_Model_Results/R/Task5B_Negative_Binomial_Diagnostics.csv",
  row.names = FALSE
)


# ============================================================================
# PART C - REGULARIZED REGRESSION DATA PREPARATION
# ============================================================================


# ----------------------------------------------------------------------------
# Step 15: Create Encoded Predictor Matrices
# ----------------------------------------------------------------------------
# Ridge, LASSO and Elastic Net use the broader initial predictor set:
# Product Name + Export Market + Month + Day of Week + Time Trend.
# model.matrix() automatically creates dummy variables for categorical fields.
# glmnet standardizes predictors internally by default.

regularized_formula =
  ~ `Product Name` + `Export Market` + Month + Day_of_Week + Time_Trend

x_train = model.matrix(
  regularized_formula,
  data = train_df
)

x_test = model.matrix(
  regularized_formula,
  data = test_df
)

# Remove the model-matrix intercept because glmnet adds its own intercept.
x_train = x_train[, colnames(x_train) != "(Intercept)", drop = FALSE]
x_test = x_test[, colnames(x_test) != "(Intercept)", drop = FALSE]

y_train = train_df$Quantity
y_test = test_df$Quantity

cat("\nREGULARIZED MODEL MATRIX\n")
cat("Training matrix:", nrow(x_train), "rows x", ncol(x_train), "predictors\n")
cat("Testing matrix:", nrow(x_test), "rows x", ncol(x_test), "predictors\n")

print(colnames(x_train))

stopifnot(
  nrow(x_train) == 1599,
  nrow(x_test) == 415,
  identical(colnames(x_train), colnames(x_test))
)


# ----------------------------------------------------------------------------
# Step 16: Use the Same 5 Chronological Validation Folds as Task 5A
# ----------------------------------------------------------------------------

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


# ----------------------------------------------------------------------------
# Step 17: Function for Chronological glmnet Validation
# ----------------------------------------------------------------------------
# This is a time-aware alternative to random k-fold cross-validation.
# Each validation period occurs after its corresponding training period.

chronological_glmnet_cv = function(
  alpha_value,
  train_data,
  x_matrix,
  y_vector,
  fold_table,
  lambda_grid
) {

  all_results = data.frame()

  for(i in 1:nrow(fold_table)) {

    fold_train_index =
      train_data$Date >= fold_table$Train_Start[i] &
      train_data$Date <= fold_table$Train_End[i]

    fold_valid_index =
      train_data$Date >= fold_table$Validation_Start[i] &
      train_data$Date <= fold_table$Validation_End[i]

    x_fold_train = x_matrix[fold_train_index, , drop = FALSE]
    y_fold_train = y_vector[fold_train_index]

    x_fold_valid = x_matrix[fold_valid_index, , drop = FALSE]
    y_fold_valid = y_vector[fold_valid_index]

    fold_fit = glmnet(
      x_fold_train,
      y_fold_train,
      alpha = alpha_value,
      lambda = lambda_grid,
      family = "gaussian",
      standardize = TRUE
    )

    fold_predictions = predict(
      fold_fit,
      newx = x_fold_valid,
      s = lambda_grid
    )

    actual_matrix = matrix(
      y_fold_valid,
      nrow = length(y_fold_valid),
      ncol = length(lambda_grid)
    )

    mae_values = colMeans(
      abs(actual_matrix - fold_predictions)
    )

    rmse_values = sqrt(
      colMeans(
        (actual_matrix - fold_predictions)^2
      )
    )

    sst = sum(
      (y_fold_valid - mean(y_fold_valid))^2
    )

    r2_values = 1 -
      colSums(
        (actual_matrix - fold_predictions)^2
      ) / sst

    fold_result = data.frame(
      Alpha = alpha_value,
      Fold = i,
      Lambda = lambda_grid,
      MAE = mae_values,
      RMSE = rmse_values,
      R2 = r2_values
    )

    all_results = rbind(
      all_results,
      fold_result
    )
  }

  return(all_results)
}


# ----------------------------------------------------------------------------
# Step 18: Function to Summarize Lambda Performance
# ----------------------------------------------------------------------------

summarize_lambda_results = function(cv_results) {

  lambda_values = sort(
    unique(cv_results$Lambda),
    decreasing = TRUE
  )

  summary_table = data.frame()

  for(lambda_value in lambda_values) {

    temp = cv_results[
      cv_results$Lambda == lambda_value,
    ]

    summary_table = rbind(
      summary_table,
      data.frame(
        Alpha = unique(temp$Alpha)[1],
        Lambda = lambda_value,
        Mean_MAE = mean(temp$MAE),
        SD_MAE = sd(temp$MAE),
        Mean_RMSE = mean(temp$RMSE),
        SD_RMSE = sd(temp$RMSE),
        Mean_R2 = mean(temp$R2),
        SD_R2 = sd(temp$R2)
      )
    )
  }

  summary_table = summary_table[
    order(summary_table$Mean_RMSE),
  ]

  rownames(summary_table) = NULL

  return(summary_table)
}


# ============================================================================
# PART D - RIDGE REGRESSION
# ============================================================================


# ----------------------------------------------------------------------------
# Step 19: Tune Ridge Lambda with Chronological Validation
# ----------------------------------------------------------------------------

cat("\n============================================================\n")
cat("RIDGE REGRESSION\n")
cat("============================================================\n")

ridge_path = glmnet(
  x_train,
  y_train,
  alpha = 0,
  family = "gaussian",
  standardize = TRUE,
  nlambda = 80
)

ridge_lambda_grid = ridge_path$lambda

ridge_cv_results = chronological_glmnet_cv(
  alpha_value = 0,
  train_data = train_df,
  x_matrix = x_train,
  y_vector = y_train,
  fold_table = validation_folds,
  lambda_grid = ridge_lambda_grid
)

ridge_cv_summary = summarize_lambda_results(
  ridge_cv_results
)

best_ridge_lambda = ridge_cv_summary$Lambda[1]

cat("Best Ridge lambda:", best_ridge_lambda, "\n")
print(head(ridge_cv_summary, 10))

write.csv(
  ridge_cv_results,
  "08_Model_Results/R/Task5B_Ridge_Chronological_CV_All.csv",
  row.names = FALSE
)

write.csv(
  ridge_cv_summary,
  "08_Model_Results/R/Task5B_Ridge_Chronological_CV_Summary.csv",
  row.names = FALSE
)


# ----------------------------------------------------------------------------
# Step 20: Fit and Evaluate Final Ridge Model
# ----------------------------------------------------------------------------

ridge_final = glmnet(
  x_train,
  y_train,
  alpha = 0,
  lambda = best_ridge_lambda,
  family = "gaussian",
  standardize = TRUE
)

ridge_train_pred = as.numeric(
  predict(
    ridge_final,
    newx = x_train
  )
)

ridge_test_pred = as.numeric(
  predict(
    ridge_final,
    newx = x_test
  )
)

ridge_train_metrics = regression_metrics(
  y_train,
  ridge_train_pred
)

ridge_test_metrics = regression_metrics(
  y_test,
  ridge_test_pred
)

cat("Ridge training metrics:\n")
print(ridge_train_metrics)

cat("Ridge testing metrics:\n")
print(ridge_test_metrics)

ridge_coef_matrix = as.matrix(
  coef(ridge_final)
)

ridge_coef_table = data.frame(
  Variable = rownames(ridge_coef_matrix),
  Coefficient = as.numeric(ridge_coef_matrix[, 1]),
  row.names = NULL
)

print(ridge_coef_table)

write.csv(
  ridge_coef_table,
  "08_Model_Results/R/Task5B_Ridge_Coefficients.csv",
  row.names = FALSE
)


# ============================================================================
# PART E - LASSO REGRESSION
# ============================================================================


# ----------------------------------------------------------------------------
# Step 21: Tune LASSO Lambda with Chronological Validation
# ----------------------------------------------------------------------------

cat("\n============================================================\n")
cat("LASSO REGRESSION\n")
cat("============================================================\n")

lasso_path = glmnet(
  x_train,
  y_train,
  alpha = 1,
  family = "gaussian",
  standardize = TRUE,
  nlambda = 80
)

lasso_lambda_grid = lasso_path$lambda

lasso_cv_results = chronological_glmnet_cv(
  alpha_value = 1,
  train_data = train_df,
  x_matrix = x_train,
  y_vector = y_train,
  fold_table = validation_folds,
  lambda_grid = lasso_lambda_grid
)

lasso_cv_summary = summarize_lambda_results(
  lasso_cv_results
)

best_lasso_lambda = lasso_cv_summary$Lambda[1]

cat("Best LASSO lambda:", best_lasso_lambda, "\n")
print(head(lasso_cv_summary, 10))

write.csv(
  lasso_cv_results,
  "08_Model_Results/R/Task5B_LASSO_Chronological_CV_All.csv",
  row.names = FALSE
)

write.csv(
  lasso_cv_summary,
  "08_Model_Results/R/Task5B_LASSO_Chronological_CV_Summary.csv",
  row.names = FALSE
)


# ----------------------------------------------------------------------------
# Step 22: Fit and Evaluate Final LASSO Model
# ----------------------------------------------------------------------------

lasso_final = glmnet(
  x_train,
  y_train,
  alpha = 1,
  lambda = best_lasso_lambda,
  family = "gaussian",
  standardize = TRUE
)

lasso_train_pred = as.numeric(
  predict(
    lasso_final,
    newx = x_train
  )
)

lasso_test_pred = as.numeric(
  predict(
    lasso_final,
    newx = x_test
  )
)

lasso_train_metrics = regression_metrics(
  y_train,
  lasso_train_pred
)

lasso_test_metrics = regression_metrics(
  y_test,
  lasso_test_pred
)

cat("LASSO training metrics:\n")
print(lasso_train_metrics)

cat("LASSO testing metrics:\n")
print(lasso_test_metrics)

lasso_coef_matrix = as.matrix(
  coef(lasso_final)
)

lasso_coef_table = data.frame(
  Variable = rownames(lasso_coef_matrix),
  Coefficient = as.numeric(lasso_coef_matrix[, 1]),
  row.names = NULL
)

lasso_selected = lasso_coef_table[
  lasso_coef_table$Variable != "(Intercept)" &
  lasso_coef_table$Coefficient != 0,
]

lasso_dropped = lasso_coef_table[
  lasso_coef_table$Variable != "(Intercept)" &
  lasso_coef_table$Coefficient == 0,
]

cat("\nLASSO selected predictors:\n")
print(lasso_selected)

cat("\nLASSO predictors shrunk to zero:\n")
print(lasso_dropped)

cat(
  "LASSO retained",
  nrow(lasso_selected),
  "of",
  ncol(x_train),
  "encoded predictors.\n"
)

write.csv(
  lasso_coef_table,
  "08_Model_Results/R/Task5B_LASSO_Coefficients.csv",
  row.names = FALSE
)

write.csv(
  lasso_selected,
  "08_Model_Results/R/Task5B_LASSO_Selected_Predictors.csv",
  row.names = FALSE
)

write.csv(
  lasso_dropped,
  "08_Model_Results/R/Task5B_LASSO_Dropped_Predictors.csv",
  row.names = FALSE
)


# ============================================================================
# PART F - ELASTIC NET REGRESSION
# ============================================================================


# ----------------------------------------------------------------------------
# Step 23: Tune Elastic Net Alpha and Lambda
# ----------------------------------------------------------------------------
# Alpha = 0 is Ridge and Alpha = 1 is LASSO.
# Intermediate alpha values combine both penalties.

cat("\n============================================================\n")
cat("ELASTIC NET REGRESSION\n")
cat("============================================================\n")

elastic_alpha_grid = c(
  0.25,
  0.50,
  0.75
)

elastic_all_results = data.frame()
elastic_all_summary = data.frame()

for(alpha_value in elastic_alpha_grid) {

  elastic_path = glmnet(
    x_train,
    y_train,
    alpha = alpha_value,
    family = "gaussian",
    standardize = TRUE,
    nlambda = 80
  )

  elastic_lambda_grid = elastic_path$lambda

  elastic_cv = chronological_glmnet_cv(
    alpha_value = alpha_value,
    train_data = train_df,
    x_matrix = x_train,
    y_vector = y_train,
    fold_table = validation_folds,
    lambda_grid = elastic_lambda_grid
  )

  elastic_summary = summarize_lambda_results(
    elastic_cv
  )

  elastic_all_results = rbind(
    elastic_all_results,
    elastic_cv
  )

  elastic_all_summary = rbind(
    elastic_all_summary,
    elastic_summary
  )
}

elastic_all_summary = elastic_all_summary[
  order(elastic_all_summary$Mean_RMSE),
]

rownames(elastic_all_summary) = NULL

best_elastic_alpha = elastic_all_summary$Alpha[1]
best_elastic_lambda = elastic_all_summary$Lambda[1]

cat("Best Elastic Net alpha:", best_elastic_alpha, "\n")
cat("Best Elastic Net lambda:", best_elastic_lambda, "\n")
print(head(elastic_all_summary, 15))

write.csv(
  elastic_all_results,
  "08_Model_Results/R/Task5B_ElasticNet_Chronological_CV_All.csv",
  row.names = FALSE
)

write.csv(
  elastic_all_summary,
  "08_Model_Results/R/Task5B_ElasticNet_Chronological_CV_Summary.csv",
  row.names = FALSE
)


# ----------------------------------------------------------------------------
# Step 24: Fit and Evaluate Final Elastic Net Model
# ----------------------------------------------------------------------------

elastic_final = glmnet(
  x_train,
  y_train,
  alpha = best_elastic_alpha,
  lambda = best_elastic_lambda,
  family = "gaussian",
  standardize = TRUE
)

elastic_train_pred = as.numeric(
  predict(
    elastic_final,
    newx = x_train
  )
)

elastic_test_pred = as.numeric(
  predict(
    elastic_final,
    newx = x_test
  )
)

elastic_train_metrics = regression_metrics(
  y_train,
  elastic_train_pred
)

elastic_test_metrics = regression_metrics(
  y_test,
  elastic_test_pred
)

cat("Elastic Net training metrics:\n")
print(elastic_train_metrics)

cat("Elastic Net testing metrics:\n")
print(elastic_test_metrics)

elastic_coef_matrix = as.matrix(
  coef(elastic_final)
)

elastic_coef_table = data.frame(
  Variable = rownames(elastic_coef_matrix),
  Coefficient = as.numeric(elastic_coef_matrix[, 1]),
  row.names = NULL
)

elastic_selected = elastic_coef_table[
  elastic_coef_table$Variable != "(Intercept)" &
  elastic_coef_table$Coefficient != 0,
]

elastic_dropped = elastic_coef_table[
  elastic_coef_table$Variable != "(Intercept)" &
  elastic_coef_table$Coefficient == 0,
]

cat("\nElastic Net selected predictors:\n")
print(elastic_selected)

cat("\nElastic Net predictors shrunk to zero:\n")
print(elastic_dropped)

write.csv(
  elastic_coef_table,
  "08_Model_Results/R/Task5B_ElasticNet_Coefficients.csv",
  row.names = FALSE
)

write.csv(
  elastic_selected,
  "08_Model_Results/R/Task5B_ElasticNet_Selected_Predictors.csv",
  row.names = FALSE
)


# ============================================================================
# PART G - FINAL MODEL COMPARISON
# ============================================================================


# ----------------------------------------------------------------------------
# Step 25: Build Comprehensive Model Comparison Table
# ----------------------------------------------------------------------------

model_comparison = data.frame(
  Model = c(
    "Training Mean Baseline",
    "Reduced MLR",
    "Poisson GLM",
    "Negative Binomial GLM",
    "Ridge Regression",
    "LASSO Regression",
    "Elastic Net Regression"
  ),

  Train_MAE = c(
    baseline_train_metrics["MAE"],
    mlr_train_metrics["MAE"],
    poisson_train_metrics["MAE"],
    nb_train_metrics["MAE"],
    ridge_train_metrics["MAE"],
    lasso_train_metrics["MAE"],
    elastic_train_metrics["MAE"]
  ),

  Train_RMSE = c(
    baseline_train_metrics["RMSE"],
    mlr_train_metrics["RMSE"],
    poisson_train_metrics["RMSE"],
    nb_train_metrics["RMSE"],
    ridge_train_metrics["RMSE"],
    lasso_train_metrics["RMSE"],
    elastic_train_metrics["RMSE"]
  ),

  Train_R2 = c(
    baseline_train_metrics["R2"],
    mlr_train_metrics["R2"],
    poisson_train_metrics["R2"],
    nb_train_metrics["R2"],
    ridge_train_metrics["R2"],
    lasso_train_metrics["R2"],
    elastic_train_metrics["R2"]
  ),

  Test_MAE = c(
    baseline_test_metrics["MAE"],
    mlr_test_metrics["MAE"],
    poisson_test_metrics["MAE"],
    nb_test_metrics["MAE"],
    ridge_test_metrics["MAE"],
    lasso_test_metrics["MAE"],
    elastic_test_metrics["MAE"]
  ),

  Test_RMSE = c(
    baseline_test_metrics["RMSE"],
    mlr_test_metrics["RMSE"],
    poisson_test_metrics["RMSE"],
    nb_test_metrics["RMSE"],
    ridge_test_metrics["RMSE"],
    lasso_test_metrics["RMSE"],
    elastic_test_metrics["RMSE"]
  ),

  Test_R2 = c(
    baseline_test_metrics["R2"],
    mlr_test_metrics["R2"],
    poisson_test_metrics["R2"],
    nb_test_metrics["R2"],
    ridge_test_metrics["R2"],
    lasso_test_metrics["R2"],
    elastic_test_metrics["R2"]
  ),

  AIC = c(
    NA,
    AIC(reduced_mlr),
    AIC(poisson_model),
    AIC(nb_model),
    NA,
    NA,
    NA
  ),

  Adjusted_R2 = c(
    NA,
    summary(reduced_mlr)$adj.r.squared,
    NA,
    NA,
    NA,
    NA,
    NA
  ),

  Tuning = c(
    "None",
    "Training-only specification comparison",
    "None",
    "Poisson overdispersion motivated NB comparison",
    paste0("Lambda = ", signif(best_ridge_lambda, 6)),
    paste0("Lambda = ", signif(best_lasso_lambda, 6)),
    paste0(
      "Alpha = ",
      best_elastic_alpha,
      "; Lambda = ",
      signif(best_elastic_lambda, 6)
    )
  ),

  stringsAsFactors = FALSE
)

model_comparison = model_comparison[
  order(model_comparison$Test_RMSE),
]

rownames(model_comparison) = NULL

print(model_comparison)

write.csv(
  model_comparison,
  "08_Model_Results/R/Task5B_Final_Model_Comparison.csv",
  row.names = FALSE
)


# ----------------------------------------------------------------------------
# Step 26: Identify Best Holdout Prediction Metric
# ----------------------------------------------------------------------------
# This identifies the lowest test RMSE only.
# It is NOT automatically the final organizational recommendation because the
# assignment requires interpretability, assumptions and usability as well.

best_test_model = model_comparison$Model[1]
best_test_rmse = model_comparison$Test_RMSE[1]
best_test_mae = model_comparison$Test_MAE[1]
best_test_r2 = model_comparison$Test_R2[1]

cat("\nLOWEST HOLDOUT RMSE MODEL\n")
cat("Model:", best_test_model, "\n")
cat("Test MAE:", best_test_mae, "\n")
cat("Test RMSE:", best_test_rmse, "\n")
cat("Test R2:", best_test_r2, "\n")

cat(
  "Important: final organizational model selection must also consider assumptions, interpretability, robustness and business usability.\n"
)


# ----------------------------------------------------------------------------
# Step 27: Create Model Strengths and Limitations Table
# ----------------------------------------------------------------------------

model_evaluation_notes = data.frame(
  Model = c(
    "Reduced MLR",
    "Poisson GLM",
    "Negative Binomial GLM",
    "Ridge Regression",
    "LASSO Regression",
    "Elastic Net Regression"
  ),
  Main_Strength = c(
    "Simple, interpretable coefficients and strong benchmark performance.",
    "Natural count-model structure with positive fitted means and multiplicative interpretation.",
    "Handles overdispersion better than Poisson while preserving a count-model interpretation.",
    "Stabilizes correlated predictors through coefficient shrinkage.",
    "Performs regularization and can remove weak encoded predictors.",
    "Combines Ridge shrinkage and LASSO variable selection."
  ),
  Main_Limitation = c(
    "Residual heteroscedasticity requires robust inference and predictions shrink toward the middle.",
    "Poisson assumes conditional mean and variance are similar; strong overdispersion makes this unsuitable.",
    "Less straightforward to explain than MLR and still inherits observational-data limitations.",
    "Does not set coefficients exactly to zero and is less directly interpretable after standardization/shrinkage.",
    "Selected variables can depend on the tuning period and correlated predictors may be selected unstably.",
    "Requires tuning both alpha and lambda and is less transparent to non-technical users."
  ),
  stringsAsFactors = FALSE
)

print(model_evaluation_notes)

write.csv(
  model_evaluation_notes,
  "08_Model_Results/R/Task5B_Model_Strengths_Limitations.csv",
  row.names = FALSE
)


# ============================================================================
# PART H - VISUALIZATIONS
# ============================================================================


# ----------------------------------------------------------------------------
# Step 28: Poisson Actual vs Predicted
# ----------------------------------------------------------------------------

png(
  "06_Charts/Task5B_01_Poisson_Actual_vs_Predicted.png",
  width = 1000,
  height = 700
)

plot(
  test_df$Quantity,
  poisson_test_pred,
  main = "Poisson GLM: Actual vs Predicted Quantity",
  xlab = "Actual Quantity",
  ylab = "Predicted Quantity"
)

abline(0, 1, col = "red", lty = 2)

dev.off()


# ----------------------------------------------------------------------------
# Step 29: Negative Binomial Actual vs Predicted
# ----------------------------------------------------------------------------

png(
  "06_Charts/Task5B_02_Negative_Binomial_Actual_vs_Predicted.png",
  width = 1000,
  height = 700
)

plot(
  test_df$Quantity,
  nb_test_pred,
  main = "Negative Binomial GLM: Actual vs Predicted Quantity",
  xlab = "Actual Quantity",
  ylab = "Predicted Quantity"
)

abline(0, 1, col = "red", lty = 2)

dev.off()


# ----------------------------------------------------------------------------
# Step 30: Ridge Chronological CV Curve
# ----------------------------------------------------------------------------

ridge_plot_data = ridge_cv_summary[
  order(ridge_cv_summary$Lambda),
]

png(
  "06_Charts/Task5B_03_Ridge_Chronological_CV.png",
  width = 1000,
  height = 700
)

plot(
  log(ridge_plot_data$Lambda),
  ridge_plot_data$Mean_RMSE,
  type = "l",
  main = "Ridge: Chronological Validation RMSE",
  xlab = "log(Lambda)",
  ylab = "Mean Validation RMSE"
)

abline(
  v = log(best_ridge_lambda),
  col = "red",
  lty = 2
)

dev.off()


# ----------------------------------------------------------------------------
# Step 31: LASSO Chronological CV Curve
# ----------------------------------------------------------------------------

lasso_plot_data = lasso_cv_summary[
  order(lasso_cv_summary$Lambda),
]

png(
  "06_Charts/Task5B_04_LASSO_Chronological_CV.png",
  width = 1000,
  height = 700
)

plot(
  log(lasso_plot_data$Lambda),
  lasso_plot_data$Mean_RMSE,
  type = "l",
  main = "LASSO: Chronological Validation RMSE",
  xlab = "log(Lambda)",
  ylab = "Mean Validation RMSE"
)

abline(
  v = log(best_lasso_lambda),
  col = "red",
  lty = 2
)

dev.off()


# ----------------------------------------------------------------------------
# Step 32: Elastic Net Alpha Comparison
# ----------------------------------------------------------------------------

elastic_alpha_summary = data.frame()

for(alpha_value in elastic_alpha_grid) {

  temp = elastic_all_summary[
    elastic_all_summary$Alpha == alpha_value,
  ]

  best_temp = temp[
    which.min(temp$Mean_RMSE),
  ]

  elastic_alpha_summary = rbind(
    elastic_alpha_summary,
    data.frame(
      Alpha = alpha_value,
      Best_Lambda = best_temp$Lambda,
      Mean_MAE = best_temp$Mean_MAE,
      Mean_RMSE = best_temp$Mean_RMSE,
      Mean_R2 = best_temp$Mean_R2
    )
  )
}

print(elastic_alpha_summary)

write.csv(
  elastic_alpha_summary,
  "08_Model_Results/R/Task5B_ElasticNet_Alpha_Comparison.csv",
  row.names = FALSE
)

png(
  "06_Charts/Task5B_05_ElasticNet_Alpha_Comparison.png",
  width = 900,
  height = 700
)

plot(
  elastic_alpha_summary$Alpha,
  elastic_alpha_summary$Mean_RMSE,
  type = "o",
  pch = 16,
  main = "Elastic Net: Alpha Comparison",
  xlab = "Alpha",
  ylab = "Best Mean Validation RMSE"
)

dev.off()


# ----------------------------------------------------------------------------
# Step 33: Final Test RMSE Comparison Chart
# ----------------------------------------------------------------------------

plot_comparison = model_comparison[
  model_comparison$Model != "Training Mean Baseline",
]

png(
  "06_Charts/Task5B_06_Final_Test_RMSE_Comparison.png",
  width = 1200,
  height = 750
)

barplot(
  plot_comparison$Test_RMSE,
  names.arg = plot_comparison$Model,
  main = "Predictive Model Comparison - Test RMSE",
  xlab = "Model",
  ylab = "Test RMSE",
  col = "steelblue",
  las = 2
)

dev.off()


# ============================================================================
# PART I - REPORT-READY SUMMARY
# ============================================================================


# ----------------------------------------------------------------------------
# Step 34: Save Report-Ready Task 5B Summary
# ----------------------------------------------------------------------------

sink(
  "08_Model_Results/R/Task5B_Report_Ready_Summary.txt"
)

cat("TASK 5B - ALTERNATIVE PREDICTIVE MODELS\n\n")

cat("MODELLING POPULATION\n")
cat("Current-regime Regular Orders:", nrow(current_df), "\n")
cat("Training observations:", nrow(train_df), "\n")
cat("Testing observations:", nrow(test_df), "\n")
cat("Training period: 2025-04-01 to 2026-03-31\n")
cat("Testing period: 2026-04-01 to 2026-06-30\n\n")

cat("POISSON GLM\n")
cat("Poisson AIC:", round(AIC(poisson_model), 4), "\n")
cat("Poisson residual deviance:", round(deviance(poisson_model), 4), "\n")
cat("Poisson Pearson dispersion ratio:", round(poisson_dispersion, 4), "\n")
cat("Poisson test MAE:", round(poisson_test_metrics["MAE"], 4), "\n")
cat("Poisson test RMSE:", round(poisson_test_metrics["RMSE"], 4), "\n")
cat("Poisson test R-squared:", round(poisson_test_metrics["R2"], 4), "\n")

if(poisson_dispersion > 1.5) {
  cat(
    "Interpretation: The Pearson dispersion ratio indicates strong overdispersion, so the Poisson mean-variance assumption is not adequate for this dataset.\n\n"
  )
} else {
  cat(
    "Interpretation: Strong Poisson overdispersion was not detected by the Pearson dispersion ratio.\n\n"
  )
}

cat("NEGATIVE BINOMIAL GLM\n")
cat("Theta:", round(nb_model$theta, 4), "\n")
cat("AIC:", round(AIC(nb_model), 4), "\n")
cat("Test MAE:", round(nb_test_metrics["MAE"], 4), "\n")
cat("Test RMSE:", round(nb_test_metrics["RMSE"], 4), "\n")
cat("Test R-squared:", round(nb_test_metrics["R2"], 4), "\n")
cat(
  "Interpretation: Negative Binomial regression was evaluated because it allows variance to exceed the mean and is therefore more flexible than Poisson for overdispersed count-like demand.\n\n"
)

cat("RIDGE REGRESSION\n")
cat("Optimal chronological-validation lambda:", signif(best_ridge_lambda, 8), "\n")
cat("Test MAE:", round(ridge_test_metrics["MAE"], 4), "\n")
cat("Test RMSE:", round(ridge_test_metrics["RMSE"], 4), "\n")
cat("Test R-squared:", round(ridge_test_metrics["R2"], 4), "\n")
cat(
  "Interpretation: Ridge was used to shrink correlated encoded predictors without deleting them.\n\n"
)

cat("LASSO REGRESSION\n")
cat("Optimal chronological-validation lambda:", signif(best_lasso_lambda, 8), "\n")
cat("Encoded predictors retained:", nrow(lasso_selected), "of", ncol(x_train), "\n")
cat("Test MAE:", round(lasso_test_metrics["MAE"], 4), "\n")
cat("Test RMSE:", round(lasso_test_metrics["RMSE"], 4), "\n")
cat("Test R-squared:", round(lasso_test_metrics["R2"], 4), "\n")
cat(
  "Interpretation: LASSO was used for both shrinkage and automatic selection of encoded predictors.\n\n"
)

cat("ELASTIC NET REGRESSION\n")
cat("Selected alpha:", best_elastic_alpha, "\n")
cat("Selected lambda:", signif(best_elastic_lambda, 8), "\n")
cat("Test MAE:", round(elastic_test_metrics["MAE"], 4), "\n")
cat("Test RMSE:", round(elastic_test_metrics["RMSE"], 4), "\n")
cat("Test R-squared:", round(elastic_test_metrics["R2"], 4), "\n")
cat(
  "Interpretation: Elastic Net combines Ridge and LASSO penalties and was tuned using chronological validation.\n\n"
)

cat("MODEL COMPARISON\n")
cat("Lowest holdout RMSE model:", best_test_model, "\n")
cat("Lowest holdout RMSE:", round(best_test_rmse, 4), "\n")
cat("Corresponding MAE:", round(best_test_mae, 4), "\n")
cat("Corresponding R-squared:", round(best_test_r2, 4), "\n\n")

cat(
  "Important: Prediction accuracy alone does not determine the final organizational recommendation. The final choice must also consider model assumptions, interpretability, robustness, ease of implementation and business usefulness.\n\n"
)

cat("GENERAL INTERPRETATION LIMITATIONS\n")
cat("- Historical observational data do not establish causality.\n")
cat("- Japan transactions are structurally concentrated within one buyer and the Main Dealer/current-regime structure.\n")
cat("- Repeated transactions from the same buyers may violate complete observation-level independence.\n")
cat("- The models predict transaction-level Quantity, not aggregate monthly demand.\n")
cat("- The Apr-Jun 2026 holdout period is one future evaluation period and does not guarantee identical future performance.\n")
cat("- Regularized-model coefficients are affected by shrinkage and should not be interpreted like ordinary unpenalized regression coefficients.\n")

sink()


# ============================================================================
# PART J - FINAL VALIDATION
# ============================================================================


# ----------------------------------------------------------------------------
# Step 35: Validate Task 5B Outputs
# ----------------------------------------------------------------------------

stopifnot(
  # Data and split
  nrow(current_df) == 2014,
  nrow(train_df) == 1599,
  nrow(test_df) == 415,
  max(train_df$Date) < min(test_df$Date),

  # Reference Task 5A results reproduced
  abs(baseline_test_metrics["MAE"] - 137.0608) < 0.02,
  abs(baseline_test_metrics["RMSE"] - 178.3421) < 0.02,
  abs(mlr_test_metrics["MAE"] - 116.8403) < 0.02,
  abs(mlr_test_metrics["RMSE"] - 137.9482) < 0.02,
  abs(mlr_test_metrics["R2"] - 0.3987) < 0.001,

  # Poisson / NB validity
  all(is.finite(poisson_train_pred)),
  all(is.finite(poisson_test_pred)),
  all(poisson_train_pred > 0),
  all(poisson_test_pred > 0),
  is.finite(poisson_dispersion),
  poisson_dispersion > 0,
  all(is.finite(nb_train_pred)),
  all(is.finite(nb_test_pred)),
  all(nb_train_pred > 0),
  all(nb_test_pred > 0),
  is.finite(nb_model$theta),
  nb_model$theta > 0,

  # Regularized tuning
  is.finite(best_ridge_lambda),
  best_ridge_lambda > 0,
  is.finite(best_lasso_lambda),
  best_lasso_lambda > 0,
  best_elastic_alpha %in% elastic_alpha_grid,
  is.finite(best_elastic_lambda),
  best_elastic_lambda > 0,

  # Regularized predictions
  all(is.finite(ridge_test_pred)),
  all(is.finite(lasso_test_pred)),
  all(is.finite(elastic_test_pred)),

  # Comparison
  nrow(model_comparison) == 7,
  all(is.finite(model_comparison$Test_MAE)),
  all(is.finite(model_comparison$Test_RMSE)),
  all(is.finite(model_comparison$Test_R2))
)

print("All Task 5B validation checks passed.")


# ----------------------------------------------------------------------------
# Step 36: Show Created Output Files
# ----------------------------------------------------------------------------

cat("\nTask 5B result files:\n")

print(
  list.files(
    "08_Model_Results/R",
    pattern = "^Task5B",
    full.names = TRUE
  )
)

cat("\nTask 5B chart files:\n")

print(
  list.files(
    "06_Charts",
    pattern = "^Task5B",
    full.names = TRUE
  )
)


# ----------------------------------------------------------------------------
# Step 37: Final Message
# ----------------------------------------------------------------------------

cat("\n============================================================\n")
cat("TASK 5B COMPLETE\n")
cat("============================================================\n")

cat("Poisson dispersion ratio:", round(poisson_dispersion, 4), "\n")
cat("Negative Binomial theta:", round(nb_model$theta, 4), "\n")
cat("Ridge best lambda:", signif(best_ridge_lambda, 6), "\n")
cat("LASSO best lambda:", signif(best_lasso_lambda, 6), "\n")
cat("Elastic Net best alpha:", best_elastic_alpha, "\n")
cat("Elastic Net best lambda:", signif(best_elastic_lambda, 6), "\n")
cat("Lowest holdout RMSE model:", best_test_model, "\n")
cat("Lowest holdout RMSE:", round(best_test_rmse, 4), "\n")
cat("Task 5B alternative predictive modelling completed successfully.\n")
