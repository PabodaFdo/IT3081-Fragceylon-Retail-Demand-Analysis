# ============================================================
# IT3081 - Statistical Modelling
# Retail Product Demand Analysis at Fragceylon
# TASK 8 - CRITICAL EVALUATION OF BAYESIAN STATISTICAL METHODS
# R SCRIPT - APPLICABILITY + EXPLORATORY DEMONSTRATIONS
# ============================================================
#
# IMPORTANT:
# Task 8 requires critical evaluation of:
# 1. Naive Bayes
# 2. Bayesian Regression
# 3. Bayesian Decision Making
#
#
# The purpose is to:
# 1. Evaluate whether each Bayesian approach fits the Fragceylon problem.
# 2. Demonstrate Naive Bayes on LOW / MEDIUM / HIGH demand classes.
# 3. Quantify information loss caused by converting Quantity to classes.
# 4. Demonstrate a normal-prior Bayesian regression sensitivity analysis.
# 5. Illustrate Bayesian decision making using posterior class
#    probabilities and hypothetical loss functions.
# 6. Save report-ready tables, charts and interpretation notes.
#
# No company-specific prior beliefs or inventory cost values are invented.
# Prior scales and decision losses used below are explicitly labelled
# as illustrative sensitivity assumptions only.
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

df = read_excel(
  regular_file
)

df$Date = as.Date(
  df$Date
)

cat(
  "\n============================================================\n"
)

cat(
  "TASK 8 - BAYESIAN METHODS DATA CONTEXT\n"
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
  "Quantity range:",
  min(df$Quantity),
  "to",
  max(df$Quantity),
  "\n"
)

cat(
  "Unique Quantity values:",
  length(unique(df$Quantity)),
  "\n"
)


# ============================================================
# 5. USE SAME CURRENT REGIME AND CHRONOLOGICAL SPLIT AS TASK 5
# ============================================================

current_df =
  df[
    df$Date >=
      as.Date("2025-04-01"),
  ]

current_df =
  current_df[
    order(
      current_df$Date,
      current_df$`Order ID`
    ),
  ]

rownames(
  current_df
) = NULL

TEST_START_DATE =
  as.Date("2026-04-01")

train_df =
  current_df[
    current_df$Date <
      TEST_START_DATE,
  ]

test_df =
  current_df[
    current_df$Date >=
      TEST_START_DATE,
  ]

train_df =
  train_df[
    order(
      train_df$Date,
      train_df$`Order ID`
    ),
  ]

test_df =
  test_df[
    order(
      test_df$Date,
      test_df$`Order ID`
    ),
  ]

rownames(
  train_df
) = NULL

rownames(
  test_df
) = NULL

cat(
  "\nCURRENT REGIME / HOLDOUT SPLIT\n"
)

cat(
  "Current-regime rows:",
  nrow(current_df),
  "\n"
)

cat(
  "Training rows:",
  nrow(train_df),
  "\n"
)

cat(
  "Testing rows:",
  nrow(test_df),
  "\n"
)

cat(
  "Training period:",
  format(min(train_df$Date)),
  "to",
  format(max(train_df$Date)),
  "\n"
)

cat(
  "Testing period:",
  format(min(test_df$Date)),
  "to",
  format(max(test_df$Date)),
  "\n"
)


# ============================================================
# 6. SET FACTOR LEVELS CONSISTENTLY
# ============================================================

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

train_df$`Product Name` =
  factor(
    train_df$`Product Name`,
    levels = product_levels
  )

test_df$`Product Name` =
  factor(
    test_df$`Product Name`,
    levels = product_levels
  )

train_df$`Export Market` =
  factor(
    train_df$`Export Market`,
    levels = market_levels
  )

test_df$`Export Market` =
  factor(
    test_df$`Export Market`,
    levels = market_levels
  )

train_df$Month =
  factor(
    train_df$Month,
    levels = month_levels
  )

test_df$Month =
  factor(
    test_df$Month,
    levels = month_levels
  )

train_df$Day_of_Week =
  factor(
    train_df$Day_of_Week,
    levels = day_levels
  )

test_df$Day_of_Week =
  factor(
    test_df$Day_of_Week,
    levels = day_levels
  )


# ============================================================
# 7. TASK 8A - NAIVE BAYES APPLICABILITY
# ============================================================
#
# Naive Bayes is naturally a classification method.
#
# Fragceylon's main outcome Quantity is numeric.
# To demonstrate Naive Bayes, Quantity must first be converted into
# categories. This creates information loss and therefore should be
# treated as an exploratory classification use case, not a replacement
# for the Task 5 numerical demand models.
#
# Demand thresholds are derived using TRAINING DATA ONLY.
# ============================================================

q_low =
  as.numeric(
    quantile(
      train_df$Quantity,
      probs = 1 / 3,
      type = 7
    )
  )

q_high =
  as.numeric(
    quantile(
      train_df$Quantity,
      probs = 2 / 3,
      type = 7
    )
  )

cat(
  "\n============================================================\n"
)

cat(
  "NAIVE BAYES DEMAND CLASS DEFINITION\n"
)

cat(
  "============================================================\n"
)

cat(
  "Low upper threshold:",
  q_low,
  "\n"
)

cat(
  "Medium upper threshold:",
  q_high,
  "\n"
)

create_demand_class = function(
  quantity
) {
  cut(
    quantity,
    breaks = c(
      -Inf,
      q_low,
      q_high,
      Inf
    ),
    labels = c(
      "Low",
      "Medium",
      "High"
    ),
    right = TRUE,
    ordered_result = FALSE
  )
}

train_df$Demand_Class =
  create_demand_class(
    train_df$Quantity
  )

test_df$Demand_Class =
  create_demand_class(
    test_df$Quantity
  )

train_df$Demand_Class =
  factor(
    train_df$Demand_Class,
    levels = c(
      "Low",
      "Medium",
      "High"
    )
  )

test_df$Demand_Class =
  factor(
    test_df$Demand_Class,
    levels = c(
      "Low",
      "Medium",
      "High"
    )
  )

cat(
  "\nTraining class distribution:\n"
)

print(
  table(
    train_df$Demand_Class
  )
)

cat(
  "\nTesting class distribution:\n"
)

print(
  table(
    test_df$Demand_Class
  )
)

class_definition = data.frame(
  Demand_Class = c(
    "Low",
    "Medium",
    "High"
  ),
  Quantity_Rule = c(
    paste0(
      "Quantity <= ",
      round(q_low, 4)
    ),
    paste0(
      round(q_low, 4),
      " < Quantity <= ",
      round(q_high, 4)
    ),
    paste0(
      "Quantity > ",
      round(q_high, 4)
    )
  ),
  Threshold_Source = c(
    "Training-data tertile",
    "Training-data tertiles",
    "Training-data tertile"
  ),
  stringsAsFactors = FALSE
)

print(
  class_definition
)

write.csv(
  class_definition,
  "08_Model_Results/R/Task8_Naive_Bayes_Demand_Class_Definition.csv",
  row.names = FALSE
)


# ============================================================
# 8. QUANTIFY INFORMATION LOSS FROM CATEGORIZATION
# ============================================================

class_levels = c(
  "Low",
  "Medium",
  "High"
)

class_information = do.call(
  rbind,
  lapply(
    class_levels,
    function(class_name) {
      x =
        train_df$Quantity[
          train_df$Demand_Class ==
            class_name
        ]

      data.frame(
        Demand_Class =
          class_name,
        N =
          length(x),
        Minimum =
          min(x),
        Maximum =
          max(x),
        Mean =
          mean(x),
        Median =
          median(x),
        SD =
          sd(x),
        Unique_Quantity_Values =
          length(
            unique(x)
          ),
        stringsAsFactors = FALSE
      )
    }
  )
)

information_loss_summary = data.frame(
  Measure = c(
    "Unique training Quantity values",
    "Demand classes after conversion",
    "Percentage reduction in response-state resolution"
  ),
  Value = c(
    length(
      unique(
        train_df$Quantity
      )
    ),
    length(class_levels),
    (
      1 -
      length(class_levels) /
      length(
        unique(
          train_df$Quantity
        )
      )
    ) * 100
  ),
  stringsAsFactors = FALSE
)

cat(
  "\nDemand-class information summary:\n"
)

print(
  class_information
)

print(
  information_loss_summary
)

write.csv(
  class_information,
  "08_Model_Results/R/Task8_Naive_Bayes_Class_Information.csv",
  row.names = FALSE
)

write.csv(
  information_loss_summary,
  "08_Model_Results/R/Task8_Naive_Bayes_Information_Loss_Summary.csv",
  row.names = FALSE
)


# ============================================================
# 9. MANUAL CATEGORICAL NAIVE BAYES
# ============================================================
#
# Predictors:
# Product Name
# Export Market
# Month
# Day of Week
#
# These are categorical and therefore fit a categorical Naive Bayes
# illustration naturally.
#
# Laplace smoothing = 1.
#
# We implement the classifier manually so that no additional package
# is required.
# ============================================================

nb_predictors = c(
  "Product Name",
  "Export Market",
  "Month",
  "Day_of_Week"
)

LAPLACE_ALPHA = 1

fit_categorical_naive_bayes = function(
  data,
  response,
  predictors,
  alpha = 1
) {
  classes =
    levels(
      data[[response]]
    )

  n =
    nrow(data)

  class_counts =
    table(
      data[[response]]
    )

  class_priors =
    (
      class_counts +
      alpha
    ) /
    (
      n +
      alpha *
      length(classes)
    )

  conditional_tables =
    list()

  for(predictor in predictors) {
    predictor_levels =
      levels(
        data[[predictor]]
      )

    prob_matrix =
      matrix(
        0,
        nrow =
          length(classes),
        ncol =
          length(predictor_levels),
        dimnames = list(
          classes,
          predictor_levels
        )
      )

    for(class_name in classes) {
      class_rows =
        data[
          data[[response]] ==
            class_name,
        ]

      counts =
        table(
          factor(
            class_rows[[predictor]],
            levels = predictor_levels
          )
        )

      prob_matrix[
        class_name,
      ] =
        (
          as.numeric(counts) +
          alpha
        ) /
        (
          nrow(class_rows) +
          alpha *
          length(predictor_levels)
        )
    }

    conditional_tables[[predictor]] =
      prob_matrix
  }

  return(
    list(
      classes = classes,
      priors = class_priors,
      conditional =
        conditional_tables,
      predictors = predictors,
      alpha = alpha
    )
  )
}

predict_categorical_naive_bayes = function(
  model,
  newdata
) {
  classes =
    model$classes

  posterior_matrix =
    matrix(
      0,
      nrow =
        nrow(newdata),
      ncol =
        length(classes),
      dimnames = list(
        NULL,
        classes
      )
    )

  predicted_class =
    character(
      nrow(newdata)
    )

  for(i in 1:nrow(newdata)) {
    log_prob =
      log(
        as.numeric(
          model$priors[
            classes
          ]
        )
      )

    names(log_prob) =
      classes

    for(predictor in model$predictors) {
      value =
        as.character(
          newdata[[predictor]][i]
        )

      probability_table =
        model$conditional[[predictor]]

      if(
        value %in%
        colnames(
          probability_table
        )
      ) {
        log_prob =
          log_prob +
          log(
            probability_table[
              classes,
              value
            ]
          )
      }
    }

    max_log =
      max(log_prob)

    unnormalized =
      exp(
        log_prob -
        max_log
      )

    posterior =
      unnormalized /
      sum(
        unnormalized
      )

    posterior_matrix[
      i,
    ] =
      posterior

    predicted_class[i] =
      classes[
        which.max(
          posterior
        )
      ]
  }

  return(
    list(
      class = factor(
        predicted_class,
        levels = classes
      ),
      posterior =
        posterior_matrix
    )
  )
}


# ============================================================
# 10. FIT AND PREDICT NAIVE BAYES
# ============================================================

nb_model =
  fit_categorical_naive_bayes(
    data = train_df,
    response = "Demand_Class",
    predictors = nb_predictors,
    alpha = LAPLACE_ALPHA
  )

nb_test =
  predict_categorical_naive_bayes(
    model = nb_model,
    newdata = test_df
  )

nb_confusion =
  table(
    Actual =
      test_df$Demand_Class,
    Predicted =
      nb_test$class
  )

cat(
  "\n============================================================\n"
)

cat(
  "NAIVE BAYES CONFUSION MATRIX\n"
)

cat(
  "============================================================\n"
)

print(
  nb_confusion
)


# ============================================================
# 11. NAIVE BAYES CLASSIFICATION METRICS
# ============================================================

accuracy =
  sum(
    diag(
      nb_confusion
    )
  ) /
  sum(
    nb_confusion
  )

per_class_metrics =
  data.frame()

for(class_name in class_levels) {
  tp =
    nb_confusion[
      class_name,
      class_name
    ]

  fp =
    sum(
      nb_confusion[
        ,
        class_name
      ]
    ) -
    tp

  fn =
    sum(
      nb_confusion[
        class_name,
      ]
    ) -
    tp

  tn =
    sum(
      nb_confusion
    ) -
    tp -
    fp -
    fn

  precision =
    ifelse(
      tp + fp == 0,
      NA,
      tp /
        (
          tp +
          fp
        )
    )

  recall =
    ifelse(
      tp + fn == 0,
      NA,
      tp /
        (
          tp +
          fn
        )
    )

  f1 =
    ifelse(
      is.na(precision) |
      is.na(recall) |
      precision + recall == 0,
      NA,
      2 *
        precision *
        recall /
        (
          precision +
          recall
        )
    )

  specificity =
    ifelse(
      tn + fp == 0,
      NA,
      tn /
        (
          tn +
          fp
        )
    )

  per_class_metrics =
    rbind(
      per_class_metrics,
      data.frame(
        Demand_Class =
          class_name,
        Precision =
          precision,
        Recall =
          recall,
        F1 =
          f1,
        Specificity =
          specificity,
        stringsAsFactors = FALSE
      )
    )
}

macro_f1 =
  mean(
    per_class_metrics$F1,
    na.rm = TRUE
  )

balanced_accuracy =
  mean(
    per_class_metrics$Recall,
    na.rm = TRUE
  )

majority_class =
  names(
    which.max(
      table(
        train_df$Demand_Class
      )
    )
  )

majority_prediction =
  factor(
    rep(
      majority_class,
      nrow(test_df)
    ),
    levels = class_levels
  )

majority_accuracy =
  mean(
    majority_prediction ==
      test_df$Demand_Class
  )

nb_performance = data.frame(
  Model = c(
    "Naive Bayes",
    "Majority-Class Baseline"
  ),
  Accuracy = c(
    accuracy,
    majority_accuracy
  ),
  Balanced_Accuracy = c(
    balanced_accuracy,
    NA
  ),
  Macro_F1 = c(
    macro_f1,
    NA
  ),
  stringsAsFactors = FALSE
)

cat(
  "\nNaive Bayes per-class metrics:\n"
)

print(
  per_class_metrics
)

cat(
  "\nNaive Bayes overall performance:\n"
)

print(
  nb_performance
)

write.csv(
  as.data.frame.matrix(
    nb_confusion
  ),
  "08_Model_Results/R/Task8_Naive_Bayes_Confusion_Matrix.csv",
  row.names = TRUE
)

write.csv(
  per_class_metrics,
  "08_Model_Results/R/Task8_Naive_Bayes_Per_Class_Metrics.csv",
  row.names = FALSE
)

write.csv(
  nb_performance,
  "08_Model_Results/R/Task8_Naive_Bayes_Performance.csv",
  row.names = FALSE
)


# ============================================================
# 12. NAIVE BAYES INDEPENDENCE LIMITATION
# ============================================================
#
# Naive Bayes assumes predictors are conditionally independent
# within each class.
#
# Product, month, market and day can be structurally associated in
# the business data, so this assumption is unlikely to be literally true.
#
# We create a simple Cramer's V audit for categorical associations.
# ============================================================

cramers_v = function(
  x,
  y
) {
  tab =
    table(
      x,
      y
    )

  chi =
    suppressWarnings(
      chisq.test(
        tab,
        correct = FALSE
      )
    )

  n =
    sum(tab)

  r =
    nrow(tab)

  c =
    ncol(tab)

  denominator =
    n *
    min(
      r - 1,
      c - 1
    )

  if(
    denominator == 0
  ) {
    return(
      NA_real_
    )
  }

  sqrt(
    as.numeric(
      chi$statistic
    ) /
    denominator
  )
}

association_pairs =
  combn(
    nb_predictors,
    2,
    simplify = FALSE
  )

nb_predictor_association =
  do.call(
    rbind,
    lapply(
      association_pairs,
      function(pair) {
        data.frame(
          Predictor_1 =
            pair[1],
          Predictor_2 =
            pair[2],
          Cramers_V =
            cramers_v(
              train_df[[pair[1]]],
              train_df[[pair[2]]]
            ),
          stringsAsFactors = FALSE
        )
      }
    )
  )

cat(
  "\nCategorical predictor association audit:\n"
)

print(
  nb_predictor_association
)

write.csv(
  nb_predictor_association,
  "08_Model_Results/R/Task8_Naive_Bayes_Predictor_Association.csv",
  row.names = FALSE
)


# ============================================================
# 13. TASK 8B - BAYESIAN REGRESSION APPLICABILITY
# ============================================================
#
# A full production Bayesian regression would require defensible prior
# distributions and a full posterior computation strategy.
#
# For this critical-evaluation task, we perform a transparent
# NORMAL-PRIOR sensitivity illustration using the same Reduced MLR
# predictor set as Task 5:
#
# Quantity ~ Product Name + Export Market + Day of Week
#
# Assumptions for this demonstration:
# - Gaussian likelihood
# - residual variance is estimated from OLS and treated as fixed
# - intercept has a very weak prior centered on training mean
# - slope coefficients have zero-centered Normal priors
#
# This is an illustrative Bayesian-style posterior sensitivity analysis,
# NOT a claim that these priors represent Fragceylon management beliefs.
# ============================================================

bayes_formula =
  ~ `Product Name` +
    `Export Market` +
    Day_of_Week

X_train =
  model.matrix(
    bayes_formula,
    data = train_df
  )

X_test =
  model.matrix(
    bayes_formula,
    data = test_df
  )

y_train =
  train_df$Quantity

y_test =
  test_df$Quantity

ols_reduced =
  lm(
    Quantity ~
      `Product Name` +
      `Export Market` +
      Day_of_Week,
    data = train_df
  )

sigma2_hat =
  sum(
    residuals(
      ols_reduced
    )^2
  ) /
  df.residual(
    ols_reduced
  )

cat(
  "\n============================================================\n"
)

cat(
  "BAYESIAN REGRESSION PRIOR SENSITIVITY\n"
)

cat(
  "============================================================\n"
)

cat(
  "OLS residual variance estimate:",
  sigma2_hat,
  "\n"
)


# ============================================================
# 14. REGRESSION METRICS
# ============================================================

regression_metrics = function(
  actual,
  predicted
) {
  mae =
    mean(
      abs(
        actual -
        predicted
      )
    )

  rmse =
    sqrt(
      mean(
        (
          actual -
          predicted
        )^2
      )
    )

  r2 =
    1 -
    sum(
      (
        actual -
        predicted
      )^2
    ) /
    sum(
      (
        actual -
        mean(actual)
      )^2
    )

  c(
    MAE = mae,
    RMSE = rmse,
    R2 = r2
  )
}


# ============================================================
# 15. NORMAL-PRIOR POSTERIOR FUNCTION
# ============================================================

fit_normal_prior_regression = function(
  X_train,
  y_train,
  X_test,
  sigma2,
  slope_prior_sd,
  intercept_prior_sd = 1000
) {
  p =
    ncol(
      X_train
    )

  prior_mean =
    rep(
      0,
      p
    )

  prior_mean[1] =
    mean(
      y_train
    )

  prior_sd =
    rep(
      slope_prior_sd,
      p
    )

  prior_sd[1] =
    intercept_prior_sd

  prior_precision =
    diag(
      1 /
      (
        prior_sd^2
      ),
      nrow = p,
      ncol = p
    )

  posterior_precision =
    crossprod(
      X_train
    ) /
    sigma2 +
    prior_precision

  posterior_cov =
    solve(
      posterior_precision
    )

  posterior_mean =
    posterior_cov %*%
    (
      crossprod(
        X_train,
        y_train
      ) /
      sigma2 +
      prior_precision %*%
        prior_mean
    )

  posterior_sd =
    sqrt(
      diag(
        posterior_cov
      )
    )

  test_pred =
    as.numeric(
      X_test %*%
        posterior_mean
    )

  metrics =
    regression_metrics(
      y_test,
      test_pred
    )

  list(
    posterior_mean =
      as.numeric(
        posterior_mean
      ),
    posterior_sd =
      posterior_sd,
    metrics =
      metrics,
    test_pred =
      test_pred
  )
}


# ============================================================
# 16. PRIOR SCALE SENSITIVITY
# ============================================================
#
# Smaller prior SD = stronger shrinkage toward 0 for slope coefficients.
# Larger prior SD = weaker prior / closer to ordinary regression.
# ============================================================

prior_scales = c(
  25,
  50,
  100,
  250,
  1000
)

bayes_regression_results =
  data.frame()

bayes_fits =
  list()

for(prior_sd in prior_scales) {
  fit =
    fit_normal_prior_regression(
      X_train =
        X_train,
      y_train =
        y_train,
      X_test =
        X_test,
      sigma2 =
        sigma2_hat,
      slope_prior_sd =
        prior_sd
    )

  bayes_fits[[as.character(prior_sd)]] =
    fit

  bayes_regression_results =
    rbind(
      bayes_regression_results,
      data.frame(
        Slope_Prior_SD =
          prior_sd,
        Test_MAE =
          as.numeric(
            fit$metrics[
              "MAE"
            ]
          ),
        Test_RMSE =
          as.numeric(
            fit$metrics[
              "RMSE"
            ]
          ),
        Test_R2 =
          as.numeric(
            fit$metrics[
              "R2"
            ]
          ),
        stringsAsFactors = FALSE
      )
    )
}

ols_test_pred =
  predict(
    ols_reduced,
    newdata = test_df
  )

ols_metrics =
  regression_metrics(
    y_test,
    ols_test_pred
  )

bayes_regression_comparison =
  rbind(
    data.frame(
      Model =
        "Reduced MLR - Task 5",
      Prior_SD =
        NA,
      Test_MAE =
        as.numeric(
          ols_metrics[
            "MAE"
          ]
        ),
      Test_RMSE =
        as.numeric(
          ols_metrics[
            "RMSE"
          ]
        ),
      Test_R2 =
        as.numeric(
          ols_metrics[
            "R2"
          ]
        ),
      stringsAsFactors = FALSE
    ),
    data.frame(
      Model =
        paste0(
          "Normal-Prior Regression SD=",
          bayes_regression_results$Slope_Prior_SD
        ),
      Prior_SD =
        bayes_regression_results$Slope_Prior_SD,
      Test_MAE =
        bayes_regression_results$Test_MAE,
      Test_RMSE =
        bayes_regression_results$Test_RMSE,
      Test_R2 =
        bayes_regression_results$Test_R2,
      stringsAsFactors = FALSE
    )
  )

cat(
  "\nNormal-prior regression sensitivity:\n"
)

print(
  bayes_regression_comparison
)

write.csv(
  bayes_regression_comparison,
  "08_Model_Results/R/Task8_Bayesian_Regression_Prior_Sensitivity.csv",
  row.names = FALSE
)


# ============================================================
# 17. SAVE ILLUSTRATIVE POSTERIOR COEFFICIENTS
# ============================================================
#
# Use SD = 100 as a middle sensitivity setting.
# This is NOT a company-informed prior recommendation.
# ============================================================

SELECTED_PRIOR_SD = 100

selected_bayes_fit =
  bayes_fits[[as.character(
    SELECTED_PRIOR_SD
  )]]

coefficient_names =
  colnames(
    X_train
  )

bayes_coefficient_table = data.frame(
  Coefficient =
    coefficient_names,
  Posterior_Mean =
    selected_bayes_fit$posterior_mean,
  Posterior_SD =
    selected_bayes_fit$posterior_sd,
  Approx_Lower_95 =
    selected_bayes_fit$posterior_mean -
    1.96 *
    selected_bayes_fit$posterior_sd,
  Approx_Upper_95 =
    selected_bayes_fit$posterior_mean +
    1.96 *
    selected_bayes_fit$posterior_sd,
  Prior_Setting =
    paste0(
      "Slope prior SD = ",
      SELECTED_PRIOR_SD,
      "; illustrative only"
    ),
  stringsAsFactors = FALSE
)

cat(
  "\nIllustrative posterior coefficient table:\n"
)

print(
  bayes_coefficient_table
)

write.csv(
  bayes_coefficient_table,
  "08_Model_Results/R/Task8_Bayesian_Regression_Illustrative_Coefficients.csv",
  row.names = FALSE
)


# ============================================================
# 18. TASK 8C - BAYESIAN DECISION MAKING ILLUSTRATION
# ============================================================
#
# Bayesian decision making combines:
# Posterior probabilities + loss/cost consequences.
#
# We use the FIRST CHRONOLOGICAL TEST OBSERVATION.
#
# The actual Quantity is NOT used to choose the action.
#
# The posterior probabilities come from the Naive Bayes model.
#
# Two HYPOTHETICAL loss structures are used:
# 1. Balanced mismatch loss
# 2. Higher stock-out loss
#
# These are relative illustrative loss units only.
# They are NOT Fragceylon's real financial costs.
# ============================================================

decision_row_index = 1

decision_profile = data.frame(
  Order_ID =
    test_df$`Order ID`[
      decision_row_index
    ],
  Date =
    as.character(
      test_df$Date[
        decision_row_index
      ]
    ),
  Product =
    as.character(
      test_df$`Product Name`[
        decision_row_index
      ]
    ),
  Export_Market =
    as.character(
      test_df$`Export Market`[
        decision_row_index
      ]
    ),
  Month =
    as.character(
      test_df$Month[
        decision_row_index
      ]
    ),
  Day_of_Week =
    as.character(
      test_df$Day_of_Week[
        decision_row_index
      ]
    ),
  Posterior_Low =
    nb_test$posterior[
      decision_row_index,
      "Low"
    ],
  Posterior_Medium =
    nb_test$posterior[
      decision_row_index,
      "Medium"
    ],
  Posterior_High =
    nb_test$posterior[
      decision_row_index,
      "High"
    ],
  Predicted_Demand_Class =
    as.character(
      nb_test$class[
        decision_row_index
      ]
    ),
  Actual_Demand_Class_For_Later_Evaluation =
    as.character(
      test_df$Demand_Class[
        decision_row_index
      ]
    ),
  stringsAsFactors = FALSE
)

cat(
  "\n============================================================\n"
)

cat(
  "BAYESIAN DECISION-MAKING EXAMPLE\n"
)

cat(
  "============================================================\n"
)

print(
  decision_profile
)

write.csv(
  decision_profile,
  "08_Model_Results/R/Task8_Bayesian_Decision_Posterior_Example.csv",
  row.names = FALSE
)


# ============================================================
# 19. DEFINE HYPOTHETICAL LOSS MATRICES
# ============================================================

actions = c(
  "Prepare Low Stock",
  "Prepare Medium Stock",
  "Prepare High Stock"
)

true_classes = c(
  "Low",
  "Medium",
  "High"
)

balanced_loss =
  matrix(
    c(
      0, 1, 2,
      1, 0, 1,
      2, 1, 0
    ),
    nrow = 3,
    byrow = TRUE,
    dimnames = list(
      actions,
      true_classes
    )
  )

stockout_high_loss =
  matrix(
    c(
      0, 2, 6,
      1, 0, 3,
      3, 1, 0
    ),
    nrow = 3,
    byrow = TRUE,
    dimnames = list(
      actions,
      true_classes
    )
  )

posterior_vector =
  c(
    Low =
      decision_profile$Posterior_Low,
    Medium =
      decision_profile$Posterior_Medium,
    High =
      decision_profile$Posterior_High
  )

calculate_expected_loss = function(
  loss_matrix,
  posterior,
  scenario
) {
  expected_loss =
    as.numeric(
      loss_matrix %*%
        posterior
    )

  data.frame(
    Scenario =
      scenario,
    Action =
      rownames(
        loss_matrix
      ),
    Expected_Loss =
      expected_loss,
    stringsAsFactors = FALSE
  )
}

decision_balanced =
  calculate_expected_loss(
    balanced_loss,
    posterior_vector,
    "Balanced mismatch loss"
  )

decision_stockout =
  calculate_expected_loss(
    stockout_high_loss,
    posterior_vector,
    "Higher stock-out loss"
  )

decision_expected_loss =
  rbind(
    decision_balanced,
    decision_stockout
  )

decision_expected_loss$Chosen_Action =
  FALSE

for(scenario_name in
    unique(
      decision_expected_loss$Scenario
    )) {
  scenario_rows =
    which(
      decision_expected_loss$Scenario ==
        scenario_name
    )

  best_row =
    scenario_rows[
      which.min(
        decision_expected_loss$Expected_Loss[
          scenario_rows
        ]
      )
    ]

  decision_expected_loss$Chosen_Action[
    best_row
  ] =
    TRUE
}

cat(
  "\nExpected-loss decision table:\n"
)

print(
  decision_expected_loss
)

write.csv(
  decision_expected_loss,
  "08_Model_Results/R/Task8_Bayesian_Decision_Expected_Loss.csv",
  row.names = FALSE
)

write.csv(
  as.data.frame(
    balanced_loss
  ),
  "08_Model_Results/R/Task8_Bayesian_Decision_Balanced_Loss_Matrix.csv",
  row.names = TRUE
)

write.csv(
  as.data.frame(
    stockout_high_loss
  ),
  "08_Model_Results/R/Task8_Bayesian_Decision_Stockout_Loss_Matrix.csv",
  row.names = TRUE
)


# ============================================================
# 20. BAYESIAN METHODS APPLICABILITY TABLE
# ============================================================

best_prior_row =
  bayes_regression_results[
    which.min(
      bayes_regression_results$Test_RMSE
    ),
  ]

bayesian_applicability = data.frame(
  Method = c(
    "Naive Bayes",
    "Bayesian Regression",
    "Bayesian Decision Making"
  ),
  Potential_Fragceylon_Use = c(
    "Classify future orders into broad Low, Medium or High demand categories.",
    "Estimate continuous Quantity while incorporating prior information and updating uncertainty as new data arrive.",
    "Combine predictive probabilities with the business costs of excess stock, stock-outs, shipping and missed sales."
  ),
  Main_Advantage = c(
    "Simple probabilistic classifier with direct class probabilities.",
    "Produces posterior distributions and can incorporate previous knowledge or expert information.",
    "Connects uncertainty directly to business actions and consequences."
  ),
  Main_Limitation = c(
    "Quantity must be categorized, causing substantial information loss; conditional-independence assumption is also strong.",
    "Requires defensible priors, more statistical expertise and usually more computational work than ordinary regression.",
    "Requires credible cost/loss values and posterior predictive distributions; current dataset does not contain full inventory-cost information."
  ),
  Current_Assessment = c(
    paste0(
      "Useful only as an optional coarse classification view. Holdout accuracy = ",
      round(
        accuracy,
        4
      ),
      "."
    ),
    paste0(
      "Potentially useful, but generic prior sensitivity gives performance close to the existing Reduced MLR. Best illustrative prior RMSE = ",
      round(
        best_prior_row$Test_RMSE,
        4
      ),
      "."
    ),
    "Conceptually highly relevant for inventory decisions, but exact action recommendations should wait until real stock-out, holding, shipping and margin costs are available."
  ),
  stringsAsFactors = FALSE
)

cat(
  "\n============================================================\n"
)

cat(
  "BAYESIAN METHOD APPLICABILITY ASSESSMENT\n"
)

cat(
  "============================================================\n"
)

print(
  bayesian_applicability
)

write.csv(
  bayesian_applicability,
  "08_Model_Results/R/Task8_Bayesian_Methods_Applicability.csv",
  row.names = FALSE
)


# ============================================================
# 21. CHART 1 - DEMAND CLASS DISTRIBUTION
# ============================================================

png(
  "06_Charts/R/Task8_01_Demand_Class_Distribution.png",
  width = 1000,
  height = 700
)

barplot(
  table(
    train_df$Demand_Class
  ),
  main = "Training Demand Classes for Naive Bayes Illustration",
  xlab = "Demand Class",
  ylab = "Number of Training Orders"
)

dev.off()


# ============================================================
# 22. CHART 2 - NAIVE BAYES CLASS F1
# ============================================================

png(
  "06_Charts/R/Task8_02_Naive_Bayes_Class_F1.png",
  width = 1000,
  height = 700
)

barplot(
  per_class_metrics$F1,
  names.arg =
    per_class_metrics$Demand_Class,
  ylim = c(
    0,
    1
  ),
  main = "Naive Bayes F1 Score by Demand Class",
  xlab = "Demand Class",
  ylab = "F1 Score"
)

dev.off()


# ============================================================
# 23. CHART 3 - BAYESIAN REGRESSION PRIOR SENSITIVITY
# ============================================================

png(
  "06_Charts/R/Task8_03_Bayesian_Regression_Prior_Sensitivity.png",
  width = 1000,
  height = 700
)

plot(
  bayes_regression_results$Slope_Prior_SD,
  bayes_regression_results$Test_RMSE,
  type = "b",
  pch = 16,
  log = "x",
  main = "Illustrative Bayesian Regression Prior Sensitivity",
  xlab = "Slope Prior Standard Deviation (log scale)",
  ylab = "Test RMSE"
)

abline(
  h =
    as.numeric(
      ols_metrics[
        "RMSE"
      ]
    ),
  lty = 2
)

dev.off()


# ============================================================
# 24. CHART 4 - BAYESIAN DECISION EXPECTED LOSS
# ============================================================

decision_matrix_for_plot =
  reshape(
    decision_expected_loss[
      ,
      c(
        "Scenario",
        "Action",
        "Expected_Loss"
      )
    ],
    idvar = "Action",
    timevar = "Scenario",
    direction = "wide"
  )

plot_values =
  as.matrix(
    decision_matrix_for_plot[
      ,
      setdiff(
        names(
          decision_matrix_for_plot
        ),
        "Action"
      )
    ]
  )

rownames(
  plot_values
) =
  decision_matrix_for_plot$Action

png(
  "06_Charts/R/Task8_04_Bayesian_Decision_Expected_Loss.png",
  width = 1200,
  height = 750
)

barplot(
  t(
    plot_values
  ),
  beside = TRUE,
  names.arg =
    rownames(
      plot_values
    ),
  las = 2,
  main = "Illustrative Bayesian Decision Expected Loss",
  xlab = "Action",
  ylab = "Expected Relative Loss",
  legend.text = c(
    "Balanced mismatch loss",
    "Higher stock-out loss"
  ),
  args.legend = list(
    x = "topright",
    cex = 0.8
  )
)

dev.off()


# ============================================================
# 25. REPORT-READY SUMMARY
# ============================================================

sink(
  "08_Model_Results/R/Task8_Report_Ready_Summary.txt"
)

cat(
  "TASK 8 - CRITICAL EVALUATION OF BAYESIAN STATISTICAL METHODS\n\n"
)

cat(
  "NAIVE BAYES\n"
)

cat(
  "Naive Bayes is naturally a classification approach, while Fragceylon's main response Quantity is numeric. An exploratory three-class demand version was therefore created using training-data tertiles only. This conversion is useful for a coarse demand-risk view but loses substantial numerical information.\n\n"
)

cat(
  "Naive Bayes holdout accuracy:",
  round(
    accuracy,
    4
  ),
  "\n"
)

cat(
  "Naive Bayes balanced accuracy:",
  round(
    balanced_accuracy,
    4
  ),
  "\n"
)

cat(
  "Naive Bayes macro F1:",
  round(
    macro_f1,
    4
  ),
  "\n"
)

cat(
  "Majority-class baseline accuracy:",
  round(
    majority_accuracy,
    4
  ),
  "\n\n"
)

cat(
  "BAYESIAN REGRESSION\n"
)

cat(
  "Bayesian regression is conceptually suitable because it can estimate continuous Quantity, incorporate prior information and update uncertainty as new data arrive. A normal-prior sensitivity illustration was performed using the same Reduced MLR predictor structure as Task 5. The priors are generic sensitivity assumptions and do not represent company expert beliefs.\n\n"
)

cat(
  "Reduced MLR holdout RMSE:",
  round(
    as.numeric(
      ols_metrics[
        "RMSE"
      ]
    ),
    4
  ),
  "\n"
)

cat(
  "Best illustrative normal-prior holdout RMSE:",
  round(
    best_prior_row$Test_RMSE,
    4
  ),
  "\n"
)

cat(
  "Best illustrative prior SD:",
  best_prior_row$Slope_Prior_SD,
  "\n\n"
)

cat(
  "BAYESIAN DECISION MAKING\n"
)

cat(
  "Bayesian decision making is highly relevant in principle because management decisions should combine predictive uncertainty with the costs of excess stock, stock-outs, shipping and missed sales. The script demonstrates this by combining posterior demand-class probabilities with two hypothetical loss structures. These losses are illustrative only and must not be interpreted as real Fragceylon cost values.\n\n"
)

cat(
  "CURRENT RECOMMENDATION\n"
)

cat(
  "Naive Bayes may be useful as an optional demand-category classifier but should not replace continuous Quantity modelling because categorization loses information. Bayesian regression is a plausible future extension, especially if Fragceylon can define defensible prior information and wants posterior uncertainty that can be updated over time. Bayesian decision making has strong future business relevance, but exact inventory actions require real cost and inventory information that is not currently available.\n\n"
)

cat(
  "LITERATURE REQUIREMENT\n"
)

cat(
  "The final Task 8 report section must support the evaluation with verified published research. No research references are fabricated by this R script.\n"
)

sink()


# ============================================================
# 26. FINAL VALIDATION
# ============================================================

posterior_row_sums =
  rowSums(
    nb_test$posterior
  )

stopifnot(
  nrow(df) == 3116,
  nrow(current_df) == 2014,
  nrow(train_df) == 1599,
  nrow(test_df) == 415,
  length(
    unique(
      train_df$Demand_Class
    )
  ) == 3,
  length(
    unique(
      test_df$Demand_Class
    )
  ) == 3,
  all(
    abs(
      posterior_row_sums -
      1
    ) <
      0.000001
  ),
  !any(
    is.na(
      nb_test$class
    )
  ),
  is.finite(
    accuracy
  ),
  is.finite(
    macro_f1
  ),
  nrow(
    bayes_regression_results
  ) ==
    length(
      prior_scales
    ),
  all(
    is.finite(
      bayes_regression_results$Test_RMSE
    )
  ),
  all(
    is.finite(
      decision_expected_loss$Expected_Loss
    )
  ),
  sum(
    decision_expected_loss$Chosen_Action
  ) ==
    length(
      unique(
        decision_expected_loss$Scenario
      )
    )
)

cat(
  "\n============================================================\n"
)

print(
  "All Task 8 Bayesian evaluation validation checks passed."
)

cat(
  "============================================================\n"
)


# ============================================================
# 27. LIST CREATED TASK 8 FILES
# ============================================================

cat(
  "\nTask 8 result files:\n"
)

print(
  list.files(
    "08_Model_Results/R",
    pattern = "^Task8",
    full.names = TRUE
  )
)

cat(
  "\nTask 8 chart files:\n"
)

print(
  list.files(
    "06_Charts/R",
    pattern = "^Task8",
    full.names = TRUE
  )
)


# ============================================================
# 28. COMPLETION MESSAGE
# ============================================================

cat(
  "\n============================================================\n"
)

cat(
  "TASK 8 R SCRIPT COMPLETE\n"
)

cat(
  "============================================================\n"
)

cat(
  "Naive Bayes, Bayesian regression and Bayesian decision-making applicability were evaluated.\n"
)

cat(
  "Naive Bayes was demonstrated only as a coarse demand-classification sensitivity analysis.\n"
)

cat(
  "Bayesian regression priors and decision losses used here are illustrative, not company-validated assumptions.\n"
)

