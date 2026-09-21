# ============================================================
# IT3081 - Statistical Modelling
# Retail Product Demand Analysis at Fragceylon
# TASK 7 - CRITICAL EVALUATION OF PRINCIPAL COMPONENT ANALYSIS
# R SCRIPT - PCA APPLICABILITY + EXPLORATORY SENSITIVITY ANALYSIS
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
  "TASK 7 - PCA DATASET AUDIT\n"
)

cat(
  "============================================================\n"
)

cat(
  "Rows:",
  nrow(df),
  "\n"
)

cat(
  "Columns:",
  ncol(df),
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
  "\nVariable names:\n"
)

print(
  names(df)
)

cat(
  "\nVariable classes:\n"
)

print(
  sapply(
    df,
    class
  )
)


# ============================================================
# 5. IDENTIFY NUMERIC VARIABLES
# ============================================================
#
# PCA is naturally designed for multiple numeric variables measured
# on the same observational units.
#
# A variable being stored as numeric in R does NOT automatically mean
# that it should enter PCA.
#
# Examples:
# - Quantity is the response variable.
# - Total Value contains Quantity and creates target leakage.
# - Year / Month_Number may be numeric codes but are time labels.
# - Is_Regular_Order is constant in the Regular Order dataset.
# ============================================================

numeric_names =
  names(df)[
    sapply(
      df,
      is.numeric
    )
  ]

numeric_audit = data.frame(
  Variable = numeric_names,
  Class = sapply(
    df[numeric_names],
    function(x) {
      paste(
        class(x),
        collapse = "/"
      )
    }
  ),
  Unique_Values = sapply(
    df[numeric_names],
    function(x) {
      length(
        unique(x)
      )
    }
  ),
  Minimum = sapply(
    df[numeric_names],
    function(x) {
      min(
        x,
        na.rm = TRUE
      )
    }
  ),
  Maximum = sapply(
    df[numeric_names],
    function(x) {
      max(
        x,
        na.rm = TRUE
      )
    }
  ),
  stringsAsFactors = FALSE
)

cat(
  "\nNumeric-variable audit:\n"
)

print(
  numeric_audit
)

write.csv(
  numeric_audit,
  "08_Model_Results/R/Task7_Numeric_Variable_Audit.csv",
  row.names = FALSE
)


# ============================================================
# 6. VARIABLE SUITABILITY AUDIT FOR PCA
# ============================================================
#
# This table evaluates analytical suitability, not simply R storage type.
# ============================================================

variable_suitability = data.frame(
  Variable = c(
    "Order ID",
    "Date",
    "Product Name",
    "Product Code",
    "Channel Type",
    "Buyer",
    "Line Type",
    "Quantity",
    "Unit Price (LKR)",
    "Total Value (LKR)",
    "Export Market",
    "Year",
    "Month",
    "Month_Number",
    "Quarter",
    "Day_of_Week",
    "Year_Month",
    "Time_Trend",
    "Sales_Regime",
    "Is_Regular_Order",
    "Market_Group",
    "Outlier_IQR_Flag"
  ),
  Analytical_Role = c(
    "Identifier",
    "Time identifier",
    "Categorical predictor",
    "Product identifier",
    "Categorical predictor",
    "High-cardinality categorical variable",
    "Record-type variable",
    "Response",
    "Numeric business variable",
    "Derived outcome/value variable",
    "Categorical predictor",
    "Derived time variable",
    "Categorical time variable",
    "Numeric-coded month",
    "Categorical time variable",
    "Categorical time variable",
    "Derived time label",
    "Numeric time predictor",
    "Derived regime variable",
    "Regular-order indicator",
    "Categorical market variable",
    "Response-derived diagnostic flag"
  ),
  Suitable_For_Standard_PCA = c(
    "No",
    "No",
    "No",
    "No",
    "No",
    "No",
    "No",
    "No",
    "Cautious / not primary",
    "No",
    "No",
    "No",
    "No",
    "No",
    "No",
    "No",
    "No",
    "Yes",
    "No",
    "No",
    "No",
    "No"
  ),
  Reason = c(
    "Identifier has no continuous analytical meaning.",
    "Raw date is better represented by derived time features.",
    "Categorical variable; standard PCA is not designed for nominal labels.",
    "Redundant with Product Name and functions as an identifier.",
    "Categorical and structurally confounded with business regime.",
    "Categorical with many levels and risk of memorizing buyers.",
    "All rows in this analytical dataset are Regular Orders.",
    "Quantity is the response to be predicted, not a predictor to compress.",
    "Numeric, but follows business pricing rules and was excluded from the primary Task 5 predictor set.",
    "Contains Quantity through Quantity x Unit Price and would create target leakage.",
    "Categorical variable; dummy encoding is possible but reduces component interpretability.",
    "Only a derived time label and not a rich continuous business measurement.",
    "Nominal/cyclical time category.",
    "The integers 1-12 are labels for a cyclical category, not an ordinary continuous measurement.",
    "Categorical time grouping.",
    "Categorical cyclical variable.",
    "Time label rather than an independent continuous measurement.",
    "Meaningful numeric time index, but only one clean continuous predictor is insufficient for useful PCA.",
    "Categorical regime indicator and constant in the current modelling regime.",
    "Constant because this dataset contains Regular Orders only.",
    "Categorical/redundant representation of market.",
    "Derived from Quantity and therefore unsuitable as a predictor."
  ),
  stringsAsFactors = FALSE
)

cat(
  "\n============================================================\n"
)

cat(
  "VARIABLE SUITABILITY FOR STANDARD PCA\n"
)

cat(
  "============================================================\n"
)

print(
  variable_suitability
)

write.csv(
  variable_suitability,
  "08_Model_Results/R/Task7_Variable_Suitability_Audit.csv",
  row.names = FALSE
)


# ============================================================
# 7. COUNT CLEAN CONTINUOUS PCA CANDIDATES
# ============================================================

clean_continuous_candidates = c(
  "Time_Trend"
)

cat(
  "\nClean continuous predictor candidates for standard PCA:\n"
)

print(
  clean_continuous_candidates
)

cat(
  "Number of clean continuous predictor candidates:",
  length(clean_continuous_candidates),
  "\n"
)

if(length(clean_continuous_candidates) < 2) {
  cat(
    "Conclusion: Standard PCA on the original meaningful predictor set is not useful because fewer than two clean continuous predictors are available.\n"
  )
}


# ============================================================
# 8. CURRENT-REGIME MODELLING POPULATION
# ============================================================
#
# To assess PCA's possible impact on prediction, use the same current
# regime and chronological train/test split used in Task 5.
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
  "\n============================================================\n"
)

cat(
  "TASK 5-CONSISTENT PCA/PCR SPLIT\n"
)

cat(
  "============================================================\n"
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
# 9. SET FACTOR LEVELS CONSISTENTLY
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
# 10. CREATE TASK 5-CONSISTENT ENCODED PREDICTOR MATRIX
# ============================================================
#
# IMPORTANT:
# This is an exploratory technical demonstration.
#
# Standard PCA is most natural for continuous variables.
# Here, Product, Market, Month and Day are dummy encoded because these
# are the predictors already considered in Task 5.
#
# This allows us to quantify what PCA would do to the existing model
# space, but the resulting principal components mix category indicators
# and time trend, reducing business interpretability.
# ============================================================

pca_formula =
  ~ `Product Name` +
    `Export Market` +
    Month +
    Day_of_Week +
    Time_Trend

x_train_raw =
  model.matrix(
    pca_formula,
    data = train_df
  )

x_test_raw =
  model.matrix(
    pca_formula,
    data = test_df
  )

x_train_raw =
  x_train_raw[
    ,
    colnames(x_train_raw) !=
      "(Intercept)",
    drop = FALSE
  ]

x_test_raw =
  x_test_raw[
    ,
    colnames(x_test_raw) !=
      "(Intercept)",
    drop = FALSE
  ]

y_train =
  train_df$Quantity

y_test =
  test_df$Quantity

cat(
  "\nEncoded predictor matrix dimensions:\n"
)

cat(
  "Training:",
  nrow(x_train_raw),
  "x",
  ncol(x_train_raw),
  "\n"
)

cat(
  "Testing:",
  nrow(x_test_raw),
  "x",
  ncol(x_test_raw),
  "\n"
)

cat(
  "\nEncoded predictor names:\n"
)

print(
  colnames(x_train_raw)
)


# ============================================================
# 11. CHECK ZERO-VARIANCE PREDICTORS
# ============================================================

train_sds =
  apply(
    x_train_raw,
    2,
    sd
  )

zero_variance =
  names(
    train_sds[
      train_sds == 0
    ]
  )

cat(
  "\nZero-variance encoded predictors:\n"
)

print(
  zero_variance
)

if(length(zero_variance) > 0) {
  x_train =
    x_train_raw[
      ,
      !(colnames(x_train_raw) %in%
          zero_variance),
      drop = FALSE
    ]

  x_test =
    x_test_raw[
      ,
      !(colnames(x_test_raw) %in%
          zero_variance),
      drop = FALSE
    ]
} else {
  x_train =
    x_train_raw

  x_test =
    x_test_raw
}

cat(
  "Predictors retained for exploratory PCA:",
  ncol(x_train),
  "\n"
)


# ============================================================
# 12. DIMENSIONALITY / CORRELATION AUDIT
# ============================================================

scaled_train =
  scale(
    x_train
  )

matrix_rank =
  qr(
    scaled_train
  )$rank

condition_number =
  kappa(
    scaled_train
  )

cor_matrix =
  cor(
    x_train
  )

upper_cor =
  abs(
    cor_matrix[
      upper.tri(
        cor_matrix
      )
    ]
  )

max_abs_correlation =
  max(
    upper_cor
  )

high_cor_pairs =
  sum(
    upper_cor >= 0.70
  )

predictor_matrix_audit = data.frame(
  Measure = c(
    "Training observations",
    "Encoded predictors before zero-variance removal",
    "Encoded predictors used for PCA",
    "Observation-to-predictor ratio",
    "Matrix rank",
    "Condition number of standardized matrix",
    "Maximum absolute pairwise correlation",
    "Number of absolute correlations >= 0.70"
  ),
  Value = c(
    nrow(x_train_raw),
    ncol(x_train_raw),
    ncol(x_train),
    nrow(x_train) /
      ncol(x_train),
    matrix_rank,
    condition_number,
    max_abs_correlation,
    high_cor_pairs
  ),
  stringsAsFactors = FALSE
)

cat(
  "\n============================================================\n"
)

cat(
  "PREDICTOR MATRIX AUDIT\n"
)

cat(
  "============================================================\n"
)

print(
  predictor_matrix_audit
)

write.csv(
  predictor_matrix_audit,
  "08_Model_Results/R/Task7_PCA_Predictor_Matrix_Audit.csv",
  row.names = FALSE
)


# ============================================================
# 13. FIT EXPLORATORY PCA ON TRAINING PREDICTORS ONLY
# ============================================================
#
# Centering and scaling are important because Time_Trend and dummy
# variables have different numerical scales.
#
# PCA is fitted only on TRAINING data to avoid test-data leakage.
# ============================================================

pca_fit =
  prcomp(
    x_train,
    center = TRUE,
    scale. = TRUE
  )

cat(
  "\n============================================================\n"
)

cat(
  "EXPLORATORY PCA SUMMARY\n"
)

cat(
  "============================================================\n"
)

print(
  summary(pca_fit)
)


# ============================================================
# 14. EXPLAINED VARIANCE
# ============================================================

variance =
  pca_fit$sdev^2

explained_variance =
  variance /
  sum(variance)

cumulative_variance =
  cumsum(
    explained_variance
  )

pca_variance_table = data.frame(
  Principal_Component =
    paste0(
      "PC",
      seq_along(
        explained_variance
      )
    ),
  Eigenvalue = variance,
  Explained_Variance =
    explained_variance,
  Explained_Variance_Percent =
    explained_variance * 100,
  Cumulative_Variance =
    cumulative_variance,
  Cumulative_Variance_Percent =
    cumulative_variance * 100,
  stringsAsFactors = FALSE
)

print(
  pca_variance_table
)

write.csv(
  pca_variance_table,
  "08_Model_Results/R/Task7_PCA_Explained_Variance.csv",
  row.names = FALSE
)


# ============================================================
# 15. COMPONENT COUNTS FOR COMMON VARIANCE THRESHOLDS
# ============================================================

components_for_threshold = function(
  threshold
) {
  which(
    cumulative_variance >=
      threshold
  )[1]
}

k_80 =
  components_for_threshold(
    0.80
  )

k_90 =
  components_for_threshold(
    0.90
  )

k_95 =
  components_for_threshold(
    0.95
  )

threshold_summary = data.frame(
  Variance_Threshold = c(
    "80%",
    "90%",
    "95%"
  ),
  Components_Required = c(
    k_80,
    k_90,
    k_95
  ),
  Original_Predictors = rep(
    ncol(x_train),
    3
  ),
  Reduction_Percent = c(
    (
      1 -
      k_80 /
      ncol(x_train)
    ) * 100,
    (
      1 -
      k_90 /
      ncol(x_train)
    ) * 100,
    (
      1 -
      k_95 /
      ncol(x_train)
    ) * 100
  ),
  stringsAsFactors = FALSE
)

cat(
  "\nVariance-threshold component counts:\n"
)

print(
  threshold_summary
)

write.csv(
  threshold_summary,
  "08_Model_Results/R/Task7_PCA_Variance_Thresholds.csv",
  row.names = FALSE
)


# ============================================================
# 16. PCA LOADINGS
# ============================================================

loadings =
  as.data.frame(
    pca_fit$rotation
  )

loadings$Original_Variable =
  rownames(loadings)

loadings =
  loadings[
    ,
    c(
      "Original_Variable",
      setdiff(
        names(loadings),
        "Original_Variable"
      )
    )
  ]

rownames(
  loadings
) = NULL

cat(
  "\nPCA loadings:\n"
)

print(
  loadings
)

write.csv(
  loadings,
  "08_Model_Results/R/Task7_PCA_Loadings.csv",
  row.names = FALSE
)


# ============================================================
# 17. TOP ABSOLUTE LOADINGS FOR PC1 AND PC2
# ============================================================

loading_pc1 = data.frame(
  Variable =
    rownames(
      pca_fit$rotation
    ),
  Loading =
    pca_fit$rotation[
      ,
      1
    ],
  Absolute_Loading =
    abs(
      pca_fit$rotation[
        ,
        1
      ]
    ),
  stringsAsFactors = FALSE
)

loading_pc1 =
  loading_pc1[
    order(
      loading_pc1$Absolute_Loading,
      decreasing = TRUE
    ),
  ]

loading_pc2 = data.frame(
  Variable =
    rownames(
      pca_fit$rotation
    ),
  Loading =
    pca_fit$rotation[
      ,
      2
    ],
  Absolute_Loading =
    abs(
      pca_fit$rotation[
        ,
        2
      ]
    ),
  stringsAsFactors = FALSE
)

loading_pc2 =
  loading_pc2[
    order(
      loading_pc2$Absolute_Loading,
      decreasing = TRUE
    ),
  ]

cat(
  "\nTop PC1 absolute loadings:\n"
)

print(
  head(
    loading_pc1,
    10
  )
)

cat(
  "\nTop PC2 absolute loadings:\n"
)

print(
  head(
    loading_pc2,
    10
  )
)

write.csv(
  loading_pc1,
  "08_Model_Results/R/Task7_PC1_Loadings_Ranked.csv",
  row.names = FALSE
)

write.csv(
  loading_pc2,
  "08_Model_Results/R/Task7_PC2_Loadings_Ranked.csv",
  row.names = FALSE
)


# ============================================================
# 18. CREATE PCA SCORES FOR TRAIN AND TEST
# ============================================================

train_scores =
  pca_fit$x

test_scores =
  predict(
    pca_fit,
    newdata = x_test
  )

cat(
  "\nTraining PCA score dimensions:",
  dim(train_scores),
  "\n"
)

cat(
  "Testing PCA score dimensions:",
  dim(test_scores),
  "\n"
)


# ============================================================
# 19. REGRESSION METRICS FUNCTION
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

  return(
    c(
      MAE = mae,
      RMSE = rmse,
      R2 = r2
    )
  )
}


# ============================================================
# 20. BASELINE PERFORMANCE
# ============================================================

baseline_value =
  mean(
    y_train
  )

baseline_test_pred =
  rep(
    baseline_value,
    length(y_test)
  )

baseline_metrics =
  regression_metrics(
    y_test,
    baseline_test_pred
  )

cat(
  "\nBaseline holdout metrics:\n"
)

print(
  baseline_metrics
)


# ============================================================
# 21. RECREATE TASK 5 REDUCED MLR
# ============================================================

reduced_mlr = lm(
  Quantity ~
    `Product Name` +
    `Export Market` +
    Day_of_Week,
  data = train_df
)

reduced_mlr_test_pred =
  predict(
    reduced_mlr,
    newdata = test_df
  )

reduced_mlr_metrics =
  regression_metrics(
    y_test,
    reduced_mlr_test_pred
  )

cat(
  "\nReduced MLR holdout metrics:\n"
)

print(
  reduced_mlr_metrics
)


# ============================================================
# 22. FUNCTION TO FIT PRINCIPAL COMPONENT REGRESSION
# ============================================================
#
# PCA is unsupervised. PCR then uses selected principal components
# as predictors of Quantity.
# ============================================================

fit_pcr = function(
  k,
  model_label
) {
  train_pc =
    as.data.frame(
      train_scores[
        ,
        1:k,
        drop = FALSE
      ]
    )

  test_pc =
    as.data.frame(
      test_scores[
        ,
        1:k,
        drop = FALSE
      ]
    )

  train_pc$Quantity =
    y_train

  pcr_model =
    lm(
      Quantity ~ .,
      data = train_pc
    )

  pcr_test_pred =
    predict(
      pcr_model,
      newdata = test_pc
    )

  metrics =
    regression_metrics(
      y_test,
      pcr_test_pred
    )

  result = data.frame(
    Model = model_label,
    Components = k,
    Test_MAE =
      as.numeric(
        metrics["MAE"]
      ),
    Test_RMSE =
      as.numeric(
        metrics["RMSE"]
      ),
    Test_R2 =
      as.numeric(
        metrics["R2"]
      ),
    stringsAsFactors = FALSE
  )

  return(
    result
  )
}


# ============================================================
# 23. EXPLORATORY PCR MODELS
# ============================================================

pcr_80 =
  fit_pcr(
    k_80,
    paste0(
      "PCR - 80% Variance (",
      k_80,
      " PCs)"
    )
  )

pcr_90 =
  fit_pcr(
    k_90,
    paste0(
      "PCR - 90% Variance (",
      k_90,
      " PCs)"
    )
  )

pcr_95 =
  fit_pcr(
    k_95,
    paste0(
      "PCR - 95% Variance (",
      k_95,
      " PCs)"
    )
  )

pcr_all =
  fit_pcr(
    ncol(train_scores),
    paste0(
      "PCR - All PCs (",
      ncol(train_scores),
      " PCs)"
    )
  )

pcr_results =
  rbind(
    pcr_80,
    pcr_90,
    pcr_95,
    pcr_all
  )

cat(
  "\n============================================================\n"
)

cat(
  "EXPLORATORY PCR HOLDOUT RESULTS\n"
)

cat(
  "============================================================\n"
)

print(
  pcr_results
)


# ============================================================
# 24. FINAL PERFORMANCE COMPARISON
# ============================================================

performance_comparison =
  rbind(
    data.frame(
      Model =
        "Training Mean Baseline",
      Components =
        NA,
      Test_MAE =
        as.numeric(
          baseline_metrics["MAE"]
        ),
      Test_RMSE =
        as.numeric(
          baseline_metrics["RMSE"]
        ),
      Test_R2 =
        as.numeric(
          baseline_metrics["R2"]
        ),
      stringsAsFactors = FALSE
    ),
    data.frame(
      Model =
        "Reduced MLR - Task 5",
      Components =
        NA,
      Test_MAE =
        as.numeric(
          reduced_mlr_metrics["MAE"]
        ),
      Test_RMSE =
        as.numeric(
          reduced_mlr_metrics["RMSE"]
        ),
      Test_R2 =
        as.numeric(
          reduced_mlr_metrics["R2"]
        ),
      stringsAsFactors = FALSE
    ),
    pcr_results
  )

performance_comparison =
  performance_comparison[
    order(
      performance_comparison$Test_RMSE
    ),
  ]

rownames(
  performance_comparison
) = NULL

cat(
  "\n============================================================\n"
)

cat(
  "PCA/PCR VS EXISTING MODELS\n"
)

cat(
  "============================================================\n"
)

print(
  performance_comparison
)

write.csv(
  performance_comparison,
  "08_Model_Results/R/Task7_PCR_Performance_Comparison.csv",
  row.names = FALSE
)


# ============================================================
# 25. CALCULATE PCR PERFORMANCE DIFFERENCE FROM REDUCED MLR
# ============================================================

best_pcr_row =
  pcr_results[
    which.min(
      pcr_results$Test_RMSE
    ),
  ]

best_pcr_rmse =
  best_pcr_row$Test_RMSE

mlr_rmse =
  as.numeric(
    reduced_mlr_metrics["RMSE"]
  )

pcr_rmse_difference =
  best_pcr_rmse -
  mlr_rmse

pcr_rmse_difference_percent =
  (
    pcr_rmse_difference /
    mlr_rmse
  ) * 100

cat(
  "\nBest PCR model:",
  best_pcr_row$Model,
  "\n"
)

cat(
  "Best PCR RMSE:",
  best_pcr_rmse,
  "\n"
)

cat(
  "Reduced MLR RMSE:",
  mlr_rmse,
  "\n"
)

cat(
  "PCR minus MLR RMSE:",
  pcr_rmse_difference,
  "\n"
)

cat(
  "PCR minus MLR RMSE (% of MLR RMSE):",
  pcr_rmse_difference_percent,
  "%\n"
)


# ============================================================
# 26. PCA APPLICABILITY ASSESSMENT
# ============================================================

pca_applicability = data.frame(
  Evaluation_Area = c(
    "Need for dimensionality reduction",
    "Original continuous-variable suitability",
    "Categorical predictor issue",
    "Target leakage risk",
    "Scaling requirement",
    "Interpretability",
    "Potential multicollinearity benefit",
    "Expected predictive benefit",
    "Business usability",
    "Future usefulness"
  ),
  Assessment = c(
    "Limited",
    "Low",
    "Important limitation",
    "Must be controlled",
    "Required",
    "Reduced by PCA",
    "Possible",
    "Must be evaluated empirically",
    "Lower than direct interpretable predictors",
    "Potentially high with richer high-dimensional numeric data"
  ),
  Fragceylon_Reason = c(
    paste0(
      "The Task 5 encoded matrix has ",
      ncol(x_train),
      " predictors for ",
      nrow(x_train),
      " training observations, so the problem is not high-dimensional in the usual p-near-n sense."
    ),
    "Only Time_Trend is a clean continuous primary predictor after excluding the response, leakage variables, identifiers and business-rule variables.",
    "Important predictors such as Product, Market, Month and Day are categorical and require dummy encoding before ordinary PCA.",
    "Quantity and Total Value must not enter predictor PCA because Quantity is the response and Total Value contains Quantity.",
    "Encoded dummy variables and Time_Trend have different scales, so centering and scaling are necessary.",
    "Principal components combine several original variables, making management interpretation harder than the Reduced MLR.",
    "PCA can transform correlated encoded predictors into orthogonal components, but simpler model reduction already solved the severe Task 5 VIF problem.",
    paste0(
      "The best exploratory PCR holdout RMSE is ",
      round(
        best_pcr_rmse,
        4
      ),
      " compared with ",
      round(
        mlr_rmse,
        4
      ),
      " for the Reduced MLR."
    ),
    "Management can interpret Product, Market and Day effects more directly than abstract principal components.",
    "PCA could become more useful if future data include many correlated continuous marketing, customer, web, inventory and product-attribute measures."
  ),
  stringsAsFactors = FALSE
)

print(
  pca_applicability
)

write.csv(
  pca_applicability,
  "08_Model_Results/R/Task7_PCA_Applicability_Assessment.csv",
  row.names = FALSE
)


# ============================================================
# 27. CHART 1 - SCREE PLOT
# ============================================================

png(
  "06_Charts/R/Task7_01_PCA_Scree_Plot.png",
  width = 1000,
  height = 700
)

plot(
  seq_along(
    explained_variance
  ),
  explained_variance * 100,
  type = "b",
  pch = 16,
  main = "Exploratory PCA Scree Plot",
  xlab = "Principal Component",
  ylab = "Explained Variance (%)"
)

dev.off()


# ============================================================
# 28. CHART 2 - CUMULATIVE EXPLAINED VARIANCE
# ============================================================

png(
  "06_Charts/R/Task7_02_PCA_Cumulative_Variance.png",
  width = 1000,
  height = 700
)

plot(
  seq_along(
    cumulative_variance
  ),
  cumulative_variance * 100,
  type = "b",
  pch = 16,
  ylim = c(
    0,
    100
  ),
  main = "Exploratory PCA Cumulative Explained Variance",
  xlab = "Number of Principal Components",
  ylab = "Cumulative Explained Variance (%)"
)

abline(
  h = c(
    80,
    90,
    95
  ),
  lty = c(
    2,
    3,
    4
  )
)

dev.off()


# ============================================================
# 29. CHART 3 - PC1 VS PC2 SCORES
# ============================================================
#
# This plot is descriptive only.
# It does not prove market clusters or causal differences.
# ============================================================

market_symbol =
  ifelse(
    train_df$`Export Market` ==
      "Japan",
    17,
    16
  )

png(
  "06_Charts/R/Task7_03_PC1_PC2_Scores_By_Market.png",
  width = 1000,
  height = 700
)

plot(
  train_scores[
    ,
    1
  ],
  train_scores[
    ,
    2
  ],
  pch = market_symbol,
  main = "Exploratory PCA Scores: PC1 vs PC2",
  xlab = "PC1 Score",
  ylab = "PC2 Score"
)

legend(
  "topright",
  legend = c(
    "Local",
    "Japan"
  ),
  pch = c(
    16,
    17
  )
)

dev.off()


# ============================================================
# 30. CHART 4 - PCR / MLR HOLDOUT RMSE COMPARISON
# ============================================================

png(
  "06_Charts/R/Task7_04_PCR_RMSE_Comparison.png",
  width = 1200,
  height = 750
)

barplot(
  performance_comparison$Test_RMSE,
  names.arg =
    performance_comparison$Model,
  las = 2,
  main = "Holdout RMSE: PCA/PCR Sensitivity Analysis",
  ylab = "Test RMSE",
  cex.names = 0.75
)

dev.off()


# ============================================================
# 31. REPORT-READY SUMMARY
# ============================================================

sink(
  "08_Model_Results/R/Task7_Report_Ready_Summary.txt"
)

cat(
  "TASK 7 - CRITICAL EVALUATION OF PRINCIPAL COMPONENT ANALYSIS\n\n"
)

cat(
  "ASSIGNMENT PURPOSE\n"
)

cat(
  "Task 7 requires a critical evaluation of whether PCA is beneficial for the Fragceylon dataset. PCA is not forced into the final model.\n\n"
)

cat(
  "ORIGINAL DATA SUITABILITY\n"
)

cat(
  "The current dataset has limited suitability for standard PCA because most important predictors are categorical. Quantity is the response variable, Total Value contains Quantity and would create target leakage, Unit Price follows business pricing rules, and several other numeric-looking variables are time codes rather than independent continuous measurements.\n\n"
)

cat(
  "CLEAN CONTINUOUS PREDICTORS\n"
)

cat(
  "The main clean continuous predictor suitable for ordinary PCA is Time_Trend. A useful standard PCA normally requires multiple continuous variables, so PCA is not naturally necessary on the original predictor set.\n\n"
)

cat(
  "EXPLORATORY ENCODED PCA\n"
)

cat(
  "For sensitivity analysis, the same broader Task 5 predictor set was dummy encoded and PCA was fitted on the training period only. This is a technical demonstration rather than a recommendation because principal components combine categorical dummy variables with Time_Trend and reduce business interpretability.\n\n"
)

cat(
  "ENCODED PREDICTOR COUNT\n"
)

cat(
  ncol(x_train),
  "predictors for",
  nrow(x_train),
  "training observations.\n\n"
)

cat(
  "COMPONENTS REQUIRED\n"
)

cat(
  "80% variance:",
  k_80,
  "components\n"
)

cat(
  "90% variance:",
  k_90,
  "components\n"
)

cat(
  "95% variance:",
  k_95,
  "components\n\n"
)

cat(
  "PREDICTIVE SENSITIVITY CHECK\n"
)

cat(
  "Best exploratory PCR model:",
  best_pcr_row$Model,
  "\n"
)

cat(
  "Best PCR test RMSE:",
  round(
    best_pcr_rmse,
    4
  ),
  "\n"
)

cat(
  "Reduced MLR test RMSE:",
  round(
    mlr_rmse,
    4
  ),
  "\n"
)

cat(
  "PCR minus MLR RMSE:",
  round(
    pcr_rmse_difference,
    4
  ),
  "\n\n"
)

cat(
  "INTERPRETATION\n"
)

if(best_pcr_rmse < mlr_rmse) {
  cat(
    "The best exploratory PCR produced a lower holdout RMSE than the Reduced MLR in this sensitivity analysis. However, predictive performance alone does not make PCA the preferred method because the PCA inputs are largely dummy-coded categorical predictors and the resulting components are difficult to interpret operationally.\n\n"
  )
} else {
  cat(
    "The best exploratory PCR did not improve holdout RMSE relative to the Reduced MLR. This provides no predictive reason to replace the simpler and more interpretable Task 5 model with PCA-based regression.\n\n"
  )
}

cat(
  "CURRENT RECOMMENDATION\n"
)

cat(
  "PCA is not recommended as the primary dimensionality-reduction or predictive approach for the current Fragceylon dataset. The dataset is not strongly high-dimensional, has few clean continuous independent predictors, and relies heavily on meaningful categorical business variables whose interpretation would be obscured by principal components.\n\n"
)

cat(
  "WHEN PCA COULD BECOME USEFUL\n"
)

cat(
  "PCA could be reconsidered if future data collection adds many correlated continuous variables such as marketing spend metrics, website engagement measures, customer behaviour scores, inventory measures, product/fragrance attributes, ratings and other high-dimensional business indicators.\n\n"
)

cat(
  "LITERATURE REQUIREMENT\n"
)

cat(
  "The final Task 7 report section must support the critical evaluation with verified research literature. No research references are fabricated by this R script.\n"
)

sink()


# ============================================================
# 32. FINAL VALIDATION
# ============================================================

stopifnot(
  nrow(df) == 3116,
  nrow(current_df) == 2014,
  nrow(train_df) == 1599,
  nrow(test_df) == 415,
  length(clean_continuous_candidates) == 1,
  ncol(x_train_raw) == 22,
  ncol(x_train) >= 2,
  nrow(train_scores) == 1599,
  nrow(test_scores) == 415,
  length(explained_variance) ==
    ncol(x_train),
  tail(
    cumulative_variance,
    1
  ) > 0.999999,
  k_80 <= k_90,
  k_90 <= k_95,
  k_95 <= ncol(x_train),
  is.finite(best_pcr_rmse),
  is.finite(mlr_rmse)
)

cat(
  "\n============================================================\n"
)

print(
  "All Task 7 PCA evaluation validation checks passed."
)

cat(
  "============================================================\n"
)


# ============================================================
# 33. LIST CREATED TASK 7 FILES
# ============================================================

cat(
  "\nTask 7 result files:\n"
)

print(
  list.files(
    "08_Model_Results/R",
    pattern = "^Task7",
    full.names = TRUE
  )
)

cat(
  "\nTask 7 chart files:\n"
)

print(
  list.files(
    "06_Charts/R",
    pattern = "^Task7",
    full.names = TRUE
  )
)


# ============================================================
# 34. COMPLETION MESSAGE
# ============================================================

cat(
  "\n============================================================\n"
)

cat(
  "TASK 7 R SCRIPT COMPLETE\n"
)

cat(
  "============================================================\n"
)

cat(
  "PCA suitability was audited before PCA was attempted.\n"
)

cat(
  "An exploratory PCA/PCR sensitivity analysis was completed using the Task 5 predictor space and chronological holdout split.\n"
)

cat(
  "No PCA result should be treated as causal.\n"
)



