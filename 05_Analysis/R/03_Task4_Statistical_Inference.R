# ============================================================================
# IT3081 - Statistical Modelling
# Fragceylon Retail Product Demand Analysis
# TASK 4 - Statistical Inference
# LAB-SHEET STYLE FINAL R VERSION
#
# IMPORTANT:
#   Run Task 3A first so that this file exists:
#   04_Cleaned_Data/Fragceylon_Regular_Orders.xlsx
#
# Main output folders:
#   08_Model_Results/R/
#   06_Charts/
#
# Packages used:
#   readxl
#   car
#
# If needed, install once with:
# install.packages(c("readxl", "car"))
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

df$`Product Name` = factor(df$`Product Name`)
df$`Channel Type` = factor(df$`Channel Type`)
df$`Export Market` = factor(df$`Export Market`)
df$Sales_Regime = factor(df$Sales_Regime)

df$Month = factor(
  df$Month,
  levels = month.name
)


# ----------------------------------------------------------------------------
# Step 5: Examine and Verify the Dataset
# ----------------------------------------------------------------------------

head(df)
str(df)
dim(df)
nrow(df)
ncol(df)
colnames(df)

print(table(df$`Product Name`))
print(table(df$`Channel Type`))
print(table(df$`Export Market`))
print(table(df$Sales_Regime))
print(table(df$Outlier_IQR_Flag))

print(min(df$Date))
print(max(df$Date))
print(length(unique(df$Date)))

stopifnot(
  nrow(df) == 3116,
  ncol(df) == 22,
  sum(is.na(df)) == 0,
  sum(duplicated(df)) == 0,
  all(df$`Line Type` == "Regular Order"),
  all(df$Quantity > 0),
  sum(df$Outlier_IQR_Flag == "Yes") == 41
)

print("Task 4 input-data checks passed.")


# ----------------------------------------------------------------------------
# Step 6: Structural Checks Before Statistical Inference
# ----------------------------------------------------------------------------
# These checks are important because Channel Type, Export Market and
# Sales Regime are not independent of the April 2025 business change.

print(table(df$`Channel Type`, df$Sales_Regime))
print(table(df$`Export Market`, df$Sales_Regime))
print(table(df$`Export Market`, df$`Channel Type`))
print(table(df$`Export Market`, df$Outlier_IQR_Flag))
print(table(df$`Channel Type`, df$Outlier_IQR_Flag))
print(table(df$Sales_Regime, df$Outlier_IQR_Flag))


# Japan buyer structure

japan_buyers = unique(
  df$Buyer[df$`Export Market` == "Japan"]
)

print(japan_buyers)

japan_buyer_records =
  df[df$Buyer %in% japan_buyers, ]

print(table(japan_buyer_records$`Export Market`))
print(
  table(
    japan_buyer_records$`Export Market`,
    japan_buyer_records$Sales_Regime
  )
)
print(
  table(
    japan_buyer_records$`Export Market`,
    japan_buyer_records$`Channel Type`
  )
)
print(
  table(
    japan_buyer_records$`Product Name`,
    japan_buyer_records$`Export Market`
  )
)


# ----------------------------------------------------------------------------
# Step 7: Create Analytical Subsets
# ----------------------------------------------------------------------------

# Full Regular Orders dataset
df_full = df

# Post-April-2025 regime
df_post =
  df[df$Sales_Regime == "Post-April-2025", ]

# Pre-April-2025 regime
df_pre =
  df[df$Sales_Regime == "Pre-April-2025", ]

# Controlled Local vs Japan subset:
# same buyer, same Main Dealer channel, same post-April regime
market_buyer = "Prabath Liyanarachchi"

df_market =
  df[
    df$Buyer == market_buyer &
    df$Sales_Regime == "Post-April-2025" &
    df$`Channel Type` == "Main Dealer",
  ]

# Pre-April channel comparison:
# Shop vs Small Dealer within the same regime
df_channel_pre =
  df[
    df$Sales_Regime == "Pre-April-2025" &
    df$`Channel Type` %in% c("Shop", "Small Dealer"),
  ]


cat("\nANALYTICAL SUBSET SIZES\n")

cat("Full Regular Orders:", nrow(df_full), "\n")
cat("Post-April-2025:", nrow(df_post), "\n")
cat("Pre-April-2025:", nrow(df_pre), "\n")
cat("Controlled market subset:", nrow(df_market), "\n")
cat("Pre-April channel subset:", nrow(df_channel_pre), "\n")

print(table(df_market$`Export Market`))
print(table(df_channel_pre$`Channel Type`))


# ----------------------------------------------------------------------------
# Step 8: Create Helper Functions
# ----------------------------------------------------------------------------
# Lab Sheet 01 introduces user-defined functions.
# These functions keep repeated calculations simple and reproducible.


# Sample skewness

sample_skewness = function(x) {

  n = length(x)
  x_bar = mean(x)
  s = sd(x)

  value =
    n / ((n - 1) * (n - 2)) *
    sum(((x - x_bar) / s)^3)

  return(value)
}


# Descriptive statistics

distribution_summary = function(x) {

  result = c(
    Count = length(x),
    Mean = mean(x),
    Median = median(x),
    Std_Dev = sd(x),
    Variance = var(x),
    Minimum = min(x),
    Q1 = as.numeric(quantile(x, 0.25)),
    Q3 = as.numeric(quantile(x, 0.75)),
    IQR = IQR(x),
    Maximum = max(x),
    Skewness = sample_skewness(x)
  )

  return(result)
}


# Grouped descriptive statistics

group_distribution = function(data, group_column) {

  groups = split(
    data$Quantity,
    data[[group_column]]
  )

  result = t(
    sapply(
      groups,
      distribution_summary
    )
  )

  result = as.data.frame(result)

  result = data.frame(
    Group = rownames(result),
    result,
    row.names = NULL
  )

  return(result)
}


# Hedges' g for two independent groups.
# Difference is defined as x - y.

hedges_g_test = function(x, y) {

  n_x = length(x)
  n_y = length(y)

  mean_x = mean(x)
  mean_y = mean(y)

  var_x = var(x)
  var_y = var(y)

  pooled_sd = sqrt(
    (
      (n_x - 1) * var_x +
      (n_y - 1) * var_y
    ) /
    (n_x + n_y - 2)
  )

  mean_difference = mean_x - mean_y

  cohens_d = mean_difference / pooled_sd

  effect_df = n_x + n_y - 2

  J =
    1 -
    3 / (4 * effect_df - 1)

  hedges_g = J * cohens_d

  result = c(
    Mean_Difference = mean_difference,
    Pooled_SD = pooled_sd,
    Cohens_d = cohens_d,
    Hedges_g = hedges_g
  )

  return(result)
}


# Ordinary ANOVA effect sizes

anova_effect_sizes = function(model) {

  aov_table = summary(model)[[1]]

  ss_between = aov_table[1, "Sum Sq"]
  ss_within = aov_table[2, "Sum Sq"]

  df_between = aov_table[1, "Df"]
  df_within = aov_table[2, "Df"]

  ms_within = aov_table[2, "Mean Sq"]

  ss_total = ss_between + ss_within

  eta_squared =
    ss_between / ss_total

  omega_squared_raw =
    (
      ss_between -
      df_between * ms_within
    ) /
    (
      ss_total +
      ms_within
    )

  omega_squared =
    max(0, omega_squared_raw)

  result = c(
    Eta_Squared = eta_squared,
    Omega_Squared_Raw = omega_squared_raw,
    Omega_Squared = omega_squared
  )

  return(result)
}


# ----------------------------------------------------------------------------
# Step 9: Preliminary Distribution Assessment
# ----------------------------------------------------------------------------

overall_distribution =
  distribution_summary(df_full$Quantity)

print(overall_distribution)

product_distribution =
  group_distribution(
    df_full,
    "Product Name"
  )

names(product_distribution)[1] =
  "Product_Name"

print(product_distribution)

write.csv(
  product_distribution,
  "08_Model_Results/R/Task4_Product_Distribution.csv",
  row.names = FALSE
)


# ----------------------------------------------------------------------------
# Step 10: Product Distribution Diagnostic Plots
# ----------------------------------------------------------------------------

png(
  "06_Charts/Task4_01_Product_Quantity_Boxplot.png",
  width = 1000,
  height = 700
)

boxplot(
  Quantity ~ `Product Name`,
  data = df_full,
  main = "Regular Order Quantity by Product",
  xlab = "Product",
  ylab = "Quantity",
  col = "lightblue",
  las = 2
)

dev.off()


png(
  "06_Charts/Task4_02_Product_QQ_Plots.png",
  width = 1200,
  height = 1000
)

par(mfrow = c(2, 2))

for(product in levels(df_full$`Product Name`)) {

  x =
    df_full$Quantity[
      df_full$`Product Name` == product
    ]

  qqnorm(
    x,
    main = paste("Q-Q Plot:", product)
  )

  qqline(
    x,
    col = "red"
  )
}

par(mfrow = c(1, 1))

dev.off()


# ============================================================================
# TEST 1 - PRODUCT MEAN DIFFERENCES
# ============================================================================


# ----------------------------------------------------------------------------
# Step 11: Brown-Forsythe Test for Product Variances
# ----------------------------------------------------------------------------

cat("\n============================================================\n")
cat("TEST 1A - PRODUCT VARIABILITY\n")
cat("============================================================\n")

cat(
  "Business Question: Does Regular Order Quantity variability differ among the four perfumes?\n"
)

cat(
  "H0: Quantity variance is equal across the four products.\n"
)

cat(
  "H1: At least one product has a different Quantity variance.\n"
)


product_bf =
  leveneTest(
    Quantity ~ `Product Name`,
    data = df_full,
    center = median
  )

print(product_bf)

product_bf_F =
  product_bf[1, "F value"]

product_bf_p =
  product_bf[1, "Pr(>F)"]

product_variances =
  tapply(
    df_full$Quantity,
    df_full$`Product Name`,
    var
  )

product_variance_ratio =
  max(product_variances) /
  min(product_variances)

print(product_variances)

cat(
  "Largest / smallest product variance ratio:",
  product_variance_ratio,
  "\n"
)


# ----------------------------------------------------------------------------
# Step 12: One-Way ANOVA for Product Mean Quantity
# ----------------------------------------------------------------------------

cat("\n============================================================\n")
cat("TEST 1B - PRODUCT MEAN QUANTITY\n")
cat("============================================================\n")

cat(
  "Business Question: Does mean Regular Order Quantity differ among the four perfumes?\n"
)

cat(
  "H0: Mean Quantity is equal across all four products.\n"
)

cat(
  "H1: At least one product has a different mean Quantity.\n"
)

cat(
  "Selected Test: One-way ANOVA.\n"
)

cat(
  "Reason: Quantity is numeric, four product groups are compared, and Brown-Forsythe does not indicate unequal product variances.\n"
)


product_anova =
  aov(
    Quantity ~ `Product Name`,
    data = df_full
  )

product_anova_summary =
  summary(product_anova)

print(product_anova_summary)


product_anova_table =
  product_anova_summary[[1]]

product_F =
  product_anova_table[1, "F value"]

product_p =
  product_anova_table[1, "Pr(>F)"]

product_df1 =
  product_anova_table[1, "Df"]

product_df2 =
  product_anova_table[2, "Df"]


# Product effect sizes

product_effect =
  anova_effect_sizes(product_anova)

print(product_effect)


# ----------------------------------------------------------------------------
# Step 13: 95% Confidence Intervals for Product Means
# ----------------------------------------------------------------------------

product_order =
  c(
    "Kennedy",
    "Mesmerose",
    "Secret Ambrosia",
    "Dark Matrix"
  )

product_ci =
  data.frame(
    Product = character(),
    n = integer(),
    Mean = numeric(),
    Std_Dev = numeric(),
    SE = numeric(),
    CI_95_Lower = numeric(),
    CI_95_Upper = numeric()
  )


for(product in product_order) {

  x =
    df_full$Quantity[
      df_full$`Product Name` == product
    ]

  n_x = length(x)
  mean_x = mean(x)
  sd_x = sd(x)
  se_x = sd_x / sqrt(n_x)

  t_critical =
    qt(
      0.975,
      df = n_x - 1
    )

  lower =
    mean_x -
    t_critical * se_x

  upper =
    mean_x +
    t_critical * se_x

  row =
    data.frame(
      Product = product,
      n = n_x,
      Mean = mean_x,
      Std_Dev = sd_x,
      SE = se_x,
      CI_95_Lower = lower,
      CI_95_Upper = upper
    )

  product_ci =
    rbind(
      product_ci,
      row
    )
}

print(product_ci)

write.csv(
  product_ci,
  "08_Model_Results/R/Task4_Product_Mean_Confidence_Intervals.csv",
  row.names = FALSE
)


# Product conclusion

if(product_p < 0.05) {

  print("Reject H0.")
  print("At least one product has a different mean Regular Order Quantity.")

} else {

  print("Fail to reject H0.")
  print("There is insufficient statistical evidence that mean Regular Order Quantity differs among the four products.")
}


# Save product inference summary

product_results =
  data.frame(
    Analysis = c(
      "Product mean Quantity",
      "Product Quantity variability"
    ),
    Test = c(
      "One-way ANOVA",
      "Brown-Forsythe"
    ),
    Statistic = c(
      product_F,
      product_bf_F
    ),
    DF1 = c(
      product_df1,
      3
    ),
    DF2 = c(
      product_df2,
      3112
    ),
    p_value = c(
      product_p,
      product_bf_p
    ),
    Effect = c(
      product_effect["Eta_Squared"],
      product_variance_ratio
    )
  )

print(product_results)

write.csv(
  product_results,
  "08_Model_Results/R/Task4_Product_Inference.csv",
  row.names = FALSE
)


# ============================================================================
# TEST 2 - CONTROLLED LOCAL VS JAPAN MARKET COMPARISON
# ============================================================================


# ----------------------------------------------------------------------------
# Step 14: Controlled Market Distribution
# ----------------------------------------------------------------------------

market_distribution =
  group_distribution(
    df_market,
    "Export Market"
  )

names(market_distribution)[1] =
  "Export_Market"

print(market_distribution)

write.csv(
  market_distribution,
  "08_Model_Results/R/Task4_Controlled_Market_Distribution.csv",
  row.names = FALSE
)


local_quantity =
  df_market$Quantity[
    df_market$`Export Market` == "Local"
  ]

japan_quantity =
  df_market$Quantity[
    df_market$`Export Market` == "Japan"
  ]


# ----------------------------------------------------------------------------
# Step 15: Brown-Forsythe Test for Local vs Japan Variance
# ----------------------------------------------------------------------------

cat("\n============================================================\n")
cat("TEST 2A - CONTROLLED LOCAL VS JAPAN VARIABILITY\n")
cat("============================================================\n")

market_bf =
  leveneTest(
    Quantity ~ `Export Market`,
    data = df_market,
    center = median
  )

print(market_bf)

market_bf_F =
  market_bf[1, "F value"]

market_bf_p =
  market_bf[1, "Pr(>F)"]

local_variance =
  var(local_quantity)

japan_variance =
  var(japan_quantity)

market_variance_ratio =
  max(
    local_variance,
    japan_variance
  ) /
  min(
    local_variance,
    japan_variance
  )

cat(
  "Local variance:",
  local_variance,
  "\n"
)

cat(
  "Japan variance:",
  japan_variance,
  "\n"
)

cat(
  "Largest / smallest variance ratio:",
  market_variance_ratio,
  "\n"
)


# ----------------------------------------------------------------------------
# Step 16: Welch t-Test for Controlled Local vs Japan Means
# ----------------------------------------------------------------------------

cat("\n============================================================\n")
cat("TEST 2B - CONTROLLED LOCAL VS JAPAN MEAN QUANTITY\n")
cat("============================================================\n")

cat(
  "Business Question: For the same buyer, Main Dealer channel and post-April regime, does mean Quantity differ between Japan and Local transactions?\n"
)

cat(
  "H0: Mean Japan Quantity = Mean Local Quantity.\n"
)

cat(
  "H1: Mean Japan Quantity != Mean Local Quantity.\n"
)

cat(
  "Selected Test: Welch independent-samples t-test.\n"
)

cat(
  "Reason: The two groups have unequal variances and unequal sample sizes.\n"
)


market_t_test =
  t.test(
    japan_quantity,
    local_quantity,
    var.equal = FALSE,
    conf.level = 0.95
  )

print(market_t_test)

market_t_stat =
  as.numeric(
    market_t_test$statistic
  )

market_t_df =
  as.numeric(
    market_t_test$parameter
  )

market_t_p =
  market_t_test$p.value

market_ci_lower =
  market_t_test$conf.int[1]

market_ci_upper =
  market_t_test$conf.int[2]

market_mean_japan =
  mean(japan_quantity)

market_mean_local =
  mean(local_quantity)

market_mean_difference =
  market_mean_japan -
  market_mean_local


# Effect size

market_effect =
  hedges_g_test(
    japan_quantity,
    local_quantity
  )

print(market_effect)

cat(
  "Japan mean:",
  market_mean_japan,
  "\n"
)

cat(
  "Local mean:",
  market_mean_local,
  "\n"
)

cat(
  "Mean difference (Japan - Local):",
  market_mean_difference,
  "\n"
)

cat(
  "95% CI:",
  market_ci_lower,
  "to",
  market_ci_upper,
  "\n"
)


# ----------------------------------------------------------------------------
# Step 17: Controlled Market Boxplot and Q-Q Plots
# ----------------------------------------------------------------------------

png(
  "06_Charts/Task4_03_Controlled_Local_vs_Japan_Boxplot.png",
  width = 900,
  height = 700
)

boxplot(
  Quantity ~ `Export Market`,
  data = df_market,
  main = "Controlled Local vs Japan Quantity",
  xlab = "Market",
  ylab = "Quantity",
  col = "lightblue"
)

dev.off()


png(
  "06_Charts/Task4_04_Controlled_Market_QQ_Plots.png",
  width = 1200,
  height = 600
)

par(mfrow = c(1, 2))

qqnorm(
  local_quantity,
  main = "Q-Q Plot: Local"
)
qqline(
  local_quantity,
  col = "red"
)

qqnorm(
  japan_quantity,
  main = "Q-Q Plot: Japan"
)
qqline(
  japan_quantity,
  col = "red"
)

par(mfrow = c(1, 1))

dev.off()


# ----------------------------------------------------------------------------
# Step 18: Sensitivity Analysis - Exclude 41 High-Volume Orders
# ----------------------------------------------------------------------------

df_market_sensitivity =
  df_market[
    df_market$Outlier_IQR_Flag == "No",
  ]

local_sens =
  df_market_sensitivity$Quantity[
    df_market_sensitivity$`Export Market` == "Local"
  ]

japan_sens =
  df_market_sensitivity$Quantity[
    df_market_sensitivity$`Export Market` == "Japan"
  ]


market_sens_bf =
  leveneTest(
    Quantity ~ `Export Market`,
    data = df_market_sensitivity,
    center = median
  )

market_sens_bf_F =
  market_sens_bf[1, "F value"]

market_sens_bf_p =
  market_sens_bf[1, "Pr(>F)"]


market_sens_t_test =
  t.test(
    japan_sens,
    local_sens,
    var.equal = FALSE,
    conf.level = 0.95
  )

market_sens_effect =
  hedges_g_test(
    japan_sens,
    local_sens
  )

market_sens_mean_difference =
  mean(japan_sens) -
  mean(local_sens)


market_sensitivity_results =
  data.frame(
    Analysis = c(
      "Primary - All Valid Orders",
      "Sensitivity - Flagged High Orders Excluded"
    ),
    Local_n = c(
      length(local_quantity),
      length(local_sens)
    ),
    Japan_n = c(
      length(japan_quantity),
      length(japan_sens)
    ),
    Local_Mean = c(
      mean(local_quantity),
      mean(local_sens)
    ),
    Japan_Mean = c(
      mean(japan_quantity),
      mean(japan_sens)
    ),
    Mean_Difference_Japan_minus_Local = c(
      market_mean_difference,
      market_sens_mean_difference
    ),
    CI_Lower = c(
      market_ci_lower,
      market_sens_t_test$conf.int[1]
    ),
    CI_Upper = c(
      market_ci_upper,
      market_sens_t_test$conf.int[2]
    ),
    Hedges_g = c(
      market_effect["Hedges_g"],
      market_sens_effect["Hedges_g"]
    )
  )

print(market_sensitivity_results)

write.csv(
  market_sensitivity_results,
  "08_Model_Results/R/Task4_Market_Sensitivity_Analysis.csv",
  row.names = FALSE
)


# Save primary market result

market_result =
  data.frame(
    Comparison = "Japan - Local",
    Scope = "Same buyer, Main Dealer, Post-April-2025",
    Local_n = length(local_quantity),
    Japan_n = length(japan_quantity),
    Local_Mean = mean(local_quantity),
    Japan_Mean = mean(japan_quantity),
    Mean_Difference = market_mean_difference,
    t_Statistic = market_t_stat,
    Degrees_of_Freedom = market_t_df,
    p_value = market_t_p,
    CI_95_Lower = market_ci_lower,
    CI_95_Upper = market_ci_upper,
    Cohens_d = market_effect["Cohens_d"],
    Hedges_g = market_effect["Hedges_g"],
    Brown_Forsythe_F = market_bf_F,
    Brown_Forsythe_p = market_bf_p,
    Variance_Ratio = market_variance_ratio
  )

print(market_result)

write.csv(
  market_result,
  "08_Model_Results/R/Task4_Controlled_Local_vs_Japan.csv",
  row.names = FALSE
)


# ============================================================================
# TEST 3 - PRE-APRIL SHOP VS SMALL DEALER
# ============================================================================


# ----------------------------------------------------------------------------
# Step 19: Pre-April Channel Distribution
# ----------------------------------------------------------------------------

channel_distribution =
  group_distribution(
    df_channel_pre,
    "Channel Type"
  )

names(channel_distribution)[1] =
  "Channel_Type"

print(channel_distribution)

write.csv(
  channel_distribution,
  "08_Model_Results/R/Task4_PreApril_Channel_Distribution.csv",
  row.names = FALSE
)


shop_quantity =
  df_channel_pre$Quantity[
    df_channel_pre$`Channel Type` == "Shop"
  ]

small_dealer_quantity =
  df_channel_pre$Quantity[
    df_channel_pre$`Channel Type` == "Small Dealer"
  ]


# ----------------------------------------------------------------------------
# Step 20: Brown-Forsythe Test for Channel Variances
# ----------------------------------------------------------------------------

channel_bf =
  leveneTest(
    Quantity ~ `Channel Type`,
    data = df_channel_pre,
    center = median
  )

print(channel_bf)

channel_bf_F =
  channel_bf[1, "F value"]

channel_bf_p =
  channel_bf[1, "Pr(>F)"]

shop_variance =
  var(shop_quantity)

small_dealer_variance =
  var(small_dealer_quantity)

channel_variance_ratio =
  max(
    shop_variance,
    small_dealer_variance
  ) /
  min(
    shop_variance,
    small_dealer_variance
  )

cat(
  "Shop variance:",
  shop_variance,
  "\n"
)

cat(
  "Small Dealer variance:",
  small_dealer_variance,
  "\n"
)

cat(
  "Variance ratio:",
  channel_variance_ratio,
  "\n"
)


# ----------------------------------------------------------------------------
# Step 21: Welch t-Test - Small Dealer vs Shop
# ----------------------------------------------------------------------------

cat("\n============================================================\n")
cat("TEST 3 - PRE-APRIL CHANNEL DIFFERENCE\n")
cat("============================================================\n")

cat(
  "Business Question: Within the pre-April-2025 regime, does mean Quantity differ between Small Dealer and Shop transactions?\n"
)

cat(
  "H0: Mean Small Dealer Quantity = Mean Shop Quantity.\n"
)

cat(
  "H1: Mean Small Dealer Quantity != Mean Shop Quantity.\n"
)

cat(
  "Selected Test: Welch independent-samples t-test.\n"
)

cat(
  "Reason: The two channel groups have extremely unequal variances.\n"
)


channel_t_test =
  t.test(
    small_dealer_quantity,
    shop_quantity,
    var.equal = FALSE,
    conf.level = 0.95
  )

print(channel_t_test)

channel_t_stat =
  as.numeric(
    channel_t_test$statistic
  )

channel_t_df =
  as.numeric(
    channel_t_test$parameter
  )

channel_t_p =
  channel_t_test$p.value

channel_ci_lower =
  channel_t_test$conf.int[1]

channel_ci_upper =
  channel_t_test$conf.int[2]

channel_mean_difference =
  mean(small_dealer_quantity) -
  mean(shop_quantity)


channel_effect =
  hedges_g_test(
    small_dealer_quantity,
    shop_quantity
  )

print(channel_effect)

cat(
  "Shop mean:",
  mean(shop_quantity),
  "\n"
)

cat(
  "Small Dealer mean:",
  mean(small_dealer_quantity),
  "\n"
)

cat(
  "Mean difference (Small Dealer - Shop):",
  channel_mean_difference,
  "\n"
)

cat(
  "95% CI:",
  channel_ci_lower,
  "to",
  channel_ci_upper,
  "\n"
)


# Save channel result

channel_result =
  data.frame(
    Comparison = "Small Dealer - Shop",
    Scope = "Pre-April-2025 only",
    Shop_n = length(shop_quantity),
    Small_Dealer_n = length(small_dealer_quantity),
    Shop_Mean = mean(shop_quantity),
    Small_Dealer_Mean = mean(small_dealer_quantity),
    Mean_Difference = channel_mean_difference,
    t_Statistic = channel_t_stat,
    Degrees_of_Freedom = channel_t_df,
    p_value = channel_t_p,
    CI_95_Lower = channel_ci_lower,
    CI_95_Upper = channel_ci_upper,
    Cohens_d = channel_effect["Cohens_d"],
    Hedges_g = channel_effect["Hedges_g"],
    Brown_Forsythe_F = channel_bf_F,
    Brown_Forsythe_p = channel_bf_p,
    Variance_Ratio = channel_variance_ratio
  )

print(channel_result)

write.csv(
  channel_result,
  "08_Model_Results/R/Task4_PreApril_Shop_vs_SmallDealer.csv",
  row.names = FALSE
)


# ----------------------------------------------------------------------------
# Step 22: Channel Boxplot
# ----------------------------------------------------------------------------

png(
  "06_Charts/Task4_05_PreApril_Channel_Boxplot.png",
  width = 900,
  height = 700
)

boxplot(
  Quantity ~ `Channel Type`,
  data = df_channel_pre,
  main = "Pre-April-2025 Quantity by Channel",
  xlab = "Channel",
  ylab = "Quantity",
  col = "lightblue"
)

dev.off()


# ============================================================================
# TEST 4 - POST-APRIL CALENDAR-MONTH DIFFERENCES
# ============================================================================


# ----------------------------------------------------------------------------
# Step 23: Examine Post-April Monthly Structure
# ----------------------------------------------------------------------------

cat("\n============================================================\n")
cat("TEST 4 - POST-APRIL CALENDAR MONTH DIFFERENCES\n")
cat("============================================================\n")

cat(
  "Business Question: Within the post-April-2025 regime, does mean Quantity differ across calendar months?\n"
)

cat(
  "H0: Mean Quantity is equal across all represented calendar months.\n"
)

cat(
  "H1: At least one calendar month has a different mean Quantity.\n"
)


print(min(df_post$Date))
print(max(df_post$Date))
print(nrow(df_post))

print(table(df_post$Year_Month))
print(table(df_post$Month, df_post$`Export Market`))


# ----------------------------------------------------------------------------
# Step 24: Monthly Distribution
# ----------------------------------------------------------------------------

month_distribution =
  group_distribution(
    df_post,
    "Month"
  )

names(month_distribution)[1] =
  "Month"

month_distribution$Month =
  factor(
    month_distribution$Month,
    levels = month.name
  )

month_distribution =
  month_distribution[
    order(month_distribution$Month),
  ]

month_distribution$Month =
  as.character(month_distribution$Month)

print(month_distribution)

write.csv(
  month_distribution,
  "08_Model_Results/R/Task4_PostApril_Month_Distribution.csv",
  row.names = FALSE
)


# ----------------------------------------------------------------------------
# Step 25: Brown-Forsythe Test for Monthly Variances
# ----------------------------------------------------------------------------

month_bf =
  leveneTest(
    Quantity ~ Month,
    data = df_post,
    center = median
  )

print(month_bf)

month_bf_F =
  month_bf[1, "F value"]

month_bf_p =
  month_bf[1, "Pr(>F)"]


month_variances =
  tapply(
    df_post$Quantity,
    df_post$Month,
    var
  )

month_variances =
  month_variances[
    !is.na(month_variances)
  ]

month_variance_ratio =
  max(month_variances) /
  min(month_variances)

print(month_variances)

cat(
  "Largest / smallest month variance ratio:",
  month_variance_ratio,
  "\n"
)


# ----------------------------------------------------------------------------
# Step 26: Welch One-Way ANOVA Across Calendar Months
# ----------------------------------------------------------------------------
# Welch ANOVA is used as a precaution because monthly sample sizes are
# very unequal, even though Brown-Forsythe does not indicate unequal variance.

month_welch =
  oneway.test(
    Quantity ~ Month,
    data = df_post,
    var.equal = FALSE
  )

print(month_welch)

month_F =
  as.numeric(
    month_welch$statistic
  )

month_df1 =
  as.numeric(
    month_welch$parameter[1]
  )

month_df2 =
  as.numeric(
    month_welch$parameter[2]
  )

month_p =
  month_welch$p.value


# ----------------------------------------------------------------------------
# Step 27: Welch-Compatible Month Effect Size
# ----------------------------------------------------------------------------
# This reproduces the effect-size approach used in the completed notebook.
# f^2 is based on inverse-variance weighted group means.

month_groups =
  split(
    df_post$Quantity,
    df_post$Month
  )

month_groups =
  month_groups[
    sapply(
      month_groups,
      length
    ) > 0
  ]

month_means =
  sapply(
    month_groups,
    mean
  )

month_group_variances =
  sapply(
    month_groups,
    var
  )

month_group_n =
  sapply(
    month_groups,
    length
  )

month_weights =
  month_group_n /
  month_group_variances

month_weighted_mean =
  sum(
    month_weights *
    month_means
  ) /
  sum(month_weights)

welch_f_squared =
  sum(
    month_weights *
    (
      month_means -
      month_weighted_mean
    )^2
  ) /
  sum(month_group_n)

welch_f =
  sqrt(welch_f_squared)

cat(
  "Welch f-squared:",
  welch_f_squared,
  "\n"
)

cat(
  "Welch Cohen-type f:",
  welch_f,
  "\n"
)


if(month_p < 0.05) {

  print("Reject H0.")
  print("There is statistical evidence that mean Quantity differs across at least some calendar months.")

} else {

  print("Fail to reject H0.")
  print("There is insufficient statistical evidence that mean Quantity differs across calendar months in the post-April dataset.")
}


# Save monthly result

month_result =
  data.frame(
    Analysis = "Calendar-month mean Quantity",
    Scope = "Post-April-2025 only",
    Test = "Welch ANOVA",
    F_Statistic = month_F,
    DF1 = month_df1,
    DF2 = month_df2,
    p_value = month_p,
    Brown_Forsythe_F = month_bf_F,
    Brown_Forsythe_p = month_bf_p,
    Variance_Ratio = month_variance_ratio,
    Welch_f_squared = welch_f_squared,
    Welch_f = welch_f
  )

print(month_result)

write.csv(
  month_result,
  "08_Model_Results/R/Task4_PostApril_Month_Welch_ANOVA.csv",
  row.names = FALSE
)


# ----------------------------------------------------------------------------
# Step 28: Monthly Boxplot
# ----------------------------------------------------------------------------

png(
  "06_Charts/Task4_06_PostApril_Month_Boxplot.png",
  width = 1200,
  height = 750
)

boxplot(
  Quantity ~ Month,
  data = df_post,
  main = "Post-April-2025 Quantity by Calendar Month",
  xlab = "Month",
  ylab = "Quantity",
  col = "lightblue",
  las = 2
)

dev.off()


# ============================================================================
# ROBUSTNESS ANALYSES
# ============================================================================


# ----------------------------------------------------------------------------
# Step 29: Product Sensitivity - Exclude 41 High-Volume Orders
# ----------------------------------------------------------------------------

df_product_sensitivity =
  df_full[
    df_full$Outlier_IQR_Flag == "No",
  ]

print(nrow(df_product_sensitivity))
print(table(df_product_sensitivity$`Product Name`))


product_sensitivity_bf =
  leveneTest(
    Quantity ~ `Product Name`,
    data = df_product_sensitivity,
    center = median
  )

product_sensitivity_bf_F =
  product_sensitivity_bf[1, "F value"]

product_sensitivity_bf_p =
  product_sensitivity_bf[1, "Pr(>F)"]


product_sensitivity_anova =
  aov(
    Quantity ~ `Product Name`,
    data = df_product_sensitivity
  )

print(summary(product_sensitivity_anova))

product_sensitivity_table =
  summary(product_sensitivity_anova)[[1]]

product_sensitivity_F =
  product_sensitivity_table[1, "F value"]

product_sensitivity_p =
  product_sensitivity_table[1, "Pr(>F)"]

product_sensitivity_effect =
  anova_effect_sizes(
    product_sensitivity_anova
  )

print(product_sensitivity_effect)


# ----------------------------------------------------------------------------
# Step 30: Product Comparison Within Post-April Regime
# ----------------------------------------------------------------------------

post_product_distribution =
  group_distribution(
    df_post,
    "Product Name"
  )

names(post_product_distribution)[1] =
  "Product_Name"

print(post_product_distribution)


post_product_bf =
  leveneTest(
    Quantity ~ `Product Name`,
    data = df_post,
    center = median
  )

post_product_bf_F =
  post_product_bf[1, "F value"]

post_product_bf_p =
  post_product_bf[1, "Pr(>F)"]


post_product_anova =
  aov(
    Quantity ~ `Product Name`,
    data = df_post
  )

print(summary(post_product_anova))

post_product_table =
  summary(post_product_anova)[[1]]

post_product_F =
  post_product_table[1, "F value"]

post_product_p =
  post_product_table[1, "Pr(>F)"]

post_product_effect =
  anova_effect_sizes(
    post_product_anova
  )

print(post_product_effect)


# Save product robustness results

product_robustness =
  data.frame(
    Analysis = c(
      "Primary complete-data product ANOVA",
      "High-volume orders temporarily excluded",
      "Post-April-2025 regime only"
    ),
    n = c(
      nrow(df_full),
      nrow(df_product_sensitivity),
      nrow(df_post)
    ),
    F_Statistic = c(
      product_F,
      product_sensitivity_F,
      post_product_F
    ),
    p_value = c(
      product_p,
      product_sensitivity_p,
      post_product_p
    ),
    Eta_Squared = c(
      product_effect["Eta_Squared"],
      product_sensitivity_effect["Eta_Squared"],
      post_product_effect["Eta_Squared"]
    ),
    Omega_Squared = c(
      product_effect["Omega_Squared"],
      product_sensitivity_effect["Omega_Squared"],
      post_product_effect["Omega_Squared"]
    ),
    Brown_Forsythe_F = c(
      product_bf_F,
      product_sensitivity_bf_F,
      post_product_bf_F
    ),
    Brown_Forsythe_p = c(
      product_bf_p,
      product_sensitivity_bf_p,
      post_product_bf_p
    )
  )

print(product_robustness)

write.csv(
  product_robustness,
  "08_Model_Results/R/Task4_Product_Robustness.csv",
  row.names = FALSE
)


# ----------------------------------------------------------------------------
# Step 31: Comparison of Proportions - Not Selected
# ----------------------------------------------------------------------------

cat("\nCOMPARISON OF PROPORTIONS\n")

cat(
  "A comparison-of-proportions test was considered but was not selected because the current Task 4 business questions use Quantity as a continuous/count response and there is no natural binary success/failure outcome that should be created only for the purpose of forcing a proportions test.\n"
)


# ============================================================================
# FINAL TASK 4 SUMMARY
# ============================================================================


# ----------------------------------------------------------------------------
# Step 32: Consolidated Statistical Inference Results
# ----------------------------------------------------------------------------

task4_results =
  data.frame(
    Analysis = c(
      "Product mean Quantity",
      "Product Quantity variability",
      "Japan vs Local Quantity",
      "Small Dealer vs Shop Quantity",
      "Calendar-month mean Quantity"
    ),
    Scope = c(
      "All Regular Orders",
      "All Regular Orders",
      "Same buyer, Main Dealer, post-April",
      "Pre-April-2025 only",
      "Post-April-2025 only"
    ),
    Test = c(
      "One-way ANOVA",
      "Brown-Forsythe",
      "Welch t-test",
      "Welch t-test",
      "Welch ANOVA"
    ),
    Statistic = c(
      product_F,
      product_bf_F,
      market_t_stat,
      channel_t_stat,
      month_F
    ),
    DF1_or_df = c(
      product_df1,
      3,
      market_t_df,
      channel_t_df,
      month_df1
    ),
    DF2 = c(
      product_df2,
      3112,
      NA,
      NA,
      month_df2
    ),
    p_value = c(
      product_p,
      product_bf_p,
      market_t_p,
      channel_t_p,
      month_p
    ),
    Effect_Size_Name = c(
      "Eta-squared",
      "Variance ratio",
      "Hedges g",
      "Hedges g",
      "Welch f-squared"
    ),
    Effect_Size = c(
      product_effect["Eta_Squared"],
      product_variance_ratio,
      market_effect["Hedges_g"],
      channel_effect["Hedges_g"],
      welch_f_squared
    ),
    Mean_Difference = c(
      NA,
      NA,
      market_mean_difference,
      channel_mean_difference,
      NA
    ),
    CI_95_Lower = c(
      NA,
      NA,
      market_ci_lower,
      channel_ci_lower,
      NA
    ),
    CI_95_Upper = c(
      NA,
      NA,
      market_ci_upper,
      channel_ci_upper,
      NA
    ),
    Decision = c(
      "Fail to reject H0",
      "Fail to reject H0",
      "Reject H0",
      "Reject H0",
      "Fail to reject H0"
    )
  )

print(task4_results)

write.csv(
  task4_results,
  "08_Model_Results/R/Task4_Final_Inference_Summary.csv",
  row.names = FALSE
)


# ----------------------------------------------------------------------------
# Step 33: Save Report-Ready Interpretation Notes
# ----------------------------------------------------------------------------

sink(
  "08_Model_Results/R/Task4_Report_Ready_Summary.txt"
)

cat("TASK 4 - STATISTICAL INFERENCE\n\n")

cat("Significance level: 0.05\n")
cat("Confidence level: 95%\n")
cat("Main response variable: Quantity\n")
cat("Main dataset: Regular Orders only\n\n")


cat("1. PRODUCT MEAN DIFFERENCES\n")
cat("Business Question: Does mean Regular Order Quantity differ among the four perfumes?\n")
cat("H0: Mean Quantity is equal across all four products.\n")
cat("H1: At least one product has a different mean Quantity.\n")
cat("Selected Test: One-way ANOVA.\n")
cat(
  "Result: F(",
  product_df1,
  ", ",
  product_df2,
  ") = ",
  round(product_F, 4),
  ", p = ",
  round(product_p, 6),
  ".\n",
  sep = ""
)
cat(
  "Eta-squared = ",
  round(product_effect["Eta_Squared"], 6),
  "; adjusted omega-squared = ",
  round(product_effect["Omega_Squared"], 6),
  ".\n",
  sep = ""
)
cat(
  "Conclusion: Fail to reject H0. Mean transaction-level Quantity does not show a statistically supported product difference.\n"
)
cat(
  "Business meaning: Differences in total product demand are likely influenced more by order frequency than by substantially different average transaction sizes.\n\n"
)


cat("2. PRODUCT QUANTITY VARIABILITY\n")
cat("H0: Quantity variance is equal across the four products.\n")
cat("H1: At least one product has a different variance.\n")
cat(
  "Brown-Forsythe F = ",
  round(product_bf_F, 4),
  ", p = ",
  round(product_bf_p, 6),
  ".\n",
  sep = ""
)
cat(
  "Variance ratio = ",
  round(product_variance_ratio, 4),
  ".\n",
  sep = ""
)
cat(
  "Conclusion: Fail to reject H0. The data do not show a statistically significant difference in product-level transaction Quantity variability.\n\n"
)


cat("3. CONTROLLED LOCAL VS JAPAN COMPARISON\n")
cat(
  "Scope: Same buyer (Prabath Liyanarachchi), Main Dealer channel, post-April-2025 regime.\n"
)
cat("H0: Mean Japan Quantity = Mean Local Quantity.\n")
cat("H1: Mean Japan Quantity differs from Mean Local Quantity.\n")
cat("Selected Test: Welch independent-samples t-test.\n")
cat(
  "Local n = ",
  length(local_quantity),
  ", mean = ",
  round(mean(local_quantity), 4),
  ".\n",
  sep = ""
)
cat(
  "Japan n = ",
  length(japan_quantity),
  ", mean = ",
  round(mean(japan_quantity), 4),
  ".\n",
  sep = ""
)
cat(
  "t(",
  round(market_t_df, 2),
  ") = ",
  round(market_t_stat, 4),
  ", p = ",
  format(market_t_p, scientific = TRUE),
  ".\n",
  sep = ""
)
cat(
  "Japan - Local mean difference = ",
  round(market_mean_difference, 4),
  " units; 95% CI [",
  round(market_ci_lower, 4),
  ", ",
  round(market_ci_upper, 4),
  "].\n",
  sep = ""
)
cat(
  "Hedges g = ",
  round(market_effect["Hedges_g"], 4),
  ".\n",
  sep = ""
)
cat(
  "Conclusion: Reject H0. The controlled subset shows a statistically significant association between market label and transaction Quantity.\n"
)
cat(
  "Limitation: This is observational repeated-buyer data and should not be interpreted as a causal market effect.\n\n"
)


cat("4. PRE-APRIL SHOP VS SMALL DEALER\n")
cat("H0: Mean Small Dealer Quantity = Mean Shop Quantity.\n")
cat("H1: Mean quantities differ.\n")
cat("Selected Test: Welch independent-samples t-test.\n")
cat(
  "Shop mean = ",
  round(mean(shop_quantity), 4),
  "; Small Dealer mean = ",
  round(mean(small_dealer_quantity), 4),
  ".\n",
  sep = ""
)
cat(
  "t(",
  round(channel_t_df, 2),
  ") = ",
  round(channel_t_stat, 4),
  ", p = ",
  format(channel_t_p, scientific = TRUE),
  ".\n",
  sep = ""
)
cat(
  "Small Dealer - Shop mean difference = ",
  round(channel_mean_difference, 4),
  " units; 95% CI [",
  round(channel_ci_lower, 4),
  ", ",
  round(channel_ci_upper, 4),
  "].\n",
  sep = ""
)
cat(
  "Hedges g = ",
  round(channel_effect["Hedges_g"], 4),
  ".\n",
  sep = ""
)
cat(
  "Conclusion: Reject H0. Mean transaction Quantity differed strongly between Shop and Small Dealer records in the pre-April operating regime.\n"
)
cat(
  "Limitation: Channel differences are connected to the historical business structure and should not be described as causal channel effects.\n\n"
)


cat("5. POST-APRIL CALENDAR MONTH COMPARISON\n")
cat("H0: Mean Quantity is equal across represented calendar months.\n")
cat("H1: At least one calendar month differs.\n")
cat("Selected Test: Welch one-way ANOVA.\n")
cat(
  "F(",
  round(month_df1, 2),
  ", ",
  round(month_df2, 2),
  ") = ",
  round(month_F, 4),
  ", p = ",
  round(month_p, 6),
  ".\n",
  sep = ""
)
cat(
  "Welch f-squared = ",
  round(welch_f_squared, 6),
  ".\n",
  sep = ""
)
cat(
  "Conclusion: Fail to reject H0. The post-April data do not provide statistically significant evidence of different mean transaction Quantity across calendar months.\n"
)
cat(
  "Limitation: The available data do not contain multiple complete annual cycles, so this is not proof that seasonality is absent.\n\n"
)


cat("6. PRODUCT ROBUSTNESS\n")
cat(
  "After excluding the 41 valid high-volume orders: F = ",
  round(product_sensitivity_F, 4),
  ", p = ",
  round(product_sensitivity_p, 6),
  ".\n",
  sep = ""
)
cat(
  "Post-April product-only comparison: F = ",
  round(post_product_F, 4),
  ", p = ",
  round(post_product_p, 6),
  ".\n",
  sep = ""
)
cat(
  "Both robustness analyses produce the same substantive conclusion as the primary product ANOVA.\n\n"
)


cat("GENERAL LIMITATIONS\n")
cat(
  "- Historical observational data do not establish causality.\n"
)
cat(
  "- Multiple transactions may belong to the same buyer, so complete observation-level independence may not hold.\n"
)
cat(
  "- Channel Type and Sales Regime are structurally linked around 1 April 2025.\n"
)
cat(
  "- All Japan records belong to one buyer in the Main Dealer/post-April structure.\n"
)
cat(
  "- The 41 high-volume transactions are valid business observations and are retained in the primary analyses.\n"
)
cat(
  "- Calendar-month analysis should not be interpreted as a complete seasonal study because the dataset does not contain multiple complete annual cycles.\n"
)

sink()


# ----------------------------------------------------------------------------
# Step 34: Final Validation Against the Completed Task 4 Notebook
# ----------------------------------------------------------------------------

stopifnot(

  # Dataset
  nrow(df_full) == 3116,
  ncol(df_full) == 22,
  sum(is.na(df_full)) == 0,
  sum(duplicated(df_full)) == 0,

  # Subsets
  nrow(df_post) == 2014,
  nrow(df_pre) == 1102,
  nrow(df_market) == 336,
  nrow(df_channel_pre) == 1102,
  sum(df_full$Outlier_IQR_Flag == "Yes") == 41,

  # Product ANOVA
  abs(product_F - 0.3821) < 0.001,
  abs(product_p - 0.765907) < 0.0001,
  abs(product_effect["Eta_Squared"] - 0.000368) < 0.00001,

  # Product variance
  abs(product_bf_F - 0.1349) < 0.01,
  abs(product_bf_p - 0.939264) < 0.001,

  # Controlled market
  length(local_quantity) == 151,
  length(japan_quantity) == 185,
  abs(mean(local_quantity) - 283.7020) < 0.001,
  abs(mean(japan_quantity) - 622.5297) < 0.001,
  abs(market_t_stat - 18.0612) < 0.001,
  abs(market_t_df - 308.8171) < 0.01,
  abs(market_mean_difference - 338.8277) < 0.001,
  abs(market_ci_lower - 301.9143) < 0.01,
  abs(market_ci_upper - 375.7412) < 0.01,
  abs(market_effect["Hedges_g"] - 1.8845) < 0.001,

  # Market sensitivity
  length(local_sens) == 151,
  length(japan_sens) == 144,
  abs(as.numeric(market_sens_t_test$statistic) - 15.0819) < 0.001,
  abs(market_sens_effect["Hedges_g"] - 1.7617) < 0.001,

  # Channel
  length(shop_quantity) == 751,
  length(small_dealer_quantity) == 351,
  abs(mean(shop_quantity) - 29.8921) < 0.001,
  abs(mean(small_dealer_quantity) - 173.1225) < 0.001,
  abs(channel_t_stat - 37.0431) < 0.001,
  abs(channel_t_df - 351.1352) < 0.01,
  abs(channel_mean_difference - 143.2304) < 0.001,
  abs(channel_ci_lower - 135.6258) < 0.01,
  abs(channel_ci_upper - 150.8349) < 0.01,
  abs(channel_effect["Hedges_g"] - 3.4927) < 0.001,

  # Month
  abs(month_F - 1.1124) < 0.01,
  abs(month_df1 - 11) < 0.01,
  abs(month_df2 - 173.5519) < 0.1,
  abs(month_p - 0.354153) < 0.001,
  abs(welch_f_squared - 0.006309) < 0.0001,

  # Product robustness
  nrow(df_product_sensitivity) == 3075,
  abs(product_sensitivity_F - 0.5290) < 0.001,
  abs(product_sensitivity_p - 0.662352) < 0.001,
  abs(post_product_F - 0.2901) < 0.001,
  abs(post_product_p - 0.832578) < 0.001
)

print("All Task 4 validation checks passed.")


# ----------------------------------------------------------------------------
# Step 35: Show Created Output Files
# ----------------------------------------------------------------------------

cat("\nTask 4 result files:\n")

print(
  list.files(
    "08_Model_Results/R",
    pattern = "^Task4",
    full.names = TRUE
  )
)

cat("\nTask 4 chart files:\n")

print(
  list.files(
    "06_Charts",
    pattern = "^Task4",
    full.names = TRUE
  )
)


# ----------------------------------------------------------------------------
# Step 36: Final Message
# ----------------------------------------------------------------------------

cat("\n============================================================\n")
cat("TASK 4 COMPLETE\n")
cat("============================================================\n")

cat(
  "Product ANOVA: F(",
  product_df1,
  ", ",
  product_df2,
  ") = ",
  round(product_F, 4),
  ", p = ",
  round(product_p, 4),
  "\n",
  sep = ""
)

cat(
  "Controlled Japan vs Local: t(",
  round(market_t_df, 2),
  ") = ",
  round(market_t_stat, 4),
  ", p < 0.001\n",
  sep = ""
)

cat(
  "Pre-April Small Dealer vs Shop: t(",
  round(channel_t_df, 2),
  ") = ",
  round(channel_t_stat, 4),
  ", p < 0.001\n",
  sep = ""
)

cat(
  "Post-April month Welch ANOVA: F(",
  round(month_df1, 2),
  ", ",
  round(month_df2, 2),
  ") = ",
  round(month_F, 4),
  ", p = ",
  round(month_p, 4),
  "\n",
  sep = ""
)

cat(
  "Task 4 statistical inference completed successfully.\n"
)
