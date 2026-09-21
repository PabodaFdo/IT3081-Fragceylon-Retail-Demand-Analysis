# ============================================================================
# IT3081 - Statistical Modelling
# Fragceylon Retail Product Demand Analysis
# TASK 3B - Descriptive Analysis and Visualisation



# ----------------------------------------------------------------------------
# Step 1: Set the Working Directory
# ----------------------------------------------------------------------------

setwd("C:/Users/saths/Desktop/Sliit/3rd year/3rd Year 1st sem/IT3081-SM/Project/IT3081-Fragceylon-Retail-Demand-Analysis")

getwd()


# ----------------------------------------------------------------------------
# Step 2: Load Required Libraries
# ----------------------------------------------------------------------------

library(readxl)


# ----------------------------------------------------------------------------
# Step 3: Create Output Folders
# ----------------------------------------------------------------------------

dir.create("06_Charts", showWarnings = FALSE)
dir.create("08_Model_Results", showWarnings = FALSE)
dir.create("08_Model_Results/R", showWarnings = FALSE)


# ----------------------------------------------------------------------------
# Step 4: Load the Cleaned Data
# ----------------------------------------------------------------------------

regular_file = "04_Cleaned_Data/Fragceylon_Regular_Orders.xlsx"
master_file = "04_Cleaned_Data/Fragceylon_Cleaned_Master.xlsx"

if(!file.exists(regular_file)) {
  stop("Fragceylon_Regular_Orders.xlsx was not found. Run Task 3A first.")
}

if(!file.exists(master_file)) {
  stop("Fragceylon_Cleaned_Master.xlsx was not found. Run Task 3A first.")
}

regular_orders = read_excel(regular_file)
cleaned_master = read_excel(master_file)


# ----------------------------------------------------------------------------
# Step 5: Examine the Cleaned Data
# ----------------------------------------------------------------------------

head(regular_orders)
str(regular_orders)
dim(regular_orders)
nrow(regular_orders)
ncol(regular_orders)
colnames(regular_orders)
summary(regular_orders)

head(cleaned_master)
dim(cleaned_master)


# ----------------------------------------------------------------------------
# Step 6: Prepare Variables for Analysis
# ----------------------------------------------------------------------------

regular_orders$Date = as.Date(regular_orders$Date)
cleaned_master$Date = as.Date(cleaned_master$Date)

regular_orders$`Product Name` = factor(regular_orders$`Product Name`)
regular_orders$`Channel Type` = factor(regular_orders$`Channel Type`)
regular_orders$`Export Market` = factor(regular_orders$`Export Market`)
regular_orders$Sales_Regime = factor(regular_orders$Sales_Regime)

regular_orders$Month = factor(
  regular_orders$Month,
  levels = month.name
)

regular_orders$Year = as.integer(regular_orders$Year)
regular_orders$Month_Number = as.integer(regular_orders$Month_Number)


# ----------------------------------------------------------------------------
# Step 7: Final Check Before Descriptive Analysis
# ----------------------------------------------------------------------------

print(nrow(regular_orders))
print(sum(is.na(regular_orders)))
print(sum(regular_orders$Quantity))
print(sum(regular_orders$`Total Value (LKR)`))

stopifnot(
  nrow(regular_orders) == 3116,
  nrow(cleaned_master) == 5481,
  sum(is.na(regular_orders)) == 0,
  sum(regular_orders$Quantity) == 705810,
  abs(sum(regular_orders$`Total Value (LKR)`) - 246708257.80) < 0.01
)

print("Task 3B input-data checks passed.")


# ----------------------------------------------------------------------------
# Step 8: Create a Descriptive Statistics Function
# ----------------------------------------------------------------------------
# Lab Sheet 01 introduces user-defined functions.
# This function calculates the descriptive statistics required for Quantity.

descriptive_statistics = function(x) {

  result = c(
    Count = length(x),
    Mean = mean(x),
    Median = median(x),
    Standard_Deviation = sd(x),
    Variance = var(x),
    Minimum = min(x),
    Q1 = as.numeric(quantile(x, 0.25)),
    Q3 = as.numeric(quantile(x, 0.75)),
    IQR = IQR(x),
    Maximum = max(x)
  )

  return(result)
}


# ----------------------------------------------------------------------------
# Step 9: Overall Descriptive Statistics for Regular Order Quantity
# ----------------------------------------------------------------------------

overall_quantity_stats = descriptive_statistics(
  regular_orders$Quantity
)

print(overall_quantity_stats)

overall_stats_table = data.frame(
  Statistic = names(overall_quantity_stats),
  Value = as.numeric(overall_quantity_stats)
)

print(overall_stats_table)

write.csv(
  overall_stats_table,
  "08_Model_Results/R/Task3B_Overall_Descriptive_Statistics.csv",
  row.names = FALSE
)


# ----------------------------------------------------------------------------
# Step 10: Check Main Quantity Distribution
# ----------------------------------------------------------------------------

summary(regular_orders$Quantity)

quantile(
  regular_orders$Quantity,
  probs = c(0, 0.25, 0.50, 0.75, 1)
)

sd(regular_orders$Quantity)
var(regular_orders$Quantity)
IQR(regular_orders$Quantity)


# ----------------------------------------------------------------------------
# Step 11: Create Grouped Descriptive Statistics Function
# ----------------------------------------------------------------------------

group_descriptive_statistics = function(data, group_column, group_label) {

  groups = split(
    data$Quantity,
    data[[group_column]]
  )

  result = t(
    sapply(
      groups,
      descriptive_statistics
    )
  )

  result = as.data.frame(result)

  result = data.frame(
    Group = rownames(result),
    result,
    row.names = NULL
  )

  names(result)[1] = group_label

  return(result)
}


# ----------------------------------------------------------------------------
# Step 12: Descriptive Statistics by Product
# ----------------------------------------------------------------------------

product_stats = group_descriptive_statistics(
  regular_orders,
  "Product Name",
  "Product_Name"
)

print(product_stats)

write.csv(
  product_stats,
  "08_Model_Results/R/Task3B_Descriptive_By_Product.csv",
  row.names = FALSE
)


# ----------------------------------------------------------------------------
# Step 13: Descriptive Statistics by Channel
# ----------------------------------------------------------------------------

channel_stats = group_descriptive_statistics(
  regular_orders,
  "Channel Type",
  "Channel_Type"
)

print(channel_stats)

write.csv(
  channel_stats,
  "08_Model_Results/R/Task3B_Descriptive_By_Channel.csv",
  row.names = FALSE
)


# ----------------------------------------------------------------------------
# Step 14: Descriptive Statistics by Market
# ----------------------------------------------------------------------------

market_stats = group_descriptive_statistics(
  regular_orders,
  "Export Market",
  "Export_Market"
)

print(market_stats)

write.csv(
  market_stats,
  "08_Model_Results/R/Task3B_Descriptive_By_Market.csv",
  row.names = FALSE
)


# ----------------------------------------------------------------------------
# Step 15: Descriptive Statistics by Month
# ----------------------------------------------------------------------------

month_stats = group_descriptive_statistics(
  regular_orders,
  "Month",
  "Month"
)

month_stats$Month = factor(
  month_stats$Month,
  levels = month.name
)

month_stats = month_stats[
  order(month_stats$Month),
]

month_stats$Month = as.character(
  month_stats$Month
)

print(month_stats)

write.csv(
  month_stats,
  "08_Model_Results/R/Task3B_Descriptive_By_Month.csv",
  row.names = FALSE
)


# ----------------------------------------------------------------------------
# Step 16: Descriptive Statistics by Year
# ----------------------------------------------------------------------------

year_stats = group_descriptive_statistics(
  regular_orders,
  "Year",
  "Year"
)

print(year_stats)

write.csv(
  year_stats,
  "08_Model_Results/R/Task3B_Descriptive_By_Year.csv",
  row.names = FALSE
)


# ----------------------------------------------------------------------------
# Step 17: Descriptive Statistics by Sales Regime
# ----------------------------------------------------------------------------

regime_stats = group_descriptive_statistics(
  regular_orders,
  "Sales_Regime",
  "Sales_Regime"
)

print(regime_stats)

write.csv(
  regime_stats,
  "08_Model_Results/R/Task3B_Descriptive_By_Sales_Regime.csv",
  row.names = FALSE
)


# ----------------------------------------------------------------------------
# Step 18: Total Demand by Product
# ----------------------------------------------------------------------------

demand_by_product = aggregate(
  Quantity ~ `Product Name`,
  data = regular_orders,
  FUN = sum
)

names(demand_by_product)[2] = "Total_Demand"

demand_by_product = demand_by_product[
  order(demand_by_product$Total_Demand, decreasing = TRUE),
]

print(demand_by_product)

write.csv(
  demand_by_product,
  "08_Model_Results/R/Task3B_Demand_By_Product.csv",
  row.names = FALSE
)


# ----------------------------------------------------------------------------
# Step 19: Average Order Quantity by Product
# ----------------------------------------------------------------------------

average_quantity_by_product = aggregate(
  Quantity ~ `Product Name`,
  data = regular_orders,
  FUN = mean
)

names(average_quantity_by_product)[2] =
  "Average_Order_Quantity"

average_quantity_by_product =
  average_quantity_by_product[
    order(
      average_quantity_by_product$Average_Order_Quantity,
      decreasing = TRUE
    ),
  ]

print(average_quantity_by_product)

write.csv(
  average_quantity_by_product,
  "08_Model_Results/R/Task3B_Average_Order_Quantity_By_Product.csv",
  row.names = FALSE
)


# ----------------------------------------------------------------------------
# Step 20: Total Demand by Channel
# ----------------------------------------------------------------------------

demand_by_channel = aggregate(
  Quantity ~ `Channel Type`,
  data = regular_orders,
  FUN = sum
)

names(demand_by_channel)[2] = "Total_Demand"

demand_by_channel = demand_by_channel[
  order(demand_by_channel$Total_Demand, decreasing = TRUE),
]

print(demand_by_channel)

write.csv(
  demand_by_channel,
  "08_Model_Results/R/Task3B_Demand_By_Channel.csv",
  row.names = FALSE
)


# ----------------------------------------------------------------------------
# Step 21: Total Demand by Market
# ----------------------------------------------------------------------------

demand_by_market = aggregate(
  Quantity ~ `Export Market`,
  data = regular_orders,
  FUN = sum
)

names(demand_by_market)[2] = "Total_Demand"

demand_by_market = demand_by_market[
  order(demand_by_market$Total_Demand, decreasing = TRUE),
]

print(demand_by_market)

write.csv(
  demand_by_market,
  "08_Model_Results/R/Task3B_Demand_By_Market.csv",
  row.names = FALSE
)


# ----------------------------------------------------------------------------
# Step 22: Monthly Total Demand
# ----------------------------------------------------------------------------

monthly_demand = aggregate(
  Quantity ~ Year_Month,
  data = regular_orders,
  FUN = sum
)

names(monthly_demand)[2] = "Total_Demand"

monthly_demand = monthly_demand[
  order(monthly_demand$Year_Month),
]

print(monthly_demand)

write.csv(
  monthly_demand,
  "08_Model_Results/R/Task3B_Monthly_Total_Demand.csv",
  row.names = FALSE
)


# ----------------------------------------------------------------------------
# Step 23: Product Demand Over Time
# ----------------------------------------------------------------------------

product_monthly_demand = aggregate(
  Quantity ~ Year_Month + `Product Name`,
  data = regular_orders,
  FUN = sum
)

names(product_monthly_demand)[3] = "Total_Demand"

product_monthly_demand = product_monthly_demand[
  order(
    product_monthly_demand$Year_Month,
    product_monthly_demand$`Product Name`
  ),
]

print(product_monthly_demand)

write.csv(
  product_monthly_demand,
  "08_Model_Results/R/Task3B_Product_Monthly_Demand.csv",
  row.names = FALSE
)


# ----------------------------------------------------------------------------
# Step 24: Revenue Statistics
# ----------------------------------------------------------------------------

total_revenue = sum(
  regular_orders$`Total Value (LKR)`
)

cat(
  "Total Regular Order Revenue (LKR):",
  formatC(
    total_revenue,
    format = "f",
    digits = 2,
    big.mark = ","
  ),
  "\n"
)


# Revenue by Product

revenue_by_product = aggregate(
  `Total Value (LKR)` ~ `Product Name`,
  data = regular_orders,
  FUN = sum
)

names(revenue_by_product)[2] = "Revenue_LKR"

revenue_by_product = revenue_by_product[
  order(revenue_by_product$Revenue_LKR, decreasing = TRUE),
]

print(revenue_by_product)

write.csv(
  revenue_by_product,
  "08_Model_Results/R/Task3B_Revenue_By_Product.csv",
  row.names = FALSE
)


# Revenue by Channel

revenue_by_channel = aggregate(
  `Total Value (LKR)` ~ `Channel Type`,
  data = regular_orders,
  FUN = sum
)

names(revenue_by_channel)[2] = "Revenue_LKR"

revenue_by_channel = revenue_by_channel[
  order(revenue_by_channel$Revenue_LKR, decreasing = TRUE),
]

print(revenue_by_channel)

write.csv(
  revenue_by_channel,
  "08_Model_Results/R/Task3B_Revenue_By_Channel.csv",
  row.names = FALSE
)


# Revenue by Market

revenue_by_market = aggregate(
  `Total Value (LKR)` ~ `Export Market`,
  data = regular_orders,
  FUN = sum
)

names(revenue_by_market)[2] = "Revenue_LKR"

revenue_by_market = revenue_by_market[
  order(revenue_by_market$Revenue_LKR, decreasing = TRUE),
]

print(revenue_by_market)

write.csv(
  revenue_by_market,
  "08_Model_Results/R/Task3B_Revenue_By_Market.csv",
  row.names = FALSE
)


# Monthly Revenue

monthly_revenue = aggregate(
  `Total Value (LKR)` ~ Year_Month,
  data = regular_orders,
  FUN = sum
)

names(monthly_revenue)[2] = "Revenue_LKR"

monthly_revenue = monthly_revenue[
  order(monthly_revenue$Year_Month),
]

print(monthly_revenue)

write.csv(
  monthly_revenue,
  "08_Model_Results/R/Task3B_Monthly_Revenue.csv",
  row.names = FALSE
)


# ----------------------------------------------------------------------------
# Step 25: Regular Orders vs Free Samples
# ----------------------------------------------------------------------------

line_type_rows = as.data.frame(
  table(cleaned_master$`Line Type`)
)

names(line_type_rows) = c(
  "Line_Type",
  "Number_of_Rows"
)

print(line_type_rows)

write.csv(
  line_type_rows,
  "08_Model_Results/R/Task3B_Regular_vs_Free_Sample_Rows.csv",
  row.names = FALSE
)


line_type_quantity = aggregate(
  Quantity ~ `Line Type`,
  data = cleaned_master,
  FUN = sum
)

names(line_type_quantity)[2] =
  "Total_Quantity"

print(line_type_quantity)

write.csv(
  line_type_quantity,
  "08_Model_Results/R/Task3B_Regular_vs_Free_Sample_Quantity.csv",
  row.names = FALSE
)


# ============================================================================
# VISUALISATIONS
# ============================================================================


# ----------------------------------------------------------------------------
# Step 26: Histogram of Regular Order Quantity
# ----------------------------------------------------------------------------

png(
  "06_Charts/Task3B_01_Quantity_Histogram.png",
  width = 1000,
  height = 700
)

hist(
  regular_orders$Quantity,
  main = "Distribution of Regular Order Quantity",
  xlab = "Quantity",
  ylab = "Frequency",
  col = "steelblue",
  border = "white"
)

dev.off()


# ----------------------------------------------------------------------------
# Step 27: Overall Boxplot of Regular Order Quantity
# ----------------------------------------------------------------------------

png(
  "06_Charts/Task3B_02_Quantity_Boxplot.png",
  width = 900,
  height = 700
)

boxplot(
  regular_orders$Quantity,
  main = "Boxplot of Regular Order Quantity",
  ylab = "Quantity",
  col = "lightblue"
)

dev.off()


# ----------------------------------------------------------------------------
# Step 28: Total Demand by Product
# ----------------------------------------------------------------------------

png(
  "06_Charts/Task3B_03_Total_Demand_By_Product.png",
  width = 1000,
  height = 700
)

barplot(
  demand_by_product$Total_Demand,
  names.arg = demand_by_product$`Product Name`,
  main = "Total Demand by Product",
  xlab = "Product",
  ylab = "Total Quantity",
  col = "steelblue",
  las = 2
)

dev.off()


# ----------------------------------------------------------------------------
# Step 29: Average Order Quantity by Product
# ----------------------------------------------------------------------------

png(
  "06_Charts/Task3B_04_Average_Order_Quantity_By_Product.png",
  width = 1000,
  height = 700
)

barplot(
  average_quantity_by_product$Average_Order_Quantity,
  names.arg = average_quantity_by_product$`Product Name`,
  main = "Average Order Quantity by Product",
  xlab = "Product",
  ylab = "Average Quantity",
  col = "steelblue",
  las = 2
)

dev.off()


# ----------------------------------------------------------------------------
# Step 30: Quantity Boxplot by Product
# ----------------------------------------------------------------------------

png(
  "06_Charts/Task3B_05_Quantity_Boxplot_By_Product.png",
  width = 1000,
  height = 700
)

boxplot(
  Quantity ~ `Product Name`,
  data = regular_orders,
  main = "Regular Order Quantity by Product",
  xlab = "Product",
  ylab = "Quantity",
  col = "lightblue",
  las = 2
)

dev.off()


# ----------------------------------------------------------------------------
# Step 31: Monthly Total Demand
# ----------------------------------------------------------------------------

png(
  "06_Charts/Task3B_06_Monthly_Total_Demand.png",
  width = 1200,
  height = 700
)

plot(
  1:nrow(monthly_demand),
  monthly_demand$Total_Demand,
  type = "o",
  pch = 16,
  main = "Monthly Total Demand",
  xlab = "Year-Month",
  ylab = "Total Quantity",
  xaxt = "n"
)

axis(
  1,
  at = 1:nrow(monthly_demand),
  labels = monthly_demand$Year_Month,
  las = 2,
  cex.axis = 0.75
)

dev.off()


# ----------------------------------------------------------------------------
# Step 32: Product Demand Over Time
# ----------------------------------------------------------------------------

product_monthly_wide = reshape(
  product_monthly_demand,
  idvar = "Year_Month",
  timevar = "Product Name",
  direction = "wide"
)

product_monthly_wide = product_monthly_wide[
  order(product_monthly_wide$Year_Month),
]

product_columns = grep(
  "^Total_Demand\\.",
  names(product_monthly_wide),
  value = TRUE
)

product_matrix = as.matrix(
  product_monthly_wide[, product_columns]
)

product_labels = sub(
  "^Total_Demand\\.",
  "",
  product_columns
)

png(
  "06_Charts/Task3B_07_Product_Demand_Over_Time.png",
  width = 1200,
  height = 750
)

matplot(
  1:nrow(product_monthly_wide),
  product_matrix,
  type = "o",
  lty = 1,
  pch = 1:length(product_columns),
  main = "Product Demand Over Time",
  xlab = "Year-Month",
  ylab = "Total Quantity",
  xaxt = "n"
)

axis(
  1,
  at = 1:nrow(product_monthly_wide),
  labels = product_monthly_wide$Year_Month,
  las = 2,
  cex.axis = 0.75
)

legend(
  "topleft",
  legend = product_labels,
  lty = 1,
  pch = 1:length(product_columns),
  cex = 0.8
)

dev.off()


# ----------------------------------------------------------------------------
# Step 33: Total Demand by Channel
# ----------------------------------------------------------------------------

png(
  "06_Charts/Task3B_08_Total_Demand_By_Channel.png",
  width = 900,
  height = 700
)

barplot(
  demand_by_channel$Total_Demand,
  names.arg = demand_by_channel$`Channel Type`,
  main = "Total Demand by Channel",
  xlab = "Channel",
  ylab = "Total Quantity",
  col = "steelblue",
  las = 2
)

dev.off()


# ----------------------------------------------------------------------------
# Step 34: Local vs Japan Demand
# ----------------------------------------------------------------------------

png(
  "06_Charts/Task3B_09_Demand_By_Market.png",
  width = 900,
  height = 700
)

barplot(
  demand_by_market$Total_Demand,
  names.arg = demand_by_market$`Export Market`,
  main = "Total Demand by Market",
  xlab = "Market",
  ylab = "Total Quantity",
  col = "steelblue"
)

dev.off()


# ----------------------------------------------------------------------------
# Step 35: Monthly Revenue
# ----------------------------------------------------------------------------

png(
  "06_Charts/Task3B_10_Monthly_Revenue.png",
  width = 1200,
  height = 700
)

plot(
  1:nrow(monthly_revenue),
  monthly_revenue$Revenue_LKR,
  type = "o",
  pch = 16,
  main = "Monthly Regular Order Revenue",
  xlab = "Year-Month",
  ylab = "Revenue (LKR)",
  xaxt = "n"
)

axis(
  1,
  at = 1:nrow(monthly_revenue),
  labels = monthly_revenue$Year_Month,
  las = 2,
  cex.axis = 0.75
)

dev.off()


# ----------------------------------------------------------------------------
# Step 36: Regular Orders vs Free Samples
# ----------------------------------------------------------------------------

png(
  "06_Charts/Task3B_11_Regular_Orders_vs_Free_Samples.png",
  width = 900,
  height = 700
)

barplot(
  line_type_rows$Number_of_Rows,
  names.arg = line_type_rows$Line_Type,
  main = "Regular Orders vs Free Samples",
  xlab = "Line Type",
  ylab = "Number of Records",
  col = "steelblue",
  las = 2
)

dev.off()


# ----------------------------------------------------------------------------
# Step 37: Quantity Distribution by Channel
# ----------------------------------------------------------------------------

png(
  "06_Charts/Task3B_12_Quantity_Boxplot_By_Channel.png",
  width = 1000,
  height = 700
)

boxplot(
  Quantity ~ `Channel Type`,
  data = regular_orders,
  main = "Regular Order Quantity by Channel",
  xlab = "Channel",
  ylab = "Quantity",
  col = "lightblue",
  las = 2
)

dev.off()


# ----------------------------------------------------------------------------
# Step 38: Quantity Distribution by Market
# ----------------------------------------------------------------------------

png(
  "06_Charts/Task3B_13_Quantity_Boxplot_By_Market.png",
  width = 900,
  height = 700
)

boxplot(
  Quantity ~ `Export Market`,
  data = regular_orders,
  main = "Regular Order Quantity by Market",
  xlab = "Market",
  ylab = "Quantity",
  col = "lightblue"
)

dev.off()


# ----------------------------------------------------------------------------
# Step 39: Demand by Sales Regime
# ----------------------------------------------------------------------------

demand_by_regime = aggregate(
  Quantity ~ Sales_Regime,
  data = regular_orders,
  FUN = sum
)

names(demand_by_regime)[2] = "Total_Demand"

print(demand_by_regime)

write.csv(
  demand_by_regime,
  "08_Model_Results/R/Task3B_Demand_By_Sales_Regime.csv",
  row.names = FALSE
)

png(
  "06_Charts/Task3B_14_Demand_By_Sales_Regime.png",
  width = 900,
  height = 700
)

barplot(
  demand_by_regime$Total_Demand,
  names.arg = demand_by_regime$Sales_Regime,
  main = "Total Demand by Sales Regime",
  xlab = "Sales Regime",
  ylab = "Total Quantity",
  col = "steelblue",
  las = 2
)

dev.off()


# ============================================================================
# INITIAL BUSINESS INSIGHTS
# ============================================================================


# ----------------------------------------------------------------------------
# Step 40: Identify Main Descriptive Findings
# ----------------------------------------------------------------------------

highest_total_product =
  demand_by_product[
    which.max(demand_by_product$Total_Demand),
  ]

lowest_total_product =
  demand_by_product[
    which.min(demand_by_product$Total_Demand),
  ]

highest_average_product =
  average_quantity_by_product[
    which.max(
      average_quantity_by_product$Average_Order_Quantity
    ),
  ]

peak_month =
  monthly_demand[
    which.max(monthly_demand$Total_Demand),
  ]

lowest_month =
  monthly_demand[
    which.min(monthly_demand$Total_Demand),
  ]

highest_channel =
  demand_by_channel[
    which.max(demand_by_channel$Total_Demand),
  ]

highest_market =
  demand_by_market[
    which.max(demand_by_market$Total_Demand),
  ]


cat("\n============================================================\n")
cat("TASK 3B - INITIAL BUSINESS INSIGHTS\n")
cat("============================================================\n")

cat(
  "Highest total-demand product:",
  as.character(highest_total_product$`Product Name`),
  "-",
  highest_total_product$Total_Demand,
  "units\n"
)

cat(
  "Lowest total-demand product:",
  as.character(lowest_total_product$`Product Name`),
  "-",
  lowest_total_product$Total_Demand,
  "units\n"
)

cat(
  "Highest average order quantity product:",
  as.character(highest_average_product$`Product Name`),
  "-",
  round(highest_average_product$Average_Order_Quantity, 2),
  "units per order\n"
)

cat(
  "Highest-demand Year-Month:",
  peak_month$Year_Month,
  "-",
  peak_month$Total_Demand,
  "units\n"
)

cat(
  "Lowest-demand Year-Month:",
  lowest_month$Year_Month,
  "-",
  lowest_month$Total_Demand,
  "units\n"
)

cat(
  "Largest total-demand channel:",
  as.character(highest_channel$`Channel Type`),
  "-",
  highest_channel$Total_Demand,
  "units\n"
)

cat(
  "Largest total-demand market:",
  as.character(highest_market$`Export Market`),
  "-",
  highest_market$Total_Demand,
  "units\n"
)

cat(
  "\nImportant interpretation note:\n",
  "Channel and sales regime are strongly connected because the business structure changed on 1 April 2025.\n",
  "Japan records also belong to one Main Dealer buyer in the post-April-2025 period.\n",
  "Therefore channel and Local/Japan differences are descriptive associations, not causal effects.\n",
  sep = ""
)


# ----------------------------------------------------------------------------
# Step 41: Save Initial Business Insights
# ----------------------------------------------------------------------------

sink(
  "08_Model_Results/R/Task3B_Initial_Business_Insights.txt"
)

cat("TASK 3B - INITIAL BUSINESS INSIGHTS\n\n")

cat(
  "Overall Regular Orders:",
  nrow(regular_orders),
  "\n"
)

cat(
  "Overall Commercial Demand:",
  sum(regular_orders$Quantity),
  "units\n"
)

cat(
  "Overall Regular Order Revenue (LKR):",
  formatC(
    total_revenue,
    format = "f",
    digits = 2,
    big.mark = ","
  ),
  "\n\n"
)

cat(
  "Highest total-demand product:",
  as.character(highest_total_product$`Product Name`),
  "-",
  highest_total_product$Total_Demand,
  "units\n"
)

cat(
  "Lowest total-demand product:",
  as.character(lowest_total_product$`Product Name`),
  "-",
  lowest_total_product$Total_Demand,
  "units\n"
)

cat(
  "Highest average order quantity product:",
  as.character(highest_average_product$`Product Name`),
  "-",
  round(highest_average_product$Average_Order_Quantity, 2),
  "units per order\n"
)

cat(
  "Highest-demand Year-Month:",
  peak_month$Year_Month,
  "-",
  peak_month$Total_Demand,
  "units\n"
)

cat(
  "Lowest-demand Year-Month:",
  lowest_month$Year_Month,
  "-",
  lowest_month$Total_Demand,
  "units\n"
)

cat(
  "Largest total-demand channel:",
  as.character(highest_channel$`Channel Type`),
  "-",
  highest_channel$Total_Demand,
  "units\n"
)

cat(
  "Largest total-demand market:",
  as.character(highest_market$`Export Market`),
  "-",
  highest_market$Total_Demand,
  "units\n\n"
)

cat(
  "INTERPRETATION LIMITATION\n"
)

cat(
  "Channel and Sales_Regime are strongly associated because the sales-channel structure changed on 1 April 2025.\n"
)

cat(
  "Japan records are associated with one buyer, Main Dealer channel, and the post-April-2025 regime.\n"
)

cat(
  "Therefore these descriptive differences should not be interpreted as causal channel or market effects.\n"
)

sink()


# ----------------------------------------------------------------------------
# Step 42: Validate Important Task 3B Results
# ----------------------------------------------------------------------------

stopifnot(
  length(overall_quantity_stats) == 10,
  overall_quantity_stats["Count"] == 3116,
  abs(overall_quantity_stats["Mean"] - 226.5115533) < 0.0001,
  overall_quantity_stats["Median"] == 199.5,
  abs(overall_quantity_stats["Standard_Deviation"] - 183.8781774) < 0.0001,
  abs(overall_quantity_stats["Variance"] - 33811.18414) < 0.01,
  overall_quantity_stats["Minimum"] == 23,
  overall_quantity_stats["Q1"] == 55,
  overall_quantity_stats["Q3"] == 354,
  overall_quantity_stats["IQR"] == 299,
  overall_quantity_stats["Maximum"] == 998,
  sum(demand_by_product$Total_Demand) == 705810,
  sum(demand_by_channel$Total_Demand) == 705810,
  sum(demand_by_market$Total_Demand) == 705810,
  sum(monthly_demand$Total_Demand) == 705810,
  abs(sum(revenue_by_product$Revenue_LKR) - 246708257.80) < 0.01,
  as.character(highest_total_product$`Product Name`) == "Mesmerose",
  highest_total_product$Total_Demand == 215866,
  peak_month$Year_Month == "2025-12",
  peak_month$Total_Demand == 115818
)

print("All Task 3B validation checks passed.")


# ----------------------------------------------------------------------------
# Step 43: Show Created Output Files
# ----------------------------------------------------------------------------

cat("\nTask 3B result files:\n")
print(
  list.files(
    "08_Model_Results/R",
    pattern = "^Task3B",
    full.names = TRUE
  )
)

cat("\nTask 3B chart files:\n")
print(
  list.files(
    "06_Charts",
    pattern = "^Task3B",
    full.names = TRUE
  )
)


# ----------------------------------------------------------------------------
# Step 44: Final Message
# ----------------------------------------------------------------------------

cat("\n============================================================\n")
cat("TASK 3B COMPLETE\n")
cat("============================================================\n")

cat("Overall Regular Orders:", nrow(regular_orders), "\n")
cat("Commercial Demand:", sum(regular_orders$Quantity), "units\n")
cat(
  "Regular Order Revenue (LKR):",
  formatC(
    total_revenue,
    format = "f",
    digits = 2,
    big.mark = ","
  ),
  "\n"
)

cat("Descriptive tables were saved to 08_Model_Results/R.\n")
cat("Charts were saved to 06_Charts.\n")
cat("TASK 3B descriptive analysis completed successfully.\n")

