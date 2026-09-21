# ============================================================================
# IT3081 - Statistical Modelling
# Fragceylon Retail Product Demand Analysis
# TASK 3A - Dataset Audit and Data Cleaning

# ----------------------------------------------------------------------------
# Step 1: Set the Working Directory
# ----------------------------------------------------------------------------

setwd("C:/Users/saths/Desktop/Sliit/3rd year/3rd Year 1st sem/IT3081-SM/Project/IT3081-Fragceylon-Retail-Demand-Analysis")

getwd()


# ----------------------------------------------------------------------------
# Step 2: Load Required Libraries
# ----------------------------------------------------------------------------

library(readxl)
library(writexl)


# ----------------------------------------------------------------------------
# Step 3: Create Output Folders
# ----------------------------------------------------------------------------

dir.create("04_Cleaned_Data", showWarnings = FALSE)
dir.create("08_Model_Results", showWarnings = FALSE)
dir.create("08_Model_Results/R", showWarnings = FALSE)


# ----------------------------------------------------------------------------
# Step 4: Load the Raw Excel Dataset
# ----------------------------------------------------------------------------

raw_file = "03_Raw_Data/Fragceylon_Sales_Dataset.xlsx"

sheets = excel_sheets(raw_file)
print(sheets)

df_raw = read_excel(raw_file, sheet = "Sales Data")

# Keep a separate working copy.
df = df_raw


# ----------------------------------------------------------------------------
# Step 5: Examine the Dataset
# ----------------------------------------------------------------------------

head(df)
str(df)
dim(df)
nrow(df)
ncol(df)
colnames(df)
summary(df)


# ----------------------------------------------------------------------------
# Step 6: Standardize Data Types
# ----------------------------------------------------------------------------

# Convert Date to R Date format.
df$Date = as.Date(df$Date)

# Remove leading/trailing spaces from text columns.
text_columns = c(
  "Order ID",
  "Product Name",
  "Product Code",
  "Channel Type",
  "Buyer",
  "Line Type",
  "Export Market"
)

for(col in text_columns) {
  df[[col]] = trimws(as.character(df[[col]]))
}

# Convert numeric variables.
df$Quantity = as.numeric(df$Quantity)
df$`Unit Price (LKR)` = as.numeric(df$`Unit Price (LKR)`)
df$`Total Value (LKR)` = as.numeric(df$`Total Value (LKR)`)


# ----------------------------------------------------------------------------
# Step 7: Missing Value Analysis
# ----------------------------------------------------------------------------

missing_values = sapply(df, function(x) sum(is.na(x)))
missing_percent = round((missing_values / nrow(df)) * 100, 2)

missing_table = data.frame(
  Variable = names(missing_values),
  Missing = as.numeric(missing_values),
  Missing_Percent = as.numeric(missing_percent)
)

print(missing_table)

total_missing = sum(is.na(df))
print(total_missing)


# ----------------------------------------------------------------------------
# Step 8: Date Validation
# ----------------------------------------------------------------------------

invalid_dates = sum(is.na(df$Date))
earliest_date = min(df$Date, na.rm = TRUE)
latest_date = max(df$Date, na.rm = TRUE)
unique_dates = length(unique(df$Date))

calendar_span = as.integer(latest_date - earliest_date) + 1
dates_without_transactions = calendar_span - unique_dates

print(invalid_dates)
print(earliest_date)
print(latest_date)
print(unique_dates)
print(calendar_span)
print(dates_without_transactions)


# ----------------------------------------------------------------------------
# Step 9: Duplicate Analysis
# ----------------------------------------------------------------------------

# Exact duplicate rows
exact_duplicates = sum(duplicated(df))
print(exact_duplicates)

# Duplicate Order IDs
duplicate_order_ids = sum(duplicated(df$`Order ID`))
print(duplicate_order_ids)

# Order ID format check
invalid_order_ids = sum(!grepl("^FC-[0-9]{5}$", df$`Order ID`))
print(invalid_order_ids)

# Order ID sequence check
order_numbers = as.integer(sub("FC-", "", df$`Order ID`))

min_order_number = min(order_numbers)
max_order_number = max(order_numbers)

expected_order_numbers = seq(min_order_number, max_order_number)
missing_order_numbers = setdiff(expected_order_numbers, order_numbers)

print(sprintf("FC-%05d", min_order_number))
print(sprintf("FC-%05d", max_order_number))
print(length(missing_order_numbers))


# ----------------------------------------------------------------------------
# Step 10: Check Duplicate-Looking Regular Orders
# ----------------------------------------------------------------------------

regular_check = df[df$`Line Type` == "Regular Order", ]

transaction_columns = setdiff(names(regular_check), "Order ID")

duplicate_regular_flag =
  duplicated(regular_check[, transaction_columns]) |
  duplicated(regular_check[, transaction_columns], fromLast = TRUE)

duplicate_regular_records =
  regular_check[duplicate_regular_flag, ]

print(nrow(duplicate_regular_records))
print(duplicate_regular_records)


# ----------------------------------------------------------------------------
# Step 11: Product and Product-Code Validation
# ----------------------------------------------------------------------------

product_code_table = table(df$`Product Name`, df$`Product Code`)
print(product_code_table)

unique_products = length(unique(df$`Product Name`))
unique_product_codes = length(unique(df$`Product Code`))

print(unique_products)
print(unique_product_codes)

product_pairs = unique(
  df[, c("Product Name", "Product Code")]
)

print(product_pairs)


# ----------------------------------------------------------------------------
# Step 12: Category Counts
# ----------------------------------------------------------------------------

channel_counts = table(df$`Channel Type`)
market_counts = table(df$`Export Market`)
line_type_counts = table(df$`Line Type`)
unique_buyers = length(unique(df$Buyer))

print(channel_counts)
print(market_counts)
print(line_type_counts)
print(unique_buyers)


# ----------------------------------------------------------------------------
# Step 13: Numeric Validity Checks
# ----------------------------------------------------------------------------

quantity_zero = sum(df$Quantity == 0, na.rm = TRUE)
quantity_negative = sum(df$Quantity < 0, na.rm = TRUE)

unit_price_zero = sum(df$`Unit Price (LKR)` == 0, na.rm = TRUE)
unit_price_negative = sum(df$`Unit Price (LKR)` < 0, na.rm = TRUE)

total_value_zero = sum(df$`Total Value (LKR)` == 0, na.rm = TRUE)
total_value_negative = sum(df$`Total Value (LKR)` < 0, na.rm = TRUE)

print(quantity_zero)
print(quantity_negative)
print(unit_price_zero)
print(unit_price_negative)
print(total_value_zero)
print(total_value_negative)

print(min(df$Quantity))
print(max(df$Quantity))

print(min(df$`Unit Price (LKR)`))
print(max(df$`Unit Price (LKR)`))

print(min(df$`Total Value (LKR)`))
print(max(df$`Total Value (LKR)`))

print(sort(unique(df$`Unit Price (LKR)`)))


# ----------------------------------------------------------------------------
# Step 14: Separate Regular Orders and Free Samples
# ----------------------------------------------------------------------------

regular_orders = df[df$`Line Type` == "Regular Order", ]
free_samples = df[df$`Line Type` == "Free Sample", ]

print(nrow(regular_orders))
print(nrow(free_samples))

free_sample_wrong_quantity =
  sum(free_samples$Quantity != 1)

free_sample_wrong_value =
  sum(free_samples$`Total Value (LKR)` != 0)

print(free_sample_wrong_quantity)
print(free_sample_wrong_value)

# Channel x Line Type
print(table(df$`Channel Type`, df$`Line Type`))

# Quantity totals
total_raw_quantity = sum(df$Quantity)
free_sample_quantity = sum(free_samples$Quantity)
regular_order_quantity = sum(regular_orders$Quantity)

print(total_raw_quantity)
print(free_sample_quantity)
print(regular_order_quantity)


# ----------------------------------------------------------------------------
# Step 15: Validate Pricing Rules
# ----------------------------------------------------------------------------

# Pricing rules:
# Quantity > 100 = LKR 348
# Otherwise Shop / Small Dealer = LKR 450
# Otherwise Main Dealer = LKR 380
# January receives an additional 5% reduction.

expected_price = ifelse(
  df$Quantity > 100,
  348,
  ifelse(
    df$`Channel Type` %in% c("Shop", "Small Dealer"),
    450,
    ifelse(
      df$`Channel Type` == "Main Dealer",
      380,
      NA
    )
  )
)

january_rows = format(df$Date, "%m") == "01"

expected_price[january_rows] =
  round(expected_price[january_rows] * 0.95, 2)

price_match =
  abs(df$`Unit Price (LKR)` - expected_price) < 0.000001

price_mismatches = sum(!price_match, na.rm = TRUE)

print(price_mismatches)


# ----------------------------------------------------------------------------
# Step 16: Validate Total Value
# ----------------------------------------------------------------------------

expected_total_value =
  round(df$Quantity * df$`Unit Price (LKR)`, 2)

regular_value_mismatch =
  df$`Line Type` == "Regular Order" &
  abs(df$`Total Value (LKR)` - expected_total_value) >= 0.000001

regular_value_mismatches =
  sum(regular_value_mismatch, na.rm = TRUE)

invalid_free_sample_values =
  sum(
    df$`Line Type` == "Free Sample" &
    df$`Total Value (LKR)` != 0
  )

regular_revenue =
  sum(
    df$`Total Value (LKR)`[
      df$`Line Type` == "Regular Order"
    ]
  )

print(regular_value_mismatches)
print(invalid_free_sample_values)

cat(
  "Total Regular Order revenue (LKR):",
  formatC(
    regular_revenue,
    format = "f",
    digits = 2,
    big.mark = ","
  ),
  "\n"
)


# ----------------------------------------------------------------------------
# Step 17: Outlier Detection Using the IQR Method
# ----------------------------------------------------------------------------

regular_quantity =
  df$Quantity[df$`Line Type` == "Regular Order"]

Q1 = as.numeric(
  quantile(regular_quantity, 0.25)
)

Q3 = as.numeric(
  quantile(regular_quantity, 0.75)
)

IQR_value = Q3 - Q1

lower_fence = Q1 - 1.5 * IQR_value
upper_fence = Q3 + 1.5 * IQR_value

print(Q1)
print(Q3)
print(IQR_value)
print(lower_fence)
print(upper_fence)

df$Outlier_IQR_Flag = "No"

df$Outlier_IQR_Flag[
  df$`Line Type` == "Regular Order" &
  (
    df$Quantity < lower_fence |
    df$Quantity > upper_fence
  )
] = "Yes"

outlier_records =
  df[df$Outlier_IQR_Flag == "Yes", ]

print(nrow(outlier_records))
print(min(outlier_records$Quantity))
print(max(outlier_records$Quantity))

print(
  table(
    outlier_records$`Channel Type`,
    outlier_records$`Export Market`,
    outlier_records$Buyer
  )
)


# ----------------------------------------------------------------------------
# Step 18: Check the April 2025 Business-Structure Change
# ----------------------------------------------------------------------------

regime_change_date = as.Date("2025-04-01")

pre_change =
  df[df$Date < regime_change_date, ]

post_change =
  df[df$Date >= regime_change_date, ]

print(table(pre_change$`Channel Type`))
print(table(post_change$`Channel Type`))

print(max(pre_change$Date))
print(min(post_change$Date))


# ----------------------------------------------------------------------------
# Step 19: Check Japan Market Structure
# ----------------------------------------------------------------------------

japan_data =
  df[df$`Export Market` == "Japan", ]

print(nrow(japan_data))
print(table(japan_data$`Line Type`))
print(unique(japan_data$`Channel Type`))
print(unique(japan_data$Buyer))
print(min(japan_data$Date))
print(max(japan_data$Date))


# ----------------------------------------------------------------------------
# Step 20: Create Derived Variables
# ----------------------------------------------------------------------------

df$Year =
  as.integer(format(df$Date, "%Y"))

df$Month =
  month.name[as.integer(format(df$Date, "%m"))]

df$Month_Number =
  as.integer(format(df$Date, "%m"))

df$Quarter =
  paste0(
    "Q",
    ceiling(df$Month_Number / 3)
  )

df$Day_of_Week =
  weekdays(df$Date)

df$Year_Month =
  format(df$Date, "%Y-%m")

df$Time_Trend =
  as.integer(df$Date - min(df$Date))

df$Sales_Regime =
  ifelse(
    df$Date < as.Date("2025-04-01"),
    "Pre-April-2025",
    "Post-April-2025"
  )

df$Is_Regular_Order =
  ifelse(
    df$`Line Type` == "Regular Order",
    1,
    0
  )

df$Market_Group =
  ifelse(
    df$`Export Market` == "Local",
    "Local",
    "Export"
  )


# ----------------------------------------------------------------------------
# Step 21: Create Final Cleaned Datasets
# ----------------------------------------------------------------------------

cleaned_master = df

regular_orders_clean =
  cleaned_master[
    cleaned_master$`Line Type` == "Regular Order",
  ]

free_samples_clean =
  cleaned_master[
    cleaned_master$`Line Type` == "Free Sample",
  ]


# ----------------------------------------------------------------------------
# Step 22: Examine Final Cleaned Datasets
# ----------------------------------------------------------------------------

dim(cleaned_master)
dim(regular_orders_clean)
dim(free_samples_clean)

summary(regular_orders_clean$Quantity)

print(sum(is.na(cleaned_master)))
print(sum(regular_orders_clean$Quantity))
print(sum(regular_orders_clean$Outlier_IQR_Flag == "Yes"))


# ----------------------------------------------------------------------------
# Step 23: Final Validation
# ----------------------------------------------------------------------------

expected_duplicate_pair =
  c("FC-00986", "FC-00993")

expected_product_pairs =
  c(
    "Kennedy|003B",
    "Mesmerose|006B",
    "Secret Ambrosia|008A",
    "Dark Matrix|009A"
  )

actual_product_pairs =
  paste(
    product_pairs$`Product Name`,
    product_pairs$`Product Code`,
    sep = "|"
  )

stopifnot(
  nrow(df_raw) == 5481,
  ncol(df_raw) == 11,
  invalid_dates == 0,
  total_missing == 0,
  earliest_date == as.Date("2024-10-18"),
  latest_date == as.Date("2026-06-30"),
  unique_dates == 474,
  calendar_span == 621,
  dates_without_transactions == 147,
  exact_duplicates == 0,
  duplicate_order_ids == 0,
  invalid_order_ids == 0,
  min_order_number == 1,
  max_order_number == 5481,
  length(missing_order_numbers) == 0,
  nrow(duplicate_regular_records) == 2,
  setequal(
    duplicate_regular_records$`Order ID`,
    expected_duplicate_pair
  ),
  unique_products == 4,
  unique_product_codes == 4,
  setequal(actual_product_pairs, expected_product_pairs),
  unique_buyers == 49,
  nrow(regular_orders_clean) == 3116,
  nrow(free_samples_clean) == 2365,
  quantity_zero == 0,
  quantity_negative == 0,
  unit_price_zero == 0,
  unit_price_negative == 0,
  total_value_zero == 2365,
  total_value_negative == 0,
  free_sample_wrong_quantity == 0,
  free_sample_wrong_value == 0,
  price_mismatches == 0,
  regular_value_mismatches == 0,
  invalid_free_sample_values == 0,
  total_raw_quantity == 708175,
  free_sample_quantity == 2365,
  regular_order_quantity == 705810,
  abs(regular_revenue - 246708257.80) < 0.01,
  Q1 == 55,
  Q3 == 354,
  IQR_value == 299,
  lower_fence == -393.5,
  upper_fence == 802.5,
  nrow(outlier_records) == 41,
  min(outlier_records$Quantity) == 810,
  max(outlier_records$Quantity) == 998,
  nrow(cleaned_master) == 5481,
  ncol(cleaned_master) == 22,
  sum(is.na(cleaned_master)) == 0,
  sum(regular_orders_clean$Quantity) == 705810,
  sum(regular_orders_clean$Outlier_IQR_Flag == "Yes") == 41
)

print("All Task 3A validation checks passed.")


# ----------------------------------------------------------------------------
# Step 24: Export Cleaned Datasets
# ----------------------------------------------------------------------------

write_xlsx(
  cleaned_master,
  "04_Cleaned_Data/Fragceylon_Cleaned_Master.xlsx"
)

write_xlsx(
  regular_orders_clean,
  "04_Cleaned_Data/Fragceylon_Regular_Orders.xlsx"
)

write_xlsx(
  free_samples_clean,
  "04_Cleaned_Data/Fragceylon_Free_Samples.xlsx"
)


# ----------------------------------------------------------------------------
# Step 25: Save Audit Detail Files
# ----------------------------------------------------------------------------

write.csv(
  duplicate_regular_records,
  "08_Model_Results/R/Task3A_Duplicate_Looking_Regular_Orders.csv",
  row.names = FALSE
)

write.csv(
  outlier_records,
  "08_Model_Results/R/Task3A_IQR_Outliers.csv",
  row.names = FALSE
)


# ----------------------------------------------------------------------------
# Step 26: Create and Save Audit Summary
# ----------------------------------------------------------------------------

audit_summary = data.frame(
  Check = c(
    "Raw rows",
    "Raw columns",
    "Earliest date",
    "Latest date",
    "Calendar span days",
    "Unique transaction dates",
    "Calendar dates without transactions",
    "Invalid dates",
    "Missing values",
    "Exact duplicate rows",
    "Duplicate Order IDs",
    "Invalid Order ID formats",
    "Minimum Order ID",
    "Maximum Order ID",
    "Missing sequential Order IDs",
    "Duplicate-looking Regular Order rows",
    "Unique products",
    "Unique product codes",
    "Unique buyers",
    "Regular Orders",
    "Free Samples",
    "Zero Quantity records",
    "Negative Quantity records",
    "Zero Total Value records",
    "Negative Total Value records",
    "Pricing-rule mismatches",
    "Regular Order Total Value mismatches",
    "Free Samples with non-zero value",
    "Regular Order revenue (LKR)",
    "Quantity Q1",
    "Quantity Q3",
    "Quantity IQR",
    "IQR lower fence",
    "IQR upper fence",
    "Regular Order IQR outliers",
    "Total raw Quantity",
    "Free Sample Quantity",
    "Total Regular Order demand",
    "Final Master rows",
    "Final Master columns"
  ),
  Result = c(
    nrow(df_raw),
    ncol(df_raw),
    as.character(earliest_date),
    as.character(latest_date),
    calendar_span,
    unique_dates,
    dates_without_transactions,
    invalid_dates,
    total_missing,
    exact_duplicates,
    duplicate_order_ids,
    invalid_order_ids,
    sprintf("FC-%05d", min_order_number),
    sprintf("FC-%05d", max_order_number),
    length(missing_order_numbers),
    nrow(duplicate_regular_records),
    unique_products,
    unique_product_codes,
    unique_buyers,
    nrow(regular_orders_clean),
    nrow(free_samples_clean),
    quantity_zero,
    quantity_negative,
    total_value_zero,
    total_value_negative,
    price_mismatches,
    regular_value_mismatches,
    invalid_free_sample_values,
    sprintf("%.2f", regular_revenue),
    Q1,
    Q3,
    IQR_value,
    lower_fence,
    upper_fence,
    nrow(outlier_records),
    total_raw_quantity,
    free_sample_quantity,
    regular_order_quantity,
    nrow(cleaned_master),
    ncol(cleaned_master)
  )
)

print(audit_summary)

write.csv(
  audit_summary,
  "08_Model_Results/R/Task3A_Audit_Summary.csv",
  row.names = FALSE
)


# ----------------------------------------------------------------------------
# Step 27: Re-Import the Saved Regular Orders File
# ----------------------------------------------------------------------------

verification_data =
  read_excel(
    "04_Cleaned_Data/Fragceylon_Regular_Orders.xlsx"
  )

dim(verification_data)
sum(is.na(verification_data))
sum(verification_data$Quantity)
sum(verification_data$Outlier_IQR_Flag == "Yes")

stopifnot(
  nrow(verification_data) == 3116,
  ncol(verification_data) == 22,
  sum(is.na(verification_data)) == 0,
  sum(verification_data$Quantity) == 705810,
  sum(verification_data$Outlier_IQR_Flag == "Yes") == 41
)


# ----------------------------------------------------------------------------
# Step 28: Final Message
# ----------------------------------------------------------------------------

print("TASK 3A COMPLETE")
print("Cleaned Master: 5481 rows x 22 columns")
print("Regular Orders: 3116 rows")
print("Free Samples: 2365 rows")
print("Commercial Demand: 705810 units")
print("Retained IQR Outliers: 41")

cat(
  "Regular Order Revenue (LKR):",
  formatC(
    regular_revenue,
    format = "f",
    digits = 2,
    big.mark = ","
  ),
  "\n"
)

print("All files were saved inside IT3081-Fragceylon-Retail-Demand-Analysis.")

