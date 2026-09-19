# Fragceylon – TASK 3A Dataset Audit and Data Cleaning Log

## Project Information

**Topic:** Retail Product Demand Analysis  
**Company:** Fragceylon  
**Raw Dataset:** `Fragceylon_Sales_Dataset.xlsx`  
**Task:** TASK 3A – Full Dataset Audit and Data Cleaning  
**Status:** Dataset audit, cleaning implementation, dataset separation, export, and post-export verification completed. No predictive modelling has been performed.

---

## 1. Raw Data Preservation

The original workbook was preserved without modification.

- Original raw file: `Fragceylon_Sales_Dataset.xlsx`
- Cleaning was performed programmatically in Python using a separate working DataFrame named `df_clean`.
- The original Excel workbook was not overwritten.
- No raw observations were deleted or altered without a documented analytical reason.

Preserving the original dataset supports data integrity, traceability, reproducibility, and the ability to reproduce the analysis from the original business records.

---

## 2. Workbook Structure

The original Excel workbook contains two worksheets:

| Sheet | Used Range | Purpose |
|---|---:|---|
| Sales Data | A1:K5482 | Transaction/order-line records |
| Summary | A1:D26 | Aggregate totals and documented pricing/business rules |

### Dataset Size

- Data records: **5,481**
- Original variables: **11**
- Excel rows including the header: **5,482**

---

## 3. Original Variables and Analytical Roles

| Variable | Data Type | Description | Analytical Role |
|---|---|---|---|
| Order ID | Text | Unique transaction/order-line identifier | Traceability only |
| Date | Date/Datetime | Transaction date | Time-based analysis |
| Product Name | Categorical | Perfume name | Product comparison |
| Product Code | Categorical | Product identifier | Reference variable |
| Channel Type | Categorical | Shop / Small Dealer / Main Dealer | Channel analysis |
| Buyer | Categorical | Buyer/dealer/shop identity | Retained; used cautiously |
| Line Type | Categorical | Regular Order / Free Sample | Dataset separation |
| Quantity | Integer | Units recorded on the transaction line | Main response variable |
| Unit Price (LKR) | Numeric | Unit selling/reference price | Descriptive analysis; treated cautiously |
| Total Value (LKR) | Numeric | Transaction value | Revenue analysis |
| Export Market | Categorical | Local / Japan | Market analysis |

### Main Demand Definition

For this project:

**Product Demand = Quantity ordered/sold on Regular Order lines.**

The main response variable for subsequent statistical analysis is therefore:

**Quantity**

---

## 4. Date Validation

The `Date` variable was converted and validated programmatically using Python.

### Results

- Invalid dates: **0**
- Earliest transaction date: **18 October 2024**
- Latest transaction date: **30 June 2026**
- Calendar span: **621 days**
- Unique transaction dates: **474**
- Calendar dates without recorded transactions: **147**

All recorded date values were valid and successfully converted to datetime format.

A date without a recorded transaction was not automatically interpreted as zero demand because the absence of a row may represent no transaction, no recorded activity, closure, or another operational condition. No artificial zero-demand rows were introduced into the transaction-level dataset.

---

## 5. Product and Product-Code Validation

The workbook contains exactly four perfume products.

| Product Name | Product Code |
|---|---|
| Kennedy | 003B |
| Mesmerose | 006B |
| Secret Ambrosia | 008A |
| Dark Matrix | 009A |

### Validation Results

- Unique products: **4**
- Unique product codes: **4**
- Product-code inconsistencies: **0**
- Product names with multiple codes: **0**
- Product codes assigned to multiple products: **0**
- Leading/trailing-space inconsistencies: **0**
- Simple case inconsistencies: **0**

No product-name or product-code corrections were required.

---

## 6. Category Validation

### Channel Type

Three valid channel categories were identified:

| Channel | Rows |
|---|---:|
| Shop | 751 |
| Small Dealer | 702 |
| Main Dealer | 4,028 |
| **Total** | **5,481** |

### Export Market

Two market categories were identified:

| Market | Rows |
|---|---:|
| Local | 5,111 |
| Japan | 370 |
| **Total** | **5,481** |

### Buyer

- Unique buyers: **49**
- Missing Buyer values: **0**
- Leading/trailing-space inconsistencies: **0**
- Simple case inconsistencies: **0**

No category standardization changes were required.

---

## 7. Regular Orders and Free Samples

The dataset contains two distinct transaction-line types.

| Line Type | Rows |
|---|---:|
| Regular Order | **3,116** |
| Free Sample | **2,365** |
| **Total** | **5,481** |

### Free Sample Structure

All **2,365 Free Sample** rows have:

- Quantity = **1**
- Total Value = **0**
- Channel = Small Dealer or Main Dealer

No Shop records were classified as Free Samples.

### Channel Breakdown

| Channel | Regular Orders | Free Samples |
|---|---:|---:|
| Shop | 751 | 0 |
| Small Dealer | 351 | 351 |
| Main Dealer | 2,014 | 2,014 |

### Quantity Totals

- Total raw quantity including Free Samples: **708,175**
- Free Sample quantity: **2,365**
- Regular Order quantity: **705,810**

### Cleaning Decision

Free Samples are valid business records and were therefore retained in the cleaned master dataset.

However, Free Samples were separated from Regular Orders because they do not represent normal commercial demand.

The main demand analysis will use:

**Dataset A – Regular Orders**
- Rows: **3,116**
- Purpose: descriptive demand analysis, statistical inference, and later predictive modelling

A separate dataset was retained for:

**Dataset B – Free Samples**
- Rows: **2,365**
- Purpose: separate descriptive/business analysis where relevant

---

## 8. Missing-Value Assessment

A complete missing-value analysis was performed for all original variables.

| Variable | Missing | Missing % |
|---|---:|---:|
| Order ID | 0 | 0.00% |
| Date | 0 | 0.00% |
| Product Name | 0 | 0.00% |
| Product Code | 0 | 0.00% |
| Channel Type | 0 | 0.00% |
| Buyer | 0 | 0.00% |
| Line Type | 0 | 0.00% |
| Quantity | 0 | 0.00% |
| Unit Price (LKR) | 0 | 0.00% |
| Total Value (LKR) | 0 | 0.00% |
| Export Market | 0 | 0.00% |

### Cleaning Decision

- No observations were removed because of missing values.
- No statistical imputation was required.

---

## 9. Duplicate Assessment

### Exact Duplicate Rows

- Exact duplicate rows: **0**

### Order ID Validation

- Total Order IDs: **5,481**
- Unique Order IDs: **5,481**
- Duplicate Order IDs: **0**
- Order ID range: `FC-00001` to `FC-05481`
- Missing values within the sequential Order ID structure: **0**
- Invalid Order ID formats: **0**

### Duplicate-Looking Regular Order Pair

One pair of Regular Orders was identical across all transaction fields except `Order ID`.

The records were:

- `FC-00986`
- `FC-00993`

Transaction details:

| Variable | Value |
|---|---|
| Date | 17 January 2025 |
| Product | Mesmerose |
| Product Code | 006B |
| Channel | Shop |
| Buyer | Shop_21 |
| Quantity | 36 |
| Unit Price | LKR 427.50 |
| Total Value | LKR 15,390 |
| Market | Local |

### Cleaning Decision

Both records were retained.

The Order IDs are unique, and there is insufficient evidence to classify either transaction as an erroneous duplicate. Removing one observation could result in the loss of a legitimate transaction.

---

## 10. Numeric Validity Assessment

### Quantity

- Minimum raw Quantity: **1**
- Maximum raw Quantity: **998**
- Zero Quantity records: **0**
- Negative Quantity records: **0**
- Non-numeric Quantity records: **0**

For Regular Orders:

- Minimum Quantity: **23**
- Maximum Quantity: **998**

### Unit Price

- Minimum Unit Price: **LKR 330.60**
- Maximum Unit Price: **LKR 450.00**
- Zero Unit Price records: **0**
- Negative Unit Price records: **0**
- Non-numeric Unit Price records: **0**

Observed Unit Price values were:

- LKR 330.60
- LKR 348.00
- LKR 361.00
- LKR 380.00
- LKR 427.50
- LKR 450.00

### Total Value

- Minimum Total Value: **LKR 0**
- Maximum Total Value: **LKR 347,304**
- Negative Total Value records: **0**
- Zero Total Value records: **2,365**

All zero Total Value records were Free Samples.

### Returns and Corrections

No negative quantities or negative transaction values were identified.

The dataset does not contain a dedicated return/cancellation field. Therefore, the audit confirms only that no return or correction records could be identified through negative Quantity or Total Value values.

---

## 11. Pricing-Rule Validation

The pricing rules documented in the workbook were reproduced programmatically in Python.

The documented rules were:

- Shop / Small Dealer base price = **LKR 450**
- Main Dealer base price = **LKR 380**
- Quantity greater than 100 = **LKR 348 bulk rate**
- January transactions receive an additional **5% reduction**

The corresponding January prices are:

- 450 × 0.95 = **LKR 427.50**
- 380 × 0.95 = **LKR 361.00**
- 348 × 0.95 = **LKR 330.60**

### Validation Result

All **5,481 records** were compared with the expected Unit Price calculated from the documented rules.

- Pricing-rule mismatches: **0**

No Unit Price corrections were required.

### Analytical Implication

Unit Price is partly determined by:

- order Quantity
- Channel Type
- January pricing rules

Therefore, Unit Price is not fully independent of the response variable `Quantity`.

It will be treated cautiously in later analysis and will not automatically be included as a predictor in the primary demand model.

Any observed relationship between Unit Price and Quantity will be interpreted as an association rather than evidence of a causal price effect.

---

## 12. Total Value Validation

For Regular Orders, the expected transaction value was calculated using:

**Expected Total Value = Quantity × Unit Price**

### Regular Orders

- Regular Orders checked: **3,116**
- Total Value calculation mismatches: **0**

### Free Samples

- Free Samples checked: **2,365**
- Free Samples with non-zero Total Value: **0**

### Total Regular Order Revenue

**LKR 246,708,257.80**

### Analytical Implication

`Total Value (LKR)` will not be used as a predictor of `Quantity`.

This is because:

**Total Value = Quantity × Unit Price**

Using Total Value to predict Quantity would introduce data leakage and circular reasoning.

Total Value remains appropriate for descriptive revenue analysis.

---

## 13. Outlier Assessment

Outlier analysis was performed on the **3,116 Regular Orders** using Quantity.

Free Samples were excluded from the calculation because their Quantity values are structurally fixed at 1 and do not represent normal commercial demand.

### Regular Order Quantity Summary

| Statistic | Quantity |
|---|---:|
| Count | 3,116 |
| Mean | 226.51 |
| Median | 199.50 |
| Standard Deviation | 183.88 |
| Minimum | 23 |
| Q1 | 55 |
| Q3 | 354 |
| IQR | 299 |
| Maximum | 998 |

### IQR Outlier Rule

- Lower fence = **-393.5**
- Upper fence = **802.5**

Regular Order quantities greater than **802.5** were therefore classified as high IQR outliers.

### Results

- High-Quantity outliers: **41**
- Low-Quantity outliers: **0**
- Lowest flagged Quantity: **810**
- Highest flagged Quantity: **998**

All 41 flagged transactions were:

- Regular Orders
- Main Dealer transactions
- Japan market transactions
- associated with buyer **Prabath Liyanarachchi**

The observations occurred across all four products and passed both the pricing-rule and Total Value validation checks.

### Cleaning Decision

All 41 observations were retained.

The observations are statistically extreme but internally consistent and represent a coherent high-volume export-order pattern rather than random data errors.

A derived variable named `Outlier_IQR_Flag` was created to identify these observations without deleting them.

Values:

- `Yes`
- `No`

---

## 14. Business-Structure Change

A major channel-structure change was identified at **1 April 2025**.

### Before 1 April 2025

| Channel | Rows |
|---|---:|
| Shop | 751 |
| Small Dealer | 702 |
| Main Dealer | 0 |
| **Total** | **1,453** |

### From 1 April 2025

| Channel | Rows |
|---|---:|
| Shop | 0 |
| Small Dealer | 0 |
| Main Dealer | 4,028 |
| **Total** | **4,028** |

- Last pre-change date: **31 March 2025**
- First post-change date: **1 April 2025**

To represent this structural change, a derived variable named `Sales_Regime` was created.

Values:

- `Pre-April-2025`
- `Post-April-2025`

### Analytical Implication

Channel Type and Sales Regime are strongly associated because the channel structure changed completely around 1 April 2025.

Consequently, later statistical comparisons involving Channel Type must be interpreted together with the historical business regime.

Observed differences across channels cannot be treated as direct causal channel effects without accounting for the structural change.

---

## 15. Japan Market Structure and Confounding

The Japan market records have a highly specific structure.

- Total Japan rows: **370**
- Japan Regular Orders: **185**
- Japan Free Samples: **185**
- Channel: **Main Dealer only**
- Buyer: **Prabath Liyanarachchi only**
- Date range: **1 April 2025 to 30 June 2026**

### Analytical Limitation

A Local-versus-Japan comparison does not represent an isolated market effect.

Japan records are simultaneously associated with:

- one specific buyer
- Main Dealer channel
- the post-April-2025 business regime
- export-order structure

Therefore, later statistical comparisons between Local and Japan will be interpreted as observed associations rather than causal market effects.

---

## 16. String and Category Consistency

The following text variables were checked for leading/trailing spaces and simple case inconsistencies:

- Order ID
- Product Name
- Product Code
- Channel Type
- Buyer
- Line Type
- Export Market

### Results

- Leading/trailing-space inconsistencies: **0**
- Simple case inconsistencies: **0**

No text-standardization corrections were required.

---

## 17. Final Cleaning Decisions

| Issue | Final Decision |
|---|---|
| Raw workbook | Preserved unchanged |
| Invalid dates | None identified |
| Missing values | No action required |
| Exact duplicate rows | None identified |
| Duplicate Order IDs | None identified |
| Duplicate-looking Regular Order pair | Retained |
| Zero Quantity | None identified |
| Negative Quantity | None identified |
| Negative Unit Price | None identified |
| Negative Total Value | None identified |
| Zero Total Value | Retained as valid Free Samples |
| Product/code inconsistencies | None identified |
| Text/category inconsistencies | None identified |
| Pricing-rule violations | None identified |
| Regular Order Total Value mismatches | None identified |
| Free Samples | Retained but separated from commercial demand |
| High IQR outliers | Retained and flagged |
| Date | Stored as datetime |
| April 2025 business change | Represented using `Sales_Regime` |
| Total Value | Retained for revenue analysis; excluded as a Quantity predictor |
| Unit Price | Retained; treated cautiously in modelling |
| Order ID | Retained for traceability only |
| Product Code | Retained for reference |
| Buyer | Retained; future modelling use will be evaluated cautiously |

No observations were removed solely because they were statistically unusual.

---

## 18. Derived Variables Created

The following variables were created from `Date`:

1. `Year`
2. `Month`
3. `Month_Number`
4. `Quarter`
5. `Day_of_Week`
6. `Year_Month`
7. `Time_Trend`

Additional derived variables:

### `Sales_Regime`

- `Pre-April-2025`
- `Post-April-2025`

### `Is_Regular_Order`

- `1` = Regular Order
- `0` = Free Sample

### `Market_Group`

- `Local`
- `Export`

### `Outlier_IQR_Flag`

- `Yes`
- `No`

No arbitrary seasonal classification was created during the cleaning stage. Seasonal or special-period classifications will only be introduced later if they are supported by the observed demand patterns and business context.

---

## 19. Final Cleaned Data Structure

### Cleaned Master Dataset

The final cleaned master dataset contains:

- **5,481 rows**
- **22 columns**

The dataset retains the complete historical record together with the derived analytical variables.

Purpose:

- preserve the complete cleaned transaction history
- maintain traceability
- support Free Sample analysis
- support revenue analysis
- enable reproducible filtering and later analyses

### Dataset A – Regular Orders

Filter:

`Line Type = Regular Order`

Final rows:

**3,116**

Main response variable:

**Quantity**

This dataset will be used for:

- descriptive demand analysis
- statistical inference
- later predictive statistical modelling

Potential explanatory variables to be evaluated in later stages include:

- Product Name
- Month
- Quarter
- Year
- Time Trend
- Channel Type
- Export Market
- Sales Regime

Buyer identity may be explored separately, but its use in a generalizable predictive model will be evaluated carefully because it may allow the model to learn buyer-specific behaviour rather than broader demand patterns.

### Dataset B – Free Samples

Filter:

`Line Type = Free Sample`

Final rows:

**2,365**

Purpose:

- separate descriptive/business analysis
- preservation of valid non-commercial transaction activity
- exclusion from the main commercial-demand model

---

## 20. Python Cleaning Implementation and Export Verification

The complete data-audit and cleaning workflow was implemented in Python using pandas and NumPy.

### Main Implementation File

`Task3_Data_Cleaning_Final.ipynb`

The notebook contains the reproducible workflow used to:

- load the original Excel workbook
- inspect workbook structure
- inspect variable names and data types
- validate transaction dates
- analyse missing values
- identify duplicate records and Order IDs
- inspect zero and negative values
- separate Regular Orders and Free Samples
- validate product and category consistency
- reproduce and validate business pricing rules
- validate Total Value calculations
- detect Quantity outliers using the IQR method
- create derived analytical variables
- create the cleaned master dataset
- separate Regular Orders and Free Samples
- export the cleaned datasets
- re-import the saved Regular Orders dataset for final verification

### Exported Cleaned Files

The following files were created in `04_Cleaned_Data`:

1. `Fragceylon_Cleaned_Master.xlsx`
2. `Fragceylon_Regular_Orders.xlsx`
3. `Fragceylon_Free_Samples.xlsx`

### Final Verification

| Dataset | Rows | Purpose |
|---|---:|---|
| Cleaned Master | 5,481 | Complete cleaned historical dataset |
| Regular Orders | 3,116 | Main commercial demand dataset |
| Free Samples | 2,365 | Separate descriptive/business dataset |

Verification confirmed:

- `3,116 + 2,365 = 5,481`
- Cleaned Master missing values = **0**
- Regular Orders missing values = **0**
- Free Samples missing values = **0**
- Regular Order outliers retained = **41**
- Saved Regular Order total demand = **705,810 units**

The saved Regular Orders Excel file was successfully re-imported into Python and verified after export.

No observations were deleted during the cleaning process.

---

## 21. Analytical Restrictions for Later Modelling

Several variables require special treatment during later predictive modelling.

### Variables Not Suitable as Primary Predictors

#### Order ID

`Order ID` is an identifier and does not represent a meaningful explanatory factor for demand.

#### Total Value (LKR)

`Total Value` directly includes Quantity in its calculation:

`Total Value = Quantity × Unit Price`

Using it to predict Quantity would introduce data leakage.

#### Product Code and Product Name Together

Both variables represent the same product identity. Including both in the same model would duplicate the same information.

#### Line Type

After restricting the main modelling dataset to Regular Orders, `Line Type` becomes constant and therefore contains no predictive variation.

### Variables Requiring Careful Treatment

#### Unit Price (LKR)

Unit Price is partly determined by Quantity, Channel Type, and pricing rules. Its inclusion in the primary Quantity model may therefore introduce target-related information.

#### Buyer

Buyer identity may result in a model learning specific dealer/customer behaviour rather than general demand relationships.

#### Channel Type and Sales Regime

These variables are strongly associated because the sales-channel structure changed completely around April 2025. Their joint inclusion will require careful assessment of redundancy and confounding.

---

## 22. Task 3A Summary

The Fragceylon dataset contains **5,481 complete transaction records and 11 original variables** covering the period from **18 October 2024 to 30 June 2026**.

The data-quality audit identified strong internal consistency:

- no missing values
- no invalid dates
- no exact duplicate rows
- no duplicate Order IDs
- no invalid product-code mappings
- no zero or negative Quantity values
- no negative prices or transaction values
- no pricing-rule mismatches
- no Regular Order Total Value calculation mismatches

The main preprocessing decisions were:

1. Separate **2,365 Free Samples** from **3,116 Regular Orders**.
2. Use Regular Orders as the main commercial-demand dataset.
3. Preserve all genuine transaction records rather than performing unnecessary row deletion.
4. Retain and flag **41 high-Quantity Japan export orders** rather than treating them as data errors.
5. Create a `Sales_Regime` variable to represent the business-structure change from **1 April 2025**.
6. Exclude Total Value from future Quantity prediction because of data leakage.
7. Treat Unit Price cautiously because its value is partly determined by Quantity and Channel.
8. Account for the structural confounding affecting Local/Japan and channel comparisons.
9. Create and verify the required derived date and business variables.
10. Export and verify separate cleaned datasets for the complete data, Regular Orders, and Free Samples.

### Final Main Demand Dataset

- Dataset: `Fragceylon_Regular_Orders.xlsx`
- Records: **3,116**
- Response variable: **Quantity**
- Total commercial demand: **705,810 units**
- Missing values: **0**
- High-Quantity IQR outliers retained: **41**

The Task 3A dataset audit and cleaning process is complete.

The next stage is **Task 3 descriptive analysis**, beginning with descriptive statistics for the Regular Order Quantity variable and comparisons across product, channel, market, time period, and sales regime.