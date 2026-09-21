# ============================================================
# IT3081 - Statistical Modelling
# Retail Product Demand Analysis at Fragceylon
# TASK 6 - CRITICAL EVALUATION OF EXPERIMENTAL DESIGN
# ============================================================
#
# IMPORTANT:
# The assignment does NOT require Fragceylon to conduct an experiment.
# This script does NOT create fake sales outcomes and does NOT claim
# that a CRD or RCBD experiment was actually performed.
#
# The purpose of this script is to:
# 1. Use the historical dataset to describe the business context.
# 2. Create an illustrative future CRD randomization schedule.
# 3. Create an illustrative future RCBD randomization schedule.
# 4. Compare the practical structure of CRD and RCBD.
# 5. Save design-planning tables and charts for the report.
#
# If Fragceylon later conducts a real experiment, the actual observed
# response can be analyzed using the commented ANOVA templates near
# the end of this script.
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
# 4. LOAD HISTORICAL REGULAR-ORDER DATA
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
  "TASK 6 - HISTORICAL BUSINESS CONTEXT\n"
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
  "Historical date range:",
  format(min(df$Date)),
  "to",
  format(max(df$Date)),
  "\n"
)

cat(
  "Products:",
  length(unique(df$`Product Name`)),
  "\n"
)

print(
  table(df$`Product Name`)
)

cat(
  "\nHistorical channels:\n"
)

print(
  table(df$`Channel Type`)
)

cat(
  "\nHistorical export markets:\n"
)

print(
  table(df$`Export Market`)
)


# ============================================================
# 5. CURRENT-REGIME CONTEXT
# ============================================================
#
# The historical analysis found an important business-structure
# change on 1 April 2025.
#
# We inspect the current regime because any future experiment should
# be designed for the business structure that Fragceylon currently
# operates rather than randomly mixing incompatible historical regimes.
# ============================================================

current_df =
  df[
    df$Date >=
      as.Date("2025-04-01"),
  ]

cat(
  "\nCURRENT REGIME\n"
)

cat(
  "Rows:",
  nrow(current_df),
  "\n"
)

cat(
  "Date range:",
  format(min(current_df$Date)),
  "to",
  format(max(current_df$Date)),
  "\n"
)

cat(
  "\nCurrent-regime channels:\n"
)

print(
  table(current_df$`Channel Type`)
)

cat(
  "\nCurrent-regime markets:\n"
)

print(
  table(current_df$`Export Market`)
)

cat(
  "\nCurrent-regime unique buyers:",
  length(unique(current_df$Buyer)),
  "\n"
)


# ============================================================
# 6. SAVE HISTORICAL CONTEXT TABLE
# ============================================================

historical_context = data.frame(
  Item = c(
    "Historical Regular Orders",
    "Historical Products",
    "Historical Buyers",
    "Current-Regime Regular Orders",
    "Current-Regime Channels",
    "Current-Regime Markets",
    "Current-Regime Buyers"
  ),
  Value = c(
    nrow(df),
    length(unique(df$`Product Name`)),
    length(unique(df$Buyer)),
    nrow(current_df),
    paste(
      unique(current_df$`Channel Type`),
      collapse = ", "
    ),
    paste(
      unique(current_df$`Export Market`),
      collapse = ", "
    ),
    length(unique(current_df$Buyer))
  ),
  stringsAsFactors = FALSE
)

print(
  historical_context
)

write.csv(
  historical_context,
  "08_Model_Results/R/Task6_Historical_Context.csv",
  row.names = FALSE
)


# ============================================================
# 7. DEFINE A POSSIBLE FUTURE PROMOTION EXPERIMENT
# ============================================================
#
# Business question:
# Would different promotion strategies change the quantity sold?
#
# Possible treatment factor:
# Promotion Strategy
#
# Treatment levels:
# 1. No Promotion / Control
# 2. Discount
# 3. Social Media Promotion
# 4. Bundle Offer
#
# Primary response:
# Quantity Sold during a fixed experimental sales window.
#
# IMPORTANT:
# These are proposed future treatment labels only.
# No real treatment was applied and no response was observed.
# ============================================================

treatments = c(
  "Control - No Promotion",
  "Discount",
  "Social Media Promotion",
  "Bundle Offer"
)

cat(
  "\n============================================================\n"
)

cat(
  "PROPOSED FUTURE TREATMENTS\n"
)

cat(
  "============================================================\n"
)

print(
  treatments
)


# ============================================================
# 8. PROPOSED COMPLETELY RANDOMIZED DESIGN - CRD
# ============================================================
#
# Illustration:
# 32 comparable future experimental units are available.
# Each unit could represent a comparable dealer-period or
# another clearly defined business unit.
#
# With four treatments, balanced allocation gives:
# 8 units per treatment.
#
# Randomization is performed before any future response is observed.
# ============================================================

set.seed(
  3081
)

n_crd_units = 32

crd_unit_id =
  paste0(
    "CRD_Unit_",
    sprintf(
      "%02d",
      1:n_crd_units
    )
  )

crd_treatment_pool =
  rep(
    treatments,
    each = n_crd_units /
      length(treatments)
  )

crd_assignment =
  sample(
    crd_treatment_pool,
    size = n_crd_units,
    replace = FALSE
  )

crd_design = data.frame(
  Experimental_Unit = crd_unit_id,
  Assigned_Treatment = crd_assignment,
  stringsAsFactors = FALSE
)

cat(
  "\n============================================================\n"
)

cat(
  "ILLUSTRATIVE CRD RANDOMIZATION SCHEDULE\n"
)

cat(
  "============================================================\n"
)

print(
  crd_design
)

cat(
  "\nCRD treatment allocation:\n"
)

print(
  table(
    crd_design$Assigned_Treatment
  )
)

write.csv(
  crd_design,
  "08_Model_Results/R/Task6_CRD_Proposed_Randomization.csv",
  row.names = FALSE
)


# ============================================================
# 9. VALIDATE CRD BALANCE
# ============================================================

crd_counts =
  table(
    crd_design$Assigned_Treatment
  )

stopifnot(
  length(crd_counts) ==
    length(treatments),
  all(crd_counts == 8)
)

print(
  "CRD randomization validation passed."
)


# ============================================================
# 10. PROPOSED RANDOMIZED COMPLETE BLOCK DESIGN - RCBD
# ============================================================
#
# RCBD can be considered when known nuisance variation should be
# controlled before treatment comparison.
#
# Example blocking concept for Fragceylon:
#
# Product can be used as an important blocking dimension.
# Two comparable block replicates are illustrated per product.
#
# This gives:
# 4 products x 2 block replicates = 8 blocks.
#
# Every block contains all four promotion treatments exactly once.
#
# Each row below represents a proposed future experimental unit.
#
# The exact real-life definition of the unit must be decided before
# implementation, for example:
# - comparable dealer-period units,
# - comparable store-period units,
# - or another operational unit where treatment interference can
#   reasonably be controlled.
# ============================================================

product_levels = c(
  "Dark Matrix",
  "Kennedy",
  "Mesmerose",
  "Secret Ambrosia"
)

block_table =
  expand.grid(
    Product_Block = product_levels,
    Replicate_Block = 1:2,
    stringsAsFactors = FALSE
  )

block_table$Block_ID =
  paste0(
    "Block_",
    sprintf(
      "%02d",
      1:nrow(block_table)
    )
  )

rcbd_design =
  data.frame()

set.seed(
  3081
)

for(i in 1:nrow(block_table)) {

  randomized_treatments =
    sample(
      treatments,
      size = length(treatments),
      replace = FALSE
    )

  block_rows =
    data.frame(
      Block_ID =
        rep(
          block_table$Block_ID[i],
          length(treatments)
        ),
      Product_Block =
        rep(
          block_table$Product_Block[i],
          length(treatments)
        ),
      Replicate_Block =
        rep(
          block_table$Replicate_Block[i],
          length(treatments)
        ),
      Unit_Within_Block =
        1:length(treatments),
      Assigned_Treatment =
        randomized_treatments,
      stringsAsFactors = FALSE
    )

  rcbd_design =
    rbind(
      rcbd_design,
      block_rows
    )
}

rownames(
  rcbd_design
) = NULL

cat(
  "\n============================================================\n"
)

cat(
  "ILLUSTRATIVE RCBD RANDOMIZATION SCHEDULE\n"
)

cat(
  "============================================================\n"
)

print(
  rcbd_design
)

cat(
  "\nRCBD treatment allocation:\n"
)

print(
  table(
    rcbd_design$Assigned_Treatment
  )
)

cat(
  "\nTreatments within each block:\n"
)

print(
  table(
    rcbd_design$Block_ID,
    rcbd_design$Assigned_Treatment
  )
)

write.csv(
  rcbd_design,
  "08_Model_Results/R/Task6_RCBD_Proposed_Randomization.csv",
  row.names = FALSE
)


# ============================================================
# 11. VALIDATE RCBD STRUCTURE
# ============================================================

rcbd_treatment_table =
  table(
    rcbd_design$Block_ID,
    rcbd_design$Assigned_Treatment
  )

stopifnot(
  nrow(rcbd_design) == 32,
  nrow(rcbd_treatment_table) == 8,
  ncol(rcbd_treatment_table) == 4,
  all(rcbd_treatment_table == 1),
  all(
    table(
      rcbd_design$Assigned_Treatment
    ) == 8
  )
)

print(
  "RCBD randomization validation passed."
)


# ============================================================
# 12. CRD VS RCBD CRITICAL COMPARISON
# ============================================================
#
# This table is qualitative.
# It is not an experimental result or numerical ranking.
# ============================================================

design_comparison = data.frame(
  Criterion = c(
    "Basic structure",
    "Randomization",
    "Control of known nuisance variation",
    "Potential use at Fragceylon",
    "Main advantage",
    "Main limitation",
    "Operational complexity",
    "Key assumption / requirement",
    "Risk of treatment spillover",
    "Practical feasibility"
  ),
  CRD = c(
    "All comparable experimental units enter one common randomization pool.",
    "Treatments are randomly assigned across all eligible units.",
    "No formal blocking; relies more heavily on unit comparability and randomization.",
    "Useful when dealer-period units are sufficiently homogeneous.",
    "Simple to design, randomize and explain.",
    "Product, dealer, market or timing differences may increase unexplained variation.",
    "Lower.",
    "Experimental units should be sufficiently comparable and treatment assignment must be genuinely random.",
    "Must be managed if customers, dealers or promotions affect nearby units.",
    "Potentially feasible for a small pilot with clearly separated comparable units."
  ),
  RCBD = c(
    "Comparable units are first grouped into blocks; treatments are randomized within each block.",
    "Each treatment appears within every complete block.",
    "Explicitly controls one important source of nuisance variation represented by the blocks.",
    "Useful when product, dealer, market or comparable-period differences are expected to affect demand.",
    "Can reduce unexplained variation and improve treatment comparison when blocks are meaningful.",
    "Requires well-defined blocks and can become difficult if units or treatments are unavailable within blocks.",
    "Higher.",
    "Blocks should be internally comparable and all treatments should be implementable within each complete block.",
    "Still possible; randomization and operational separation must prevent contamination between treatments.",
    "Potentially feasible, but requires more coordination, scheduling and record keeping than CRD."
  ),
  stringsAsFactors = FALSE
)

print(
  design_comparison
)

write.csv(
  design_comparison,
  "08_Model_Results/R/Task6_CRD_vs_RCBD_Comparison.csv",
  row.names = FALSE
)


# ============================================================
# 13. PROPOSED IMPLEMENTATION CHECKLIST
# ============================================================

implementation_checklist = data.frame(
  Stage = c(
    "Define experimental unit",
    "Define treatment",
    "Choose response window",
    "Choose primary response",
    "Check eligibility",
    "Choose CRD or RCBD",
    "Create randomization schedule",
    "Prevent treatment contamination",
    "Record implementation compliance",
    "Record contextual variables",
    "Analyze only after outcomes are collected",
    "Report practical and statistical effects"
  ),
  Fragceylon_Requirement = c(
    "Specify exactly what receives a treatment, e.g. dealer-period or store-period.",
    "Define control, discount, social-media and bundle conditions precisely.",
    "Use the same observation window for treatment comparisons.",
    "Primary outcome could be Quantity Sold; revenue and margin may be secondary outcomes.",
    "Exclude units that cannot reasonably receive all candidate treatments.",
    "Use CRD for sufficiently comparable units; consider RCBD when important block variation can be controlled.",
    "Randomize before the outcome period begins and preserve the allocation record.",
    "Avoid one treatment influencing another experimental unit through shared customers, dealers or media exposure.",
    "Document whether each assigned treatment was actually delivered as planned.",
    "Record product, dealer, market, time period, stock availability and other important context.",
    "Do not examine outcomes and then change the original treatment allocation.",
    "Report effect estimates, uncertainty, operational cost and business impact."
  ),
  stringsAsFactors = FALSE
)

print(
  implementation_checklist
)

write.csv(
  implementation_checklist,
  "08_Model_Results/R/Task6_Implementation_Checklist.csv",
  row.names = FALSE
)


# ============================================================
# 14. PROPOSED OUTCOME DATA TEMPLATE
# ============================================================
#
# No outcome is fabricated.
# Quantity_Sold is intentionally left as NA because the future
# experiment has not been conducted.
# ============================================================

crd_future_template =
  crd_design

crd_future_template$Quantity_Sold =
  NA_real_

crd_future_template$Revenue_LKR =
  NA_real_

crd_future_template$Treatment_Compliance =
  NA_character_

write.csv(
  crd_future_template,
  "08_Model_Results/R/Task6_CRD_Future_Data_Template.csv",
  row.names = FALSE
)

rcbd_future_template =
  rcbd_design

rcbd_future_template$Quantity_Sold =
  NA_real_

rcbd_future_template$Revenue_LKR =
  NA_real_

rcbd_future_template$Treatment_Compliance =
  NA_character_

write.csv(
  rcbd_future_template,
  "08_Model_Results/R/Task6_RCBD_Future_Data_Template.csv",
  row.names = FALSE
)


# ============================================================
# 15. CHART 1 - CRD TREATMENT ALLOCATION
# ============================================================

png(
  "06_Charts/R/Task6_01_CRD_Treatment_Allocation.png",
  width = 1000,
  height = 700
)

barplot(
  table(
    crd_design$Assigned_Treatment
  ),
  main = "Proposed CRD Treatment Allocation",
  xlab = "Promotion Treatment",
  ylab = "Number of Experimental Units",
  las = 2
)

dev.off()


# ============================================================
# 16. CHART 2 - RCBD TREATMENTS WITHIN BLOCKS
# ============================================================

png(
  "06_Charts/R/Task6_02_RCBD_Treatment_By_Block.png",
  width = 1200,
  height = 750
)

barplot(
  t(
    table(
      rcbd_design$Block_ID,
      rcbd_design$Assigned_Treatment
    )
  ),
  beside = FALSE,
  main = "Proposed RCBD: Complete Treatment Allocation Within Blocks",
  xlab = "Block",
  ylab = "Number of Treatment Assignments",
  legend.text = TRUE,
  args.legend = list(
    x = "topright",
    cex = 0.7
  ),
  las = 2
)

dev.off()


# ============================================================
# 17. FUTURE ANALYSIS TEMPLATE - DO NOT RUN YET
# ============================================================
#
# The following code is intentionally commented out.
#
# It should only be used AFTER Fragceylon actually conducts
# an experiment and replaces NA outcome values with real observations.
#
# ------------------------------------------------------------
# CRD analysis example
# ------------------------------------------------------------
#
# crd_results =
#   read.csv(
#     "Task6_CRD_REAL_EXPERIMENT_RESULTS.csv"
#   )
#
# crd_results$Assigned_Treatment =
#   factor(
#     crd_results$Assigned_Treatment
#   )
#
# crd_model =
#   aov(
#     Quantity_Sold ~ Assigned_Treatment,
#     data = crd_results
#   )
#
# summary(crd_model)
#
#
# ------------------------------------------------------------
# RCBD analysis example
# ------------------------------------------------------------
#
# rcbd_results =
#   read.csv(
#     "Task6_RCBD_REAL_EXPERIMENT_RESULTS.csv"
#   )
#
# rcbd_results$Assigned_Treatment =
#   factor(
#     rcbd_results$Assigned_Treatment
#   )
#
# rcbd_results$Block_ID =
#   factor(
#     rcbd_results$Block_ID
#   )
#
# rcbd_model =
#   aov(
#     Quantity_Sold ~
#       Block_ID +
#       Assigned_Treatment,
#     data = rcbd_results
#   )
#
# summary(rcbd_model)
#
#
# ------------------------------------------------------------
# After fitting a real experiment:
# ------------------------------------------------------------
#
# Check:
# - residual plots,
# - normality / unusual observations,
# - homogeneity of variance,
# - treatment implementation,
# - treatment effect estimates,
# - confidence intervals,
# - practical effect size,
# - business cost and benefit.
#
# Never report a causal effect from this planning script because
# no outcome experiment has been conducted.
# ============================================================


# ============================================================
# 18. REPORT-READY TASK 6 SUMMARY
# ============================================================

sink(
  "08_Model_Results/R/Task6_Report_Ready_Summary.txt"
)

cat(
  "TASK 6 - CRITICAL EVALUATION OF EXPERIMENTAL DESIGN\n\n"
)

cat(
  "STATUS\n"
)

cat(
  "No experiment was conducted. Task 6 is a future-design evaluation only.\n\n"
)

cat(
  "BUSINESS QUESTION\n"
)

cat(
  "A future experiment could investigate whether promotion strategies change product quantity sold.\n\n"
)

cat(
  "PROPOSED TREATMENTS\n"
)

for(t in treatments) {
  cat(
    "-",
    t,
    "\n"
  )
}

cat(
  "\nPRIMARY PROPOSED RESPONSE\n"
)

cat(
  "Quantity sold during a fixed and comparable experimental sales window.\n\n"
)

cat(
  "CRD\n"
)

cat(
  "A Completely Randomized Design could randomly assign comparable eligible business units to the four promotion strategies. It is simple and operationally easier, but product, dealer, market and time-related heterogeneity may increase unexplained variation if those factors are not balanced by randomization.\n\n"
)

cat(
  "RCBD\n"
)

cat(
  "A Randomized Complete Block Design could first form meaningful homogeneous blocks such as product-based or dealer/period-based groups and then randomize all promotion treatments within each block. If the block factor explains important background demand variation, RCBD may provide a more precise treatment comparison. However, it requires stronger operational coordination and complete treatment availability within each block.\n\n"
)

cat(
  "PRACTICAL FEASIBILITY\n"
)

cat(
  "A small controlled pilot may be feasible if Fragceylon can define independent experimental units, maintain stock availability, prevent treatment spillover, implement treatments consistently and record contextual variables. Commercial risks include revenue loss from discounts, customer confusion, contamination between promotion conditions and disruption to normal dealer relationships.\n\n"
)

cat(
  "IMPORTANT LIMITATION\n"
)

cat(
  "The randomization schedules generated by this script are illustrative planning tools. They are not evidence of treatment effectiveness because no experimental outcomes were collected.\n"
)

sink()


# ============================================================
# 19. FINAL VALIDATION
# ============================================================

stopifnot(
  nrow(df) == 3116,
  nrow(current_df) == 2014,
  length(unique(df$`Product Name`)) == 4,
  nrow(crd_design) == 32,
  nrow(rcbd_design) == 32,
  all(
    table(
      crd_design$Assigned_Treatment
    ) == 8
  ),
  all(
    table(
      rcbd_design$Assigned_Treatment
    ) == 8
  ),
  all(
    rcbd_treatment_table == 1
  )
)

print(
  "All Task 6 design-planning validation checks passed."
)


# ============================================================
# 20. LIST CREATED TASK 6 FILES
# ============================================================

cat(
  "\nTask 6 result files:\n"
)

print(
  list.files(
    "08_Model_Results/R",
    pattern = "^Task6",
    full.names = TRUE
  )
)

cat(
  "\nTask 6 chart files:\n"
)

print(
  list.files(
    "06_Charts/R",
    pattern = "^Task6",
    full.names = TRUE
  )
)


# ============================================================
# 21. COMPLETION MESSAGE
# ============================================================

cat(
  "\n============================================================\n"
)

cat(
  "TASK 6 R SCRIPT COMPLETE\n"
)

cat(
  "============================================================\n"
)

cat(
  "No real experiment was conducted.\n"
)

cat(
  "CRD and RCBD future-design structures were evaluated and randomized planning schedules were created.\n"
)


