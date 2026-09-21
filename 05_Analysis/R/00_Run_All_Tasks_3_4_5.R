# ============================================================================
# Fragceylon - Run Tasks 3, 4 and 5 in R
# ============================================================================
# Run this file only after placing the four task scripts in the same folder.
# It runs them in the correct order.
# ============================================================================

script_dir <- tryCatch({
  # Works in RStudio when this script is sourced/run.
  normalizePath(dirname(sys.frame(1)$ofile), winslash = "/", mustWork = TRUE)
}, error = function(e) {
  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
})

scripts <- c(
  "01_Task3A_Data_Cleaning.R",
  "02_Task3B_Descriptive_Analysis.R",
  "03_Task4_Statistical_Inference.R",
  "04_Task5_Predictive_Modelling.R"
)

for (script in scripts) {
  path <- file.path(script_dir, script)
  if (!file.exists(path)) stop("Missing script: ", path)
  cat("\n\n", paste(rep("=", 90), collapse = ""), "\n", sep = "")
  cat("RUNNING:", script, "\n")
  cat(paste(rep("=", 90), collapse = ""), "\n")
  source(path, echo = FALSE, chdir = FALSE)
}

cat("\nALL R TASKS 3, 4 AND 5 COMPLETED.\n")
