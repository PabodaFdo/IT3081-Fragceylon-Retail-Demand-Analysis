# ============================================================
# IT3081 - Fragceylon Dashboard Package Setup
# Run this ONCE if packages are missing.
# ============================================================

packages = c(
  "shiny",
  "shinydashboard",
  "readxl",
  "dplyr",
  "ggplot2",
  "DT",
  "scales"
)

missing =
  packages[
    !packages %in%
      rownames(
        installed.packages()
      )
  ]

if(length(missing) == 0) {
  cat("All dashboard packages are already installed.\n")
} else {
  install.packages(
    missing,
    dependencies = TRUE
  )

  cat(
    "Installed:",
    paste(
      missing,
      collapse = ", "
    ),
    "\n"
  )
}
