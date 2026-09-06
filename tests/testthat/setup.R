# Ensure snapshot expectations always run rather than skipping with
# "Reason: On CRAN". devtools sets this automatically; a bare test_dir() /
# R CMD check without it causes expect_snapshot*() to skip.
Sys.setenv(NOT_CRAN = "true")

# The narrative generators live in R/narratives.R (sourced by app.R at runtime).
# This project is a Shiny app, not a package, so testthat won't auto-load them;
# source the file here so the suite is self-sufficient.
suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
})
source(testthat::test_path("..", "..", "R", "narratives.R"))
