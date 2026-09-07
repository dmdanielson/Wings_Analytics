# Ensure snapshot expectations always run rather than skipping with
# "Reason: On CRAN". devtools sets this automatically; a bare test_dir() /
# R CMD check without it causes expect_snapshot*() to skip.
Sys.setenv(NOT_CRAN = "true")

# The narrative and parsing helpers live in R/ (sourced by app.R at runtime).
# This project is a Shiny app, not a package, so testthat won't auto-load them;
# source the files here so the suite is self-sufficient.
suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
})

# excel_to_posix_local() reads LOCAL_TZ from its enclosing (global) environment,
# matching how app.R defines it before sourcing the helpers.
assign("LOCAL_TZ", "America/New_York", envir = globalenv())

source(testthat::test_path("..", "..", "R", "narratives.R"))
source(testthat::test_path("..", "..", "R", "parsing.R"))
