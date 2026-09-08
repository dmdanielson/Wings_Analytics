library(shinytest2)

# season_summary_table, season_series_summary_table, and season_table are DT
# outputs on the default "Race Seasons" tab (season_select defaults to "All"),
# so they render on load without navigation.
#
# Like the polar tables these use DT server-side processing, so the widget value
# captured by expect_values() contains only the column headers/options, NOT the
# row data. To snapshot the ACTUAL numbers (a data/text comparison, not an
# image), we read each table's rendered <tbody> text via expect_text().
test_that("season summary tables render real numbers", {
  skip_on_cran()
  skip_if_not(
    isTRUE(nzchar(tryCatch(chromote::find_chrome(), error = function(e) "")[1])),
    "Chrome not found"
  )

  app <- AppDriver$new(
    app_dir      = test_path("..", ".."),
    name         = "app-season",
    load_timeout = 60000,
    timeout      = 30000,
    seed         = 4127
  )
  on.exit(app$stop(), add = TRUE)

  # Poll a table's <tbody> until DataTables has rendered numeric rows (the
  # server-side ajax fetch completes after the output value is already present).
  wait_for_numbers <- function(id, timeout = 30000) {
    sel <- paste0("#", id, " tbody")
    deadline <- Sys.time() + timeout / 1000
    repeat {
      t <- tryCatch(app$get_text(sel), error = function(e) character())
      if (length(t) == 1 && !is.na(t) && nchar(gsub("[^0-9.-]", "", t)) > 5) return(invisible())
      if (Sys.time() > deadline) stop("Timed out waiting for numeric rows in ", id)
      Sys.sleep(0.4)
    }
  }

  ids <- c("season_summary_table", "season_series_summary_table", "season_table")
  for (id in ids) {
    wait_for_numbers(id)
    app$expect_text(paste0("#", id, " tbody"))
  }
})
