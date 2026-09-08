library(shinytest2)

# The five polar_table_* outputs are DT tables under the Race Analytics tab's
# "Data" sub-tab (the "Boat Performance" tab holds polar *plots*, not these DT
# outputs). They live in a nested tabsetPanel that has no id, so each sub-tab is
# activated by clicking its tab link.
#
# These tables use DT server-side processing, so the widget value captured by
# expect_values() contains only the column headers/options, NOT the row data.
# To snapshot the ACTUAL polar numbers (a data/text comparison, not an image),
# we read each table's rendered <tbody> text via expect_text().
#
# We pin a data-rich race: the default (most-recent) race has no observed
# boat-speed data, so its STW/SOG tables show the "No ... data available"
# placeholder. "Regatta del Sol" (2026-05-01) is the richest race in the pinned
# track_data.rds (~244k qualifying track points). Within the 2025-2026 season
# (series left at its "All" default) it is the 2nd race by date, after the
# 2026-05-31 Sunday Race -- so ra_race_select = "2".
test_that("polar tables render real values for a data-rich race", {
  skip_on_cran()
  skip_if_not(
    isTRUE(nzchar(tryCatch(chromote::find_chrome(), error = function(e) "")[1])),
    "Chrome not found"
  )

  app <- AppDriver$new(
    app_dir      = test_path("..", ".."),
    name         = "app-polar",
    load_timeout = 60000,
    timeout      = 30000,
    seed         = 4127,
    expect_values_screenshot_args = FALSE   # data comparison only, no image diffs
  )
  on.exit(app$stop(), add = TRUE)

  # Changing the season triggers async updates to the race dropdown that can
  # clobber ra_race_select, so re-set the index until the rendered race header
  # confirms the pin. The expect_match() below then fails loudly if index drift
  # (e.g. a refreshed track_data.rds) ever selects a different race.
  app$set_inputs(main_tabs = "Race Analytics", wait_ = FALSE)
  app$set_inputs(ra_season_select = "2025-2026", wait_ = FALSE); Sys.sleep(1)

  header   <- ""
  deadline <- Sys.time() + 30
  repeat {
    app$set_inputs(ra_race_select = "2", wait_ = FALSE); Sys.sleep(0.7)
    header <- tryCatch(app$get_text("#selected_race_header"), error = function(e) "")
    if (length(header) == 1 && grepl("Regatta del Sol", header, fixed = TRUE)) break
    if (Sys.time() > deadline) break
  }
  expect_match(header, "Regatta del Sol", fixed = TRUE)

  # Poll a table's <tbody> until DataTables has rendered numeric rows (the
  # server-side ajax fetch completes after the output value is already present).
  wait_for_numbers <- function(id, timeout = 30000) {
    sel <- paste0("#", id, " tbody")
    d <- Sys.time() + timeout / 1000
    repeat {
      t <- tryCatch(app$get_text(sel), error = function(e) character())
      if (length(t) == 1 && !is.na(t) && nchar(gsub("[^0-9.-]", "", t)) > 5) return(invisible())
      if (Sys.time() > d) stop("Timed out waiting for numeric rows in ", id)
      Sys.sleep(0.4)
    }
  }

  polars <- c(
    "Observed STW Polars"   = "polar_table_stw",
    "Observed SOG Polars"   = "polar_table_sog",
    "Reference Polars"      = "polar_table_ref",
    "STW Polar Performance" = "polar_table_perf_stw",
    "SOG Polar Performance" = "polar_table_perf_sog"
  )

  for (i in seq_along(polars)) {
    tab <- names(polars)[i]
    id  <- polars[[i]]
    app$run_js(sprintf("$('a[data-value=\"%s\"]').click();", tab))
    wait_for_numbers(id)
    app$expect_text(paste0("#", id, " tbody"))
  }
})
