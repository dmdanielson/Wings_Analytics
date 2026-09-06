testthat::local_edition(3)

data_rds <- readRDS(testthat::test_path("..", "..", "track_data.rds"))

test_that("race narratives are stable", {
  expect_snapshot_value(build_all_narratives(data_rds), style = "json2")
})

test_that("season narratives are stable", {
  seasons <- sort(unique(na.omit(data_rds$race_calendar$season)))
  out <- lapply(seasons, function(s) {
    cal <- dplyr::filter(data_rds$race_calendar, !is.na(season), season == s)
    generate_season_narrative(s, cal, data_rds$track_all)
  })
  names(out) <- seasons
  expect_snapshot_value(out, style = "json2")
})

test_that("performance narrative is stable", {
  expect_snapshot_value(
    generate_performance_narrative(data_rds$race_calendar, data_rds$track_all),
    style = "json2"
  )
})
