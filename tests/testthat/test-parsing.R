testthat::local_edition(3)

# The NMEA parsing helpers are defined in R/parsing.R, sourced by setup.R.

# ---------------------------------------------------------------------------
# Inline NMEA fixtures. One vector covering every sentence type the parsers
# handle, a malformed variant per parser, and noise lines that must be ignored.
# Positions in this vector become the parsers' line_index values, so keep the
# order stable when updating snapshots.
# ---------------------------------------------------------------------------
nmea_lines <- c(
  # --- RMC (GPS position / SOG / COG / datetime) ---
  "$GPRMC,123519,A,4807.038,N,01131.000,E,022.4,084.4,230394,003.1,W*6A",  # N/E fix
  "$GPRMC,201530,A,2712.345,S,08245.678,W,006.5,215.7,150624,,*3D",        # S/W fix
  "$GPRMC,,V,,,,,,,,,*53",                                                 # malformed: no fix

  # --- VHW (boat speed through water; knots in field 6) ---
  "$IIVHW,,T,,M,6.50,N,12.04,K*4A",  # valid
  "$IIVHW,,T,,M,,N,,K*5B",           # malformed: empty speed -> dropped

  # --- MWV (wind angle + speed with units R/T and N/M/K) ---
  "$IIMWV,045.0,T,12.5,N,A*1B",  # true wind, knots
  "$IIMWV,120.0,R,6.2,M,A*22",   # apparent wind, m/s -> knots
  "$IIMWV,200.0,T,20.0,K,A*33",  # true wind, km/h -> knots
  "$IIMWV,090.0,R,5.0,X,A*44",   # malformed: unknown units -> dropped
  "$IIMWV,090.0,R,,N,A*55",      # malformed: empty speed -> dropped

  # --- MWD (true wind direction + speed) ---
  "$WIMWD,235.0,T,240.0,M,10.0,N,5.1,M*66",  # true dir present
  "$WIMWD,,T,240.0,M,10.0,N,5.1,M*77",       # true blank -> falls back to magnetic
  "$WIMWD,370.0,T,,M,,N,,M*88",              # wraps via %% 360
  "$WIMWD,,T,,M,,N,,M*99",                   # malformed: no direction -> dropped

  # --- Noise: must be ignored by every parser ---
  "$GPGGA,123519,4807.038,N,01131.000,E,1,08,0.9,545.4,M,46.9,M,,*47",
  "not a valid nmea line",
  ""
)

test_that("convert_degmin converts ddmm.mmm and applies hemisphere sign", {
  vals <- c(4807.038, 12211.000, 2712.345, 0)
  dirs <- c("N", "W", "S", "E")
  expect_snapshot_value(mapply(convert_degmin, vals, dirs), style = "json2")
})

test_that("convert_degmin returns NA for NA input", {
  expect_snapshot_value(convert_degmin(NA_real_, "N"), style = "json2")
})

test_that("parse_rmc_any extracts position, speed, and datetime", {
  expect_snapshot_value(parse_rmc_any(nmea_lines), style = "json2")
})

test_that("parse_rmc_any returns an empty typed tibble when no RMC present", {
  expect_snapshot_value(parse_rmc_any(c("not nmea", "$IIVHW,,T,,M,6.5,N,,K")),
                        style = "json2")
})

test_that("parse_boat_speed extracts knots and drops empty speeds", {
  expect_snapshot_value(parse_boat_speed(nmea_lines), style = "json2")
})

test_that("parse_wind_mwv converts units and classifies wind type", {
  expect_snapshot_value(parse_wind_mwv(nmea_lines), style = "json2")
})

test_that("parse_mwd takes true direction, falls back to magnetic, wraps mod 360", {
  expect_snapshot_value(parse_mwd(nmea_lines), style = "json2")
})

test_that("snap_nearest_idx returns nearest grid index, clamped, NA-safe", {
  grid <- c(0, 5, 10, 15, 20)
  x <- c(-3, 2, 3, 7.4, 7.6, 12.5, 25, NA)
  expect_snapshot_value(snap_nearest_idx(x, grid), style = "json2")
})

test_that("excel_to_posix_local parses character dates (incl. malformed -> NA)", {
  x <- c("2024-06-15 14:30:00", "6/15/2024 09:05", "6/15/2024", "not a date")
  expect_snapshot_value(
    format(excel_to_posix_local(x), "%Y-%m-%d %H:%M:%S %Z", tz = "America/New_York"),
    style = "json2"
  )
})

test_that("excel_to_posix_local converts Excel serial numbers", {
  expect_snapshot_value(
    format(excel_to_posix_local(c(45000, 45658.5)), "%Y-%m-%d %H:%M:%S %Z",
           tz = "America/New_York"),
    style = "json2"
  )
})

test_that("excel_to_posix_local handles Date and POSIXct inputs", {
  d <- as.Date(c("2024-06-15", "2024-12-25"))
  p <- as.POSIXct("2024-06-15 14:30:00", tz = "UTC")
  expect_snapshot_value(
    list(
      date  = format(excel_to_posix_local(d), "%Y-%m-%d %H:%M:%S %Z", tz = "America/New_York"),
      posix = format(excel_to_posix_local(p), "%Y-%m-%d %H:%M:%S %Z", tz = "America/New_York")
    ),
    style = "json2"
  )
})

test_that("excel_to_posix_local returns NA for unsupported types", {
  expect_snapshot_value(excel_to_posix_local(TRUE), style = "json2")
})
