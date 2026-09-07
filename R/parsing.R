# ---------- HELPERS ----------
convert_degmin <- function(value, dir) {
  if (is.na(value)) return(NA_real_)
  deg <- floor(value / 100)
  min <- value - deg * 100
  dec <- deg + min / 60
  if (dir %in% c("S", "W")) dec <- -dec
  dec
}

# Return empty tibbles WITH expected columns so downstream code never errors on missing names.
empty_rmc_tbl <- function() {
  tibble(
    line_index  = integer(),
    datetime_utc = as.POSIXct(character(), tz = "UTC"),
    sentence    = character(),
    latitude    = numeric(),
    longitude   = numeric(),
    sog_knots   = numeric(),
    cog_deg     = numeric()
  )
}
empty_boat_tbl <- function() {
  tibble(
    line_index       = integer(),
    sentence         = character(),
    boat_speed_knots = numeric()
  )
}
empty_wind_tbl <- function() {
  tibble(
    line_index = integer(),
    sentence  = character(),
    twa_deg   = numeric(),
    tws_knots = numeric(),
    wind_type = factor(levels = c("True", "Apparent", "Unknown"))
  )
}
empty_mwd_tbl <- function() {
  tibble(
    line_index = integer(),
    sentence   = character(),
    twd_deg    = numeric()
  )
}

parse_rmc_any <- function(lines) {
  idxs <- which(grepl("^\\$..RMC", lines))
  if (!length(idxs)) return(empty_rmc_tbl())
  
  rmc_lines <- lines[idxs]
  tibble(raw = rmc_lines, line_index = idxs) |>
    separate(raw, into = paste0("field", 1:12), sep = ",", fill = "right") |>
    transmute(
      line_index  = line_index,
      sentence    = field1,
      time_utc    = field2,
      status      = field3,
      lat_raw     = field4,
      lat_dir     = field5,
      lon_raw     = field6,
      lon_dir     = field7,
      sog_knots   = suppressWarnings(as.numeric(field8)),
      cog_deg     = suppressWarnings(as.numeric(field9)),
      date_utc    = field10
    ) |>
    mutate(
      lat_raw_num   = suppressWarnings(as.numeric(lat_raw)),
      lon_raw_num   = suppressWarnings(as.numeric(lon_raw)),
      latitude      = mapply(convert_degmin, lat_raw_num, lat_dir),
      longitude     = mapply(convert_degmin, lon_raw_num, lon_dir),
      date_fmt      = ifelse(
        nchar(date_utc) >= 6,
        paste0("20", substr(date_utc, 5, 6), "-", substr(date_utc, 3, 4), "-", substr(date_utc, 1, 2)),
        NA_character_
      ),
      time_fmt      = ifelse(
        nchar(time_utc) >= 6,
        paste0(substr(time_utc, 1, 2), ":", substr(time_utc, 3, 4), ":", substr(time_utc, 5, 6)),
        NA_character_
      ),
      datetime_utc  = suppressWarnings(ymd_hms(paste(date_fmt, time_fmt), tz = "UTC"))
    ) |>
    select(line_index, datetime_utc, sentence, latitude, longitude, sog_knots, cog_deg)
}

parse_boat_speed <- function(lines) {
  idxs <- which(grepl("^\\$(..VHW)", lines))
  if (!length(idxs)) return(empty_boat_tbl())
  
  vhw_lines <- lines[idxs]
  tibble(raw = vhw_lines, line_index = idxs) |>
    separate(raw, into = paste0("field", 1:10), sep = ",", fill = "right") |>
    transmute(
      line_index       = line_index,
      sentence         = field1,
      boat_speed_knots = suppressWarnings(as.numeric(field6))
    ) |>
    filter(!is.na(boat_speed_knots))
}

parse_wind_mwv <- function(lines) {
  idxs <- which(grepl("^\\$(..MWV)", lines))
  if (!length(idxs)) return(empty_wind_tbl())
  
  mwv_lines <- lines[idxs]
  tibble(raw = mwv_lines, line_index = idxs) |>
    separate(raw, into = paste0("field", 1:10), sep = ",", fill = "right") |>
    mutate(
      sentence   = field1,
      twa_deg    = suppressWarnings(as.numeric(field2)),
      ref        = field3,
      spd_raw    = suppressWarnings(as.numeric(field4)),
      units      = field5
    ) |>
    filter(!is.na(twa_deg), !is.na(spd_raw)) |>
    mutate(
      tws_knots = case_when(
        units == "N" ~ spd_raw,
        units == "M" ~ spd_raw * 1.94384,
        units == "K" ~ spd_raw * 0.539957,
        TRUE         ~ NA_real_
      ),
      wind_type = case_when(
        ref == "T" ~ "True",
        ref == "R" ~ "Apparent",
        TRUE       ~ "Unknown"
      )
    ) |>
    filter(!is.na(tws_knots)) |>
    mutate(wind_type = factor(wind_type, levels = c("True", "Apparent", "Unknown"))) |>
    select(line_index, sentence, twa_deg, tws_knots, wind_type)
}

# MWD = true wind DIRECTION & speed, calculated and broadcast by the H5000 CPU
# (leeway/heel/boat-speed/MHU calibrated). This is the authoritative TWD, far
# better than deriving it from apparent wind + COG.
#   $--MWD,<dir_true>,T,<dir_mag>,M,<spd_kn>,N,<spd_ms>,M*hh
# We take the True-referenced direction (degrees true) so it aligns with the
# geographic bearings used elsewhere, falling back to magnetic if true is blank.
parse_mwd <- function(lines) {
  idxs <- which(grepl("^\\$(..MWD)", lines))
  if (!length(idxs)) return(empty_mwd_tbl())

  mwd_lines <- lines[idxs]
  tibble(raw = mwd_lines, line_index = idxs) |>
    separate(raw, into = paste0("field", 1:10), sep = ",", fill = "right") |>
    mutate(
      sentence = field1,
      dir_true = suppressWarnings(as.numeric(field2)),
      dir_mag  = suppressWarnings(as.numeric(field4))
    ) |>
    transmute(
      line_index = line_index,
      sentence   = sentence,
      twd_deg    = coalesce(dir_true, dir_mag) %% 360
    ) |>
    filter(!is.na(twd_deg))
}

read_all_txt <- function(dir = data_dir) {
  files <- list.files(dir, pattern = "\\.txt$", full.names = TRUE)
  if (!length(files)) return(character())
  unlist(lapply(files, readr::read_lines))
}

nearest_sync_by_line_index <- function(df, track_clean) {
  if (nrow(df) == 0 || nrow(track_clean) == 0) return(df)
  rmc_valid <- track_clean |>
    filter(!is.na(datetime_utc)) |>
    arrange(line_index)
  if (nrow(rmc_valid) == 0) return(df)
  
  df <- df %>% arrange(line_index)
  r_idx <- rmc_valid$line_index
  
  pos_left   <- findInterval(df$line_index, r_idx)
  cand_left  <- pmax(pos_left, 1)
  cand_right <- pmin(pos_left + 1, length(r_idx))
  
  dist_left  <- abs(df$line_index - r_idx[cand_left])
  dist_right <- abs(df$line_index - r_idx[cand_right])
  
  use_right   <- dist_right < dist_left
  nearest_pos <- ifelse(use_right, cand_right, cand_left)
  
  df$datetime_utc   <- rmc_valid$datetime_utc[nearest_pos]
  df$datetime_local <- rmc_valid$datetime_local[nearest_pos]
  df$day_local      <- rmc_valid$day_local[nearest_pos]
  df
}

# Snap each value in x to the nearest element in grid (vectorized).
# Returns integer indices into grid.
snap_nearest_idx <- function(x, grid) {
  idx_left  <- findInterval(x, grid, all.inside = TRUE)
  idx_right <- pmin(idx_left + 1L, length(grid))
  closer_right <- abs(x - grid[idx_right]) < abs(x - grid[idx_left])
  ifelse(closer_right, idx_right, idx_left)
}

excel_to_posix_local <- function(x) {
  if (inherits(x, "POSIXct") || inherits(x, "POSIXt")) {
    return(lubridate::force_tz(x, tzone = LOCAL_TZ))
  }
  if (inherits(x, "Date")) {
    return(as.POSIXct(x, tz = LOCAL_TZ))
  }
  if (is.numeric(x)) {
    return(as.POSIXct(x * 86400, origin = "1899-12-30", tz = LOCAL_TZ))
  }
  if (is.character(x)) {
    return(suppressWarnings(
      lubridate::parse_date_time(
        x,
        orders = c("mdy HMS", "mdy HM", "ymd HMS", "ymd HM", "mdy", "ymd"),
        tz     = LOCAL_TZ
      )
    ))
  }
  NA
}

normalize_excel_names <- function(nms) {
  nms <- tolower(gsub("\\s+", "_", nms))
  nms <- gsub("[^a-z0-9_]+", "", nms)
  nms
}
