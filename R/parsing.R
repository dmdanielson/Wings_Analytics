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
    cog_deg     = numeric(),
    mode_indicator = character()
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
    separate(raw, into = paste0("field", 1:14), sep = ",", fill = "right", extra = "merge") |>
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
      date_utc    = field10,
      mode_indicator = field13
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
    select(line_index, datetime_utc, sentence, latitude, longitude, sog_knots, cog_deg, mode_indicator)
}

# Drop $..RMC fixes whose embedded UTC time jumps backward in file order by
# more than a trivial amount. A single NMEA feed's timestamps should be
# non-decreasing as the log is read top to bottom; investigation of the
# 2026-09-06 session (which produced a "sawtooth" - a track that repeatedly
# reverses between two lines on the map) found the raw log periodically
# re-transmitted a ~236-second-old batch of already-logged fixes at a
# materially different position (21 such bursts in one session), then caught
# back up. Because the replayed fixes have their own distinct, non-duplicate
# timestamps, resolve_duplicate_rmc_fixes()'s same-second dedup can't see
# them, and since each replayed fix is only "fast" relative to one neighbor
# (like the sawtooth case filter_gps_glitches() handles), the chord-based
# excursion filter is blind to it too since the excursion spans minutes, not
# the maneuver-length spans that filter operates over. This is a receiver-time
# problem, not a geometry problem, so it is corrected here at the source:
# any fix whose time is more than tol_sec seconds behind the maximum time
# already seen is dropped as stale/replayed data. A scan of every raw session
# on file found genuine backward jitter is essentially zero (the smallest
# spurious jump found was ~9 minutes) while corrupt or replayed timestamps
# jump backward by minutes to years, so tol_sec has wide margin.
#
# A running maximum is vulnerable to a single corrupt sentence poisoning it:
# across the full multi-season dataset, one garbled RMC line parsed to a date
# in 2032, which then made every genuine fix for the rest of the file look
# "behind" and wiped out over a third of the track. Stage 1 below removes
# such isolated single-fix time corruption first - mirroring
# filter_gps_glitches()'s isolated-teleport check, but on the time field: a
# fix is dropped only when it differs wildly from BOTH neighbors while those
# neighbors are consistent with each other (i.e. removing it restores a
# smooth sequence), so a real gap between sessions (where neighbors are NOT
# close to each other) is left untouched. Stage 2 is the running-max burst
# filter described above, now safe to run on the cleaned sequence.
drop_stale_rmc <- function(df, tol_sec = 5, isolated_thr = 3600) {
  if (nrow(df) < 3 || !"datetime_utc" %in% names(df)) return(df)
  t <- as.numeric(df$datetime_utc)

  n <- length(t)
  prev_t <- c(NA_real_, t[-n])
  next_t <- c(t[-1], NA_real_)
  isolated <- !is.na(t) & !is.na(prev_t) & !is.na(next_t) &
    abs(t - prev_t) > isolated_thr &
    abs(next_t - t) > isolated_thr &
    abs(next_t - prev_t) <= isolated_thr
  df <- df[!isolated, , drop = FALSE]
  t <- t[!isolated]

  running_max <- -Inf
  keep <- rep(TRUE, length(t))
  for (i in seq_along(t)) {
    if (is.na(t[i])) next
    if (t[i] < running_max - tol_sec) {
      keep[i] <- FALSE
    } else {
      running_max <- max(running_max, t[i])
    }
  }
  df[keep, , drop = FALSE]
}

# Resolve $..RMC fixes that share the same UTC second. Some sessions carry two
# independent GPS-capable talkers on the NMEA bus at once (e.g. two instruments
# both emitting RMC), each producing a fix for the same second at a materially
# different position. Left alone, the duplicate-timestamp trajectory-consistency
# check in filter_gps_glitches() has to guess from geometry alone and can lock
# onto the wrong stream for extended stretches whenever the preferred stream has
# a brief dropout, producing a persistent sawtooth in the track. Ranking by the
# RMC mode indicator (field 13: fix quality) resolves same-second collisions
# deterministically before any geometry is considered. Rows with an unranked or
# missing mode indicator (older, shorter RMC sentences without this field) are
# treated as lowest priority, so genuine single-source data is untouched and any
# remaining ambiguity still falls through to filter_gps_glitches()'s
# geometry-based tie-break.
resolve_duplicate_rmc_fixes <- function(df) {
  if (!"mode_indicator" %in% names(df) || nrow(df) < 2) return(df)
  mode_rank <- c(R = 1L, F = 2L, D = 3L, A = 4L, E = 5L, M = 6L, S = 7L, N = 8L)
  df$.mode_rank <- ifelse(df$mode_indicator %in% names(mode_rank),
                           mode_rank[df$mode_indicator], 9L)
  df <- df[order(df$datetime_utc, df$.mode_rank, df$line_index), , drop = FALSE]
  df <- df[!duplicated(df$datetime_utc), , drop = FALSE]
  df <- df[order(df$line_index), , drop = FALSE]
  df$.mode_rank <- NULL
  df
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
  # Restrict to the VDR/OpenCPN raw-log naming convention (vdr_<timestamp>.txt)
  # rather than every *.txt file. The data directory can accumulate unrelated
  # notes/scratch .txt files over time (e.g. a stray "app.txt" was found
  # alongside the logs); matching those would feed non-NMEA text into the
  # parsers for no benefit and can trip file-access restrictions in sandboxed
  # environments for files outside the expected log set.
  files <- list.files(dir, pattern = "^vdr_.*\\.txt$", full.names = TRUE)
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

# Remove erroneous GPS fixes from a track data frame. Three error classes:
#   1. Duplicate timestamps - the same instant carrying two or more fixes at
#      very different positions (physically impossible, one is spurious). The
#      fix kept is the one minimizing total distance to the nearest distinct-
#      time neighbors, i.e. the position most consistent with the trajectory.
#   2. Isolated teleports - a single fix implying an impossible speed to BOTH
#      temporal neighbors. The threshold is SOG-aware, pmax(cap_abs, div * SOG),
#      so genuine fast sailing is preserved while position jumps are dropped.
#   3. Off-trajectory excursions - short RUNS of fixes that depart from and
#      return to the true track (a GPS "sawtooth": the reported position jumps
#      off-line, holds for several fixes, then snaps back). These evade class 2,
#      which only compares a fix to its two immediate neighbors, so a multi-fix
#      block never looks fast on both sides. Instead we measure each fix's
#      cross-track distance from the chord joining the fixes chord_k steps
#      before and after it; brief excursions (bounded by a chord_maxgap-second
#      span, so day/leg boundaries are never chorded) lying farther than
#      chord_thr metres off that chord are dropped, iterated over chord_passes
#      so wide teeth peel away and the chord re-anchors on clean fixes.
# Requires columns: datetime_local, latitude, longitude, sog_knots.
filter_gps_glitches <- function(df, cap_abs = 20, div = 4, max_passes = 3,
                                chord_k = 15, chord_thr = 60, chord_maxgap = 120,
                                chord_passes = 6) {
  if (nrow(df) < 3) return(df)
  df <- df[order(df$datetime_local), , drop = FALSE]
  df <- df[!duplicated(df[c("datetime_local", "latitude", "longitude")]), , drop = FALSE]

  # (1) resolve duplicate timestamps by trajectory consistency
  ts <- as.numeric(df$datetime_local)
  dup_vals <- unique(ts[duplicated(ts)])
  if (length(dup_vals)) {
    keep <- rep(TRUE, nrow(df))
    for (tv in dup_vals) {
      grp <- which(ts == tv & keep)
      if (length(grp) < 2) next
      b_i <- suppressWarnings(max(which(ts < tv & keep)))
      a_i <- suppressWarnings(min(which(ts > tv & keep)))
      refs <- rbind(
        if (is.finite(b_i)) c(df$longitude[b_i], df$latitude[b_i]) else NULL,
        if (is.finite(a_i)) c(df$longitude[a_i], df$latitude[a_i]) else NULL
      )
      if (is.null(refs)) { keep[grp[-1]] <- FALSE; next }
      score <- vapply(grp, function(i)
        sum(geosphere::distHaversine(cbind(df$longitude[i], df$latitude[i]), refs)),
        numeric(1))
      keep[setdiff(grp, grp[which.min(score)])] <- FALSE
    }
    df <- df[keep, , drop = FALSE]
  }

  # (2) iterative isolated-teleport removal (SOG-aware, both neighbors)
  for (pass in seq_len(max_passes)) {
    if (nrow(df) < 3) break
    lon <- df$longitude; lat <- df$latitude; tt <- as.numeric(df$datetime_local)
    ip  <- geosphere::distHaversine(cbind(dplyr::lag(lon), dplyr::lag(lat)),
                                    cbind(lon, lat)) / pmax(tt - dplyr::lag(tt), 1) * 1.94384
    inx <- geosphere::distHaversine(cbind(lon, lat),
                                    cbind(dplyr::lead(lon), dplyr::lead(lat))) / pmax(dplyr::lead(tt) - tt, 1) * 1.94384
    thr <- pmax(cap_abs, div * ifelse(is.na(df$sog_knots), 0, df$sog_knots))
    g <- !is.na(ip) & !is.na(inx) & ip > thr & inx > thr
    if (!any(g)) break
    df <- df[!g, , drop = FALSE]
  }

  # (3) iterative off-trajectory excursion removal (chord cross-track test).
  # For each fix, form the great-circle chord between the fixes chord_k steps
  # before and after it and measure the fix's perpendicular distance from that
  # chord. A brief excursion block sits far off the chord that straddles it,
  # while smooth sailing (and even tacks/roundings) hugs it. Only spans no
  # longer than chord_maxgap seconds are chorded, so gaps between days or legs
  # are never bridged. Removing the worst offenders each pass lets wide teeth
  # peel away as the chord re-anchors on clean fixes.
  for (pass in seq_len(chord_passes)) {
    n <- nrow(df)
    if (n < 2L * chord_k + 1L) break
    ai <- pmax(1L, seq_len(n) - chord_k)
    bi <- pmin(n,  seq_len(n) + chord_k)
    tt <- as.numeric(df$datetime_local)
    span <- tt[bi] - tt[ai]
    dev <- abs(geosphere::dist2gc(
      cbind(df$longitude[ai], df$latitude[ai]),
      cbind(df$longitude[bi], df$latitude[bi]),
      cbind(df$longitude,     df$latitude)))
    g <- !is.na(dev) & dev > chord_thr & !is.na(span) & span <= chord_maxgap
    if (!any(g)) break
    df <- df[!g, , drop = FALSE]
  }
  df
}
