# ---------- RDS CREATION (LOCAL ONLY) ----------
rebuild_rds_from_raw <- function() {
  raw_lines_init <- read_all_txt()
  rmc_init   <- parse_rmc_any(raw_lines_init)
  boat_init  <- parse_boat_speed(raw_lines_init)
  wind_init  <- parse_wind_mwv(raw_lines_init)
  mwd_init   <- parse_mwd(raw_lines_init)
  
  # ---------- READ + NORMALIZE RACE CALENDAR (EMBED INTO RDS) ----------
  # Each sheet = one season; season name comes from the sheet name.
  race_cal_for_rds <- tibble()
  
  if (file.exists(race_calendar_path)) {
    sheet_names <- readxl::excel_sheets(race_calendar_path)
    # Only read season tabs (pattern: "YYYY-YYYY"); skip reference tabs like Crew, Positions, Marks
    sheet_names <- sheet_names[grepl("^\\d{4}-\\d{4}$", sheet_names)]
    all_sheets  <- list()
    
    for (sht in sheet_names) {
      sht_df <- readxl::read_excel(race_calendar_path, sheet = sht)
      names(sht_df) <- normalize_excel_names(names(sht_df))
      # Coerce columns that may differ in type across sheets
      if ("place" %in% names(sht_df)) sht_df$place <- as.character(sht_df$place)
      if ("fleet" %in% names(sht_df)) sht_df$fleet <- as.numeric(sht_df$fleet)
      if ("length" %in% names(sht_df)) sht_df$length <- as.numeric(sht_df$length)
      sht_df$season <- sht
      all_sheets[[sht]] <- sht_df
    }
    
    race_cal_raw <- bind_rows(all_sheets)
    
    # Normalize headsail / mainsail column names
    if ("head_sai" %in% names(race_cal_raw) && !"headsail" %in% names(race_cal_raw))
      race_cal_raw <- race_cal_raw |> rename(headsail = head_sai)
    if ("head_sail" %in% names(race_cal_raw) && !"headsail" %in% names(race_cal_raw))
      race_cal_raw <- race_cal_raw |> rename(headsail = head_sail)
    if ("main_sail" %in% names(race_cal_raw) && !"mainsail" %in% names(race_cal_raw))
      race_cal_raw <- race_cal_raw |> rename(mainsail = main_sail)
    
    required_cols <- c("race", "start", "end")
    if (all(required_cols %in% names(race_cal_raw))) {
      race_cal_for_rds <- race_cal_raw |>
        transmute(
          race     = as.character(race),
          season   = as.character(season),
          series   = if ("series" %in% names(race_cal_raw)) as.character(series) else NA_character_,
          helm     = if ("helm" %in% names(race_cal_raw)) as.character(helm) else NA_character_,
          headsail = if ("headsail" %in% names(race_cal_raw)) as.character(headsail) else NA_character_,
          mainsail = if ("mainsail" %in% names(race_cal_raw)) as.character(mainsail) else NA_character_,
          place    = if ("place" %in% names(race_cal_raw)) as.character(place) else NA_character_,
          fleet    = if ("fleet" %in% names(race_cal_raw)) as.numeric(fleet) else NA_real_,
          length   = if ("length" %in% names(race_cal_raw)) as.numeric(length) else NA_real_,
          start    = excel_to_posix_local(start),
          end      = excel_to_posix_local(end),
          crew_1      = if ("crew_1" %in% names(race_cal_raw)) as.character(crew_1) else NA_character_,
          position_1  = if ("position_1" %in% names(race_cal_raw)) as.character(position_1) else NA_character_,
          crew_2      = if ("crew_2" %in% names(race_cal_raw)) as.character(crew_2) else NA_character_,
          position_2  = if ("position_2" %in% names(race_cal_raw)) as.character(position_2) else NA_character_,
          crew_3      = if ("crew_3" %in% names(race_cal_raw)) as.character(crew_3) else NA_character_,
          position_3  = if ("position_3" %in% names(race_cal_raw)) as.character(position_3) else NA_character_,
          crew_4      = if ("crew_4" %in% names(race_cal_raw)) as.character(crew_4) else NA_character_,
          position_4  = if ("position_4" %in% names(race_cal_raw)) as.character(position_4) else NA_character_,
          crew_5      = if ("crew_5" %in% names(race_cal_raw)) as.character(crew_5) else NA_character_,
          position_5  = if ("position_5" %in% names(race_cal_raw)) as.character(position_5) else NA_character_,
          crew_6      = if ("crew_6" %in% names(race_cal_raw)) as.character(crew_6) else NA_character_,
          position_6  = if ("position_6" %in% names(race_cal_raw)) as.character(position_6) else NA_character_,
          crew_7      = if ("crew_7" %in% names(race_cal_raw)) as.character(crew_7) else NA_character_,
          position_7  = if ("position_7" %in% names(race_cal_raw)) as.character(position_7) else NA_character_,
          crew_8      = if ("crew_8" %in% names(race_cal_raw)) as.character(crew_8) else NA_character_,
          position_8  = if ("position_8" %in% names(race_cal_raw)) as.character(position_8) else NA_character_,
          crew_9      = if ("crew_9" %in% names(race_cal_raw)) as.character(crew_9) else NA_character_,
          position_9  = if ("position_9" %in% names(race_cal_raw)) as.character(position_9) else NA_character_,
          crew_10     = if ("crew_10" %in% names(race_cal_raw)) as.character(crew_10) else NA_character_,
          position_10 = if ("position_10" %in% names(race_cal_raw)) as.character(position_10) else NA_character_,
        ) |>
        filter(!is.na(race), nzchar(race)) |>
        mutate(
          end = if_else(is.na(end), start, end),
          start2 = pmin(start, end, na.rm = TRUE),
          end2   = pmax(start, end, na.rm = TRUE),
          start  = start2,
          end    = end2
        ) |>
        select(-start2, -end2)

      # Dynamically attach any course mark columns (start_line, mark_N, finish_line)
      course_col_names <- grep("^(start_line|mark_\\d+|finish_line)$", names(race_cal_raw), value = TRUE)
      for (mc in course_col_names) {
        race_cal_for_rds[[mc]] <- as.character(race_cal_raw[[mc]])
      }

      # Drop races missing BOTH a finish place and a fleet size — these are
      # scheduled races that didn't or haven't happened and should never be
      # persisted to the RDS. Races with a place but a blank fleet (or vice
      # versa) are real races and are kept.
      race_cal_for_rds <- race_cal_for_rds |>
        filter((!is.na(place) & nzchar(trimws(place))) | !is.na(fleet)) |>
        distinct() |>
        arrange(start)
    }
  }
  
  # ---------- RMC CHECK ----------
  if (nrow(rmc_init) == 0) {
    stop("Cannot rebuild track_data.rds: no $..RMC sentences found in *.txt files in data_dir.", call. = FALSE)
  }
  
  # Exclusion coordinate: 27°54.5454' N, 082°27.0245' W
  excl_lat <- 27 + 54.5454 / 60
  excl_lon <- -(82 + 27.0245 / 60)
  
  track_all_init <- rmc_init |>
    filter(!is.na(latitude), !is.na(longitude), !is.na(datetime_utc)) |>
    filter(is.na(sog_knots) | sog_knots <= 15) |>
    mutate(
      datetime_local = with_tz(datetime_utc, tzone = LOCAL_TZ),
      day_local      = as.Date(datetime_local)
    ) |>
    rowwise() |>
    mutate(
      dist_ft = geosphere::distHaversine(
        c(longitude, latitude),
        c(excl_lon,  excl_lat)
      ) * 3.28084
    ) |>
    ungroup() |>
    filter(dist_ft > 1000) |>
    select(
      line_index, datetime_utc, datetime_local, day_local,
      sentence, latitude, longitude, sog_knots, cog_deg
    )
  
  boat_sync  <- nearest_sync_by_line_index(boat_init,  track_all_init)
  wind_sync  <- nearest_sync_by_line_index(wind_init,  track_all_init)
  mwd_sync   <- nearest_sync_by_line_index(mwd_init,   track_all_init)
  
  stw_agg <- boat_sync |>
    group_by(datetime_utc) |>
    summarise(
      stw_knots = mean(boat_speed_knots, na.rm = TRUE),
      .groups   = "drop"
    )
  
  # Prefer True-reference wind where available. Within each fix, keep only the
  # best available reference class (True > Apparent > Unknown) before averaging;
  # averaging true and apparent angles together would be meaningless.
  wind_agg <- wind_sync |>
    mutate(wt_rank = case_when(
      wind_type == "True"     ~ 1L,
      wind_type == "Apparent" ~ 2L,
      TRUE                    ~ 3L
    )) |>
    group_by(datetime_utc) |>
    filter(wt_rank == min(wt_rank, na.rm = TRUE)) |>
    summarise(
      twa_deg   = mean(twa_deg,   na.rm = TRUE),
      tws_knots = mean(tws_knots, na.rm = TRUE),
      wind_type = first(wind_type),
      .groups   = "drop"
    )
  
  # H5000-calculated true wind direction (MWD), circular-averaged per fix.
  twd_agg <- mwd_sync |>
    group_by(datetime_utc) |>
    summarise(
      twd_deg = (atan2(mean(sin(twd_deg * pi / 180), na.rm = TRUE),
                       mean(cos(twd_deg * pi / 180), na.rm = TRUE)) * 180 / pi) %% 360,
      .groups = "drop"
    )

  track_all_init <- track_all_init |>
    left_join(stw_agg,   by = "datetime_utc") |>
    left_join(wind_agg,  by = "datetime_utc") |>
    left_join(twd_agg,   by = "datetime_utc")
  
  # ---------- POLARS ----------
  polar_ref_init <- if (file.exists(polar_path)) readxl::read_excel(polar_path) else tibble()
  
  polar_ref_long <- tibble()
  if (nrow(polar_ref_init) > 0) {
    if ("TWS" %in% names(polar_ref_init)) {
      polar_ref_long <- polar_ref_init |>
        tidyr::pivot_longer(
          cols      = -TWS,
          names_to  = "twa_label",
          values_to = "bsp_ref"
        ) |>
        mutate(
          tws = as.numeric(TWS),
          twa = readr::parse_number(twa_label)
        ) |>
        filter(!is.na(tws), !is.na(twa), !is.na(bsp_ref)) |>
        select(tws, twa, bsp_ref)
    } else if (all(c("tws", "twa", "bsp_ref") %in% names(polar_ref_init))) {
      polar_ref_long <- polar_ref_init |>
        transmute(
          tws     = as.numeric(tws),
          twa     = as.numeric(twa),
          bsp_ref = as.numeric(bsp_ref)
        ) |>
        filter(!is.na(tws), !is.na(twa), !is.na(bsp_ref))
    }
  }
  
  polar_by_time <- tibble(datetime_utc = as.POSIXct(character()), polar_bsp_knots = numeric())
  if (nrow(polar_ref_long) > 0 && nrow(track_all_init) > 0) {
    polar_tws_vals <- sort(unique(polar_ref_long$tws))
    polar_twa_vals <- sort(unique(polar_ref_long$twa))
    
    wind_for_polar <- track_all_init |>
      filter(!is.na(tws_knots), !is.na(twa_deg)) |>
      mutate(
        # Fold TWA to 0-180 (port/starboard symmetric)
        twa_folded = ifelse(twa_deg > 180, 360 - twa_deg, twa_deg),
        tws_idx   = snap_nearest_idx(tws_knots, polar_tws_vals),
        twa_idx   = snap_nearest_idx(twa_folded, polar_twa_vals),
        tws_match = polar_tws_vals[tws_idx],
        twa_match = polar_twa_vals[twa_idx]
      ) |>
      left_join(polar_ref_long,
                by = c("tws_match" = "tws", "twa_match" = "twa")) |>
      rename(polar_bsp_knots = bsp_ref)
    
    polar_by_time <- wind_for_polar |>
      group_by(datetime_utc) |>
      summarise(
        polar_bsp_knots = mean(polar_bsp_knots, na.rm = TRUE),
        .groups         = "drop"
      ) |>
      filter(!is.na(polar_bsp_knots))
  }
  
  track_all_init <- track_all_init |>
    left_join(polar_by_time, by = "datetime_utc") |>
    mutate(
      Polar_Perf_STW = if_else(
        !is.na(stw_knots) & !is.na(polar_bsp_knots),
        stw_knots - polar_bsp_knots,
        NA_real_
      ),
      Polar_Perf_SOG = if_else(
        !is.na(sog_knots) & !is.na(polar_bsp_knots),
        sog_knots - polar_bsp_knots,
        NA_real_
      )
    )
  
  # ---------- APPLY RACE CALENDAR ----------
  track_all_init <- track_all_init %>%
    mutate(race = "", helm = "", headsail = "")
  
  if (file.exists(race_calendar_path)) {
    sheet_names_2 <- readxl::excel_sheets(race_calendar_path)
    sheet_names_2 <- sheet_names_2[grepl("^\\d{4}-\\d{4}$", sheet_names_2)]
    all_sheets_2  <- list()
    for (sht in sheet_names_2) {
      sht_df <- readxl::read_excel(race_calendar_path, sheet = sht)
      names(sht_df) <- normalize_excel_names(names(sht_df))
      if ("place" %in% names(sht_df)) sht_df$place <- as.character(sht_df$place)
      if ("fleet" %in% names(sht_df)) sht_df$fleet <- as.numeric(sht_df$fleet)
      if ("length" %in% names(sht_df)) sht_df$length <- as.numeric(sht_df$length)
      all_sheets_2[[sht]] <- sht_df
    }
    race_cal_raw2 <- bind_rows(all_sheets_2)
    
    if ("head_sai" %in% names(race_cal_raw2) && !"headsail" %in% names(race_cal_raw2))
      race_cal_raw2 <- race_cal_raw2 |> rename(headsail = head_sai)
    if ("head_sail" %in% names(race_cal_raw2) && !"headsail" %in% names(race_cal_raw2))
      race_cal_raw2 <- race_cal_raw2 |> rename(headsail = head_sail)
    
    required_cols <- c("race", "start", "end")
    if (all(required_cols %in% names(race_cal_raw2))) {
      race_cal <- race_cal_raw2 |>
        transmute(
          race     = as.character(race),
          helm     = if ("helm" %in% names(race_cal_raw2)) as.character(helm) else NA_character_,
          headsail = if ("headsail" %in% names(race_cal_raw2)) as.character(headsail) else NA_character_,
          start    = excel_to_posix_local(start),
          end      = excel_to_posix_local(end)
        ) |>
        filter(!is.na(start)) |>
        mutate(end = if_else(is.na(end), start, end)) |>
        mutate(
          start2 = pmin(start, end),
          end2   = pmax(start, end),
          start  = start2,
          end    = end2
        ) |>
        select(-start2, -end2) |>
        arrange(start)
      
      if (nrow(race_cal) > 0 && nrow(track_all_init) > 0) {
        for (i in seq_len(nrow(race_cal))) {
          seg_start <- race_cal$start[i]
          seg_end   <- race_cal$end[i]
          
          idx <- which(
            !is.na(track_all_init$datetime_local) &
              track_all_init$datetime_local >= seg_start &
              track_all_init$datetime_local <= seg_end
          )
          
          if (length(idx) > 0) {
            track_all_init$race[idx]     <- ifelse(is.na(race_cal$race[i]),     "", race_cal$race[i])
            track_all_init$helm[idx]     <- ifelse(is.na(race_cal$helm[i]),     "", race_cal$helm[i])
            track_all_init$headsail[idx] <- ifelse(is.na(race_cal$headsail[i]), "", race_cal$headsail[i])
          }
        }
      }
    }
  }
  
  # ---------- PRE-COMPUTE RACE-LEVEL STATISTICS FROM TRACK DATA ----------
  # Compute aggregate performance statistics (average speed, max wind, polar
  # performance, NMEA point counts) for each race directly from the track data.
  # These stats were previously computed at runtime inside the season_races()
  # reactive, which required an expensive many-to-many join between the full
  # track dataset (millions of GPS points) and the race calendar on every
  # season/series filter change. By pre-computing here during the rebuild step,
  # the runtime reactive becomes a simple O(n_calendar) filter — effectively
  # instant regardless of track data size.
  #
  # The join matches track points to calendar races by race name, then filters
  # to the race's start-end time window. STW values below 2 knots are excluded
  # from speed averages to filter out mark-rounding and pre-start noise.
  # Duration and days-on-water are derived from the calendar's start/end times.
  if (nrow(race_cal_for_rds) > 0 && nrow(track_all_init) > 0) {
    # Calendar-derived timing columns
    race_cal_for_rds <- race_cal_for_rds |>
      mutate(
        race_date     = as.Date(start),
        duration_hrs  = as.numeric(difftime(end, start, units = "hours")),
        duration_hrs  = ifelse(!is.na(duration_hrs) & duration_hrs == 0, NA_real_, duration_hrs),
        duration_hrs  = round(duration_hrs, 2),
        days_on_water = as.integer(as.Date(end) - as.Date(start)) + 1L
      )

    # Track-derived performance statistics per race
    track_with_labels <- track_all_init |>
      filter(!is.na(race), nzchar(race))

    race_stats <- track_with_labels |>
      inner_join(
        race_cal_for_rds |> select(race, race_date, start, end),
        by = "race",
        relationship = "many-to-many"
      ) |>
      filter(datetime_local >= start, datetime_local <= end) |>
      group_by(race, race_date) |>
      summarise(
        nmea_count     = n(),
        avg_stw        = {v <- stw_knots[!is.na(stw_knots) & stw_knots >= 2]; if (length(v) == 0) NA_real_ else mean(v)},
        max_stw        = {v <- stw_knots[!is.na(stw_knots) & stw_knots >= 2]; if (length(v) == 0) NA_real_ else max(v)},
        max_tws        = {v <- tws_knots[!is.na(tws_knots)]; if (length(v) == 0) NA_real_ else max(v)},
        polar_perf_stw = {v <- Polar_Perf_STW[!is.na(Polar_Perf_STW)]; if (length(v) == 0) NA_real_ else mean(v)},
        .groups = "drop"
      )

    # Merge pre-computed stats back into the race calendar
    race_cal_for_rds <- race_cal_for_rds |>
      left_join(race_stats, by = c("race", "race_date")) |>
      mutate(
        nmea_count     = replace_na(nmea_count, 0L),
        avg_stw        = ifelse(is.nan(avg_stw), NA_real_, avg_stw),
        max_stw        = ifelse(is.infinite(max_stw), NA_real_, max_stw),
        max_tws        = ifelse(is.infinite(max_tws), NA_real_, max_tws),
        polar_perf_stw = ifelse(is.nan(polar_perf_stw), NA_real_, polar_perf_stw)
      )
  }

  # Load marks reference from Marks sheet
  marks_ref_init <- tibble()
  if (file.exists(race_calendar_path)) {
    marks_sheets <- readxl::excel_sheets(race_calendar_path)
    if ("Marks" %in% marks_sheets) {
      marks_ref_init <- readxl::read_excel(race_calendar_path, sheet = "Marks", col_names = FALSE)
      names(marks_ref_init) <- c("mark", "lat", "lon")[seq_len(ncol(marks_ref_init))]
      marks_ref_init <- marks_ref_init |> mutate(across(everything(), as.character))
    }
  }

  saveRDS(
    list(
      track_all      = track_all_init,
      polar_ref      = polar_ref_init,
      polar_ref_long = polar_ref_long,
      race_calendar  = race_cal_for_rds,
      marks_ref      = marks_ref_init
    ),
    rds_path
  )
  }
