# app.R
# Wings Analytics – consolidated time-series, fixed UTC→local handling
#
# Behavior:
# - LOCAL: if track_data.rds is missing, rebuild it from *.txt + Race Calendar.xlsx + Polars.xlsx
# - SHINYAPPS (deployed): NEVER rebuild; must have track_data.rds deployed alongside app.R
#
# Deployment requirement: app.R + track_data.rds only.

library(shiny)
library(tidyverse)
library(lubridate)
library(leaflet)
library(RColorBrewer)
library(DT)
library(readxl)
library(geosphere)
library(htmltools)

LOCAL_TZ <- "America/New_York"

# ---------- ENV DETECTION ----------
# Deployed environments typically set one or more of these.
IS_DEPLOYED <- {
  nzchar(Sys.getenv("SHINY_PORT")) ||
    nzchar(Sys.getenv("RSCONNECT_SERVER")) ||
    nzchar(Sys.getenv("RSCONNECT_URL")) ||
    nzchar(Sys.getenv("RSCONNECT_ACCOUNT")) ||
    nzchar(Sys.getenv("RS_CONNECT_SERVER")) ||
    nzchar(Sys.getenv("SHINY_SERVER_VERSION"))
}

ALLOW_REBUILD_RDS <- !IS_DEPLOYED

# ---------- PATHS ----------
app_dir_local  <- "C:/Users/mike/Projects/Wings_Analytics"
data_dir_local <- "G:/My Drive/Personal/Mike/Sailing/Data"

app_dir  <- if (dir.exists(app_dir_local)) app_dir_local else getwd()
data_dir <- if (dir.exists(data_dir_local)) data_dir_local else app_dir

# Only setwd when local path exists; never needed on shinyapps
if (dir.exists(app_dir_local)) setwd(app_dir_local)

rds_path           <- file.path(app_dir, "track_data.rds")
narratives_path    <- file.path(app_dir, "race_narratives.rds")
race_calendar_path <- file.path(data_dir, "Race Calendar.xlsx")
polar_path         <- file.path(data_dir, "Polars.xlsx")

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
empty_depth_tbl <- function() {
  tibble(
    line_index = integer(),
    sentence  = character(),
    depth_m   = numeric(),
    depth_ft  = numeric()
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

parse_depth <- function(lines) {
  idxs <- which(grepl("^\\$(..DBT|..DPT)", lines))
  if (!length(idxs)) return(empty_depth_tbl())
  
  depth_lines <- lines[idxs]
  tibble(raw = depth_lines, line_index = idxs) |>
    separate(raw, into = paste0("field", 1:10), sep = ",", fill = "right") |>
    mutate(
      sentence = field1,
      depth_m  = case_when(
        grepl("DBT", sentence) ~ suppressWarnings(as.numeric(field3)),
        grepl("DPT", sentence) ~ suppressWarnings(as.numeric(field2)),
        TRUE                   ~ NA_real_
      )
    ) |>
    filter(!is.na(depth_m)) |>
    mutate(depth_ft = depth_m * 3.28084) |>
    select(line_index, sentence, depth_m, depth_ft)
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

# ---------- RACE NARRATIVE GENERATOR ----------
generate_race_narrative <- function(race_row, completed_races) {
  race_name <- race_row$race
  race_date <- format(race_row$race_date, "%B %d, %Y")
  has_data  <- !is.na(race_row$avg_sog) && !is.nan(race_row$avg_sog)

  if (!has_data) {
    opts <- c(
      paste0("No track data is available for ", race_name, " (",
             race_date, "). The GPS apparently took the day off \u2014 even electronics need a mental health day now and then."),
      paste0("Track data for ", race_name, " (", race_date,
             ") is conspicuously absent. Whether the instruments were napping or the data was lost to the digital abyss, we may never know."),
      paste0("Alas, no track data exists for ", race_name, " (", race_date,
             "). The sailing happened, but the electrons that were supposed to record it clearly had other plans.")
    )
    idx <- (sum(utf8ToInt(paste0(race_name, race_date))) %% length(opts)) + 1
    return(opts[idx])
  }

  n_completed <- nrow(completed_races)
  paragraphs  <- character()

  # ---- Paragraph 1: Overview, distance, placement ----
  p1_parts <- character()

  # Distance
  if (!is.na(race_row$length)) {
    all_len <- completed_races$length[!is.na(completed_races$length)]
    if (length(all_len) > 2) {
      pct <- mean(race_row$length >= all_len)
      d <- if (pct < 0.25) "one of the shorter courses in the fleet\u2019s repertoire"
           else if (pct < 0.50) "a moderate-length affair"
           else if (pct < 0.75) "a respectably lengthy course"
           else "one of the longest courses Wings has tackled"
      p1_parts <- c(p1_parts, paste0("At ", race_row$length,
                                     " nautical miles, this was ", d, "."))
    } else {
      p1_parts <- c(p1_parts, paste0("The course covered ",
                                     race_row$length, " nautical miles."))
    }
  }

  # Duration
  if (!is.na(race_row$duration_hrs) && race_row$duration_hrs > 0) {
    hrs  <- floor(race_row$duration_hrs)
    mins <- round((race_row$duration_hrs - hrs) * 60)
    dur  <- if (hrs > 0) paste0(hrs, "h ", mins, "m") else paste0(mins, "m")
    p1_parts <- c(p1_parts, paste0("Wings was on the course for ", dur, "."))
  }

  # Placement
  if (!is.na(race_row$place) && !is.na(race_row$fleet)) {
    place_num <- suppressWarnings(readr::parse_number(as.character(race_row$place)))
    fleet_n   <- as.integer(race_row$fleet)
    if (!is.na(place_num) && !is.na(fleet_n) && fleet_n > 0) {
      pct_place <- place_num / fleet_n
      ptxt <- if (place_num == 1) {
        paste0("Wings claimed the top spot in a fleet of ", fleet_n,
               " \u2014 the stuff of legends (or at least a good bar story).")
      } else if (pct_place <= 0.33) {
        paste0("Finishing ", place_num, " out of ", fleet_n,
               " boats, Wings put in a strong showing near the front of the pack.")
      } else if (pct_place <= 0.50) {
        paste0("A ", place_num, " place finish in a fleet of ", fleet_n,
               " \u2014 solidly in the top half, which is where the good stories start.")
      } else if (pct_place <= 0.75) {
        paste0("Placing ", place_num, " of ", fleet_n,
               " boats \u2014 not the headline finish they were hoping for, but every race is a learning experience.")
      } else {
        paste0("At ", place_num, " of ", fleet_n,
               ", this was a character-building day. Even the best sailors have races they\u2019d rather not discuss at the yacht club.")
      }
      p1_parts <- c(p1_parts, ptxt)
    } else if (is.na(place_num)) {
      p1_parts <- c(p1_parts, paste0("Wings finished with a ",
                                     race_row$place, " in a fleet of ", fleet_n, "."))
    }
  }

  if (length(p1_parts) > 0)
    paragraphs <- c(paragraphs,
                    paste0(race_name, " on ", race_date, ". ",
                           paste(p1_parts, collapse = " ")))

  # ---- Paragraph 2: Speed & polar performance ----
  sp <- character()

  if (!is.na(race_row$avg_sog) && !is.nan(race_row$avg_sog)) {
    all_sog <- completed_races$avg_sog[!is.na(completed_races$avg_sog) &
                                        !is.nan(completed_races$avg_sog)]
    sog_pct <- if (length(all_sog) > 2) mean(race_row$avg_sog >= all_sog) else 0.5
    desc <- if (sog_pct < 0.20) "on the leisurely end of the spectrum"
            else if (sog_pct < 0.40) "below the fleet average"
            else if (sog_pct < 0.60) "right around the fleet average"
            else if (sog_pct < 0.80) "above average"
            else "among the fastest outings on record"
    peak <- if (!is.na(race_row$max_sog))
              paste0(" with a peak of ", round(race_row$max_sog, 1), " knots")
            else ""
    sp <- c(sp, paste0("Average speed over ground came in at ",
                       round(race_row$avg_sog, 1), " knots", peak,
                       " \u2014 ", desc, "."))
  }

  if (!is.na(race_row$avg_stw) && !is.nan(race_row$avg_stw) &&
      !is.na(race_row$avg_sog) && !is.nan(race_row$avg_sog)) {
    diff <- round(race_row$avg_sog - race_row$avg_stw, 2)
    if (abs(diff) > 0.15) {
      dir <- if (diff > 0) "a favorable current adding" else "current working against the boat to the tune of"
      sp <- c(sp, paste0("The gap between SOG and STW suggests ", dir, " roughly ",
                         abs(diff), " knots."))
    }
  }

  if (!is.na(race_row$polar_perf_sog) && !is.nan(race_row$polar_perf_sog)) {
    pp <- round(race_row$polar_perf_sog, 2)
    ptxt <- if (pp > 0.3) {
      paste0("Polar performance was +", pp,
             " knots above target \u2014 Wings was outpacing her own polars, which either means brilliant sailing or the polars need updating (we\u2019ll take the credit).")
    } else if (pp > 0) {
      paste0("Polar performance came in at +", pp,
             " knots, slightly above target. The crew was squeezing out a bit more than the boat\u2019s theoretical speed \u2014 well done.")
    } else if (pp > -0.3) {
      paste0("Polar performance was ", pp,
             " knots, just a whisker below target. Close enough to call it respectable.")
    } else if (pp > -0.7) {
      paste0("At ", pp,
             " knots below polar targets, there\u2019s room for improvement. The boat had more to give \u2014 or the conditions were making it difficult to extract.")
    } else {
      paste0("Polar performance of ", pp,
             " knots below target suggests the conditions (or the crew\u2019s coffee supply) weren\u2019t cooperating.")
    }
    sp <- c(sp, ptxt)
  }

  if (length(sp) > 0) paragraphs <- c(paragraphs, paste(sp, collapse = " "))

  # ---- Paragraph 3: Wind conditions ----
  wp <- character()

  if (!is.na(race_row$avg_tws) && !is.nan(race_row$avg_tws)) {
    wd <- if (race_row$avg_tws < 5)
            "a drifter \u2014 the kind of day where watching paint dry offers comparable excitement"
          else if (race_row$avg_tws < 8)
            "light air conditions that demanded patience and finesse"
          else if (race_row$avg_tws < 12)
            "moderate and manageable breeze \u2014 solid racing conditions"
          else if (race_row$avg_tws < 18)
            "a healthy breeze that kept the crew on their toes"
          else "heavy air that separated the bold from the cautious"

    gust <- if (!is.na(race_row$max_tws) && !is.nan(race_row$max_tws))
              paste0(" with gusts to ", round(race_row$max_tws, 1), " knots")
            else ""
    wp <- c(wp, paste0("Wind averaged ", round(race_row$avg_tws, 1),
                       " knots", gust, " \u2014 ", wd, "."))

    all_tws <- completed_races$avg_tws[!is.na(completed_races$avg_tws) &
                                        !is.nan(completed_races$avg_tws)]
    if (length(all_tws) > 2) {
      tws_pct <- mean(race_row$avg_tws >= all_tws)
      comp <- if (tws_pct < 0.25)
                "This was one of the lighter-air races in the dataset, making boat handling and sail trim all the more critical."
              else if (tws_pct > 0.75)
                "Relative to other races, this was a windy one \u2014 the kind of conditions where reef points earn their keep."
              else ""
      if (nzchar(comp)) wp <- c(wp, comp)
    }
  }

  if (!is.na(race_row$headsail) && nzchar(race_row$headsail)) {
    wp <- c(wp, paste0("The crew flew the ", race_row$headsail, " for this one."))
  }

  if (!is.na(race_row$helm) && nzchar(race_row$helm)) {
    wp <- c(wp, paste0(race_row$helm, " had the helm."))
  }

  if (length(wp) > 0) paragraphs <- c(paragraphs, paste(wp, collapse = " "))

  paste(paragraphs, collapse = "\n\n")
}

build_all_narratives <- function(data_rds) {
  track <- data_rds$track_all
  cal   <- data_rds$race_calendar

  # Per-race stats from track data
  race_stats <- track |>
    filter(!is.na(race), nzchar(race)) |>
    mutate(race_date = as.Date(datetime_local)) |>
    group_by(race, race_date) |>
    summarise(
      avg_sog        = mean(sog_knots, na.rm = TRUE),
      max_sog        = max(sog_knots,  na.rm = TRUE),
      avg_stw        = mean(stw_knots, na.rm = TRUE),
      max_stw        = max(stw_knots,  na.rm = TRUE),
      avg_tws        = mean(tws_knots, na.rm = TRUE),
      max_tws        = max(tws_knots,  na.rm = TRUE),
      polar_perf_stw = mean(Polar_Perf_STW, na.rm = TRUE),
      polar_perf_sog = mean(Polar_Perf_SOG, na.rm = TRUE),
      duration_hrs   = as.numeric(difftime(
        max(datetime_local), min(datetime_local), units = "hours")),
      .groups = "drop"
    ) |>
    mutate(across(where(is.numeric), ~ ifelse(is.infinite(.), NA_real_, .)))

  # Calendar info
  if (nrow(cal) > 0) {
    cal_info <- cal |>
      mutate(race_date = as.Date(start)) |>
      group_by(race, race_date) |>
      summarise(
        season   = first(season),
        series   = first(series),
        place    = first(place),
        fleet    = first(fleet),
        length   = first(length),
        helm     = first(helm),
        headsail = first(headsail),
        .groups  = "drop"
      )
    all_races <- cal_info |>
      full_join(race_stats, by = c("race", "race_date"))
  } else {
    all_races <- race_stats |>
      mutate(season = NA_character_, series = NA_character_,
             place = NA_character_, fleet = NA_real_,
             length = NA_real_, helm = NA_character_,
             headsail = NA_character_)
  }

  completed <- all_races |>
    filter(!is.na(avg_sog), !is.nan(avg_sog))

  narratives <- list()
  for (i in seq_len(nrow(all_races))) {
    row <- all_races[i, ]
    key <- paste(row$race, row$race_date, sep = "|")
    narratives[[key]] <- generate_race_narrative(row, completed)
  }
  narratives
}

# ---------- RDS CREATION (LOCAL ONLY) ----------
rebuild_rds_from_raw <- function() {
  raw_lines_init <- read_all_txt()
  rmc_init   <- parse_rmc_any(raw_lines_init)
  depth_init <- parse_depth(raw_lines_init)
  boat_init  <- parse_boat_speed(raw_lines_init)
  wind_init  <- parse_wind_mwv(raw_lines_init)
  
  # ---------- READ + NORMALIZE RACE CALENDAR (EMBED INTO RDS) ----------
  race_cal_for_rds <- tibble()
  
  if (file.exists(race_calendar_path)) {
    race_cal_raw <- readxl::read_excel(race_calendar_path)
    names(race_cal_raw) <- normalize_excel_names(names(race_cal_raw))
    
    if ("head_sai" %in% names(race_cal_raw) && !"headsail" %in% names(race_cal_raw)) {
      race_cal_raw <- race_cal_raw %>% rename(headsail = head_sai)
    }
    if ("head_sail" %in% names(race_cal_raw) && !"headsail" %in% names(race_cal_raw)) {
      race_cal_raw <- race_cal_raw %>% rename(headsail = head_sail)
    }
    
    required_cols <- c("race", "helm", "headsail", "start", "end")
    if (all(required_cols %in% names(race_cal_raw))) {
      race_cal_for_rds <- race_cal_raw %>%
        transmute(
          race     = as.character(race),
          season   = if ("season" %in% names(race_cal_raw)) as.character(season) else NA_character_,
          series   = if ("series" %in% names(race_cal_raw)) as.character(series) else NA_character_,
          helm     = as.character(helm),
          headsail = as.character(headsail),
          place    = if ("place" %in% names(race_cal_raw)) as.character(place) else NA_character_,
          fleet    = if ("fleet" %in% names(race_cal_raw)) as.numeric(fleet) else NA_real_,
          length   = if ("length" %in% names(race_cal_raw)) as.numeric(length) else NA_real_,
          start    = excel_to_posix_local(start),
          end      = excel_to_posix_local(end)
        ) %>%
        filter(!is.na(race), nzchar(race)) %>%
        mutate(
          end = if_else(is.na(end), start, end),
          start2 = pmin(start, end, na.rm = TRUE),
          end2   = pmax(start, end, na.rm = TRUE),
          start  = start2,
          end    = end2
        ) %>%
        select(race, season, series, helm, headsail, place, fleet, length, start, end) %>%
        distinct() %>%
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
  
  depth_sync <- nearest_sync_by_line_index(depth_init, track_all_init)
  boat_sync  <- nearest_sync_by_line_index(boat_init,  track_all_init)
  wind_sync  <- nearest_sync_by_line_index(wind_init,  track_all_init)
  
  depth_agg <- depth_sync |>
    group_by(datetime_utc) |>
    summarise(
      depth_m  = mean(depth_m,  na.rm = TRUE),
      depth_ft = mean(depth_ft, na.rm = TRUE),
      .groups  = "drop"
    )
  
  stw_agg <- boat_sync |>
    group_by(datetime_utc) |>
    summarise(
      stw_knots = mean(boat_speed_knots, na.rm = TRUE),
      .groups   = "drop"
    )
  
  wind_agg <- wind_sync |>
    group_by(datetime_utc) |>
    summarise(
      twa_deg   = mean(twa_deg,   na.rm = TRUE),
      tws_knots = mean(tws_knots, na.rm = TRUE),
      wind_type = first(na.omit(wind_type)),
      .groups   = "drop"
    )
  
  track_all_init <- track_all_init |>
    left_join(stw_agg,   by = "datetime_utc") |>
    left_join(wind_agg,  by = "datetime_utc") |>
    left_join(depth_agg, by = "datetime_utc")
  
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
        tws_idx   = findInterval(tws_knots, polar_tws_vals, all.inside = TRUE),
        twa_idx   = findInterval(twa_deg,  polar_twa_vals, all.inside = TRUE),
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
    race_cal_raw <- readxl::read_excel(race_calendar_path)
    names(race_cal_raw) <- normalize_excel_names(names(race_cal_raw))
    
    if ("head_sai" %in% names(race_cal_raw) && !"headsail" %in% names(race_cal_raw)) {
      race_cal_raw <- race_cal_raw %>% rename(headsail = head_sai)
    }
    if ("head_sail" %in% names(race_cal_raw) && !"headsail" %in% names(race_cal_raw)) {
      race_cal_raw <- race_cal_raw %>% rename(headsail = head_sail)
    }
    
    required_cols <- c("race", "helm", "headsail", "start", "end")
    if (all(required_cols %in% names(race_cal_raw))) {
      race_cal <- race_cal_raw %>%
        transmute(
          race     = as.character(race),
          helm     = as.character(helm),
          headsail = as.character(headsail),
          start    = excel_to_posix_local(start),
          end      = excel_to_posix_local(end)
        ) %>%
        filter(!is.na(start)) %>%
        mutate(end = if_else(is.na(end), start, end)) %>%
        mutate(
          start2 = pmin(start, end),
          end2   = pmax(start, end),
          start  = start2,
          end    = end2
        ) %>%
        select(-start2, -end2) %>%
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
  
  saveRDS(
    list(
      track_all      = track_all_init,
      polar_ref      = polar_ref_init,
      polar_ref_long = polar_ref_long,
      race_calendar  = race_cal_for_rds   # <-- ADD THIS
    ),
    rds_path
  )
  }

# ---------- LOCAL: build if missing | DEPLOYED: require RDS ----------
if (!file.exists(rds_path)) {
  if (ALLOW_REBUILD_RDS) {
    rebuild_rds_from_raw()
  } else {
    stop(
      paste0(
        "track_data.rds is missing in the deployed app directory.\n\n",
        "This deployment is RDS-only. Rebuilding from raw files is disabled on shinyapps.\n",
        "Fix: deploy track_data.rds alongside app.R."
      ),
      call. = FALSE
    )
  }
}

# ---------- LOAD RDS ----------
data_rds_raw <- readRDS(rds_path)
race_calendar_loaded <- if ("race_calendar" %in% names(data_rds_raw)) data_rds_raw$race_calendar else tibble()
track_all_loaded      <- data_rds_raw$track_all
polar_ref_loaded      <- if ("polar_ref"      %in% names(data_rds_raw)) data_rds_raw$polar_ref      else tibble()
polar_ref_long_loaded <- if ("polar_ref_long" %in% names(data_rds_raw)) data_rds_raw$polar_ref_long else tibble()

track_all_loaded <- track_all_loaded |>
  mutate(
    datetime_utc   = lubridate::force_tz(as.POSIXct(datetime_utc), "UTC"),
    datetime_local = with_tz(datetime_utc, LOCAL_TZ),
    day_local      = as.Date(datetime_local),
    race     = if ("race"     %in% names(track_all_loaded)) as.character(race) else "",
    helm     = if ("helm"     %in% names(track_all_loaded)) as.character(helm) else "",
    headsail = if ("headsail" %in% names(track_all_loaded)) as.character(headsail) else ""
  ) %>%
  mutate(
    race     = ifelse(is.na(race), "", race),
    helm     = ifelse(is.na(helm), "", helm),
    headsail = ifelse(is.na(headsail), "", headsail)
  ) %>%
  filter(is.na(sog_knots) | sog_knots <= 15)

# Drop teleport rows: a single point whose timestamp implies it jumped
# huge distances from BOTH temporal neighbors. Caused by date-rollover bugs
# in the source GPS sentences (a stray RMC tagged with the wrong UTC date),
# which would otherwise produce long zig-zag lines on the map.
track_all_loaded <- track_all_loaded |>
  arrange(datetime_local) |>
  mutate(
    tele_dist_prev_m = geosphere::distHaversine(
      cbind(dplyr::lag(longitude), dplyr::lag(latitude)),
      cbind(longitude, latitude)
    ),
    tele_dist_next_m = geosphere::distHaversine(
      cbind(longitude, latitude),
      cbind(dplyr::lead(longitude), dplyr::lead(latitude))
    ),
    tele_dt_prev_s = as.numeric(datetime_local - dplyr::lag(datetime_local), units = "secs"),
    tele_dt_next_s = as.numeric(dplyr::lead(datetime_local) - datetime_local, units = "secs"),
    tele_spd_prev_kn = (tele_dist_prev_m / pmax(tele_dt_prev_s, 1)) * 1.94384,
    tele_spd_next_kn = (tele_dist_next_m / pmax(tele_dt_next_s, 1)) * 1.94384
  ) |>
  filter(is.na(tele_spd_prev_kn) | is.na(tele_spd_next_kn) |
         tele_spd_prev_kn <= 30 | tele_spd_next_kn <= 30) |>
  select(-tele_dist_prev_m, -tele_dist_next_m, -tele_dt_prev_s, -tele_dt_next_s,
         -tele_spd_prev_kn, -tele_spd_next_kn)

data_rds <- list(
  track_all      = track_all_loaded,
  polar_ref      = polar_ref_loaded,
  polar_ref_long = polar_ref_long_loaded,
  race_calendar  = race_calendar_loaded
)

# ---------- BUILD / LOAD RACE NARRATIVES ----------
if (!file.exists(narratives_path)) {
  race_narratives_loaded <- build_all_narratives(data_rds)
  if (ALLOW_REBUILD_RDS) {
    saveRDS(race_narratives_loaded, narratives_path)
  }
} else {
  race_narratives_loaded <- readRDS(narratives_path)
}

# ---------- UI ----------
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      /* ===== Global app look (performance / tech) ===== */
      body {
        background: #0b0f17;
        color: #e8edf6;
        font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif;
      }
      .container-fluid { max-width: 1200px; }

      /* ===== Header ===== */
      .wa-header {
        display: flex;
        align-items: center;
        gap: 16px;
        padding: 16px 18px;
        margin: 18px 0 14px 0;
        border-radius: 16px;
        background: linear-gradient(180deg, rgba(255,255,255,0.06), rgba(255,255,255,0.03));
        border: 1px solid rgba(255,255,255,0.10);
        box-shadow: 0 10px 30px rgba(0,0,0,0.35);
      }
      .wa-logo {
        height: 56px;
        width: auto;
        object-fit: contain;
        border-radius: 0;
        border: none;
        box-shadow: none;
      }
      .wa-title {
        margin: 0;
        font-size: 28px;
        font-weight: 650;
        letter-spacing: 0.2px;
        line-height: 1.1;
      }
      .wa-subtitle {
        margin: 2px 0 0 0;
        color: rgba(232,237,246,0.70);
        font-size: 13px;
        letter-spacing: 0.4px;
        text-transform: uppercase;
      }
/* Make the middle title block expand so the right image is pushed to the edge */
.wa-header .wa-header-text {
  flex: 1 1 auto;
}

/* Optional: right-side header image styling */
.wa-header-right-img {
  height: 44px;          /* adjust */
  width: auto;
  object-fit: contain;
  border-radius: 0;   /* optional */
  opacity: 0.95;         /* optional */
}
/* ===== Footer ===== */
.wa-footer {
  display: flex;
  align-items: center;
  gap: 12px;

  padding: 6px 16px;
  margin: 18px 0 8px 0;   /* not centered/narrow */
  width: 100%;
  max-width: none;        /* <-- KEY: remove the 900px cap */

  border-radius: 14px;
  background: linear-gradient(180deg, rgba(255,255,255,0.05), rgba(255,255,255,0.02));
  border: 1px solid rgba(255,255,255,0.10);
  box-shadow: 0 6px 18px rgba(0,0,0,0.25);
}

.wa-footer-text {
  flex: 1 1 auto;   /* pushes image to right */
  font-size: 13px;
  color: rgba(232,237,246,0.65);
  letter-spacing: 0.3px;
}

.wa-footer-img {
  height: 40px;
  width: auto;
  object-fit: contain;
  border-radius: 0;   /* square corners */
}
      /* ===== Tabs ===== */
      .nav-tabs {
        border-bottom: 1px solid rgba(255,255,255,0.12);
        margin-bottom: 14px;
      }
      .nav-tabs > li > a {
        background: transparent !important;
        color: rgba(232,237,246,0.78);
        border: 1px solid transparent;
        border-radius: 12px 12px 0 0;
        padding: 10px 14px;
        margin-right: 6px;
      }
      .nav-tabs > li.active > a,
      .nav-tabs > li.active > a:focus,
      .nav-tabs > li.active > a:hover {
        color: #e8edf6 !important;
        background: rgba(255,255,255,0.06) !important;
        border: 1px solid rgba(255,255,255,0.14) !important;
        border-bottom-color: rgba(11,15,23,0.0) !important;
      }

      /* ===== Panels ===== */
      .well {
        background: rgba(255,255,255,0.04);
        border: 1px solid rgba(255,255,255,0.10);
        border-radius: 16px;
        box-shadow: 0 10px 28px rgba(0,0,0,0.25);
      }

      /* DT tables: dark-ish chrome */
      table.dataTable thead th {
        background: rgba(255,255,255,0.06) !important;
        color: #e8edf6 !important;
        border-bottom: 1px solid rgba(255,255,255,0.14) !important;
      }
      table.dataTable tbody td { color: rgba(232,237,246,0.90) !important; }
      .dataTables_wrapper .dataTables_info { color: rgba(232,237,246,0.65) !important; }

/* Leaflet border polish */
.leaflet-container {
  border-radius: 16px;
  border: 1px solid rgba(255,255,255,0.10);
  box-shadow: 0 12px 34px rgba(0,0,0,0.35);
}

/* ===== Swag Shop Button (Dark + Subtle) ===== */
a.wa-shop-btn {
  display: inline-block !important;
  padding: 12px 18px !important;
  border-radius: 14px !important;

  background-color: rgba(255,255,255,0.06) !important;
  border: 1px solid rgba(255,255,255,0.20) !important;

  color: #e8edf6 !important;
  text-decoration: none !important;
  font-weight: 650 !important;

  transition: all 0.18s ease-in-out !important;
}

a.wa-shop-btn:hover {
  background-color: rgba(255,255,255,0.12) !important;
  border-color: rgba(255,255,255,0.35) !important;
  box-shadow: 0 8px 22px rgba(0,0,0,0.35) !important;
  transform: translateY(-1px) !important;
}

a.wa-shop-btn:active {
  transform: translateY(0px) !important;
  box-shadow: none !important;
}

/* ===== Social Tab ===== */
.social-section {
  background: rgba(255,255,255,0.04);
  border: 1px solid rgba(255,255,255,0.10);
  border-radius: 16px;
  padding: 22px 24px;
  box-shadow: 0 10px 28px rgba(0,0,0,0.25);
  max-width: 800px;
  margin-bottom: 18px;
}
.social-section-header {
  display: flex;
  align-items: center;
  gap: 12px;
  margin-bottom: 16px;
}
.social-section-header h4 {
  margin: 0;
  font-size: 20px;
  font-weight: 650;
}
.social-section-header svg {
  flex-shrink: 0;
}
.social-subtitle {
  color: rgba(232,237,246,0.60);
  font-size: 13px;
  margin: -8px 0 14px 0;
}
.song-grid {
  display: grid;
  grid-template-columns: repeat(auto-fill, minmax(220px, 1fr));
  gap: 10px;
}
a.social-link {
  display: flex !important;
  align-items: center;
  gap: 10px;
  padding: 12px 16px !important;
  border-radius: 12px !important;
  border: 1px solid rgba(255,255,255,0.12) !important;
  background: rgba(255,255,255,0.04) !important;
  color: #e8edf6 !important;
  text-decoration: none !important;
  font-weight: 500 !important;
  font-size: 14px !important;
  transition: all 0.15s ease !important;
}
a.social-link:hover {
  background: rgba(255,255,255,0.10) !important;
  border-color: rgba(255,255,255,0.25) !important;
  transform: translateY(-1px) !important;
  box-shadow: 0 4px 12px rgba(0,0,0,0.3) !important;
}
a.social-link .link-num {
  color: rgba(232,237,246,0.40);
  font-size: 12px;
  font-weight: 600;
  min-width: 16px;
}
a.social-artist-link {
  display: inline-flex !important;
  align-items: center;
  gap: 8px;
  padding: 10px 18px !important;
  border-radius: 20px !important;
  border: 1px solid rgba(29,185,84,0.35) !important;
  background: rgba(29,185,84,0.10) !important;
  color: #1DB954 !important;
  text-decoration: none !important;
  font-weight: 600 !important;
  font-size: 14px !important;
  margin-bottom: 14px !important;
  transition: all 0.15s ease !important;
}
a.social-artist-link:hover {
  background: rgba(29,185,84,0.20) !important;
  border-color: rgba(29,185,84,0.50) !important;
}
a.social-yt-link {
  display: inline-flex !important;
  align-items: center;
  gap: 10px;
  padding: 12px 20px !important;
  border-radius: 12px !important;
  border: 1px solid rgba(255,0,0,0.25) !important;
  background: rgba(255,0,0,0.08) !important;
  color: #e8edf6 !important;
  text-decoration: none !important;
  font-weight: 600 !important;
  font-size: 14px !important;
  transition: all 0.15s ease !important;
}
a.social-yt-link:hover {
  background: rgba(255,0,0,0.15) !important;
  border-color: rgba(255,0,0,0.40) !important;
  transform: translateY(-1px) !important;
}

/* ===== Race Narrative Card ===== */
.race-narrative {
  background: linear-gradient(135deg, rgba(255,255,255,0.04), rgba(255,255,255,0.02));
  border: 1px solid rgba(255,255,255,0.10);
  border-left: 3px solid rgba(100,180,255,0.35);
  border-radius: 12px;
  padding: 22px 26px;
  margin-bottom: 18px;
  max-width: 960px;
  box-shadow: 0 8px 24px rgba(0,0,0,0.2);
}
.race-narrative-title {
  display: flex;
  align-items: center;
  gap: 10px;
  margin-bottom: 14px;
  font-size: 16px;
  font-weight: 600;
  color: rgba(232,237,246,0.90);
  letter-spacing: 0.2px;
}
.race-narrative-body {
  font-size: 14.5px;
  line-height: 1.75;
  color: rgba(232,237,246,0.80);
}
.race-narrative-body p {
  margin-bottom: 12px;
}
.race-narrative-body p:last-child {
  margin-bottom: 0;
}

/* ===== File Management Section ===== */
.file-mgmt-section {
  background: rgba(255,255,255,0.04);
  border: 1px solid rgba(255,255,255,0.10);
  border-radius: 16px;
  padding: 22px 24px;
  box-shadow: 0 10px 28px rgba(0,0,0,0.25);
  max-width: 760px;
  margin-top: 18px;
}
.file-path-display {
  font-family: 'Consolas', 'Monaco', 'Courier New', monospace;
  font-size: 12px;
  color: rgba(232,237,246,0.50);
  background: rgba(0,0,0,0.25);
  border-radius: 6px;
  padding: 6px 10px;
  margin: 6px 0 14px 0;
  word-break: break-all;
}
.wa-danger-btn {
  display: inline-block !important;
  padding: 10px 16px !important;
  border-radius: 12px !important;
  background-color: rgba(220,60,60,0.12) !important;
  border: 1px solid rgba(220,60,60,0.30) !important;
  color: #e8edf6 !important;
  text-decoration: none !important;
  font-weight: 600 !important;
  cursor: pointer !important;
  transition: all 0.18s ease-in-out !important;
  margin-right: 8px !important;
}
.wa-danger-btn:hover {
  background-color: rgba(220,60,60,0.22) !important;
  border-color: rgba(220,60,60,0.50) !important;
  box-shadow: 0 6px 16px rgba(0,0,0,0.3) !important;
}
.wa-rebuild-btn {
  display: inline-block !important;
  padding: 10px 16px !important;
  border-radius: 12px !important;
  background-color: rgba(100,180,255,0.10) !important;
  border: 1px solid rgba(100,180,255,0.25) !important;
  color: #e8edf6 !important;
  text-decoration: none !important;
  font-weight: 600 !important;
  cursor: pointer !important;
  transition: all 0.18s ease-in-out !important;
  margin-right: 8px !important;
}
.wa-rebuild-btn:hover {
  background-color: rgba(100,180,255,0.20) !important;
  border-color: rgba(100,180,255,0.45) !important;
  box-shadow: 0 6px 16px rgba(0,0,0,0.3) !important;
}
"))
  ),
  
  div(
    class = "wa-header",
    tags$img(src = "J112e.jpg", class = "wa-logo"),
    div(
      class = "wa-header-text",
      tags$h1("Wings Analytics", class = "wa-title"),
      tags$p("AI-Inspired • Sailing Performance • Racing Intelligence", class = "wa-subtitle")
    ),
    tags$img(src = "DIYC_Burgee.jpg", class = "wa-header-right-img")
  ),
  
  tabsetPanel(
    id = "main_tabs",
    tabPanel(
      "Race Seasons",
      br(),
      div(
        style = "
      background: rgba(255,255,255,0.04);
      border: 1px solid rgba(255,255,255,0.10);
      border-radius: 16px;
      padding: 18px;
      box-shadow: 0 10px 28px rgba(0,0,0,0.25);
      max-width: 960px;
    ",
        selectInput(
          "season_select",
          "Select Season",
          choices = sort(unique(data_rds$race_calendar$season[!is.na(data_rds$race_calendar$season)]), decreasing = TRUE)
        ),
        tags$p("Click a race to view its analysis.",
               style = "color: rgba(232,237,246,0.55); font-size: 13px; margin-bottom: 10px;"),
        DTOutput("season_table")
      )
    ),
    tabPanel(
      "Race Analysis",
      br(),
      div(
        style = "
          background: rgba(255,255,255,0.04);
          border: 1px solid rgba(255,255,255,0.10);
          border-radius: 16px;
          padding: 18px;
          box-shadow: 0 10px 28px rgba(0,0,0,0.25);
          max-width: 960px;
          margin-bottom: 14px;
        ",
        fluidRow(
          column(4, selectInput(
            "ra_season_select", "Season",
            choices = sort(unique(data_rds$race_calendar$season[!is.na(data_rds$race_calendar$season)]), decreasing = TRUE)
          )),
          column(8, selectInput(
            "ra_race_select", "Race",
            choices = NULL
          ))
        )
      ),
      uiOutput("selected_race_header"),
      DTOutput("race_analysis_summary"),
      br(),
      uiOutput("race_narrative_card"),
      br(),
      leafletOutput("map", height = 450),
      br(),
      h4("Boat Speed (knots, SOG & STW)"),
      plotOutput("plot_boat_speed", height = 220),
      hr(),
      h4("Polar Tables"),
      tabsetPanel(
        tabPanel("Observed STW Polars",   DTOutput("polar_table_stw")),
        tabPanel("Observed SOG Polars",   DTOutput("polar_table_sog")),
        tabPanel("Reference Polars",      DTOutput("polar_table_ref")),
        tabPanel("STW Polar Performance", DTOutput("polar_table_perf_stw")),
        tabPanel("SOG Polar Performance", DTOutput("polar_table_perf_sog"))
      )
    ),
    tabPanel(
      "Social",
      br(),
      
      # ==================== SPOTIFY ====================
      div(
        class = "social-section",
        div(
          class = "social-section-header",
          HTML('<svg width="28" height="28" viewBox="0 0 24 24" fill="#1DB954"><path d="M12 0C5.4 0 0 5.4 0 12s5.4 12 12 12 12-5.4 12-12S18.66 0 12 0zm5.521 17.34c-.24.359-.66.48-1.021.24-2.82-1.74-6.36-2.101-10.561-1.141-.418.122-.779-.179-.899-.539-.12-.421.18-.78.54-.9 4.56-1.021 8.52-.6 11.64 1.32.42.18.479.659.301 1.02zm1.44-3.3c-.301.42-.841.6-1.262.3-3.239-1.98-8.159-2.58-11.939-1.38-.479.12-1.02-.12-1.14-.6-.12-.48.12-1.021.6-1.141C9.6 9.9 15 10.561 18.72 12.84c.361.181.54.78.241 1.2zm.12-3.36C15.24 8.4 8.82 8.16 5.16 9.301c-.6.179-1.2-.181-1.38-.721-.18-.601.18-1.2.72-1.381 4.26-1.26 11.28-1.02 15.721 1.621.539.3.719 1.02.419 1.56-.299.421-1.02.599-1.559.3z"/></svg>'),
          h4("Music on Spotify")
        ),
        tags$a(
          href   = "https://open.spotify.com/artist/5dL0qEjHF2Ql499KZ2kwLl",
          target = "_blank",
          class  = "social-artist-link",
          HTML('<svg width="16" height="16" viewBox="0 0 24 24" fill="#1DB954"><path d="M12 0C5.4 0 0 5.4 0 12s5.4 12 12 12 12-5.4 12-12S18.66 0 12 0zm5.521 17.34c-.24.359-.66.48-1.021.24-2.82-1.74-6.36-2.101-10.561-1.141-.418.122-.779-.179-.899-.539-.12-.421.18-.78.54-.9 4.56-1.021 8.52-.6 11.64 1.32.42.18.479.659.301 1.02zm1.44-3.3c-.301.42-.841.6-1.262.3-3.239-1.98-8.159-2.58-11.939-1.38-.479.12-1.02-.12-1.14-.6-.12-.48.12-1.021.6-1.141C9.6 9.9 15 10.561 18.72 12.84c.361.181.54.78.241 1.2zm.12-3.36C15.24 8.4 8.82 8.16 5.16 9.301c-.6.179-1.2-.181-1.38-.721-.18-.601.18-1.2.72-1.381 4.26-1.26 11.28-1.02 15.721 1.621.539.3.719 1.02.419 1.56-.299.421-1.02.599-1.559.3z"/></svg>'),
          "Wings J112 on Spotify"
        ),
        div(
          class = "song-grid",
          tags$a(href = "https://open.spotify.com/track/23qsWYEBrgBHlA4jHSVk7k",
                 target = "_blank", class = "social-link",
                 tags$span(class = "link-num", "1"), "Wings Through the Night"),
          tags$a(href = "https://open.spotify.com/track/1062JzRoBEpNIK1r6PsXq2",
                 target = "_blank", class = "social-link",
                 tags$span(class = "link-num", "2"), "Bone Island Regatta"),
          tags$a(href = "https://open.spotify.com/track/28RM2e48vyuYdKbVMha7Pm",
                 target = "_blank", class = "social-link",
                 tags$span(class = "link-num", "3"), "Fly Me to Mexico"),
          tags$a(href = "https://open.spotify.com/track/1exg2uXC8Ev9BjURjk3juG",
                 target = "_blank", class = "social-link",
                 tags$span(class = "link-num", "4"), "Bring Her Home"),
          tags$a(href = "https://open.spotify.com/track/1mEr0gBoyo94vLkUaPRIMW",
                 target = "_blank", class = "social-link",
                 tags$span(class = "link-num", "5"), "Tearin' Through The Line")
        )
      ),
      
      # ==================== YOUTUBE ====================
      div(
        class = "social-section",
        div(
          class = "social-section-header",
          HTML('<svg width="32" height="23" viewBox="0 0 159 110" fill="none"><path d="M154 17.5c-1.8-6.7-7.1-12-13.8-13.8C128 0 79.5 0 79.5 0S31 0 18.8 3.7C12.1 5.5 6.8 10.8 5 17.5 1.2 29.7 1.2 55 1.2 55s0 25.3 3.8 37.5c1.8 6.7 7.1 12 13.8 13.8C31 110 79.5 110 79.5 110s48.5 0 60.7-3.7c6.7-1.8 12-7.1 13.8-13.8 3.8-12.2 3.8-37.5 3.8-37.5s0-25.3-3.8-37.5z" fill="#FF0000"/><path d="M64 78.8V31.2L105 55 64 78.8z" fill="#FFF"/></svg>'),
          h4("Videos on YouTube")
        ),
        tags$a(
          href   = "YOUTUBE_URL_PLACEHOLDER",
          target = "_blank",
          class  = "social-yt-link",
          HTML('<svg width="18" height="13" viewBox="0 0 159 110" fill="none"><path d="M154 17.5c-1.8-6.7-7.1-12-13.8-13.8C128 0 79.5 0 79.5 0S31 0 18.8 3.7C12.1 5.5 6.8 10.8 5 17.5 1.2 29.7 1.2 55 1.2 55s0 25.3 3.8 37.5c1.8 6.7 7.1 12 13.8 13.8C31 110 79.5 110 79.5 110s48.5 0 60.7-3.7c6.7-1.8 12-7.1 13.8-13.8 3.8-12.2 3.8-37.5 3.8-37.5s0-25.3-3.8-37.5z" fill="#FF0000"/><path d="M64 78.8V31.2L105 55 64 78.8z" fill="#FFF"/></svg>'),
          "Wings Mexico 2026"
        )
      ),
      
      # ==================== SWAG SHOP ====================
      div(
        class = "social-section",
        div(
          class = "social-section-header",
          HTML('<svg width="26" height="26" viewBox="0 0 24 24" fill="none" stroke="#e8edf6" stroke-width="1.5" stroke-linecap="round" stroke-linejoin="round"><path d="M6 2L3 6v14a2 2 0 002 2h14a2 2 0 002-2V6l-3-4z"/><line x1="3" y1="6" x2="21" y2="6"/><path d="M16 10a4 4 0 01-8 0"/></svg>'),
          h4("Swag Shop")
        ),
        tags$p(
          "Official Wings merchandise.",
          style = "color: rgba(232,237,246,0.60); font-size: 14px; margin-bottom: 14px;"
        ),
        tags$a(
          href   = "https://direct.distrokid.com/wingsj112/home",
          target = "_blank",
          class  = "wa-shop-btn",
          "Visit Swag Shop"
        )
      )
    ),
    tabPanel(
      "About",
      br(),
      div(
        style = "
      background: rgba(255,255,255,0.04);
      border: 1px solid rgba(255,255,255,0.10);
      border-radius: 16px;
      padding: 18px;
      box-shadow: 0 10px 28px rgba(0,0,0,0.25);
      max-width: 760px;
    ",
        h4("Documentation"),
        tags$p(
          "Access the full technical documentation for Wings Analytics, including data pipeline details and performance metric explanations.",
          style = "color: rgba(232,237,246,0.75); margin-bottom: 14px;"
        ),
        tags$a(
          href   = "wings_analytics_documentation.html",
          target = "_blank",
          class  = "wa-shop-btn",
          "View Documentation"
        ),
        tags$hr(style = "border-color: rgba(255,255,255,0.10); margin: 22px 0;"),
        h4("Project Source"),
        tags$p(
          "View the source code and contribute to the project on GitHub.",
          style = "color: rgba(232,237,246,0.75); margin-bottom: 14px;"
        ),
        tags$a(
          href   = "https://github.com/dmdanielson/Wings_Analytics",
          target = "_blank",
          class  = "wa-shop-btn",
          "Visit GitHub"
        )
      ),
      # ---- File Management ----
      div(
        class = "file-mgmt-section",
        h4("Data File Management"),
        tags$p(
          "Manage the data files used by Wings Analytics. These operations are only available when running locally.",
          style = "color: rgba(232,237,246,0.65); font-size: 13px; margin-bottom: 16px;"
        ),
        tags$h5("Track Data", style = "margin-bottom: 4px;"),
        div(class = "file-path-display", rds_path),
        actionButton("btn_delete_rds", "Delete track_data.rds",
                     class = "wa-danger-btn"),
        uiOutput("rds_status_msg"),
        tags$hr(style = "border-color: rgba(255,255,255,0.08); margin: 18px 0;"),
        tags$h5("Race Narratives", style = "margin-bottom: 4px;"),
        div(class = "file-path-display", narratives_path),
        actionButton("btn_rebuild_narratives", "Rebuild Race Narratives",
                     class = "wa-rebuild-btn"),
        actionButton("btn_delete_narratives", "Delete Narratives File",
                     class = "wa-danger-btn"),
        uiOutput("narratives_status_msg")
      )
    )
  ),
  
  div(
    class = "wa-footer",
    div(
      class = "wa-footer-text",
      "© 2026 Wings Analytics. Created by Mike Danielson."
    ),
    tags$img(
      src = "AI6.png",
      class = "wa-footer-img"
    )
  )
)

# ---------- SERVER ----------
server <- function(input, output, session) {
  
  # Reactive: races available for the selected season on the Race Analysis tab
  ra_season_races <- reactive({
    req(input$ra_season_select)
    cal <- data_rds$race_calendar |>
      filter(!is.na(season), season == input$ra_season_select)
    if (nrow(cal) == 0) return(tibble())
    
    cal |>
      mutate(race_date = as.Date(start)) |>
      group_by(race, race_date) |>
      summarise(
        series = first(series),
        place  = first(place),
        fleet  = first(fleet),
        length = first(length),
        start  = min(start, na.rm = TRUE),
        end    = max(end, na.rm = TRUE),
        .groups = "drop"
      ) |>
      arrange(start)
  })
  
  # Build race dropdown labels like "Race Name (MM/DD/YYYY)"
  ra_race_choices <- reactive({
    sr <- ra_season_races()
    if (nrow(sr) == 0) return(character())
    labels <- paste0(sr$race, " (", format(sr$race_date, "%m/%d/%Y"), ")")
    setNames(seq_len(nrow(sr)), labels)
  })
  
  # Update race dropdown when season changes
  observeEvent(input$ra_season_select, {
    choices <- ra_race_choices()
    updateSelectInput(session, "ra_race_select", choices = choices)
  })
  
  # Resolve the currently selected race row from the Race Analysis dropdowns
  ra_selected_row <- reactive({
    sr <- ra_season_races()
    idx <- as.integer(input$ra_race_select)
    req(idx, cancelOutput = TRUE)
    if (is.na(idx) || idx < 1 || idx > nrow(sr)) return(NULL)
    sr[idx, ]
  })
  
  # When a row is clicked in the season table, sync the RA dropdowns and navigate
  observeEvent(input$season_table_rows_selected, {
    row_idx <- input$season_table_rows_selected
    sr <- season_races()
    if (is.null(row_idx) || row_idx > nrow(sr)) return()
    
    clicked_season <- input$season_select
    clicked_race   <- sr$race[row_idx]
    clicked_date   <- sr$race_date[row_idx]
    
    # Sync season dropdown (this triggers ra_season_races update)
    updateSelectInput(session, "ra_season_select", selected = clicked_season)
    
    # After season updates, find matching race index and select it
    # Use a delayed observer so the race choices are rebuilt first
    observe({
      ra_sr <- ra_season_races()
      req(nrow(ra_sr) > 0)
      match_idx <- which(ra_sr$race == clicked_race & ra_sr$race_date == clicked_date)
      if (length(match_idx) > 0) {
        choices <- ra_race_choices()
        updateSelectInput(session, "ra_race_select", choices = choices, selected = match_idx[1])
      }
      # Self-destroy after running once
    }) |> bindEvent(ra_season_races(), once = TRUE)
    
    updateTabsetPanel(session, "main_tabs", selected = "Race Analysis")
  })
  
  # Header for Race Analysis tab showing the selected race
  output$selected_race_header <- renderUI({
    row <- ra_selected_row()
    if (is.null(row)) {
      return(h4("Select a season and race above.", style = "color: rgba(232,237,246,0.65);"))
    }
    h4(paste0(row$race, " — ", format(row$race_date, "%m/%d/%Y")))
  })
  
  # Reactive store for narratives (so rebuild button can update them)
  race_narratives <- reactiveVal(race_narratives_loaded)
  
  # Race Narrative Card
  output$race_narrative_card <- renderUI({
    row <- ra_selected_row()
    if (is.null(row)) return(NULL)
    
    key  <- paste(row$race, row$race_date, sep = "|")
    narr <- race_narratives()
    text <- narr[[key]]
    
    if (is.null(text) || !nzchar(text)) {
      text <- paste0("No narrative is available for ", row$race,
                     ". Try rebuilding the narratives file from the About tab.")
    }
    
    # Convert double-newlines to <p> tags
    paras <- strsplit(text, "\n\n", fixed = TRUE)[[1]]
    body_html <- paste0("<p>", htmltools::htmlEscape(paras), "</p>", collapse = "\n")
    
    div(
      class = "race-narrative",
      div(
        class = "race-narrative-title",
        HTML('<svg width="18" height="18" viewBox="0 0 24 24" fill="none" stroke="rgba(100,180,255,0.7)" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><path d="M12 20h9"/><path d="M16.5 3.5a2.121 2.121 0 013 3L7 19l-4 1 1-4L16.5 3.5z"/></svg>'),
        "Race Report"
      ),
      div(class = "race-narrative-body", HTML(body_html))
    )
  })
  
  # Race Analysis summary table
  output$race_analysis_summary <- renderDT({
    cal_row <- ra_selected_row()
    req(cal_row)
    df <- track()
    
    race_name     <- cal_row$race
    race_date_fmt <- format(cal_row$race_date, "%m/%d/%Y")
    distance      <- if (nrow(cal_row) > 0 && !is.na(cal_row$length)) cal_row$length else NA_real_
    place_fleet   <- if (nrow(cal_row) > 0 && !is.na(cal_row$place) && !is.na(cal_row$fleet)) {
      paste0(cal_row$place, " / ", as.integer(cal_row$fleet))
    } else if (nrow(cal_row) > 0 && !is.na(cal_row$place)) {
      as.character(cal_row$place)
    } else {
      NA_character_
    }
    
    if (nrow(df) == 0) {
      summary_df <- tibble(
        Race = race_name, Date = race_date_fmt,
        `Distance (nm)` = distance, `Place / Fleet` = place_fleet,
        `Elapsed Time` = NA_character_,
        `Avg STW` = NA_real_, `Avg SOG` = NA_real_,
        `Max STW` = NA_real_, `Max SOG` = NA_real_,
        `Avg TWS` = NA_real_, `Max TWS` = NA_real_,
        `STW Polar Perf` = NA_real_, `SOG Polar Perf` = NA_real_
      )
    } else {
      t_start <- min(df$datetime_local, na.rm = TRUE)
      t_end   <- max(df$datetime_local, na.rm = TRUE)
      elapsed_secs <- as.numeric(difftime(t_end, t_start, units = "secs"))
      hrs <- floor(elapsed_secs / 3600)
      mins <- floor((elapsed_secs %% 3600) / 60)
      secs <- round(elapsed_secs %% 60)
      elapsed_fmt <- sprintf("%dh %02dm %02ds", hrs, mins, secs)
      
      summary_df <- tibble(
        Race             = race_name,
        Date             = race_date_fmt,
        `Distance (nm)`  = distance,
        `Place / Fleet`  = place_fleet,
        `Elapsed Time`   = elapsed_fmt,
        `Avg STW`        = round(mean(df$stw_knots, na.rm = TRUE), 2),
        `Avg SOG`        = round(mean(df$sog_knots, na.rm = TRUE), 2),
        `Max STW`        = round(max(df$stw_knots, na.rm = TRUE), 2),
        `Max SOG`        = round(max(df$sog_knots, na.rm = TRUE), 2),
        `Avg TWS`        = round(mean(df$tws_knots, na.rm = TRUE), 2),
        `Max TWS`        = round(max(df$tws_knots, na.rm = TRUE), 2),
        `STW Polar Perf` = round(mean(df$Polar_Perf_STW, na.rm = TRUE), 2),
        `SOG Polar Perf` = round(mean(df$Polar_Perf_SOG, na.rm = TRUE), 2)
      )
    }
    
    # Replace NaN with NA for display
    summary_df <- summary_df %>%
      mutate(across(where(is.numeric), ~ ifelse(is.nan(.), NA_real_, .)))
    
    datatable(
      summary_df,
      options = list(dom = "t", ordering = FALSE, scrollX = TRUE),
      rownames = FALSE
    )
  })
  
  track_all <- reactive({
    df <- data_rds$track_all
    validate(need(nrow(df) > 0, "No track data in RDS file."))
    
    df %>%
      mutate(
        day_local_ui = as.Date(datetime_local, tz = LOCAL_TZ),
        helm_ui      = ifelse(!is.na(helm) & nzchar(helm), helm, "(blank)"),
        race_ui      = ifelse(!is.na(race) & nzchar(race), race, "(blank)")
      )
  })
  
  # Reactive: season table rows (without TOTAL footer) for shared access

  season_races <- reactive({
    req(input$season_select)
    cal <- data_rds$race_calendar %>%
      filter(!is.na(season), season == input$season_select)
    if (nrow(cal) == 0) return(tibble())
    
    cal %>%
      mutate(race_date = as.Date(start)) %>%
      group_by(race, race_date) %>%
      summarise(
        series = first(series),
        place  = first(place),
        fleet  = first(fleet),
        length = first(length),
        start  = min(start, na.rm = TRUE),
        end    = max(end, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(start) %>%
      mutate(
        duration_hrs = as.numeric(difftime(end, start, units = "hours")),
        duration_hrs = ifelse(!is.na(duration_hrs) & duration_hrs == 0, NA_real_, duration_hrs),
        duration_hrs = round(duration_hrs, 2)
      )
  })
  
  output$season_table <- renderDT({
    cal_summary <- season_races()
    
    if (nrow(cal_summary) == 0) {
      return(datatable(tibble::tibble(Message = "No races found for this season.")))
    }
    
    res <- cal_summary %>%
      transmute(
        `#`          = row_number(),
        Date         = format(as.Date(start), "%m/%d/%Y"),
        Series       = ifelse(is.na(series) | series == "", "", series),
        Race         = race,
        Place        = ifelse(is.na(place) | place == "", "", place),
        Fleet        = ifelse(is.na(fleet), "n/a", as.character(as.integer(fleet))),
        Length       = ifelse(is.na(length), NA_real_, length),
        Duration_Hrs = duration_hrs
      )
    
    # Footer row with totals
    footer_row <- tibble::tibble(
      `#`          = NA_integer_,
      Date         = "",
      Series       = "",
      Race         = "TOTAL",
      Place        = "",
      Fleet        = "",
      Length       = round(sum(res$Length, na.rm = TRUE), 1),
      Duration_Hrs = round(sum(res$Duration_Hrs, na.rm = TRUE), 2)
    )
    res <- bind_rows(res, footer_row)
    
    res <- res %>%
      mutate(
        `#`          = ifelse(is.na(`#`), "", as.character(`#`)),
        Length       = ifelse(is.na(Length), "", as.character(Length)),
        Duration_Hrs = ifelse(is.na(Duration_Hrs), "", as.character(Duration_Hrs))
      )
    
    datatable(
      res,
      selection = "single",
      options = list(dom = 't', pageLength = 100),
      rownames = FALSE
    )
  })
  

  # Filter track data to the race selected from the Race Analysis dropdowns
  track <- reactive({
    row <- ra_selected_row()
    req(row)
    df <- track_all()
    df |>
      filter(
        race == row$race,
        datetime_local >= row$start,
        datetime_local <= row$end
      )
  })
  
  # MAP
  output$map <- renderLeaflet({
    df <- track()
    validate(need(nrow(df) > 0, "No data to display for selected filters."))

    days       <- sort(unique(df$day_local_ui))
    day_labels <- format(days, "%Y-%m-%d")

    pal <- colorFactor(
      brewer.pal(max(3, min(9, length(days))), "Set1"),
      day_labels
    )

    m <- leaflet() |> addTiles()

    for (i in seq_along(days)) {
      d   <- days[i]
      lab <- day_labels[i]
      dfi <- df |> filter(day_local_ui == d) |> arrange(datetime_local)

      m <- m |>
        addPolylines(
          data    = dfi,
          lng     = ~longitude,
          lat     = ~latitude,
          color   = pal(lab),
          weight  = 3,
          opacity = 0.9,
          group   = lab
        )
    }

    m |>
      addLegend(
        position = "bottomright",
        colors   = pal(day_labels),
        labels   = day_labels,
        title    = "Day (local)"
      )
  })
  
  # BOAT SPEED
  output$plot_boat_speed <- renderPlot({
    df <- track()
    validate(need(nrow(df) > 0, "No data to plot for selected filters."))
    
    raw_long <- df %>%
      select(datetime_local, day_local_ui, sog_knots, stw_knots) %>%
      rename(day_local = day_local_ui) %>%
      pivot_longer(
        cols      = c(sog_knots, stw_knots),
        names_to  = "speed_type",
        values_to = "speed_knots"
      ) %>%
      mutate(kind = "Raw")
    
    ma_long <- df %>%
      mutate(bin_10 = floor_date(datetime_local, "1 minute")) %>%
      group_by(day_local_ui, bin_10) %>%
      summarise(
        sog_knots = mean(sog_knots, na.rm = TRUE),
        stw_knots = mean(stw_knots, na.rm = TRUE),
        .groups   = "drop"
      ) %>%
      rename(datetime_local = bin_10, day_local = day_local_ui) %>%
      pivot_longer(
        cols      = c(sog_knots, stw_knots),
        names_to  = "speed_type",
        values_to = "speed_knots"
      ) %>%
      mutate(kind = "1-min MA")
    
    df_long <- bind_rows(raw_long, ma_long) %>%
      mutate(
        speed_type = recode(speed_type,
                            sog_knots = "SOG",
                            stw_knots = "STW")
      )
    
    ggplot(
      df_long,
      aes(
        x        = datetime_local,
        y        = speed_knots,
        color    = speed_type,
        group    = interaction(day_local, speed_type, kind),
        size     = kind,
        alpha    = kind
      )
    ) +
      geom_line() +
      scale_size_manual(values = c("Raw" = 0.3, "1-min MA" = 1.8)) +
      scale_alpha_manual(values = c("Raw" = 0.25, "1-min MA" = 1.0)) +
      labs(
        x     = "Local Time",
        y     = "Speed (knots)",
        title = "Boat Speed: SOG vs STW (Raw + 1-min MA)",
        color = "Speed Type",
        size  = "Series",
        alpha = "Series"
      ) +
      theme_minimal()
  })
  
  # NEW AVERAGING Formulas (kept as you had it)
  overall_polar_perf <- reactive({
    list(
      STW = mean(polar_perf_stw_df()$perf_stw, na.rm = TRUE),
      SOG = mean(polar_perf_sog_df()$perf_sog, na.rm = TRUE)
    )
  })
  
  # POLAR BIN HELPERS
  track_wind <- reactive({
    tr <- track()
    tr %>% filter(!is.na(tws_knots), !is.na(twa_deg))
  })
  
  polar_bins <- reactive({
    tw       <- track_wind()
    ref_long <- data_rds$polar_ref_long
    if (nrow(tw) == 0 || nrow(ref_long) == 0)
      return(list(tw = tibble(), ref_bins = tibble()))
    
    polar_tws_vals <- sort(unique(ref_long$tws))
    polar_twa_vals <- sort(unique(ref_long$twa))
    
    tw <- tw %>%
      mutate(
        tws_idx = findInterval(tws_knots, polar_tws_vals, all.inside = TRUE),
        twa_idx = findInterval(twa_deg,  polar_twa_vals, all.inside = TRUE),
        tws_bin = polar_tws_vals[tws_idx],
        twa_bin = polar_twa_vals[twa_idx]
      )
    
    grid <- expand.grid(
      tws_bin = polar_tws_vals,
      twa_bin = polar_twa_vals
    )
    
    ref_bins <- ref_long %>%
      rename(tws_bin = tws, twa_bin = twa) %>%
      right_join(grid, by = c("tws_bin", "twa_bin"))
    
    list(tw = tw, ref_bins = ref_bins)
  })
  
  # OBSERVED STW POLARS
  polar_stw_df <- reactive({
    bins <- polar_bins()
    tw   <- bins$tw
    if (nrow(tw) == 0) return(tibble())
    
    tw %>%
      filter(!is.na(stw_knots), stw_knots > 0) %>%
      group_by(tws_bin, twa_bin) %>%
      summarise(avg_stw = mean(stw_knots, na.rm = TRUE), .groups = "drop")
  })
  
  output$polar_table_stw <- renderDT({
    df <- polar_stw_df()
    if (nrow(df) == 0) {
      return(datatable(data.frame(Message = "No STW polar data available."),
                       options = list(dom = "t"), rownames = FALSE))
    }
    
    wide <- df %>%
      mutate(tws_bin = as.numeric(tws_bin), twa_bin = as.numeric(twa_bin)) %>%
      arrange(tws_bin, twa_bin) %>%
      tidyr::pivot_wider(id_cols = tws_bin, names_from = twa_bin, values_from = avg_stw)
    
    angle_names <- setdiff(names(wide), "tws_bin")
    angle_vals  <- readr::parse_number(angle_names)
    keep <- !is.na(angle_vals)
    angle_names <- angle_names[keep]
    angle_vals  <- angle_vals[keep]
    angle_names <- angle_names[order(angle_vals)]
    
    wide <- wide %>%
      select(tws_bin, all_of(angle_names))
    
    colnames(wide)[1] <- "TWS"
    if (ncol(wide) > 1) colnames(wide)[-1] <- paste0(colnames(wide)[-1], "°")
    
    datatable(wide,
              options = list(pageLength = nrow(wide), dom = "t", scrollX = TRUE),
              rownames = FALSE) %>%
      formatRound(columns = 1:ncol(wide), digits = 2)
  })
  
  # OBSERVED SOG POLARS
  polar_sog_df <- reactive({
    bins <- polar_bins()
    tw   <- bins$tw
    if (nrow(tw) == 0) return(tibble())
    
    tw %>%
      filter(!is.na(sog_knots), sog_knots > 0) %>%
      group_by(tws_bin, twa_bin) %>%
      summarise(avg_sog = mean(sog_knots, na.rm = TRUE), .groups = "drop")
  })
  
  output$polar_table_sog <- renderDT({
    df <- polar_sog_df()
    if (nrow(df) == 0) {
      return(datatable(data.frame(Message = "No SOG polar data available."),
                       options = list(dom = "t"), rownames = FALSE))
    }
    
    wide <- df %>%
      mutate(tws_bin = as.numeric(tws_bin), twa_bin = as.numeric(twa_bin)) %>%
      arrange(tws_bin, twa_bin) %>%
      tidyr::pivot_wider(id_cols = tws_bin, names_from = twa_bin, values_from = avg_sog)
    
    angle_names <- setdiff(names(wide), "tws_bin")
    angle_vals  <- readr::parse_number(angle_names)
    keep <- !is.na(angle_vals)
    angle_names <- angle_names[keep]
    angle_vals  <- angle_vals[keep]
    angle_names <- angle_names[order(angle_vals)]
    
    wide <- wide %>%
      select(tws_bin, all_of(angle_names))
    
    colnames(wide)[1] <- "TWS"
    if (ncol(wide) > 1) colnames(wide)[-1] <- paste0(colnames(wide)[-1], "°")
    
    datatable(wide,
              options = list(pageLength = nrow(wide), dom = "t", scrollX = TRUE),
              rownames = FALSE) %>%
      formatRound(columns = 1:ncol(wide), digits = 2)
  })
  
  # REFERENCE POLARS TABLE
  polar_ref_grid_df <- reactive({
    ref_long <- data_rds$polar_ref_long
    if (nrow(ref_long) == 0) return(tibble())
    
    ref_long %>%
      group_by(tws, twa) %>%
      summarise(bsp_ref = mean(bsp_ref, na.rm = TRUE), .groups = "drop") %>%
      rename(tws_bin = tws, twa_bin = twa)
  })
  
  output$polar_table_ref <- renderDT({
    df <- polar_ref_grid_df()
    if (nrow(df) == 0) {
      return(datatable(data.frame(Message = "No reference polar data found."),
                       options = list(dom = "t"), rownames = FALSE))
    }
    
    wide <- df %>%
      mutate(tws_bin = as.numeric(tws_bin), twa_bin = as.numeric(twa_bin)) %>%
      arrange(tws_bin, twa_bin) %>%
      tidyr::pivot_wider(id_cols = tws_bin, names_from = twa_bin, values_from = bsp_ref)
    
    colnames(wide)[1] <- "TWS"
    if (ncol(wide) > 1) colnames(wide)[-1] <- paste0(colnames(wide)[-1], "°")
    
    datatable(wide,
              options = list(pageLength = nrow(wide), dom = "t", scrollX = TRUE),
              rownames = FALSE) %>%
      formatRound(columns = 1:ncol(wide), digits = 2)
  })
  
  # STW POLAR PERFORMANCE
  polar_perf_stw_df <- reactive({
    bins    <- polar_bins()
    ref_bin <- bins$ref_bins
    obs_stw <- polar_stw_df()
    
    if (nrow(ref_bin) == 0 || nrow(obs_stw) == 0) return(tibble())
    
    ref_bin %>%
      left_join(obs_stw, by = c("tws_bin", "twa_bin")) %>%
      mutate(perf_stw = avg_stw - bsp_ref)
  })
  
  output$polar_table_perf_stw <- renderDT({
    df <- polar_perf_stw_df()
    if (nrow(df) == 0) {
      return(datatable(data.frame(Message = "No STW performance data available."),
                       options = list(dom = "t"), rownames = FALSE))
    }
    
    wide <- df %>%
      mutate(tws_bin = as.numeric(tws_bin), twa_bin = as.numeric(twa_bin)) %>%
      arrange(tws_bin, twa_bin) %>%
      tidyr::pivot_wider(id_cols = tws_bin, names_from = twa_bin, values_from = perf_stw)
    
    colnames(wide)[1] <- "TWS"
    if (ncol(wide) > 1) colnames(wide)[-1] <- paste0(colnames(wide)[-1], "°")
    
    if (ncol(wide) > 1) {
      row_avg <- polar_perf_stw_df() %>%
        group_by(tws_bin) %>%
        summarise(`Ave Perf` = mean(perf_stw, na.rm = TRUE), .groups = "drop")
      wide <- wide %>%
        left_join(row_avg, by = c("TWS" = "tws_bin"))
    }
    
    sketch <- withTags(table(
      class = 'display',
      thead(tr(lapply(names(wide), th))),
      tfoot(tr(lapply(names(wide), th)))
    ))
    
    datatable(
      wide,
      container = sketch,
      options   = list(
        pageLength = nrow(wide),
        dom        = "t",
        scrollX    = TRUE,
        footerCallback = DT::JS(
          "
          function(row, data, start, end, display) {
            var api   = this.api();
            var ncols = api.columns().count();
            $(api.column(0).footer()).html('Ave Perf');
            for (var col = 1; col < ncols; col++) {
              var colData = api.column(col, {page: 'current'}).data();
              var vals = [];
              for (var i = 0; i < colData.length; i++) {
                var x = parseFloat(colData[i]);
                if (!isNaN(x)) vals.push(x);
              }
              var mean = vals.length ? vals.reduce(function(a, b) { return a + b; }, 0) / vals.length : '';
              $(api.column(col).footer()).html(mean === '' ? '' : mean.toFixed(2));
            }
          }
          "
        )
      ),
      rownames = FALSE
    ) %>%
      formatRound(columns = 1:ncol(wide), digits = 2)
  })
  
  # SOG POLAR PERFORMANCE
  polar_perf_sog_df <- reactive({
    bins    <- polar_bins()
    ref_bin <- bins$ref_bins
    obs_sog <- polar_sog_df()
    
    if (nrow(ref_bin) == 0 || nrow(obs_sog) == 0) return(tibble())
    
    ref_bin %>%
      left_join(obs_sog, by = c("tws_bin", "twa_bin")) %>%
      mutate(perf_sog = avg_sog - bsp_ref)
  })
  
  output$polar_table_perf_sog <- renderDT({
    df <- polar_perf_sog_df()
    if (nrow(df) == 0) {
      return(datatable(data.frame(Message = "No SOG performance data available."),
                       options = list(dom = "t"), rownames = FALSE))
    }
    
    wide <- df %>%
      mutate(tws_bin = as.numeric(tws_bin), twa_bin = as.numeric(twa_bin)) %>%
      arrange(tws_bin, twa_bin) %>%
      tidyr::pivot_wider(id_cols = tws_bin, names_from = twa_bin, values_from = perf_sog)
    
    colnames(wide)[1] <- "TWS"
    if (ncol(wide) > 1) colnames(wide)[-1] <- paste0(colnames(wide)[-1], "°")
    
    if (ncol(wide) > 1) {
      perf_values <- wide[, 2:ncol(wide), drop = FALSE] # (kept as you had it)
      
      row_avg <- polar_perf_sog_df() %>%
        group_by(tws_bin) %>%
        summarise(`Ave Perf` = mean(perf_sog, na.rm = TRUE), .groups = "drop")
      wide <- wide %>%
        left_join(row_avg, by = c("TWS" = "tws_bin")) %>%
        relocate(`Ave Perf`, .after = last_col())
    }
    
    sketch <- withTags(table(
      class = 'display',
      thead(tr(lapply(names(wide), th))),
      tfoot(tr(lapply(names(wide), th)))
    ))
    
    datatable(
      wide,
      container = sketch,
      options   = list(
        pageLength = nrow(wide),
        dom        = "t",
        scrollX    = TRUE,
        footerCallback = DT::JS(
          "
          function(row, data, start, end, display) {
            var api   = this.api();
            var ncols = api.columns().count();
            $(api.column(0).footer()).html('Ave Perf');
            for (var col = 1; col < ncols; col++) {
              var colData = api.column(col, {page: 'current'}).data();
              var vals = [];
              for (var i = 0; i < colData.length; i++) {
                var x = parseFloat(colData[i]);
                if (!isNaN(x)) vals.push(x);
              }
              var mean = vals.length ? vals.reduce(function(a, b) { return a + b; }, 0) / vals.length : '';
              $(api.column(col).footer()).html(mean === '' ? '' : mean.toFixed(2));
            }
          }
          "
        )
      ),
      rownames = FALSE
    ) %>%
      formatRound(columns = 1:ncol(wide), digits = 2)
  })
  
  # =========================================================
  # STATIC RACE SUMMARY TABLE (USES ALL DATA, NO FILTERS)
  # =========================================================
  race_summary_df <- reactive({
    df_track <- data_rds$track_all
    cal      <- data_rds$race_calendar
    
    # ---- Completed races from tracks ----
    # Group by race + day so same-named races on different dates stay separate
    completed <- df_track %>%
      filter(!is.na(race), nzchar(race)) %>%
      mutate(race_start_date = as.Date(datetime_local)) %>%
      group_by(race, race_start_date) %>%
      summarise(
        start_time = min(datetime_local, na.rm = TRUE),
        end_time   = max(datetime_local, na.rm = TRUE),
        race_duration_hours = as.numeric(difftime(end_time, start_time, units = "hours")),
        STW_Polar_Performance = mean(Polar_Perf_STW, na.rm = TRUE),
        SOG_Polar_Performance = mean(Polar_Perf_SOG, na.rm = TRUE),
        TWS_Min = if (all(is.na(tws_knots))) NA_real_ else min(tws_knots, na.rm = TRUE),
        TWS_Avg = if (all(is.na(tws_knots))) NA_real_ else mean(tws_knots, na.rm = TRUE),
        TWS_Max = if (all(is.na(tws_knots))) NA_real_ else max(tws_knots, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        Status  = "Completed",
        Ranking = rank(-SOG_Polar_Performance, ties.method = "first")
      )
    
    # ---- Calendar master list (includes old races with no tracks) ----
    if (nrow(cal) == 0) {
      return(completed %>% arrange(race_start_date, race))
    }
    
    now_local <- lubridate::with_tz(Sys.time(), LOCAL_TZ)
    
    cal_master <- cal %>%
      transmute(
        race  = as.character(race),
        start_time = start,
        end_time   = end,
        race_start_date = as.Date(start),
        Place = if ("place" %in% names(cal)) as.character(place) else NA_character_
      ) %>%
      filter(!is.na(race), nzchar(race), !is.na(start_time)) %>%
      group_by(race, race_start_date) %>%
      summarise(
        start_time = min(start_time, na.rm = TRUE),
        end_time   = max(end_time,   na.rm = TRUE),
        Place = dplyr::first(na.omit(Place)),
        cal_duration_hours = as.numeric(difftime(
          max(end_time, na.rm = TRUE),
          min(start_time, na.rm = TRUE),
          units = "hours"
        )),
        .groups = "drop"
      )
    
    # Overlay completed stats onto calendar.
    # Calendar rows remain even when there's no track match.
    out <- cal_master %>%
      left_join(completed, by = c("race", "race_start_date"), suffix = c("_cal", "")) %>%
      mutate(
        # If completed exists, keep completed’s times/duration; else use calendar times/duration
        start_time = dplyr::coalesce(start_time, start_time_cal),
        end_time   = dplyr::coalesce(end_time,   end_time_cal),
        race_start_date = dplyr::coalesce(race_start_date, race_start_date_cal),
        race_duration_hours = dplyr::coalesce(race_duration_hours, cal_duration_hours),
        
        Status = dplyr::case_when(
          !is.na(Status) ~ Status,               # Completed already set
          end_time_cal < now_local ~ "No Track", # past race but no track data
          TRUE ~ "Scheduled"
        )
      ) %>%
      select(
        race,
        start_time, end_time, race_start_date, race_duration_hours,
        Place,
        TWS_Min, TWS_Avg, TWS_Max,
        SOG_Polar_Performance, STW_Polar_Performance,
        Status, Ranking
      ) %>%
      arrange(race_start_date, race)
    
    out
  })
  
  output$race_summary_table <- renderDT({
    df <- race_summary_df()
    
    if (nrow(df) == 0) {
      return(datatable(
        data.frame(Message = "No race data available."),
        options = list(dom = "t"),
        rownames = FALSE
      ))
    }
    
    df_display <- df %>%
      select(
        `Race Date` = race_start_date,
        Race = race,
        Place = Place,
        `Race Duration (hrs)` = race_duration_hours,
        `TWS Min` = TWS_Min,
        `TWS Avg` = TWS_Avg,
        `TWS Max` = TWS_Max,
        `SOG Polar Performance` = SOG_Polar_Performance,
        `STW Polar Performance` = STW_Polar_Performance
      ) %>%
      mutate(`Race Date` = format(`Race Date`, "%Y-%m-%d"))
    
    datatable(
      df_display,
      options = list(
        pageLength = nrow(df_display),
        dom = "t",
        ordering = FALSE,
        columnDefs = list(
          list(className = "dt-center", targets = which(names(df_display) == "Place") - 1)
        )
      ),
      rownames = FALSE
    ) %>%
      formatRound(
        columns = c(
          "Race Duration (hrs)",
          "TWS Min", "TWS Avg", "TWS Max",
          "STW Polar Performance",
          "SOG Polar Performance"
        ),
        digits = 2
      )
  })
  
  # ---- FILE MANAGEMENT BUTTON HANDLERS ----
  
  # Rebuild narratives
  observeEvent(input$btn_rebuild_narratives, {
    narr <- build_all_narratives(data_rds)
    tryCatch({
      saveRDS(narr, narratives_path)
      race_narratives(narr)
      output$narratives_status_msg <- renderUI({
        tags$p(paste0("Narratives rebuilt successfully (",
                      length(narr), " races)."),
               style = "color: rgba(100,200,120,0.85); font-size: 13px; margin-top: 8px;")
      })
    }, error = function(e) {
      output$narratives_status_msg <- renderUI({
        tags$p(paste0("Error: ", e$message),
               style = "color: rgba(220,80,80,0.85); font-size: 13px; margin-top: 8px;")
      })
    })
  })
  
  # Delete narratives file
  observeEvent(input$btn_delete_narratives, {
    if (file.exists(narratives_path)) {
      tryCatch({
        file.remove(narratives_path)
        output$narratives_status_msg <- renderUI({
          tags$p("Narratives file deleted.",
                 style = "color: rgba(220,180,80,0.85); font-size: 13px; margin-top: 8px;")
        })
      }, error = function(e) {
        output$narratives_status_msg <- renderUI({
          tags$p(paste0("Error: ", e$message),
                 style = "color: rgba(220,80,80,0.85); font-size: 13px; margin-top: 8px;")
        })
      })
    } else {
      output$narratives_status_msg <- renderUI({
        tags$p("File does not exist.",
               style = "color: rgba(232,237,246,0.50); font-size: 13px; margin-top: 8px;")
      })
    }
  })
  
  # Delete track_data.rds
  observeEvent(input$btn_delete_rds, {
    if (file.exists(rds_path)) {
      tryCatch({
        file.remove(rds_path)
        output$rds_status_msg <- renderUI({
          tags$p("track_data.rds deleted. Restart the app to rebuild from raw data.",
                 style = "color: rgba(220,180,80,0.85); font-size: 13px; margin-top: 8px;")
        })
      }, error = function(e) {
        output$rds_status_msg <- renderUI({
          tags$p(paste0("Error: ", e$message),
                 style = "color: rgba(220,80,80,0.85); font-size: 13px; margin-top: 8px;")
        })
      })
    } else {
      output$rds_status_msg <- renderUI({
        tags$p("File does not exist.",
               style = "color: rgba(232,237,246,0.50); font-size: 13px; margin-top: 8px;")
      })
    }
  })
  }

shinyApp(ui, server)