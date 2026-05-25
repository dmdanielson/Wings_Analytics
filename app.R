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
app_dir_local  <- "G:/My Drive/Personal/Mike/Sailing/Data/Wings_Analytics"
data_dir_local <- "G:/My Drive/Personal/Mike/Sailing/Data"

app_dir  <- if (dir.exists(app_dir_local)) app_dir_local else getwd()
data_dir <- if (dir.exists(data_dir_local)) data_dir_local else app_dir

# Only setwd when local path exists; never needed on shinyapps
if (dir.exists(app_dir_local)) setwd(app_dir_local)

rds_path           <- file.path(app_dir, "track_data.rds")
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
          helm     = as.character(helm),
          headsail = as.character(headsail),
          place    = if ("place" %in% names(race_cal_raw)) as.character(place) else NA_character_,
          start    = excel_to_posix_local(start),
          end      = excel_to_posix_local(end)
        ) %>%
        filter(!is.na(race), nzchar(race), !is.na(start)) %>%
        mutate(
          end = if_else(is.na(end), start, end),
          start2 = pmin(start, end),
          end2   = pmax(start, end),
          start  = start2,
          end    = end2
        ) %>%
        select(race, helm, headsail, place, start, end) %>%
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
  race_calendar  = race_calendar_loaded   # <-- ADD THIS
)
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
    tabPanel(
      "Race Season",
      br(),
      h4("Race Summary"),
      DTOutput("race_summary_table")
    ),
    
    tabPanel(
      "Races",
      sidebarLayout(
        sidebarPanel(
          uiOutput("helm_selector"),
          uiOutput("race_selector"),
          checkboxInput(
            "use_race_filter",
            "Filter by selected races",
            value = TRUE
          )
        ),
        mainPanel(
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
    ),  
    
    tabPanel(
      "Social",
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
        
        # ======================
        # MUSIC SECTION
        # ======================
        h4("Music on Spotify", style = "margin-top: 0;"),
        
        div(
          style = "display:flex; flex-wrap:wrap; gap:10px;",
          
          tags$a(
            href   = "https://open.spotify.com/artist/5dL0qEjHF2Ql499KZ2kwLl",
            target = "_blank",
            style  = "
          display:inline-block;
          padding:10px 14px;
          border-radius:12px;
          border:1px solid rgba(255,255,255,0.16);
          background: rgba(255,255,255,0.06);
          color:#e8edf6;
          text-decoration:none;
          font-weight:600;
        ",
            "Artist: Wings"
          ),
          
          tags$a(
            href   = "https://open.spotify.com/track/23qsWYEBrgBHlA4jHSVk7k",
            target = "_blank",
            style  = "
          display:inline-block;
          padding:10px 14px;
          border-radius:12px;
          border:1px solid rgba(255,255,255,0.16);
          background: rgba(255,255,255,0.06);
          color:#e8edf6;
          text-decoration:none;
          font-weight:600;
        ",
            "Song 1: Wings Through the Night"
          ),
          
          tags$a(
            href   = "https://open.spotify.com/track/1062JzRoBEpNIK1r6PsXq2",
            target = "_blank",
            style  = "
          display:inline-block;
          padding:10px 14px;
          border-radius:12px;
          border:1px solid rgba(255,255,255,0.16);
          background: rgba(255,255,255,0.06);
          color:#e8edf6;
          text-decoration:none;
          font-weight:600;
        ",
            "Song 2: Bone Island Regatta"
          )
        ),
        
        tags$hr(style = "border-color: rgba(255,255,255,0.10); margin: 22px 0;"),
        
        # ======================
        # SWAG SHOP SECTION
        # ======================
        h4("Swag Shop"),
        
        tags$p(
          "Official Wings merchandise.",
          style = "color: rgba(232,237,246,0.75); margin-bottom: 14px;"
        ),
        
        tags$a(
          href   = "https://direct.distrokid.com/wingsj112/home",
          target = "_blank",
          class  = "wa-shop-btn",
          "Visit Swag Shop"
        )      )
    )
  )
)

# ---------- SERVER ----------
server <- function(input, output, session) {
  
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
  
  output$helm_selector <- renderUI({
    df <- track_all()
    helms <- sort(unique(df$helm_ui))
    checkboxGroupInput(
      "helm_filter",
      "Select helms to display",
      choices  = helms,
      selected = helms
    )
  })
  
  output$race_selector <- renderUI({
    df <- track_all()
    races <- sort(unique(df$race_ui[df$race_ui != "(blank)"]))
    if (length(races) > 0) {
      checkboxGroupInput(
        "race_filter",
        "Select races to display",
        choices  = races,
        selected = races[1]
      )
    } else {
      helpText("No race segments matched datetime_local from Race Calendar.xlsx.")
    }
  })
  
  # NOTE: date selector removed; this reactive now depends only on race + helm filters.
  track <- reactive({
    df <- track_all()
    
    if (isTRUE(input$use_race_filter) &&
        !is.null(input$race_filter) &&
        length(input$race_filter) > 0) {
      df <- df %>% filter(race %in% input$race_filter)
    }
    
    if (!is.null(input$helm_filter)) {
      if (length(input$helm_filter) == 0) return(df[0, ])
      df <- df %>% filter(helm_ui %in% input$helm_filter)
    }
    
    df
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
    completed <- df_track %>%
      filter(!is.na(race), nzchar(race)) %>%
      group_by(race) %>%
      summarise(
        start_time = min(datetime_local, na.rm = TRUE),
        end_time   = max(datetime_local, na.rm = TRUE),
        race_start_date = as.Date(start_time),
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
      group_by(race) %>%
      summarise(
        start_time = min(start_time, na.rm = TRUE),
        end_time   = max(end_time,   na.rm = TRUE),
        race_start_date = as.Date(min(start_time, na.rm = TRUE)),
        # keep the first non-blank place (or NA if none)
        Place = dplyr::first(na.omit(Place)),
        cal_duration_hours = as.numeric(difftime(end_time, start_time, units = "hours")),
        .groups = "drop"
      )
    
    # Overlay completed stats onto calendar.
    # Calendar rows remain even when there's no track match.
    out <- cal_master %>%
      left_join(completed, by = "race", suffix = c("_cal", "")) %>%
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
  }

shinyApp(ui, server)