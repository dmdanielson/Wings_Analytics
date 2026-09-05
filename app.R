# app.R
# Wings Analytics — Sailing Performance Analytics Platform
#
# A single-file R Shiny application that transforms raw NMEA GPS/instrument data
# from a J112e sailboat ("Wings") into interactive dashboards with maps, speed
# analysis, polar performance benchmarking, and AI-generated race narratives.
#
# Architecture:
#   The app follows a two-phase pipeline design:
#   1. REBUILD PHASE (local only): Raw NMEA .txt files + Race Calendar.xlsx +
#      Polars.xlsx are parsed, synchronized, cleaned, and serialized into
#      track_data.rds. Race-level aggregate statistics (avg/max speed, max wind,
#      polar performance, NMEA point counts) are pre-computed and stored in the
#      race_calendar element of the RDS. This ensures the deployed app never
#      needs to perform expensive track-to-calendar joins at runtime.
#   2. RUNTIME PHASE: The app loads the pre-built track_data.rds and renders
#      interactive Shiny dashboards. Reactive expressions use bindCache() to
#      avoid redundant recomputation when users navigate between races/seasons.
#
# Deployment:
#   - LOCAL: if track_data.rds is missing, rebuilds from raw data files.
#   - DEPLOYED (shinyapps.io): rebuild disabled; track_data.rds must be
#     included in the deployment bundle alongside app.R.
#   - Deployment files: app.R + track_data.rds + race_narratives.rds + www/

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

# ---------- ENVIRONMENT DETECTION ----------
# Detect whether the app is running on a deployment server (shinyapps.io,
# Posit Connect, etc.) vs. a local development machine. Deployed environments
# set one or more of these environment variables. This flag controls whether
# rebuild_rds_from_raw() is allowed to run — on deployed instances, rebuilding
# from raw NMEA files is disabled because the raw data directory doesn't exist.
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

# ---------- RACE NARRATIVE GENERATOR ----------
# Generates deterministic, varied narratives for individual races.
# Accepts used_quotes to avoid repeating closing quotes across races.
# Returns list(narrative, quote_used) when called from build_all_narratives,
# or a plain string when called standalone.
generate_race_narrative <- function(race_row, completed_races, used_quotes = character()) {
  race_name <- race_row$race
  race_date <- format(race_row$race_date, "%B %d, %Y")
  has_data  <- !is.na(race_row$avg_sog) && !is.nan(race_row$avg_sog)

  # Deterministic picker: given a seed string + slot name, pick one item from a vector.
  # The slot name ensures different narrative slots don't all land on the same index.
  seed_hash <- sum(utf8ToInt(paste0(race_name, race_date)))
  pick_from <- function(opts, slot = "") {
    h <- seed_hash + sum(utf8ToInt(slot))
    opts[((h) %% length(opts)) + 1]
  }

  # Pick from a pool while avoiding already-used items. Falls back to
  # deterministic pick if all have been used.
  pick_unique <- function(opts, slot = "", used = character()) {
    available <- setdiff(opts, used)
    if (length(available) == 0) available <- opts
    h <- seed_hash + sum(utf8ToInt(slot))
    available[((h) %% length(available)) + 1]
  }

  # Maritime / sailing quotes pool (expanded to 40 for variety)
  maritime_quotes <- c(
    "\u201cTwenty years from now you will be more disappointed by the things you didn\u2019t do than by the ones you did. Sail away from the safe harbor.\u201d \u2014 Mark Twain",
    "\u201cThe pessimist complains about the wind; the optimist expects it to change; the realist adjusts the sails.\u201d \u2014 William Arthur Ward",
    "\u201cI can\u2019t control the wind, but I can adjust my sails.\u201d \u2014 Jimmy Dean",
    "\u201cThe sea, once it casts its spell, holds one in its net of wonder forever.\u201d \u2014 Jacques Cousteau",
    "\u201cA ship in harbor is safe, but that is not what ships are built for.\u201d \u2014 John A. Shedd",
    "\u201cThere is nothing more enticing, disenchanting, and enslaving than the life at sea.\u201d \u2014 Joseph Conrad",
    "\u201cThe cure for anything is salt water: sweat, tears, or the sea.\u201d \u2014 Isak Dinesen",
    "\u201cIt is not the ship so much as the skillful sailing that assures the prosperous voyage.\u201d \u2014 George William Curtis",
    "\u201cThe wind and the waves are always on the side of the ablest navigator.\u201d \u2014 Edmund Gibbon",
    "\u201cAny fool can carry on, but a wise man knows how to shorten sail in time.\u201d \u2014 Joseph Conrad",
    "\u201cTo reach a port we must set sail \u2014 sail, not tie at anchor \u2014 sail, not drift.\u201d \u2014 Franklin D. Roosevelt",
    "\u201cHe that would learn to pray, let him go to sea.\u201d \u2014 George Herbert",
    "\u201cWe must free ourselves of the hope that the sea will ever rest. We must learn to sail in high winds.\u201d \u2014 Aristotle Onassis",
    "\u201cLand was created to provide a place for boats to visit.\u201d \u2014 Brooks Atkinson",
    "\u201cIf one does not know to which port one is sailing, no wind is favorable.\u201d \u2014 Seneca",
    "\u201cI must go down to the seas again, to the lonely sea and the sky, and all I ask is a tall ship and a star to steer her by.\u201d \u2014 John Masefield",
    "\u201cOnly the guy who isn\u2019t rowing has time to rock the boat.\u201d \u2014 Jean-Paul Sartre",
    "\u201cSail away from the safe harbor. Catch the trade winds in your sails. Explore. Dream. Discover.\u201d \u2014 H. Jackson Brown Jr.",
    "\u201cThe ocean stirs the heart, inspires the imagination and brings eternal joy to the soul.\u201d \u2014 Wyland",
    "\u201cFor whatever we lose (like a you or a me), it\u2019s always ourselves we find in the sea.\u201d \u2014 E. E. Cummings",
    "\u201cA smooth sea never made a skilled sailor.\u201d \u2014 Franklin D. Roosevelt",
    "\u201cThe sea is everything. Its breath is pure and healthy. It is an immense desert, where man is never lonely.\u201d \u2014 Jules Verne",
    "\u201cIn one drop of water are found all the secrets of all the oceans.\u201d \u2014 Kahlil Gibran",
    "\u201cThe goal is not to sail the boat, but rather to help the boat sail herself.\u201d \u2014 John Rousmaniere",
    "\u201cNot all treasure is silver and gold, mate.\u201d \u2014 Captain Jack Sparrow",
    "\u201cYou can never cross the ocean until you have the courage to lose sight of the shore.\u201d \u2014 Christopher Columbus",
    "\u201cThe boat that won\u2019t be still is the boat that gets there.\u201d \u2014 Old sailing proverb",
    "\u201cThere are good ships and wood ships, ships that sail the sea, but the best ships are friendships, may they always be.\u201d \u2014 Irish proverb",
    "\u201cOut of sight of land the sailor feels safe. It is the beach that worries him.\u201d \u2014 Charles G. Davis",
    "\u201cThe days pass happily with me wherever my ship sails.\u201d \u2014 Joshua Slocum",
    "\u201cBeing in a ship is being in a jail, with the chance of being drowned.\u201d \u2014 Samuel Johnson",
    "\u201cRopes should not be used on something a knot cannot solve.\u201d \u2014 Old rigger\u2019s saying",
    "\u201cAt sea, I learned how little a person needs, not how much.\u201d \u2014 Robin Lee Graham",
    "\u201cIf a man must be obsessed by something, I suppose a boat is as good as anything, perhaps a bit better than most.\u201d \u2014 E. B. White",
    "\u201cThe sea hates a coward.\u201d \u2014 Eugene O\u2019Neill",
    "\u201cA sailor is an artist whose medium is the wind.\u201d \u2014 Webb Chiles",
    "\u201cThere\u2019s nothing \u2014 absolutely nothing \u2014 half so much worth doing as simply messing about in boats.\u201d \u2014 Kenneth Grahame",
    "\u201cFavorable winds don\u2019t come to those who wait. They come to those who hoist the sail.\u201d \u2014 Anonymous",
    "\u201cTo young men contemplating a voyage, I\u2019d say go.\u201d \u2014 Joshua Slocum",
    "\u201cBad weather makes good sailors, but lousy vacation photos.\u201d \u2014 Dock wisdom"
  )

  # ---- No track data ----
  if (!has_data) {
    no_data_opts <- c(
      paste0("No track data is available for ", race_name, " (",
             race_date, "). The GPS apparently took the day off \u2014 even electronics need a mental health day now and then."),
      paste0("Track data for ", race_name, " (", race_date,
             ") has gone the way of Amelia Earhart \u2014 vanished without a trace. The instruments were either napping or staging a quiet mutiny."),
      paste0("Alas, no track data exists for ", race_name, " (", race_date,
             "). The sailing happened, but the electrons that were supposed to record it apparently jumped ship."),
      paste0(race_name, " (", race_date,
             ") sailed off the grid entirely. The NMEA data must have mutinied somewhere around the start line. Perhaps the GPS didn\u2019t know where it was sailing either."),
      paste0("The logbook for ", race_name, " (", race_date,
             ") is conspicuously empty. Either the instruments staged a wildcat strike or the data fell overboard. The electronics clearly got too much salt water."),
      paste0(race_name, " on ", race_date,
             " left no digital footprint. The race happened \u2014 the GPS just wasn\u2019t invited. Perhaps the instruments decided to stay ashore and guard the dock."),
      paste0("No NMEA breadcrumbs for ", race_name, " (", race_date,
             "). The data gremlins ate this one. Somewhere, a hard drive is whistling innocently and refusing to make eye contact."),
      paste0(race_name, " (", race_date,
             ") exists only in the crew\u2019s collective memory, which, after a few post-race beverages, is about as reliable as the instruments were that day."),
      paste0("The digital trail for ", race_name, " (", race_date,
             ") is thinner than the breeze on a drifter day. The race was real; the data was aspirational."),
      paste0(race_name, " on ", race_date,
             " \u2014 proof that you can race a sailboat without leaving a single byte of evidence. The perfect crime, if the crime were sailing.")
    )
    narr <- pick_from(no_data_opts, "nodata")
    return(list(narrative = narr, quote_used = NA_character_))
  }

  n_completed <- nrow(completed_races)
  paragraphs  <- character()

  # ---- Paragraph 1: Overview, distance, placement ----
  p1_parts <- character()

  # Distance
  if (!is.na(race_row$length)) {
    len <- race_row$length
    all_len <- completed_races$length[!is.na(completed_races$length)]
    if (length(all_len) > 2) {
      pct <- mean(len >= all_len)
      short_opts <- c(
        "one of the shorter jaunts on the dance card \u2014 a quick tango with the bay",
        "a sprint by Wings\u2019 standards \u2014 barely enough time to finish the first thermos of coffee",
        "the nautical equivalent of a warm-up lap \u2014 short, sharp, and over before the sunscreen soaked in",
        "a compact course that rewarded quick thinking over brute endurance",
        "a quickie \u2014 blink and you\u2019d miss the windward mark",
        "short enough that the cooler stayed closed and the snacks stayed dry",
        "a brevity-is-the-soul-of-wit kind of course \u2014 Shakespeare would approve",
        "the type of race where the pre-race briefing takes longer than the actual sailing"
      )
      mid_low_opts <- c(
        "a mid-range cruise \u2014 long enough to settle in, short enough to stay hungry",
        "a course of modest ambition \u2014 not a day sail, not an odyssey, but somewhere in the agreeable middle",
        "enough distance to find a rhythm but not so much that the crew started rationing snacks",
        "a comfortable distance \u2014 the kind where you finish wanting just a little bit more",
        "long enough to justify the effort of rigging up, short enough to still make happy hour",
        "a course that fell squarely in the \u2018Goldilocks zone\u2019 of racing distances",
        "a solid medium-length run \u2014 the kind that doesn\u2019t make headlines but always delivers good racing",
        "the perfect length for a crew that brought exactly one sandwich and half a thermos"
      )
      mid_high_opts <- c(
        "a proper voyage that demanded endurance and more than a few granola bars",
        "a course with some real estate to it \u2014 the kind that separates the prepared from the optimistic",
        "a substantial outing that tested patience, provisions, and the second wind of every crew member",
        "long enough that the crew stopped asking \u201chow much farther?\u201d and started just sailing",
        "a legit distance \u2014 the sort of race where you earn your dinner twice",
        "enough course to make everyone on board question at least one life choice",
        "a race with genuine mileage \u2014 legs got sore, attention spans got tested, and the cooler got lighter",
        "the kind of distance where the conversation shifts from tactics to \u2018what\u2019s for dinner\u2019"
      )
      long_opts <- c(
        "one of the longest courses Wings has ever stared down \u2014 marathon territory",
        "an absolute beast of a course \u2014 the kind that earns you bragging rights at the dock",
        "the sort of distance that makes you question your life choices around mile two and feel heroic by the finish",
        "a proper expedition \u2014 if nautical miles were frequent flyer points, this one would earn an upgrade",
        "an endurance event disguised as a sailboat race \u2014 the crew basically commuted to work and back",
        "so long the crew considered applying for a change-of-address form mid-race",
        "a true test of stamina, snack planning, and the human bladder",
        "the kind of course that separates the sailors from the passengers and the optimists from the realists"
      )
      d <- if (pct < 0.25) pick_from(short_opts, "dist")
           else if (pct < 0.50) pick_from(mid_low_opts, "dist")
           else if (pct < 0.75) pick_from(mid_high_opts, "dist")
           else pick_from(long_opts, "dist")

      dist_frame <- pick_from(c(
        paste0("At ", len, " nautical miles, this was ", d, "."),
        paste0("The course measured ", len, " nm \u2014 ", d, "."),
        paste0(len, " nautical miles of racing ahead: ", d, "."),
        paste0("A ", len, "-mile course: ", d, "."),
        paste0(len, " nm from gun to finish \u2014 ", d, ".")
      ), "distframe")
      p1_parts <- c(p1_parts, dist_frame)
    } else {
      p1_parts <- c(p1_parts, pick_from(c(
        paste0("The course stretched ", len, " nautical miles across the bay."),
        paste0("At ", len, " nautical miles, the course was laid and the starting gun awaited."),
        paste0(len, " nautical miles from gun to finish \u2014 every one of them earned."),
        paste0("A ", len, "-mile track across Tampa Bay \u2014 distance enough to sort out the fleet."),
        paste0(len, " nautical miles of bay to cover, and every one of them counted.")
      ), "distearly"))
    }
  }

  # Duration
  if (!is.na(race_row$duration_hrs) && race_row$duration_hrs > 0) {
    hrs  <- floor(race_row$duration_hrs)
    mins <- round((race_row$duration_hrs - hrs) * 60)
    dur  <- if (hrs > 0) paste0(hrs, "h ", mins, "m") else paste0(mins, "m")
    dur_opts <- c(
      paste0("Wings battled the course for ", dur, " \u2014 every minute earned, none gifted."),
      paste0("From start to finish: ", dur, " of concentration, sail changes, and the occasional argument with the wind."),
      paste0("The clock ran for ", dur, " \u2014 a testament to persistence if nothing else."),
      paste0(dur, " on the water, which is exactly as long as it took for the crew to remember why they love this sport."),
      paste0("Total race time: ", dur, ". That\u2019s one way to skip dinner prep."),
      paste0("Wings was out there for ", dur, " \u2014 long enough for the crew to bond, bicker, and bond again."),
      paste0(dur, " of racing that felt like both five minutes and five hours, depending on when you checked the clock."),
      paste0("From gun to finish: ", dur, ". The crew earned every second of their post-race cold one.")
    )
    p1_parts <- c(p1_parts, pick_from(dur_opts, "duration"))
  }

  # Placement
  if (!is.na(race_row$place) && !is.na(race_row$fleet)) {
    place_num <- suppressWarnings(readr::parse_number(as.character(race_row$place)))
    fleet_n   <- as.integer(race_row$fleet)
    if (!is.na(place_num) && !is.na(fleet_n) && fleet_n > 0) {
      pct_place <- place_num / fleet_n

      first_opts <- c(
        paste0("Wings seized first place in a fleet of ", fleet_n,
               " \u2014 the navigator was clearly channeling Edmund Gibbon\u2019s \u201cablest navigator\u201d energy."),
        paste0("First across the line in a fleet of ", fleet_n,
               ". The crew left nothing on the table and the competition in their wake."),
        paste0("A bullet \u2014 first place out of ", fleet_n,
               " boats. The dock walk after this one came with a little extra swagger."),
        paste0("Wings claimed the top step in a ", fleet_n,
               "-boat fleet. The kind of result that makes the post-race beverage taste a little sweeter."),
        paste0("First of ", fleet_n,
               ". The crew nailed it \u2014 the kind of race where everything clicks and the competition just watches your transom."),
        paste0("A first-place finish in a fleet of ", fleet_n,
               " \u2014 the sailing equivalent of a mic drop, except you still have to fold the sails."),
        paste0("Wings took the gun in a field of ", fleet_n,
               ". If the ocean stirs the heart, this finish sent it into overdrive."),
        paste0("First place, fleet of ", fleet_n,
               ". The kind of result that\u2019s worth framing, or at least bringing up at every dinner party for the next six months.")
      )
      top_third_opts <- c(
        paste0("Crossing the line ", place_num, " out of ", fleet_n,
               " boats, Wings carved out a top-third finish. Not too shabby."),
        paste0("A ", place_num, "-of-", fleet_n,
               " finish \u2014 comfortably in the upper tier. The sailing was sharp today."),
        paste0("Finishing ", place_num, " in a fleet of ", fleet_n,
               ", Wings punched above average. The kind of result that earns a nod from the competition."),
        paste0(place_num, " out of ", fleet_n,
               " \u2014 a top-third finish. The sea cast its spell, and Wings answered."),
        paste0("A strong ", place_num, "-place finish in a ", fleet_n,
               "-boat fleet \u2014 proof that showing up prepared still counts for something."),
        paste0(place_num, " of ", fleet_n,
               ". Upper-third territory \u2014 where the air is cleaner and the tacking is crisper."),
        paste0("Wings slotted in at ", place_num, " out of ", fleet_n,
               " \u2014 the kind of result that deserves a respectable head nod, not quite a fist pump, but close."),
        paste0("Finishing ", place_num, " of ", fleet_n,
               ". A top-third result that keeps the crew\u2019s morale tank well above empty.")
      )
      top_half_opts <- c(
        paste0("A ", place_num, "-place finish in a fleet of ", fleet_n,
               " \u2014 solidly in the top half. Skillful sailing was on display today."),
        paste0("Finishing ", place_num, " of ", fleet_n,
               " boats. Not headline news, but a respectable showing \u2014 the margins were probably measured in seconds, not boat lengths."),
        paste0(place_num, " out of ", fleet_n,
               " \u2014 the upper half of the fleet, where the tactics are a little sharper. Room to improve, but nothing to apologize for."),
        paste0("A ", place_num, "-place finish in a ", fleet_n,
               "-boat field \u2014 today the adjustments landed Wings in the top half."),
        paste0(place_num, " of ", fleet_n,
               " \u2014 top half. Not the podium, not the back \u2014 the goldilocks zone of competitive sailing where you can see the leaders and smell their wake."),
        paste0("Wings landed at ", place_num, " in a field of ", fleet_n,
               ". Solidly above the median \u2014 like finishing a marathon faster than the guy who talked trash at the start."),
        paste0("A ", place_num, " of ", fleet_n,
               " result. The kind of finish that says \u2018we belong here\u2019 without needing to say it out loud."),
        paste0(place_num, " out of ", fleet_n,
               ". Top half of the fleet \u2014 where ambition meets execution, even if they don\u2019t always shake hands.")
      )
      lower_half_opts <- c(
        paste0("Placing ", place_num, " of ", fleet_n,
               " boats \u2014 not the finish the crew ordered. Lessons were learned, and they weren\u2019t cheap."),
        paste0("A ", place_num, "-of-", fleet_n,
               " result. Sometimes the bay wins. The crew will adjust for next time."),
        paste0(place_num, " out of ", fleet_n,
               " boats \u2014 the kind of finish that builds character and fuels quiet determination."),
        paste0("Finishing ", place_num, " in a fleet of ", fleet_n,
               ". Not the podium, but not the back of the pack either \u2014 small tactical calls made all the difference."),
        paste0(place_num, " of ", fleet_n,
               ". A humbling result, but as the old saying goes, the worst day sailing still beats the best day at the office. (The crew would like to stress-test that theory.)"),
        paste0("A ", place_num, "-place finish in a ", fleet_n,
               "-boat fleet. The scoreboard was unkind, but the racing was honest. Somewhere there\u2019s a lesson; the crew is still looking."),
        paste0(place_num, " of ", fleet_n,
               " \u2014 not the stuff of victory speeches, but the stuff of improvement plans, which are arguably more useful."),
        paste0("Finishing ", place_num, " of ", fleet_n,
               ". The kind of result that gets filed under \u2018we\u2019ll get \u2019em next time\u2019 \u2014 a folder that\u2019s getting respectably thick.")
      )
      bottom_opts <- c(
        paste0("At ", place_num, " of ", fleet_n,
               ", this was what diplomats call a \u2018character-building experience.\u2019 Wings certainly did show up, and that\u2019s worth something."),
        paste0(place_num, " of ", fleet_n,
               " \u2014 not the result anyone drew up on the whiteboard. Some classrooms are tougher than others."),
        paste0("A ", place_num, "-place finish out of ", fleet_n,
               ". Sometimes the scoreboard is unkind. Wings got some sweat, tears, and sea today."),
        paste0("Finishing near the back at ", place_num, " of ", fleet_n,
               ". The sort of day where you remind yourself that it\u2019s about the journey, then immediately wonder who made up that saying."),
        paste0(place_num, " of ", fleet_n,
               ". On the bright side, Wings provided an excellent view of everyone else\u2019s transoms \u2014 a learning experience in boat design, if nothing else."),
        paste0("A ", place_num, " of ", fleet_n,
               " finish. The only trophy available was the one for most creative excuses at the dock. Competition was fierce for that one too."),
        paste0(place_num, " out of ", fleet_n,
               " \u2014 a result that the crew will reference exclusively as \u2018a challenging day\u2019 in all future conversations."),
        paste0("Finishing ", place_num, " of ", fleet_n,
               ". If sailing were graded on enthusiasm rather than elapsed time, Wings would have been on the podium.")
      )

      ptxt <- if (place_num == 1) pick_from(first_opts, "place")
              else if (pct_place <= 0.33) pick_from(top_third_opts, "place")
              else if (pct_place <= 0.50) pick_from(top_half_opts, "place")
              else if (pct_place <= 0.75) pick_from(lower_half_opts, "place")
              else pick_from(bottom_opts, "place")
      p1_parts <- c(p1_parts, ptxt)
    } else if (is.na(place_num)) {
      nonnum_opts <- c(
        paste0("Wings finished with a ", race_row$place, " in a fleet of ", fleet_n,
               " \u2014 an unconventional result, like finding a message in a bottle that just says \u2018good luck.\u2019"),
        paste0("The scoreboard reads \u2018", race_row$place, "\u2019 in a fleet of ", fleet_n,
               " \u2014 not your typical number, but then Wings has never been your typical boat."),
        paste0("A result of \u2018", race_row$place, "\u2019 out of ", fleet_n,
               ". The race committee had their reasons. Wings had the sea."),
        paste0("Wings notched a \u2018", race_row$place, "\u2019 in a fleet of ", fleet_n,
               ". The scoreboard is being creative today, and honestly, so was the racing."),
        paste0("The official result: \u2018", race_row$place, "\u2019 out of ", fleet_n,
               ". Sometimes racing defies numerical classification. This was one of those times.")
      )
      p1_parts <- c(p1_parts, pick_from(nonnum_opts, "place"))
    }
  }

  if (length(p1_parts) > 0) {
    opener_opts <- c(
      paste0(race_name, " on ", race_date, ". "),
      paste0(race_name, ", ", race_date, ". "),
      paste0(race_date, " \u2014 ", race_name, ". "),
      paste0("Race day: ", race_name, ", ", race_date, ". "),
      paste0(race_date, ". ", race_name, ". ")
    )
    paragraphs <- c(paragraphs,
                    paste0(pick_from(opener_opts, "opener"),
                           paste(p1_parts, collapse = " ")))
  }

  # ---- Paragraph 2: Speed & polar performance ----
  sp <- character()

  if (!is.na(race_row$avg_sog) && !is.nan(race_row$avg_sog)) {
    avg_sog <- round(race_row$avg_sog, 1)
    all_sog <- completed_races$avg_sog[!is.na(completed_races$avg_sog) &
                                        !is.nan(completed_races$avg_sog)]
    sog_pct <- if (length(all_sog) > 2) mean(race_row$avg_sog >= all_sog) else 0.5

    slow_opts <- c(
      "on the leisurely end of the spectrum \u2014 the sort of pace where dolphins lap you",
      "a contemplative speed, as if the boat itself was deep in thought",
      "the kind of pace that tests your patience more than your sail trim",
      "slow enough that the crew had time to rethink every tactical decision twice",
      "the maritime equivalent of being stuck behind a school bus \u2014 but with nicer views",
      "a pace best described as \u2018meditative\u2019 by optimists and \u2018painful\u2019 by everyone else",
      "slow enough that a passing pelican gave the crew a sympathetic look",
      "more scenic cruise than race pace \u2014 the bay was beautiful, at least"
    )
    below_avg_opts <- c(
      "a touch below the fleet\u2019s historical average \u2014 not embarrassing, just... modest",
      "slightly below Wings\u2019 usual clip \u2014 like showing up to a party fashionably late, but with less champagne",
      "on the conservative side of Wings\u2019 speed ledger \u2014 the conditions were stingy",
      "a half-step behind the historical pace, as if the bay was charging a toll",
      "below average, but in the \u2018gave it an honest effort\u2019 sense rather than the \u2018forgot to untie the mooring\u2019 sense",
      "not Wings\u2019 fastest showing, but the boat was moving and that\u2019s more than the dock can say",
      "a tick below the career norm \u2014 the kind of day where the wind had other plans",
      "modestly below par \u2014 like shooting a 73 at the golf course, respectable but not Instagram-worthy"
    )
    mid_opts <- c(
      "right in the middle of Wings\u2019 historical range \u2014 steady as she goes",
      "squarely in the median zone \u2014 the Goldilocks speed: not too fast, not too slow",
      "par for the course in Wings\u2019 career average \u2014 dependable if not dramatic",
      "a thoroughly average pace by Wings\u2019 standards, which is not a criticism \u2014 average is hard-earned out here",
      "the statistical center of Wings\u2019 speed universe \u2014 the mean in every sense",
      "textbook average \u2014 the kind of number that makes a statistician shrug and say \u2018yep\u2019",
      "neither a highlight reel nor a blooper reel \u2014 more of a \u2018season recap\u2019 kind of speed",
      "reliably mid-pack speed \u2014 the Honda Civic of sailing performances"
    )
    fast_opts <- c(
      "faster than most of Wings\u2019 outings \u2014 the hull was humming",
      "well above the historical norm \u2014 Wings was in the groove and the water knew it",
      "a pace that put Wings in the upper echelon of her own race history",
      "the kind of speed that makes the crew grin and the competition nervous",
      "Wings was cooking today \u2014 the wake was impressive and the knot meter was earning its keep",
      "a pace that suggests the crew had their coffee dialed in and their tactics sharper than usual",
      "appreciably quick \u2014 the kind of speed that turns heads on the racecourse and prompts dock questions",
      "above-average velocity that left the crew feeling like they actually know what they\u2019re doing"
    )
    fastest_opts <- c(
      "among the fastest performances in the logbook \u2014 a day for the record books",
      "one for the record books \u2014 Wings was absolutely flying by her own standards",
      "blazing fast in historical context \u2014 the kind of speed that makes the hull sing",
      "a top-shelf performance that put most previous outings to shame",
      "near the top of Wings\u2019 all-time speed charts \u2014 the crew was clearly channeling something special",
      "the kind of speed that makes you wonder if someone secretly installed a turbocharger below deck",
      "Wings\u2019 inner speedboat was showing \u2014 the kind of pace that leaves salt spray and smiles in equal measure",
      "an elite performance by Wings\u2019 standards \u2014 the sort of day that gets a gold star in the logbook"
    )

    desc <- if (sog_pct < 0.20) pick_from(slow_opts, "sog")
            else if (sog_pct < 0.40) pick_from(below_avg_opts, "sog")
            else if (sog_pct < 0.60) pick_from(mid_opts, "sog")
            else if (sog_pct < 0.80) pick_from(fast_opts, "sog")
            else pick_from(fastest_opts, "sog")

    peak_opts <- if (!is.na(race_row$max_sog)) {
      pk <- round(race_row$max_sog, 1)
      pick_from(c(
        paste0(" with a peak of ", pk, " knots (hold onto your hats)"),
        paste0(", topping out at ", pk, " knots in a moment of pure velocity"),
        paste0(" and a max burst of ", pk, " knots that briefly rattled the coffee mugs"),
        paste0(", hitting ", pk, " knots at the high-water mark"),
        paste0(", with a top speed of ", pk, " knots that had the speedometer doing a double-take"),
        paste0(" and a maximum of ", pk, " knots \u2014 the kind of burst that makes you check the rigging"),
        paste0(", peaking at ", pk, " knots for one glorious, white-knuckle moment"),
        paste0(", touching ", pk, " knots when the stars aligned and the puffs cooperated")
      ), "peak")
    } else ""

    sog_frame <- pick_from(c(
      paste0("Average speed over ground was ", avg_sog, " knots", peak_opts, " \u2014 ", desc, "."),
      paste0("Wings averaged ", avg_sog, " knots SOG", peak_opts, " \u2014 ", desc, "."),
      paste0("The GPS logged an average of ", avg_sog, " knots", peak_opts, ". That\u2019s ", desc, "."),
      paste0("SOG clocked in at ", avg_sog, " knots on average", peak_opts, " \u2014 ", desc, "."),
      paste0(avg_sog, " knots average SOG", peak_opts, ". In other words: ", desc, ".")
    ), "sogframe")
    sp <- c(sp, sog_frame)
  }

  if (!is.na(race_row$avg_stw) && !is.nan(race_row$avg_stw) &&
      !is.na(race_row$avg_sog) && !is.nan(race_row$avg_sog)) {
    diff <- round(race_row$avg_sog - race_row$avg_stw, 2)
    if (abs(diff) > 0.15) {
      if (diff > 0) {
        pos_opts <- c(
          paste0("a friendly current chipping in about ", abs(diff), " knots \u2014 free speed, the best kind"),
          paste0("Mother Nature\u2019s subsidy: roughly ", abs(diff), " knots of current boost, no engine required"),
          paste0("a favorable tide lending ", abs(diff), " knots \u2014 the kind of gift you don\u2019t question"),
          paste0("a helpful push of about ", abs(diff), " knots from the current \u2014 sometimes the sea is generous"),
          paste0("roughly ", abs(diff), " knots of free current assist \u2014 the bay\u2019s version of a tailwind on the highway"),
          paste0("a ", abs(diff), "-knot current bonus \u2014 the tide was clearly a Wings fan today"),
          paste0("about ", abs(diff), " knots of current charity \u2014 the kind of boost that makes you look better than you are"),
          paste0("a ", abs(diff), "-knot gift from the tide gods, who apparently owed Wings a favor")
        )
        sp <- c(sp, paste0("The SOG-STW gap reveals ", pick_from(pos_opts, "current"), "."))
      } else {
        neg_opts <- c(
          paste0("an adversarial current dragging things back by about ", abs(diff), " knots \u2014 the sea giveth and the sea taketh away"),
          paste0("the tide playing defense, stealing roughly ", abs(diff), " knots of hard-won boat speed"),
          paste0("an unfriendly current taxing the boat about ", abs(diff), " knots \u2014 sailing\u2019s version of a headwind on the freeway"),
          paste0("a current penalty of ", abs(diff), " knots \u2014 the bay extracting its toll for the privilege of racing"),
          paste0("about ", abs(diff), " knots of current working against Wings \u2014 like running on a treadmill set to incline"),
          paste0("roughly ", abs(diff), " knots of tidal resistance \u2014 the ocean\u2019s way of saying \u2018not so fast, skipper\u2019"),
          paste0("a ", abs(diff), "-knot current tax \u2014 apparently the bay has a toll booth nobody told the crew about"),
          paste0("the current clawing back about ", abs(diff), " knots \u2014 every inch of progress earned twice over")
        )
        sp <- c(sp, paste0("The SOG-STW gap reveals ", pick_from(neg_opts, "current"), "."))
      }
    }
  }

  if (!is.na(race_row$polar_perf_stw) && !is.nan(race_row$polar_perf_stw)) {
    pp <- round(race_row$polar_perf_stw, 2)
    season_start_yr <- suppressWarnings(as.integer(substr(race_row$season, 1, 4)))
    is_recent <- !is.na(season_start_yr) && season_start_yr >= 2025

    ptxt <- if (pp > 0.3 && !is_recent) {
      pick_from(c(
        paste0("Polar performance registered at +", pp,
               " knots above target. Given that the instruments were still being dialed in during this period, ",
               "this likely reflects a speed sensor calibration issue rather than genuine over-performance. ",
               "Early-season instrument readings should be taken with a grain of sea salt."),
        paste0("The polars show +", pp,
               " knots above target, but the speed sensors during this era were more aspirational than accurate. ",
               "Think of it as the instruments telling the crew what they wanted to hear. ",
               "Calibration is a journey, not a destination."),
        paste0("+", pp,
               " knots over polar targets \u2014 impressive on paper, but the paddle wheel was still in its \u2018creative interpretation\u2019 phase. ",
               "The polars are trustworthy; the sensor data from this period, less so."),
        paste0("Polar performance of +", pp,
               " knots above target. The instruments were still in their \u2018honeymoon phase\u2019 \u2014 everything looked great on paper. ",
               "Reality and the speed sensor weren\u2019t properly introduced until a later season."),
        paste0("+", pp,
               " knots over target, but the speed transducer during this era had the accuracy of a horoscope. ",
               "Enthusiastic, occasionally correct, and best enjoyed with skepticism.")
      ), "polar")
    } else if (pp > 0 && !is_recent) {
      pick_from(c(
        paste0("Polar performance was +", pp,
               " knots above target. In the early days of Wings\u2019 instrumentation, ",
               "positive polar numbers often pointed to uncalibrated speed sensors rather than superhuman sailing."),
        paste0("+", pp,
               " knots above polar target. Before the instruments were properly calibrated, ",
               "these readings were more decorative than diagnostic. Trust the trend, not the absolute number."),
        paste0("Polar performance of +", pp,
               " knots. In this pre-calibration era, the speed sensor had a tendency to flatter. ",
               "Take it with the same grain of sea salt you\u2019d apply to a fish story about \u2018the one that got away.\u2019"),
        paste0("+", pp,
               " knots above polars \u2014 but the speed sensor was still telling white lies at this point. ",
               "The boat was good; the data was fiction."),
        paste0("Polar performance of +", pp,
               " knots. The uncalibrated speed sensor from this era was basically a hype man \u2014 supportive, but unreliable.")
      ), "polar")
    } else if (pp > 0.3 && is_recent) {
      pick_from(c(
        paste0("Polar performance clocked in at +", pp,
               " knots above target \u2014 with properly calibrated instruments, this is a genuinely impressive result. ",
               "Wings was sailing faster than the polars predicted, and the crew deserves the credit."),
        paste0("+", pp,
               " knots above polar targets. With the instruments now dialed in, this is the real deal \u2014 ",
               "Wings was outrunning her own design specs. Somewhere, a naval architect is nodding approvingly."),
        paste0("Polar performance hit +", pp,
               " knots over target. Now that the sensors are trustworthy, numbers like these tell a genuine story: ",
               "the crew found speed the designers didn\u2019t promise."),
        paste0("+", pp,
               " knots above target with calibrated instruments \u2014 this isn\u2019t sensor noise, this is talent. ",
               "Wings was genuinely outperforming her theoretical ceiling."),
        paste0("Polar performance of +", pp,
               " knots. With the sensors finally telling the truth, this number is earned, not inherited. ",
               "The J112e designers said \u2018this fast\u2019; the crew said \u2018hold my beverage.\u2019")
      ), "polar")
    } else if (pp > 0 && is_recent) {
      pick_from(c(
        paste0("Polar performance was +", pp,
               " knots above target \u2014 a solid result now that the instruments are well-calibrated. ",
               "The crew squeezed out a little extra from the boat."),
        paste0("+", pp,
               " knots above polar targets \u2014 not earth-shattering, but a clean positive number with trustworthy instruments is always a good sign."),
        paste0("Polar performance of +", pp,
               " knots. With calibrated sensors, this modest over-performance is honest speed \u2014 earned by the crew, confirmed by the data."),
        paste0("+", pp,
               " knots above target. A small but legitimate edge over the polars \u2014 like finding a dollar in your jacket pocket. Not life-changing, but satisfying."),
        paste0("Polar performance: +", pp,
               " knots. The crew eked out more than the polars promised, which with calibrated instruments means they genuinely found a little extra.")
      ), "polar")
    } else if (pp > -0.3) {
      pick_from(c(
        paste0("Polar performance of ", pp,
               " knots \u2014 just a whisker below target. Close enough that the polars aren\u2019t losing sleep over it."),
        paste0("At ", pp,
               " knots relative to polars, Wings was kissing distance from target. A boat-length here, a puff there, and this number flips positive."),
        paste0("Polar performance of ", pp,
               " knots \u2014 essentially on the money. The margins at this level are measured in tenths, and tenths are measured in luck."),
        paste0(pp, " knots off polar target \u2014 close enough to call it a rounding error if you squint. The polars and the reality were in the same zip code."),
        paste0("Polar performance: ", pp,
               " knots. Functionally on target \u2014 the difference between this and \u2018nailed it\u2019 is about one well-timed puff.")
      ), "polar")
    } else if (pp > -0.7) {
      pick_from(c(
        paste0("At ", pp,
               " knots below polar targets, the boat had more in the tank. The conditions (or perhaps the crew\u2019s pre-race lunch choices) left some speed on the table."),
        paste0("Polar performance of ", pp,
               " knots below target \u2014 not disastrous, but the polars are gently clearing their throat. There was speed to be found."),
        paste0(pp, " knots under polar targets. The boat was capable of more, but the day had other ideas. ",
               "Sometimes the best-laid tactics meet a current that didn\u2019t read the playbook."),
        paste0(pp, " knots below target. The polars are quietly disappointed, like a parent watching you parallel park. There\u2019s room for improvement, and they both know it."),
        paste0("Polar performance of ", pp,
               " knots. Not awful, but the boat\u2019s theoretical performance is side-eyeing the crew right now.")
      ), "polar")
    } else {
      pick_from(c(
        paste0("Polar performance of ", pp,
               " knots below target \u2014 rough day at the office. Some days the curriculum is harder than others."),
        paste0("At ", pp,
               " knots below polar targets, this was a humbling outing. The boat\u2019s potential went largely unrealized \u2014 ",
               "like owning a sports car and getting stuck in traffic."),
        paste0(pp, " knots under target \u2014 a significant gap between what the polars promised and what the day delivered. But Wings showed up, which is always the first step."),
        paste0("Polar performance: ", pp,
               " knots. The polars and the actual performance had a disagreement today, and the polars won by a comfortable margin."),
        paste0(pp, " knots below target. The boat was designed to go faster than this; the ocean had a different syllabus. ",
               "Everyone got an education, though not the one they signed up for.")
      ), "polar")
    }
    sp <- c(sp, ptxt)
  }

  if (length(sp) > 0) paragraphs <- c(paragraphs, paste(sp, collapse = " "))

  # ---- Paragraph 3: Wind conditions ----
  wp <- character()

  if (!is.na(race_row$avg_tws) && !is.nan(race_row$avg_tws)) {
    drifter_opts <- c(
      "a drifter \u2014 the kind of day where you can hear the barnacles growing on the hull",
      "barely a whisper of wind \u2014 the sails hung like laundry and the crew practiced their patience",
      "a parking lot \u2014 the kind of conditions where the best strategy is to bring a good book",
      "glass-calm misery disguised as a race \u2014 the wind gods were clearly on vacation",
      "so light the telltales gave up and just dangled there, judging everyone",
      "the kind of breeze you\u2019d struggle to blow out a birthday candle with",
      "dead calm with occasional hopes \u2014 the meteorological equivalent of a rain dance that didn\u2019t work",
      "a day where the most productive crew member was the one who remembered to bring cards"
    )
    light_opts <- c(
      "light air that tested the crew\u2019s patience like a DMV waiting room \u2014 only with better scenery",
      "a zephyr at best \u2014 the kind of breeze that rewards finesse over horsepower",
      "gossamer conditions that demanded featherweight touch on the helm and the patience of a monk",
      "the definition of a \u2018tactician\u2019s day\u2019 \u2014 every puff was a decision and every lull a test",
      "light enough that boat handling mattered more than boat speed \u2014 finesse over firepower",
      "the kind of breeze where weight placement is a competitive advantage and the lightest crew member becomes MVP",
      "gentle conditions that separated the patient from the impatient faster than you\u2019d think",
      "a whispering breeze that rewarded the attentive and punished the distracted"
    )
    working_opts <- c(
      "a solid working breeze \u2014 the Goldilocks zone of racing conditions",
      "textbook racing weather \u2014 enough wind to move, not so much that things get exciting in the wrong way",
      "the sweet spot \u2014 steady breeze, honest sailing, and the kind of conditions that make you glad you own a boat",
      "pleasant and purposeful wind \u2014 the kind of day that reminds you why you took up sailing in the first place",
      "ideal conditions \u2014 the wind was cooperating, the sails were drawing, and nobody was complaining (a first)",
      "a textbook breeze that made everyone on board look competent, even the ones who were faking it",
      "the perfect working breeze \u2014 steady, reliable, and drama-free, which is more than you can say for most crew dynamics",
      "comfortable racing conditions \u2014 the sort of day where sailing looks easy from shore and feels earned from the cockpit"
    )
    fresh_opts <- c(
      "a healthy blow that kept everyone earning their rum rations",
      "a stiff breeze that put the boat on its ear and the crew on their toes",
      "breezy enough to warrant a second look at the reef points \u2014 the wind meant business",
      "the kind of conditions where the rail meat earns their keep and the foredeck crew earns hazard pay",
      "enough wind to make the boat feel alive and the crew feel necessary \u2014 the sweet spot of controlled chaos",
      "a proper breeze that separated the confident from the cautious, with Wings choosing the former",
      "the kind of wind where the spray hits your face and your smile gets wider",
      "brisk conditions that made the instruments dance and the crew pay attention"
    )
    heavy_opts <- c(
      "heavy air that separated the bold from the seasick",
      "a proper blow \u2014 the kind of wind that rearranges the cockpit and tests every piece of hardware on the boat",
      "serious breeze that demanded respect, solid seamanship, and a willingness to get very wet",
      "enough wind to make even experienced sailors double-check the rigging \u2014 Mother Nature was not messing around",
      "full-send conditions \u2014 the kind of day where the winch handles get a workout and the crew gets a story",
      "howling breeze that tested the boat, the gear, and the crew\u2019s commitment to the hobby",
      "big wind that turned sailing from a sport into a survival exercise with trophies",
      "the kind of conditions where you stop racing the other boats and start racing your own adrenaline"
    )

    wd <- if (race_row$avg_tws < 5) pick_from(drifter_opts, "wind")
          else if (race_row$avg_tws < 8) pick_from(light_opts, "wind")
          else if (race_row$avg_tws < 12) pick_from(working_opts, "wind")
          else if (race_row$avg_tws < 18) pick_from(fresh_opts, "wind")
          else pick_from(heavy_opts, "wind")

    gust <- if (!is.na(race_row$max_tws) && !is.nan(race_row$max_tws))
              pick_from(c(
                paste0(" with gusts to ", round(race_row$max_tws, 1), " knots"),
                paste0(" and puffs hitting ", round(race_row$max_tws, 1), " knots"),
                paste0(", gusting to ", round(race_row$max_tws, 1)),
                paste0(" with the occasional ", round(race_row$max_tws, 1), "-knot reality check"),
                paste0(", spiking to ", round(race_row$max_tws, 1), " in the puffs")
              ), "gust")
            else ""

    wind_frame <- pick_from(c(
      paste0("Wind averaged ", round(race_row$avg_tws, 1), " knots", gust, " \u2014 ", wd, "."),
      paste0("The breeze clocked in at ", round(race_row$avg_tws, 1), " knots average", gust, " \u2014 ", wd, "."),
      paste0("Conditions served up ", round(race_row$avg_tws, 1), " knots of wind on average", gust, ". In other words: ", wd, "."),
      paste0("Mean wind: ", round(race_row$avg_tws, 1), " knots", gust, ". Translation: ", wd, "."),
      paste0("The wind meter read ", round(race_row$avg_tws, 1), " knots average", gust, " \u2014 ", wd, ".")
    ), "windframe")
    wp <- c(wp, wind_frame)

    all_tws <- completed_races$avg_tws[!is.na(completed_races$avg_tws) &
                                        !is.nan(completed_races$avg_tws)]
    if (length(all_tws) > 2) {
      tws_pct <- mean(race_row$avg_tws >= all_tws)
      calm_comp <- c(
        "Relative to the fleet\u2019s history, this was one of the calmer days \u2014 sail trim and boat handling were king.",
        "Historically speaking, this ranked among the lighter-air outings \u2014 the kind of day where small gains compound.",
        "By Wings\u2019 historical standards, this was a mellow affair \u2014 finesse over force.",
        "In the context of Wings\u2019 racing career, this was a whisper \u2014 the kind of day that rewards the patient and punishes the heavy-footed.",
        "Among the calmer outings in the dataset \u2014 a day where the crew\u2019s touch mattered more than the crew\u2019s weight."
      )
      windy_comp <- c(
        "This was one of the windier races in the dataset \u2014 a day where wisdom meant knowing when to shorten sail.",
        "Historically, this ranks among the breezier outings \u2014 a day where the boat\u2019s limits and the crew\u2019s nerve were both tested.",
        "By the numbers, this was more wind than Wings usually sees \u2014 the sort of day that produces war stories and sail repair bills.",
        "One of the spicier wind days on record for Wings \u2014 the kind that gets talked about at the bar but not always enjoyed in the moment.",
        "Among the windiest outings in Wings\u2019 logbook \u2014 a day where preparation paid dividends and the faint of heart stayed home."
      )
      comp <- if (tws_pct < 0.25) pick_from(calm_comp, "windcomp")
              else if (tws_pct > 0.75) pick_from(windy_comp, "windcomp")
              else ""
      if (nzchar(comp)) wp <- c(wp, comp)
    }
  }

  if (!is.na(race_row$headsail) && nzchar(race_row$headsail)) {
    sail_opts <- c(
      paste0("The crew flew the ", race_row$headsail, " \u2014 chosen with the confidence of someone who checks the forecast twice."),
      paste0("Up front: the ", race_row$headsail, ". A deliberate choice that said everything about what the crew expected from the sky."),
      paste0("The ", race_row$headsail, " got the call \u2014 the right tool for the day\u2019s conditions, or at least the crew\u2019s best guess at them."),
      paste0("Headsail selection: ", race_row$headsail, ". In sailing, as in life, half the battle is showing up with the right gear."),
      paste0("The ", race_row$headsail, " went up the headstay \u2014 a sail choice made with cautious optimism and confirmed by the first beat."),
      paste0("Sail plan featured the ", race_row$headsail, " \u2014 because picking the right headsail is 10% science and 90% staring at the clouds while sipping coffee."),
      paste0("The ", race_row$headsail, " was the weapon of choice today \u2014 a decision the crew stood by, for better or worse."),
      paste0("Flying the ", race_row$headsail, ". The forecast said one thing, the crew believed another, and the sail bag was already open so here we are.")
    )
    wp <- c(wp, pick_from(sail_opts, "headsail"))
  }

  if (!is.na(race_row$helm) && nzchar(race_row$helm)) {
    helm_opts <- c(
      paste0(race_row$helm, " had the helm and the final say on which way the bow pointed."),
      paste0("At the wheel: ", race_row$helm, " \u2014 steering with conviction and hopefully a compass."),
      paste0(race_row$helm, " drove \u2014 every tack, every gybe, every lane change negotiated from behind the wheel."),
      paste0("The helm belonged to ", race_row$helm, " today, who guided Wings through whatever the bay threw their way."),
      paste0(race_row$helm, " was calling the shots from the helm \u2014 part driver, part strategist, part weather guesser."),
      paste0("Behind the wheel: ", race_row$helm, ". The person most likely to be blamed or credited, depending on the result."),
      paste0(race_row$helm, " steered Wings with the quiet confidence of someone who has already apologized for the last bad tack."),
      paste0("Helm duties fell to ", race_row$helm, " \u2014 a role that combines authority, responsibility, and an excellent excuse for not trimming sails.")
    )
    wp <- c(wp, pick_from(helm_opts, "helm"))
  }

  if (length(wp) > 0) paragraphs <- c(paragraphs, paste(wp, collapse = " "))

  # ---- Closing quote (deduplicated) ----
  quote <- pick_unique(maritime_quotes, "closingquote", used_quotes)
  paragraphs <- c(paragraphs, quote)

  list(narrative = paste(paragraphs, collapse = "\n\n"), quote_used = quote)
}

# ---------- SEASON NARRATIVE GENERATOR ----------
generate_season_narrative <- function(season_name, season_cal, track_data) {
  if (nrow(season_cal) == 0) return(paste0("No races found for the ", season_name, " season. The harbor was apparently too comfortable."))

  seed_hash <- sum(utf8ToInt(season_name))
  pick_from <- function(opts, slot = "") {
    h <- seed_hash + sum(utf8ToInt(slot))
    opts[((h) %% length(opts)) + 1]
  }

  n_races <- season_cal |>
    mutate(race_date = as.Date(start)) |>
    distinct(race, race_date) |>
    nrow()

  paragraphs <- character()

  # ---- Paragraph 1: Overview ----
  date_range <- paste0(
    format(min(season_cal$start, na.rm = TRUE), "%B %Y"),
    " to ",
    format(max(season_cal$start, na.rm = TRUE), "%B %Y")
  )
  series_list <- unique(season_cal$series[!is.na(season_cal$series) & nzchar(season_cal$series)])
  series_txt <- if (length(series_list) > 0)
    paste0(" across ", length(series_list), " series (", paste(series_list, collapse = ", "), ")")
  else ""

  total_nm <- sum(season_cal$length, na.rm = TRUE)
  nm_txt <- if (total_nm > 0)
    paste0(", logging ", round(total_nm, 1), " nautical miles in the process")
  else ""

  opener_opts <- c(
    paste0("The ", season_name, " season ran from ", date_range,
           " and featured ", n_races, " races", series_txt, nm_txt,
           ". Wings was thoroughly committed to the calendar \u2014 and the calendar was thoroughly committed to testing Wings."),
    paste0("From ", date_range, ", the ", season_name, " season delivered ", n_races, " races",
           series_txt, nm_txt,
           ". A full dance card and a busy crew \u2014 exactly how a racing season should look."),
    paste0(season_name, ": ", n_races, " races", series_txt, ", spanning ", date_range, nm_txt,
           ". Another season of salt, spray, and the occasional existential question about why we do this."),
    paste0("The ", season_name, " season stretched from ", date_range,
           " with ", n_races, " races on the books", series_txt, nm_txt,
           ". Wings showed up for every one of them, which is either dedication or stubbornness. Possibly both."),
    paste0("Season ", season_name, " covered ", date_range,
           " and packed in ", n_races, " races", series_txt, nm_txt,
           ". The crew accumulated experience, nautical miles, and a healthy collection of dock stories.")
  )
  paragraphs <- c(paragraphs, pick_from(opener_opts, "seasonopener"))

  # ---- Paragraph 2: Placement summary ----
  place_num <- suppressWarnings(as.numeric(season_cal$place))
  fleet_num <- suppressWarnings(as.numeric(season_cal$fleet))
  valid_place <- !is.na(place_num) & !is.na(fleet_num) & fleet_num > 0

  if (sum(valid_place) > 0) {
    avg_place <- round(mean(place_num[valid_place]), 1)
    avg_fleet <- round(mean(fleet_num[valid_place]), 1)
    wins <- sum(place_num[valid_place] == 1)
    top_half <- sum(place_num[valid_place] <= fleet_num[valid_place] / 2)

    p_parts <- paste0("Across ", sum(valid_place), " scored races, Wings averaged ",
                      avg_place, " place in an average fleet of ", avg_fleet, " boats.")

    if (wins > 0) {
      win_opts <- c(
        paste0(" Wings hoisted the victory flag ", wins, ifelse(wins == 1, " time", " times"),
               " \u2014 proof that persistence and preparation occasionally converge."),
        paste0(" ", wins, " first-place finish", ifelse(wins > 1, "es", ""),
               " this season \u2014 the kind of result that makes the early-morning rigging worth it."),
        paste0(" The crew brought home ", wins, " bullet", ifelse(wins > 1, "s", ""),
               ". Not bad for a boat that sometimes argues with its own instruments."),
        paste0(" ", wins, " win", ifelse(wins > 1, "s", ""),
               " on the season \u2014 each one earned, none gifted, all celebrated at the dock.")
      )
      p_parts <- paste0(p_parts, pick_from(win_opts, "wins"))
    }

    top_pct <- round(100 * top_half / sum(valid_place))
    humor_opts <- if (top_pct >= 75) c(
      "Consistency like that doesn\u2019t happen by accident \u2014 or does it?",
      "That kind of reliability is rare in sailing, relationships, and weather forecasts.",
      "A top-half rate that high suggests the crew knows what they\u2019re doing. Probably.",
      "At that rate, Wings is the reliable friend who always shows up on time and brings good snacks."
    ) else if (top_pct >= 50) c(
      "More hits than misses \u2014 the kind of season that keeps the crew coming back.",
      "A winning record, technically speaking. The trophy case may not be overflowing, but the trend line is friendly.",
      "Above .500 in top-half finishes \u2014 in baseball terms, that\u2019s a playoff contender. In sailing terms, that\u2019s a well-used cooler.",
      "Not dominant, but not dominated. The sweet spot of competitive sailing."
    ) else c(
      "A season of lessons. Every expert was once a beginner, and Wings is investing in the education.",
      "The results don\u2019t always match the effort, but the crew\u2019s commitment to showing up is unimpeachable.",
      "More bottom-half finishes than top-half, but each one came with a lesson the crew didn\u2019t have to pay tuition for.",
      "A tough season on the scoreboard. The crew\u2019s resilience account, however, is running a surplus."
    )
    p_parts <- paste0(p_parts, " Wings finished in the top half in ", top_half,
                      " out of ", sum(valid_place), " races (", top_pct, "%). ",
                      pick_from(humor_opts, "seasonhumor"))

    paragraphs <- c(paragraphs, p_parts)
  }

  # ---- Paragraph 3: Track data performance ----
  season_track <- track_data |>
    filter(!is.na(race), nzchar(race))

  if (nrow(season_track) > 0) {
    matched_track <- tibble()
    for (i in seq_len(nrow(season_cal))) {
      tr <- season_track |>
        filter(race == season_cal$race[i],
               datetime_local >= season_cal$start[i],
               datetime_local <= season_cal$end[i])
      matched_track <- bind_rows(matched_track, tr)
    }

    if (nrow(matched_track) > 0) {
      sp <- character()
      avg_sog <- mean(matched_track$sog_knots, na.rm = TRUE)
      avg_stw <- mean(matched_track$stw_knots, na.rm = TRUE)

      if (!is.nan(avg_sog)) {
        sog_opts <- c(
          paste0("Season average SOG was ", round(avg_sog, 1), " knots \u2014 the cruising speed of a boat with places to be."),
          paste0("Wings averaged ", round(avg_sog, 1), " knots SOG across the season \u2014 not breaking records, but definitely breaking a sweat."),
          paste0("The season\u2019s average SOG came in at ", round(avg_sog, 1), " knots. Respectable, repeatable, and a solid baseline to build on."),
          paste0("Across all tracked races this season, Wings maintained ", round(avg_sog, 1), " knots average SOG \u2014 steady as the crew\u2019s coffee consumption.")
        )
        sp <- c(sp, pick_from(sog_opts, "seasonsog"))
      }

      if (!is.nan(avg_stw) && !is.nan(avg_sog)) {
        diff <- round(avg_sog - avg_stw, 2)
        if (abs(diff) > 0.1) {
          current_opts <- if (diff > 0) c(
            paste0("A season-long SOG-STW differential of +", diff, " knots suggests the currents were generally in Wings\u2019 corner \u2014 free speed, graciously accepted."),
            paste0("The tides chipped in about +", diff, " knots on average across the season \u2014 the bay\u2019s way of saying \u2018you\u2019re welcome.\u2019"),
            paste0("Net current contribution: +", diff, " knots. The tide was more teammate than obstacle this season."),
            paste0("+", diff, " knots of current assist on average \u2014 sometimes the universe cooperates, and this season it mostly did.")
          ) else c(
            paste0("A SOG-STW gap of ", diff, " knots hints at currents that were, on balance, not exactly rooting for Wings."),
            paste0("The tides clawed back about ", abs(diff), " knots on average \u2014 the bay charging rent for the privilege of racing."),
            paste0("Net current: ", diff, " knots against. The tide was playing for the other team this season."),
            paste0(diff, " knots of current headwind across the season. The crew worked harder than the numbers suggest.")
          )
          sp <- c(sp, pick_from(current_opts, "seasoncurrent"))
        }
      }

      avg_tws <- mean(matched_track$tws_knots, na.rm = TRUE)
      if (!is.nan(avg_tws)) {
        wind_opts <- if (avg_tws < 6) c(
          "Light enough to make a Laser sailor weep.",
          "A light-air season that tested patience more than hardware.",
          "Breeze was at a premium this season \u2014 the wind gods were being stingy."
        ) else if (avg_tws < 10) c(
          "Enough breeze to keep things interesting without requiring heroics.",
          "A moderate-wind season \u2014 the kind that rewards good tactics over brute force.",
          "Pleasant racing conditions, on average. Nobody\u2019s complaining about the breeze."
        ) else if (avg_tws < 15) c(
          "Solid, reliable wind \u2014 the kind you\u2019d write home about.",
          "A well-winded season that gave the crew plenty to work with.",
          "Good, honest breeze all season long. The sails earned their keep."
        ) else c(
          "Plenty of wind \u2014 reef points were not decorative this season.",
          "A breezy season that kept the crew honest and the rigging under review.",
          "Big wind all season. The crew\u2019s dry-suit budget went up accordingly."
        )
        sp <- c(sp, paste0("Average wind speed across the season was ",
                           round(avg_tws, 1), " knots. ", pick_from(wind_opts, "seasonwind")))
      }

      avg_polar_stw <- mean(matched_track$Polar_Perf_STW, na.rm = TRUE)
      if (!is.nan(avg_polar_stw)) {
        season_start_yr <- suppressWarnings(as.integer(substr(season_name, 1, 4)))
        is_recent <- !is.na(season_start_yr) && season_start_yr >= 2025

        pp_opts <- if (avg_polar_stw > 0.2 && !is_recent) c(
          paste0("At +", round(avg_polar_stw, 2),
                 " knots above polar targets on average \u2014 however, instrument calibration during this earlier season was still being refined. ",
                 "Positive polar numbers from this period likely reflect speed sensor inaccuracies rather than genuine over-performance."),
          paste0("Polar performance averaged +", round(avg_polar_stw, 2),
                 " knots above target, but the speed sensor during this era was still telling tales. ",
                 "The polars are right; the paddle wheel was being generous.")
        ) else if (avg_polar_stw > 0 && !is_recent) c(
          paste0("At +", round(avg_polar_stw, 2),
                 " knots above polar targets \u2014 though in this earlier season the speed instruments were not yet properly calibrated, ",
                 "so the true performance was likely closer to (or below) target."),
          paste0("Polar performance: +", round(avg_polar_stw, 2),
                 " knots. Flattering, but the instruments from this era were essentially writing fan fiction about boat speed.")
        ) else if (avg_polar_stw > 0.2 && is_recent) c(
          paste0("At +", round(avg_polar_stw, 2),
                 " knots above polar targets on average, Wings was genuinely outperforming her design specs this season. ",
                 "With properly calibrated instruments, this is a result the crew can take real pride in."),
          paste0("Polar performance of +", round(avg_polar_stw, 2),
                 " knots with calibrated instruments \u2014 the crew was extracting speed the designers didn\u2019t advertise. Impressive stuff.")
        ) else if (avg_polar_stw > 0 && is_recent) c(
          paste0("At +", round(avg_polar_stw, 2),
                 " knots above polar targets with well-calibrated instruments, Wings was edging past her theoretical ceiling. ",
                 "Every fraction of a knot earned the hard way."),
          paste0("Polar performance: +", round(avg_polar_stw, 2),
                 " knots above target. With trustworthy sensors, this modest over-performance is the real deal \u2014 genuine speed, honestly measured.")
        ) else if (avg_polar_stw > -0.3) c(
          paste0("At ", round(avg_polar_stw, 2),
                 " knots relative to polar targets, Wings was sailing close to her design envelope \u2014 minor tuning could close the gap."),
          paste0("Polar performance of ", round(avg_polar_stw, 2),
                 " knots. Essentially on target \u2014 the gap between actual and theoretical is small enough to blame on weather rather than technique.")
        ) else c(
          paste0("At ", round(avg_polar_stw, 2),
                 " knots below polar targets, there\u2019s room to coax more speed from the hull. The potential is there; the execution is a work in progress."),
          paste0("Polar performance: ", round(avg_polar_stw, 2),
                 " knots below target. The boat knows how to go faster \u2014 the crew is still negotiating the terms.")
        )
        sp <- c(sp, pick_from(pp_opts, "seasonpolar"))
      }

      if (length(sp) > 0) paragraphs <- c(paragraphs, paste(sp, collapse = " "))
    }
  }

  # ---- Closing (varied, no hardcoded quote) ----
  closing_opts <- c(
    paste0("Another season in the books for Wings. The bay keeps teaching, and the crew keeps showing up for class."),
    paste0("Season ", season_name, " is one for the logbook. The miles are in the keel, the lessons are in the crew, and the stories are at the bar."),
    paste0("That\u2019s a wrap on ", season_name, ". The boat is faster, the crew is wiser, and the cooler is empty. All signs of a season well spent."),
    paste0("The ", season_name, " chapter closes with more experience, more data, and the same unshakeable belief that next season will be even better."),
    paste0("Another season, another stack of NMEA files. Wings\u2019 story is still being written, one race at a time."),
    paste0("Season ", season_name, " \u2014 proof that the best way to get better at sailing is to do more of it. And possibly to read the weather forecast more carefully."),
    paste0("The ", season_name, " season reminded everyone aboard Wings that racing is a process, not a destination. The process, so far, involves a lot of sunscreen."),
    paste0("With ", season_name, " in the rearview, Wings looks ahead. The data is building, the skills are sharpening, and the snack planning is improving \u2014 priorities in that order.")
  )
  paragraphs <- c(paragraphs, pick_from(closing_opts, "seasonclosing"))

  paste(paragraphs, collapse = "\n\n")
}

# ---------- OVERALL PERFORMANCE NARRATIVE GENERATOR ----------
generate_performance_narrative <- function(race_calendar, track_data) {
  if (nrow(race_calendar) == 0) return("No race data available to generate a performance report.")

  paragraphs <- character()
  seasons <- sort(unique(race_calendar$season[!is.na(race_calendar$season)]))
  n_seasons <- length(seasons)
  n_total_races <- race_calendar |>
    mutate(race_date = as.Date(start)) |>
    distinct(race, race_date) |>
    nrow()
  total_nm <- sum(race_calendar$length, na.rm = TRUE)

  # ---- Paragraph 1: The grand overview ----
  first_race <- min(race_calendar$start, na.rm = TRUE)
  last_race  <- max(race_calendar$start, na.rm = TRUE)
  span_months <- round(as.numeric(difftime(last_race, first_race, units = "days")) / 30.44)

  overview_opts <- c(
    paste0("Across ", n_seasons, " seasons and ", n_total_races, " races spanning roughly ",
           span_months, " months, Wings has covered ", round(total_nm, 1),
           " nautical miles of competitive racing from ",
           format(first_race, "%B %Y"), " through ", format(last_race, "%B %Y"),
           ". These seasons are about building experience \u2014 learning the boat, learning the crew, and learning the water. ",
           "The foundation is being laid; the performance chapter is being written one race at a time."),
    paste0("Wings\u2019 racing logbook now spans ", n_seasons, " seasons, ", n_total_races, " races, and ",
           round(total_nm, 1), " nautical miles \u2014 a ", span_months,
           "-month journey from ", format(first_race, "%B %Y"), " to ", format(last_race, "%B %Y"),
           ". The data tells a story of a crew investing in experience before chasing silverware. Every race adds to the foundation."),
    paste0("From ", format(first_race, "%B %Y"), " to ", format(last_race, "%B %Y"),
           ", Wings has raced ", n_total_races, " times across ", n_seasons, " seasons, covering ",
           round(total_nm, 1), " nautical miles in roughly ", span_months,
           " months. That\u2019s a lot of salt water, sunscreen, and tactical arguments \u2014 all in the service of getting faster.")
  )
  seed_hash <- sum(utf8ToInt("overall_performance"))
  pick_from <- function(opts, slot = "") {
    h <- seed_hash + sum(utf8ToInt(slot))
    opts[((h) %% length(opts)) + 1]
  }
  paragraphs <- c(paragraphs, pick_from(overview_opts, "overview"))

  # ---- Paragraph 2: Placement trajectory across seasons ----
  place_num_all <- suppressWarnings(as.numeric(race_calendar$place))
  fleet_num_all <- suppressWarnings(as.numeric(race_calendar$fleet))
  valid_all <- !is.na(place_num_all) & !is.na(fleet_num_all) & fleet_num_all > 0

  if (sum(valid_all) > 0) {
    overall_avg <- round(mean(place_num_all[valid_all]), 1)
    overall_fleet <- round(mean(fleet_num_all[valid_all]), 1)
    total_wins <- sum(place_num_all[valid_all] == 1)
    top_half_all <- sum(place_num_all[valid_all] <= fleet_num_all[valid_all] / 2)
    top_pct_all <- round(100 * top_half_all / sum(valid_all))

    # Per-season breakdown
    season_stats <- lapply(seasons, function(s) {
      sc <- race_calendar[race_calendar$season == s, ]
      pn <- suppressWarnings(as.numeric(sc$place))
      fn <- suppressWarnings(as.numeric(sc$fleet))
      v  <- !is.na(pn) & !is.na(fn) & fn > 0
      if (sum(v) == 0) return(NULL)
      list(
        season = s,
        avg_place = round(mean(pn[v]), 1),
        avg_fleet = round(mean(fn[v]), 1),
        wins = sum(pn[v] == 1),
        n_scored = sum(v),
        top_half_pct = round(100 * sum(pn[v] <= fn[v] / 2) / sum(v))
      )
    })
    season_stats <- Filter(Negate(is.null), season_stats)

    p2 <- paste0("Overall, Wings has averaged ", overall_avg, " place in an average fleet of ",
                 overall_fleet, " boats across ", sum(valid_all), " scored races, finishing in the top half ",
                 top_pct_all, "% of the time.")

    if (total_wins > 0) {
      win_opts <- c(
        paste0(" Wings has claimed ", total_wins, " first-place finish",
               ifelse(total_wins > 1, "es", ""), " \u2014 proof that the crew can find the front of the fleet when conditions align."),
        paste0(" ", total_wins, " bullet", ifelse(total_wins > 1, "s", ""),
               " in the record book \u2014 each one a reminder that the potential is real."),
        paste0(" The crew has tasted victory ", total_wins, " time", ifelse(total_wins > 1, "s", ""),
               ". Not a dynasty, but not a drought either.")
      )
      p2 <- paste0(p2, pick_from(win_opts, "overallwins"))
    }

    # Trend detection
    if (length(season_stats) >= 2) {
      first_avg <- season_stats[[1]]$avg_place
      last_avg  <- season_stats[[length(season_stats)]]$avg_place
      first_thp <- season_stats[[1]]$top_half_pct
      last_thp  <- season_stats[[length(season_stats)]]$top_half_pct

      trend <- if (last_avg < first_avg - 0.5 && last_thp > first_thp + 5)
        paste0("The trajectory is encouraging \u2014 average placement has improved from ",
               first_avg, " (", seasons[1], ") to ", last_avg, " (", seasons[length(seasons)],
               "), and top-half finishes have climbed from ", first_thp, "% to ", last_thp,
               "%. The crew is clearly sharpening their game.")
      else if (last_avg > first_avg + 0.5)
        paste0("Average placement has shifted from ", first_avg, " (", seasons[1],
               ") to ", last_avg, " (", seasons[length(seasons)],
               "). The competition has gotten tougher, but the crew is adapting. Growth isn\u2019t always linear.")
      else
        paste0("Placement has been remarkably consistent across seasons (", first_avg,
               " in ", seasons[1], " vs ", last_avg, " in ", seasons[length(seasons)],
               ") \u2014 steady as the North Star.")

      p2 <- paste0(p2, " ", trend)
    }

    # Per-season breakdown line
    season_lines <- sapply(season_stats, function(ss) {
      paste0(ss$season, ": avg ", ss$avg_place, " place, ",
             ss$wins, " win", ifelse(ss$wins != 1, "s", ""),
             ", top half ", ss$top_half_pct, "%")
    })
    p2 <- paste0(p2, " Season by season: ", paste(season_lines, collapse = "; "), ".")

    paragraphs <- c(paragraphs, p2)
  }

  # ---- Paragraph 3: Speed and polar evolution ----
  track_with_race <- track_data |>
    dplyr::filter(!is.na(race), nzchar(race))

  if (nrow(track_with_race) > 0) {
    sp <- character()

    # Match track data to calendar for per-season stats
    season_perf <- lapply(seasons, function(s) {
      sc <- race_calendar[race_calendar$season == s, ]
      matched <- tibble::tibble()
      for (i in seq_len(nrow(sc))) {
        tr <- track_with_race |>
          dplyr::filter(race == sc$race[i],
                        datetime_local >= sc$start[i],
                        datetime_local <= sc$end[i])
        matched <- dplyr::bind_rows(matched, tr)
      }
      if (nrow(matched) == 0) return(NULL)
      list(
        season = s,
        avg_sog = mean(matched$sog_knots, na.rm = TRUE),
        avg_tws = mean(matched$tws_knots, na.rm = TRUE),
        avg_polar_stw = mean(matched$Polar_Perf_STW, na.rm = TRUE),
        n_pts = nrow(matched)
      )
    })
    season_perf <- Filter(Negate(is.null), season_perf)

    overall_sog <- mean(track_with_race$sog_knots, na.rm = TRUE)
    overall_tws <- mean(track_with_race$tws_knots, na.rm = TRUE)
    overall_polar <- mean(track_with_race$Polar_Perf_STW, na.rm = TRUE)

    if (!is.nan(overall_sog)) {
      sog_opts <- c(
        paste0("Across all instrumented races, Wings\u2019 overall average SOG is ", round(overall_sog, 1), " knots."),
        paste0("The career average SOG stands at ", round(overall_sog, 1), " knots \u2014 the pace of a boat learning its own potential."),
        paste0("Wings has averaged ", round(overall_sog, 1), " knots SOG across the entire dataset \u2014 a baseline that\u2019s only going to improve.")
      )
      sp <- c(sp, pick_from(sog_opts, "overallsog"))
    }

    if (!is.nan(overall_tws)) {
      wind_desc <- if (overall_tws < 8) "generally light-air diet"
                   else if (overall_tws < 12) "moderate breeze menu"
                   else "hearty wind buffet"
      sp <- c(sp, paste0("Average wind across the entire dataset is ",
                         round(overall_tws, 1), " knots \u2014 a ", wind_desc, "."))
    }

    if (!is.nan(overall_polar)) {
      polar_desc <- if (overall_polar > 0)
        paste0("Overall polar performance across all seasons is +", round(overall_polar, 2),
               " knots above target, though this aggregate figure is influenced by earlier seasons ",
               "when instrument calibration was still being refined. The most recent seasons provide ",
               "the most trustworthy picture of how Wings truly performs against her polars.")
      else if (overall_polar > -0.3)
        paste0("At ", round(overall_polar, 2), " knots relative to polars overall, Wings is sailing close to her design envelope \u2014 not far off the mark at all.")
      else
        paste0("At ", round(overall_polar, 2), " knots below polar targets overall, there\u2019s untapped speed in the hull. The crew is still unlocking the boat\u2019s full potential.")
      sp <- c(sp, polar_desc)
    }

    # Polar trend across seasons
    if (length(season_perf) >= 2) {
      polar_vals <- sapply(season_perf, function(x) x$avg_polar_stw)
      sog_vals   <- sapply(season_perf, function(x) x$avg_sog)
      s_names    <- sapply(season_perf, function(x) x$season)

      if (!any(is.nan(polar_vals))) {
        season_yrs <- suppressWarnings(as.integer(substr(s_names, 1, 4)))
        recent_idx <- which(!is.na(season_yrs) & season_yrs >= 2025)
        older_idx  <- which(!is.na(season_yrs) & season_yrs < 2025)

        trend_parts <- character()

        if (length(older_idx) > 0 && any(polar_vals[older_idx] > 0)) {
          older_pos <- older_idx[polar_vals[older_idx] > 0]
          trend_parts <- c(trend_parts,
            paste0("Earlier seasons (",
                   paste(s_names[older_pos], collapse = ", "),
                   ") show positive polar numbers, but these likely reflect instruments that were not yet properly calibrated rather than genuine over-performance."))
        }

        if (length(recent_idx) > 0) {
          recent_avg <- round(mean(polar_vals[recent_idx]), 2)
          recent_detail <- paste(paste0(round(polar_vals[recent_idx], 2), " in ", s_names[recent_idx]), collapse = ", ")
          trend_parts <- c(trend_parts,
            paste0("The most recent seasons (", recent_detail,
                   ") provide the most reliable benchmark with properly calibrated instruments, ",
                   "averaging ", recent_avg, " knots relative to polar targets."))
        }

        if (length(trend_parts) > 0) sp <- c(sp, paste(trend_parts, collapse = " "))
      }
    }

    if (length(sp) > 0) paragraphs <- c(paragraphs, paste(sp, collapse = " "))
  }

  # ---- Closing ----
  closing_opts <- c(
    paste0("From the first starting gun to the latest finish line, Wings\u2019 journey across ", n_seasons,
           " seasons tells a story of a crew investing in experience before chasing trophies. Every race sharpens the instincts, every mile deepens the understanding. ",
           "The exploration phase is well underway \u2014 and the data is building the roadmap for what comes next."),
    paste0(n_seasons, " seasons of racing. ", n_total_races, " starting guns. ", round(total_nm, 1),
           " nautical miles of lessons, wins, near-misses, and the occasional creative excuse. ",
           "The crew isn\u2019t done yet \u2014 if anything, they\u2019re just getting warmed up."),
    paste0("The Wings project continues. ", n_seasons, " seasons of data suggest a crew that\u2019s learning, adapting, and slowly turning experience into results. ",
           "The best races may still be ahead \u2014 the logbook is far from full.")
  )
  paragraphs <- c(paragraphs, pick_from(closing_opts, "overallclosing"))

  paste(paragraphs, collapse = "\n\n")
}

build_all_narratives <- function(data_rds) {
  track <- data_rds$track_all
  cal   <- data_rds$race_calendar

  if (nrow(cal) == 0) return(list())

  # Build per-race stats using the calendar's start/end to filter track data.
  cal_rows <- cal |>
    mutate(race_date = as.Date(start)) |>
    group_by(race, race_date) |>
    summarise(
      season   = first(season),
      series   = first(series),
      place    = first(place),
      fleet    = first(fleet),
      length   = first(length),
      helm     = first(helm),
      headsail = if ("headsail" %in% names(cal)) first(headsail) else NA_character_,
      cal_start = min(start, na.rm = TRUE),
      cal_end   = max(end, na.rm = TRUE),
      .groups  = "drop"
    )

  all_races <- cal_rows |>
    rowwise() |>
    mutate(
      track_subset = list({
        tr <- track |>
          filter(race == .env$race,
                 datetime_local >= cal_start,
                 datetime_local <= cal_end)
        if (nrow(tr) == 0) NULL else tr
      }),
      avg_sog        = if (is.null(track_subset)) NA_real_ else mean(track_subset$sog_knots, na.rm = TRUE),
      max_sog        = if (is.null(track_subset)) NA_real_ else max(track_subset$sog_knots, na.rm = TRUE),
      avg_stw        = if (is.null(track_subset)) NA_real_ else mean(track_subset$stw_knots, na.rm = TRUE),
      max_stw        = if (is.null(track_subset)) NA_real_ else max(track_subset$stw_knots, na.rm = TRUE),
      avg_tws        = if (is.null(track_subset)) NA_real_ else mean(track_subset$tws_knots, na.rm = TRUE),
      max_tws        = if (is.null(track_subset)) NA_real_ else max(track_subset$tws_knots, na.rm = TRUE),
      polar_perf_stw = if (is.null(track_subset)) NA_real_ else mean(track_subset$Polar_Perf_STW, na.rm = TRUE),
      polar_perf_sog = if (is.null(track_subset)) NA_real_ else mean(track_subset$Polar_Perf_SOG, na.rm = TRUE),
      duration_hrs   = if (is.null(track_subset)) NA_real_ else
        as.numeric(difftime(max(track_subset$datetime_local), min(track_subset$datetime_local), units = "hours"))
    ) |>
    ungroup() |>
    select(-track_subset, -cal_start, -cal_end) |>
    mutate(across(where(is.numeric), ~ ifelse(is.infinite(.), NA_real_, .)))

  completed <- all_races |>
    filter(!is.na(avg_sog), !is.nan(avg_sog))

  # Track used closing quotes across all races to avoid repeats
  narratives <- list()
  used_quotes <- character()
  for (i in seq_len(nrow(all_races))) {
    row <- all_races[i, ]
    key <- paste(row$race, row$race_date, sep = "|")
    result <- generate_race_narrative(row, completed, used_quotes)
    narratives[[key]] <- result$narrative
    if (!is.na(result$quote_used)) used_quotes <- c(used_quotes, result$quote_used)
  }
  narratives
}

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
# Load the pre-built dataset. The RDS contains four elements:
#   - track_all:      GPS track points with synchronized speed/wind data
#   - race_calendar:  Race metadata with pre-computed performance statistics
#   - polar_ref:      Wide-format reference polar table (TWS columns × TWA rows)
#   - polar_ref_long: Long-format polars (tws, twa, bsp_ref)
#
# After loading, the track data undergoes additional startup processing:
# timezone enforcement, teleport detection, and pre-computation of UI display
# columns. The race calendar is filtered to exclude future races.
data_rds_raw <- readRDS(rds_path)
race_calendar_loaded <- if ("race_calendar" %in% names(data_rds_raw)) data_rds_raw$race_calendar else tibble()
track_all_loaded      <- data_rds_raw$track_all
polar_ref_loaded      <- if ("polar_ref"      %in% names(data_rds_raw)) data_rds_raw$polar_ref      else tibble()
polar_ref_long_loaded <- if ("polar_ref_long" %in% names(data_rds_raw)) data_rds_raw$polar_ref_long else tibble()
marks_ref_loaded      <- if ("marks_ref"      %in% names(data_rds_raw)) data_rds_raw$marks_ref      else tibble(mark = character(), lat = character(), lon = character())

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

# ---------- TELEPORT DETECTION ----------
# Remove isolated GPS anomalies caused by UTC date-rollover bugs in NMEA
# sentences. These "teleport" points appear when a stray RMC sentence carries
# the wrong UTC date, causing its computed position to jump thousands of miles
# from its true temporal neighbors. The detection algorithm computes implied
# speed (via Haversine distance / time delta) to both the preceding and
# following points. If a point implies speeds exceeding 30 knots to BOTH
# neighbors, it is a spurious jump (real sailing never exceeds ~15 knots on
# this boat) and is dropped. Points at the start/end of the dataset (where
# lag/lead produces NA) are always retained.
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

# Pre-compute UI display columns once at startup. These derived columns are
# used repeatedly in reactive expressions and renderers. Computing them once
# here avoids redundant ifelse/as.Date calls on every reactive invalidation.
track_all_loaded <- track_all_loaded |>
  mutate(
    day_local_ui = as.Date(datetime_local, tz = LOCAL_TZ),
    helm_ui      = ifelse(!is.na(helm) & nzchar(helm), helm, "(blank)"),
    race_ui      = ifelse(!is.na(race) & nzchar(race), race, "(blank)")
  )

# Filter out future races — only past and current races should appear in the UI.
# This allows the race calendar Excel to contain upcoming scheduled races
# without cluttering the analytics views. Races that haven't happened (or were
# scheduled but not sailed) also lack a finish place and fleet size in the
# calendar spreadsheet, so require both to be present as a second guard against
# including races that didn't or haven't occurred.
now_local <- lubridate::with_tz(Sys.time(), LOCAL_TZ)
if (nrow(race_calendar_loaded) > 0) {
  race_calendar_loaded <- race_calendar_loaded |>
    filter(
      !is.na(start), start <= now_local,
      # Drop races missing BOTH a finish place and a fleet size — these are
      # scheduled races that didn't or haven't happened. Races with a place
      # but a blank fleet (or vice versa) are real and are kept.
      (!is.na(place) & nzchar(trimws(place))) | !is.na(fleet)
    )
}

# Assemble the final in-memory dataset used by all Shiny server reactives.
# This list is the single source of truth for the running application.
data_rds <- list(
  track_all      = track_all_loaded,
  polar_ref      = polar_ref_loaded,
  polar_ref_long = polar_ref_long_loaded,
  race_calendar  = race_calendar_loaded,
  marks_ref      = marks_ref_loaded
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

# ---------- RACE ANALYTICS DEFAULTS (most recent race) ----------
ra_all_seasons <- sort(unique(data_rds$race_calendar$season[!is.na(data_rds$race_calendar$season)]), decreasing = TRUE)
ra_most_recent <- data_rds$race_calendar |> filter(!is.na(start)) |> arrange(desc(start)) |> head(1)
ra_default_season <- if (nrow(ra_most_recent) > 0 && !is.na(ra_most_recent$season)) ra_most_recent$season else ra_all_seasons[1]
ra_default_season_cal <- data_rds$race_calendar |> filter(!is.na(season), season == ra_default_season)
ra_default_series_choices <- c("All", sort(unique(ra_default_season_cal$series[!is.na(ra_default_season_cal$series) & nzchar(ra_default_season_cal$series)])))
ra_default_race_list <- ra_default_season_cal |>
  mutate(race_date = as.Date(start)) |>
  group_by(race, race_date) |>
  summarise(start = min(start, na.rm = TRUE), .groups = "drop") |>
  arrange(desc(start))
ra_default_race_choices <- if (nrow(ra_default_race_list) > 0) {
  labels <- paste0(ra_default_race_list$race, " (", format(ra_default_race_list$race_date, "%m/%d/%Y"), ")")
  setNames(seq_len(nrow(ra_default_race_list)), labels)
} else {
  character()
}

# ---------- UI ----------
ui <- fluidPage(
  tags$head(
    # Open Graph meta tags for link previews when URL is shared
    tags$meta(property = "og:title", content = "Wings Analytics"),
    tags$meta(property = "og:description", content = "AI-Inspired Sailing Performance & Racing Intelligence"),
    tags$meta(property = "og:image", content = "https://dmdanielson.shinyapps.io/Wings_Analytics/og_image.png"),
    tags$meta(property = "og:type", content = "website"),
    tags$meta(property = "og:url", content = "https://dmdanielson.shinyapps.io/Wings_Analytics/"),
    tags$meta(name = "twitter:card", content = "summary_large_image"),
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
      table.dataTable tfoot th {
        font-weight: 700 !important;
        color: rgba(100,180,255,1.0) !important;
        background: rgba(100,180,255,0.08) !important;
        border-top: 2px solid rgba(100,180,255,0.40) !important;
        font-size: 14px !important;
      }
      .dataTables_wrapper .dataTables_info { color: rgba(232,237,246,0.65) !important; }

      /* Force aligned columns across the 3 Race Seasons tables */
      #season_summary_table table.dataTable,
      #season_series_summary_table table.dataTable,
      #season_table table.dataTable {
        table-layout: fixed !important;
        width: 100% !important;
      }
      /* Allow Race column text to wrap in the detail table */
      #season_table table.dataTable td:nth-child(2) {
        white-space: normal;
        word-wrap: break-word;
        overflow-wrap: break-word;
      }

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
      tags$p("June through May Seasons",
             style = "font-size: 0.85em; color: rgba(255,255,255,0.55); margin-bottom: 6px; font-style: italic;"),
      div(
        style = "
      background: rgba(255,255,255,0.04);
      border: 1px solid rgba(255,255,255,0.10);
      border-radius: 16px;
      padding: 18px;
      box-shadow: 0 10px 28px rgba(0,0,0,0.25);
      max-width: 960px;
    ",
        fluidRow(
          column(4, selectInput(
            "season_select",
            "Select Season",
            choices = c("All", sort(unique(data_rds$race_calendar$season[!is.na(data_rds$race_calendar$season)]), decreasing = TRUE))
          )),
          column(4, selectInput(
            "season_series_select",
            "Race Series",
            choices = c("All")
          ))
        ),
        uiOutput("season_narrative_card"),
        DTOutput("season_summary_table"),
        br(),
        DTOutput("season_series_summary_table"),
        br(),
        DTOutput("season_table")
      )
    ),
    tabPanel(
      "Race Analytics",
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
            choices = ra_all_seasons,
            selected = ra_default_season
          )),
          column(4, selectInput(
            "ra_series_select", "Race Series",
            choices = ra_default_series_choices,
            selected = "All"
          )),
          column(4, selectInput(
            "ra_race_select", "Race",
            choices = ra_default_race_choices
          ))
        )
      ),
      uiOutput("selected_race_header"),
      uiOutput("race_narrative_card"),
      br(),
      tabsetPanel(
        id = "ra_detail_tabs",
        tabPanel("Data", br(),
          DTOutput("race_analysis_summary"),
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
        tabPanel("Course", br(),
          DTOutput("race_course_table"),
          br(),
          leafletOutput("map", height = 450)
        ),
        tabPanel("Crew", br(), DTOutput("race_crew_table"))
      )
    ),
    tabPanel(
      "Boat Performance",
      br(),
      tags$p("ORC Speed Guide for Wings J112E",
             style = "font-size: 0.85em; color: rgba(255,255,255,0.55); margin-bottom: 6px; font-style: italic;"),
      div(
        style = "
          background: rgba(255,255,255,0.04);
          border: 1px solid rgba(255,255,255,0.10);
          border-radius: 16px;
          padding: 18px;
        ",
        tabsetPanel(
          tabPanel("All Wind Speeds",
            tags$p("This polar diagram overlays target boat speeds for all wind conditions (6\u201324 knots TWS) on a single chart. ",
                   "Each curve represents a different true wind speed, plotted by wind angle (TWA) and target speed (in knots). ",
                   "The farther a curve extends from the center, the faster the boat should be sailing at that angle. ",
                   "Use this view to compare how the boat\u2019s speed potential changes across wind strengths and angles.",
                   style = "font-size: 0.85em; color: rgba(255,255,255,0.55); margin-bottom: 10px; line-height: 1.5;"),
            tags$iframe(src = "speed_guide_1.html", width = "100%", height = "900px", style = "border:none; background:white; border-radius:8px;")
          ),
          tabPanel("TWS 6 kts",
            tags$p("Polar diagram for 6 knots of true wind speed \u2014 light air conditions. ",
                   "The curves show target boat speed by wind angle for each sail configuration. ",
                   "In these conditions, boat speed is modest and sail selection is critical to maintaining momentum.",
                   style = "font-size: 0.85em; color: rgba(255,255,255,0.55); margin-bottom: 10px; line-height: 1.5;"),
            tags$iframe(src = "speed_guide_2.html", width = "100%", height = "900px", style = "border:none; background:white; border-radius:8px;")
          ),
          tabPanel("TWS 8 kts",
            tags$p("Polar diagram for 8 knots of true wind speed \u2014 a light to moderate breeze. ",
                   "The boat begins to develop more speed, and the differences between sail configurations become more pronounced. ",
                   "Optimal angles for upwind and downwind VMG start to become clearer at this wind speed.",
                   style = "font-size: 0.85em; color: rgba(255,255,255,0.55); margin-bottom: 10px; line-height: 1.5;"),
            tags$iframe(src = "speed_guide_3.html", width = "100%", height = "900px", style = "border:none; background:white; border-radius:8px;")
          ),
          tabPanel("TWS 10 kts",
            tags$p("Polar diagram for 10 knots of true wind speed \u2014 a moderate working breeze. ",
                   "This is a common racing condition where the J112E is well-powered and sail trim becomes the primary performance differentiator. ",
                   "The polar curves show increasing separation between sail configurations, especially downwind.",
                   style = "font-size: 0.85em; color: rgba(255,255,255,0.55); margin-bottom: 10px; line-height: 1.5;"),
            tags$iframe(src = "speed_guide_4.html", width = "100%", height = "900px", style = "border:none; background:white; border-radius:8px;")
          ),
          tabPanel("TWS 12 kts",
            tags$p("Polar diagram for 12 knots of true wind speed \u2014 solid medium-air conditions. ",
                   "The boat is reaching target hull speed upwind and generating strong downwind angles. ",
                   "This wind range often produces the best VMG performance for the J112E.",
                   style = "font-size: 0.85em; color: rgba(255,255,255,0.55); margin-bottom: 10px; line-height: 1.5;"),
            tags$iframe(src = "speed_guide_5.html", width = "100%", height = "900px", style = "border:none; background:white; border-radius:8px;")
          ),
          tabPanel("TWS 14 kts",
            tags$p("Polar diagram for 14 knots of true wind speed \u2014 the upper end of the medium-air range. ",
                   "The boat is fully powered up and may require depowering techniques upwind. ",
                   "Downwind targets increase, and sail selection between asymmetric configurations matters more.",
                   style = "font-size: 0.85em; color: rgba(255,255,255,0.55); margin-bottom: 10px; line-height: 1.5;"),
            tags$iframe(src = "speed_guide_6.html", width = "100%", height = "900px", style = "border:none; background:white; border-radius:8px;")
          ),
          tabPanel("TWS 16 kts",
            tags$p("Polar diagram for 16 knots of true wind speed \u2014 fresh breeze conditions. ",
                   "The boat is overpowered on some points of sail. Upwind target angles may widen as the crew works to keep the boat flat. ",
                   "Downwind, a reduced-hoist asymmetric (75%) may outperform the full hoist in these conditions.",
                   style = "font-size: 0.85em; color: rgba(255,255,255,0.55); margin-bottom: 10px; line-height: 1.5;"),
            tags$iframe(src = "speed_guide_7.html", width = "100%", height = "900px", style = "border:none; background:white; border-radius:8px;")
          ),
          tabPanel("TWS 20 kts",
            tags$p("Polar diagram for 20 knots of true wind speed \u2014 heavy air. ",
                   "The crew is managing loads and the boat is at or near maximum speed potential. ",
                   "Upwind performance plateaus while downwind speeds continue to climb. Sail selection and depowering are critical.",
                   style = "font-size: 0.85em; color: rgba(255,255,255,0.55); margin-bottom: 10px; line-height: 1.5;"),
            tags$iframe(src = "speed_guide_8.html", width = "100%", height = "900px", style = "border:none; background:white; border-radius:8px;")
          ),
          tabPanel("TWS 24 kts",
            tags$p("Polar diagram for 24 knots of true wind speed \u2014 storm-level racing conditions. ",
                   "Boat speed is near its maximum, and the focus shifts from speed to control. ",
                   "The polar shows relatively flat upwind targets with high downwind potential, but real-world performance depends heavily on sea state and crew work.",
                   style = "font-size: 0.85em; color: rgba(255,255,255,0.55); margin-bottom: 10px; line-height: 1.5;"),
            tags$iframe(src = "speed_guide_9.html", width = "100%", height = "900px", style = "border:none; background:white; border-radius:8px;")
          ),
          tabPanel("Best Performance",
            tags$p("This table summarizes the boat\u2019s optimal performance targets across all wind speeds. ",
                   "It includes best VMG (velocity made good) angles and speeds for both upwind and downwind legs, ",
                   "along with reaching speeds at key wind angles. Use these numbers as benchmarks when evaluating race performance.",
                   style = "font-size: 0.85em; color: rgba(255,255,255,0.55); margin-bottom: 10px; line-height: 1.5;"),
            tags$iframe(src = "speed_guide_10.html", width = "100%", height = "900px", style = "border:none; background:white; border-radius:8px;")
          ),
          tabPanel("Jib",
            tags$p("Sail plan and performance tables for the jib \u2014 the primary upwind headsail. ",
                   "The diagram shows the sail\u2019s dimensions, and the tables list target boat speeds at each combination of true wind speed and wind angle. ",
                   "The jib is used on all upwind legs and close-reaching angles.",
                   style = "font-size: 0.85em; color: rgba(255,255,255,0.55); margin-bottom: 10px; line-height: 1.5;"),
            tags$iframe(src = "speed_guide_11.html", width = "100%", height = "900px", style = "border:none; background:white; border-radius:8px;")
          ),
          tabPanel("Asymmetric 75%",
            tags$p("Sail plan and performance tables for the asymmetric spinnaker at 75% hoist. ",
                   "This is a reduced-power configuration where the sail is flown at three-quarters of its full luff length. ",
                   "It is typically used in heavier winds when a full hoist would overpower the boat, providing better control while still maximizing downwind speed.",
                   style = "font-size: 0.85em; color: rgba(255,255,255,0.55); margin-bottom: 10px; line-height: 1.5;"),
            tags$iframe(src = "speed_guide_12.html", width = "100%", height = "900px", style = "border:none; background:white; border-radius:8px;")
          ),
          tabPanel("Asymmetric 100%",
            tags$p("Sail plan and performance tables for the asymmetric spinnaker at full (100%) hoist. ",
                   "This is the maximum sail area configuration, providing the most power for downwind and deep-reaching angles. ",
                   "It is the preferred setup in light to moderate winds where the crew can handle the full sail area effectively.",
                   style = "font-size: 0.85em; color: rgba(255,255,255,0.55); margin-bottom: 10px; line-height: 1.5;"),
            tags$iframe(src = "speed_guide_13.html", width = "100%", height = "900px", style = "border:none; background:white; border-radius:8px;")
          )
        )
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
          href   = "https://youtu.be/ZAzhhFulEes",
          target = "_blank",
          class  = "social-yt-link",
          HTML('<svg width="18" height="13" viewBox="0 0 159 110" fill="none"><path d="M154 17.5c-1.8-6.7-7.1-12-13.8-13.8C128 0 79.5 0 79.5 0S31 0 18.8 3.7C12.1 5.5 6.8 10.8 5 17.5 1.2 29.7 1.2 55 1.2 55s0 25.3 3.8 37.5c1.8 6.7 7.1 12 13.8 13.8C31 110 79.5 110 79.5 110s48.5 0 60.7-3.7c6.7-1.8 12-7.1 13.8-13.8 3.8-12.2 3.8-37.5 3.8-37.5s0-25.3-3.8-37.5z" fill="#FF0000"/><path d="M64 78.8V31.2L105 55 64 78.8z" fill="#FFF"/></svg>'),
          "Wings Mexico 2026"
        ),
        tags$a(
          href   = "https://youtu.be/av06m8_RlKQ",
          target = "_blank",
          class  = "social-yt-link",
          HTML('<svg width="18" height="13" viewBox="0 0 159 110" fill="none"><path d="M154 17.5c-1.8-6.7-7.1-12-13.8-13.8C128 0 79.5 0 79.5 0S31 0 18.8 3.7C12.1 5.5 6.8 10.8 5 17.5 1.2 29.7 1.2 55 1.2 55s0 25.3 3.8 37.5c1.8 6.7 7.1 12 13.8 13.8C31 110 79.5 110 79.5 110s48.5 0 60.7-3.7c6.7-1.8 12-7.1 13.8-13.8 3.8-12.2 3.8-37.5 3.8-37.5s0-25.3-3.8-37.5z" fill="#FF0000"/><path d="M64 78.8V31.2L105 55 64 78.8z" fill="#FFF"/></svg>'),
          "Wings - Bone Island Regatta"
        ),
        tags$a(
          href   = "https://youtu.be/jpdw4J9VsK4",
          target = "_blank",
          class  = "social-yt-link",
          HTML('<svg width="18" height="13" viewBox="0 0 159 110" fill="none"><path d="M154 17.5c-1.8-6.7-7.1-12-13.8-13.8C128 0 79.5 0 79.5 0S31 0 18.8 3.7C12.1 5.5 6.8 10.8 5 17.5 1.2 29.7 1.2 55 1.2 55s0 25.3 3.8 37.5c1.8 6.7 7.1 12 13.8 13.8C31 110 79.5 110 79.5 110s48.5 0 60.7-3.7c6.7-1.8 12-7.1 13.8-13.8 3.8-12.2 3.8-37.5 3.8-37.5s0-25.3-3.8-37.5z" fill="#FF0000"/><path d="M64 78.8V31.2L105 55 64 78.8z" fill="#FFF"/></svg>'),
          "Wings Key West Return"
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
      # ---- Documentation ----
      div(
        class = "social-section",
        div(
          class = "social-section-header",
          HTML('<svg width="26" height="26" viewBox="0 0 24 24" fill="none" stroke="#e8edf6" stroke-width="1.5" stroke-linecap="round" stroke-linejoin="round"><path d="M14 2H6a2 2 0 00-2 2v16a2 2 0 002 2h12a2 2 0 002-2V8z"/><polyline points="14 2 14 8 20 8"/><line x1="16" y1="13" x2="8" y2="13"/><line x1="16" y1="17" x2="8" y2="17"/><polyline points="10 9 9 9 8 9"/></svg>'),
          h4("Documentation")
        ),
        tags$p(
          "Access the full technical documentation for Wings Analytics, including data pipeline details and performance metric explanations.",
          style = "color: rgba(232,237,246,0.60); font-size: 14px; margin-bottom: 14px;"
        ),
        tags$a(
          href   = "wings_analytics_documentation.html",
          target = "_blank",
          class  = "wa-shop-btn",
          "View Documentation"
        )
      ),
      # ---- Project Source ----
      div(
        class = "social-section",
        div(
          class = "social-section-header",
          HTML('<svg width="26" height="26" viewBox="0 0 24 24" fill="#e8edf6"><path d="M12 0C5.37 0 0 5.37 0 12c0 5.31 3.435 9.795 8.205 11.385.6.105.825-.255.825-.57 0-.285-.015-1.23-.015-2.235-3.015.555-3.795-.735-4.035-1.41-.135-.345-.72-1.41-1.23-1.695-.42-.225-1.02-.78-.015-.795.945-.015 1.62.87 1.845 1.23 1.08 1.815 2.805 1.305 3.495.99.105-.78.42-1.305.765-1.605-2.67-.3-5.46-1.335-5.46-5.925 0-1.305.465-2.385 1.23-3.225-.12-.3-.54-1.53.12-3.18 0 0 1.005-.315 3.3 1.23.96-.27 1.98-.405 3-.405s2.04.135 3 .405c2.295-1.56 3.3-1.23 3.3-1.23.66 1.65.24 2.88.12 3.18.765.84 1.23 1.905 1.23 3.225 0 4.605-2.805 5.625-5.475 5.925.435.375.81 1.095.81 2.22 0 1.605-.015 2.895-.015 3.3 0 .315.225.69.825.57A12.02 12.02 0 0024 12c0-6.63-5.37-12-12-12z"/></svg>'),
          h4("Project Source")
        ),
        tags$p(
          "View the source code and contribute to the project on GitHub.",
          style = "color: rgba(232,237,246,0.60); font-size: 14px; margin-bottom: 14px;"
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
        class = "social-section",
        div(
          class = "social-section-header",
          HTML('<svg width="26" height="26" viewBox="0 0 24 24" fill="none" stroke="#e8edf6" stroke-width="1.5" stroke-linecap="round" stroke-linejoin="round"><ellipse cx="12" cy="5" rx="9" ry="3"/><path d="M21 12c0 1.66-4 3-9 3s-9-1.34-9-3"/><path d="M3 5v14c0 1.66 4 3 9 3s9-1.34 9-3V5"/></svg>'),
          h4("Data File Management")
        ),
        tags$p(
          "Manage the data files used by Wings Analytics. These operations are only available when running locally.",
          style = "color: rgba(232,237,246,0.60); font-size: 14px; margin-bottom: 16px;"
        ),
        passwordInput("file_mgmt_password", "Enter password to unlock:",
                      placeholder = "Password"),
        uiOutput("file_mgmt_controls")
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
  
  # Reactive: all races for the selected season on the Race Analytics tab
  ra_season_races <- reactive({
    req(input$ra_season_select)
    cal <- data_rds$race_calendar |>
      filter(!is.na(season), season == input$ra_season_select)
    if (nrow(cal) == 0) return(tibble())

    # Ensure new columns exist even if RDS was built before they were added
    # Ensure crew columns exist even if RDS predates them
    crew_cols <- paste0(rep(c("crew_", "position_"), 10), rep(1:10, each = 2))
    for (col in crew_cols) {
      if (!col %in% names(cal)) cal[[col]] <- NA_character_
    }

    cal |>
      mutate(race_date = as.Date(start)) |>
      group_by(race, race_date) |>
      summarise(
        series      = first(series),
        place       = first(place),
        fleet       = first(fleet),
        length      = first(length),
        start       = min(start, na.rm = TRUE),
        end         = max(end, na.rm = TRUE),
        crew_1      = first(crew_1),
        position_1  = first(position_1),
        crew_2      = first(crew_2),
        position_2  = first(position_2),
        crew_3      = first(crew_3),
        position_3  = first(position_3),
        crew_4      = first(crew_4),
        position_4  = first(position_4),
        crew_5      = first(crew_5),
        position_5  = first(position_5),
        crew_6      = first(crew_6),
        position_6  = first(position_6),
        crew_7      = first(crew_7),
        position_7  = first(position_7),
        crew_8      = first(crew_8),
        position_8  = first(position_8),
        crew_9      = first(crew_9),
        position_9  = first(position_9),
        crew_10     = first(crew_10),
        position_10 = first(position_10),
        # Dynamically summarise all course mark columns
        across(matches("^(start_line|mark_\\d+|finish_line)$"), first),
        .groups = "drop"
      ) |>
      arrange(start)
  })
  
  # Update series dropdown when season changes
  observeEvent(input$ra_season_select, {
    sr <- ra_season_races()
    series_choices <- sort(unique(sr$series[!is.na(sr$series) & nzchar(sr$series)]))
    updateSelectInput(session, "ra_series_select", choices = c("All", series_choices))
  })
  
  # Reactive: races filtered to selected series (or all if "All")
  ra_series_races <- reactive({
    sr <- ra_season_races()
    req(input$ra_series_select)
    if (input$ra_series_select != "All") {
      sr <- sr |> filter(!is.na(series), series == input$ra_series_select)
    }
    # Order from most recent to oldest
    sr |> arrange(desc(start))
  })
  
  # Build race dropdown labels like "Race Name (MM/DD/YYYY)"
  # Ordered most recent to oldest, default to most recent
  ra_race_choices <- reactive({
    sr <- ra_series_races()
    if (nrow(sr) == 0) return(character())
    labels <- paste0(sr$race, " (", format(sr$race_date, "%m/%d/%Y"), ")")
    setNames(seq_len(nrow(sr)), labels)
  })
  
  # Update race dropdown when series changes
  observeEvent(input$ra_series_select, {
    choices <- ra_race_choices()
    updateSelectInput(session, "ra_race_select", choices = choices)
  })
  
  # Auto-update selected race row when dropdown changes
  ra_selected_row <- reactive({
    sr <- ra_series_races()
    req(input$ra_race_select)
    idx <- as.integer(input$ra_race_select)
    if (is.na(idx) || idx < 1 || idx > nrow(sr)) return(NULL)
    sr[idx, ]
  })
  
  # Course marks (with decimal-degree coordinates) for the selected race.
  # Shared by the course table's map so points/labels match the leg table.
  ra_course_points <- reactive({
    cal_row <- ra_selected_row()
    req(cal_row)
    marks_ref <- data_rds$marks_ref

    course_cols <- grep("^(start_line|mark_\\d+|finish_line)$", names(cal_row), value = TRUE)
    if (length(course_cols) == 0) return(tibble())
    mark_nums <- suppressWarnings(as.integer(gsub("mark_", "", course_cols)))
    course_cols <- course_cols[order(
      ifelse(course_cols == "start_line", -1,
             ifelse(course_cols == "finish_line", Inf, mark_nums))
    )]

    mark_values <- vapply(course_cols, function(cc) as.character(cal_row[[cc]]), character(1))
    keep <- !is.na(mark_values)
    if (!any(keep)) return(tibble())

    pts <- tibble(order = seq_len(sum(keep)), mark = mark_values[keep])
    if (!is.null(marks_ref) && nrow(marks_ref) > 0) {
      pts <- pts |> left_join(marks_ref, by = "mark")
    } else {
      pts$lat <- NA_character_; pts$lon <- NA_character_
    }

    # Marks stored as degrees-decimal-minutes strings (e.g. "27° 54.22").
    dm_to_dd <- function(x) {
      m <- regmatches(x, regexec("(-?[0-9]+)[^0-9]+([0-9.]+)", x))
      vapply(m, function(p) {
        if (length(p) < 3) return(NA_real_)
        d <- suppressWarnings(as.numeric(p[2]))
        mm <- suppressWarnings(as.numeric(p[3]))
        if (is.na(d) || is.na(mm)) return(NA_real_)
        sign(d) * (abs(d) + mm / 60)
      }, numeric(1))
    }

    pts |>
      mutate(
        lat_dd =  abs(dm_to_dd(lat)),
        lon_dd = -abs(dm_to_dd(lon))
      )
  })

  # Header for Race Analysis tab showing the selected race
  output$selected_race_header <- renderUI({
    row <- ra_selected_row()
    if (is.null(row)) return(NULL)
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
        "AI Race Report"
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
  
  # ---- Course table for Race Analytics tab ----
  output$race_course_table <- renderDT({
    cal_row <- ra_selected_row()
    req(cal_row)

    marks_ref <- data_rds$marks_ref

    # Collect course columns in order: start_line, mark_1..N, finish_line
    course_cols <- grep("^(start_line|mark_\\d+|finish_line)$", names(cal_row), value = TRUE)
    # Sort: start_line first, then mark_N numerically, then finish_line
    mark_nums <- suppressWarnings(as.integer(gsub("mark_", "", course_cols)))
    col_order <- order(
      ifelse(course_cols == "start_line", -1, ifelse(course_cols == "finish_line", Inf, mark_nums))
    )
    course_cols <- course_cols[col_order]

    # Build display labels
    col_labels <- dplyr::case_when(
      course_cols == "start_line"  ~ "Start Line",
      course_cols == "finish_line" ~ "Finish Line",
      TRUE ~ paste("Mark", gsub("mark_", "", course_cols))
    )

    # Extract mark values, keep only non-NA rows
    mark_values <- vapply(course_cols, function(cc) as.character(cal_row[[cc]]), character(1))
    keep <- !is.na(mark_values)

    if (!any(keep)) {
      course_df <- tibble(`#` = integer(), Leg = character(), Mark = character(),
                          Latitude = character(), Longitude = character())
    } else {
      course_df <- tibble(
        `#`  = seq_len(sum(keep)),
        Leg  = col_labels[keep],
        Mark = mark_values[keep]
      )

      # Join lat/lon from marks reference
      if (!is.null(marks_ref) && nrow(marks_ref) > 0) {
        course_df <- course_df |>
          left_join(marks_ref, by = c("Mark" = "mark")) |>
          rename(Latitude = lat, Longitude = lon)
      } else {
        course_df$Latitude  <- NA_character_
        course_df$Longitude <- NA_character_
      }

      # Marks are stored as degrees-decimal-minutes strings (e.g. "27° 54.22").
      # Convert to decimal degrees for distance math.
      dm_to_dd <- function(x) {
        m <- regmatches(x, regexec("(-?[0-9]+)[^0-9]+([0-9.]+)", x))
        vapply(m, function(p) {
          if (length(p) < 3) return(NA_real_)
          d <- suppressWarnings(as.numeric(p[2]))
          mm <- suppressWarnings(as.numeric(p[3]))
          if (is.na(d) || is.na(mm)) return(NA_real_)
          sign(d) * (abs(d) + mm / 60)
        }, numeric(1))
      }

      # Distance (nm) of each leg from the previous mark, via haversine.
      # Tampa Bay: latitude North (+), longitude West (-).
      lat_num <-  abs(dm_to_dd(course_df$Latitude))
      lon_num <- -abs(dm_to_dd(course_df$Longitude))
      dist_nm <- rep(NA_real_, nrow(course_df))
      if (nrow(course_df) > 1) {
        for (i in 2:nrow(course_df)) {
          if (!is.na(lat_num[i]) && !is.na(lon_num[i]) &&
              !is.na(lat_num[i - 1]) && !is.na(lon_num[i - 1])) {
            dist_nm[i] <- geosphere::distHaversine(
              c(lon_num[i - 1], lat_num[i - 1]),
              c(lon_num[i], lat_num[i])
            ) / 1852
          }
        }
      }
      course_df$`Distance (nm)` <- round(dist_nm, 2)

      # Bearing (BTW) from the previous mark to this mark, as a compass heading.
      btw_col <- rep(NA_real_, nrow(course_df))
      if (nrow(course_df) > 1) {
        for (i in 2:nrow(course_df)) {
          if (!is.na(lat_num[i]) && !is.na(lon_num[i]) &&
              !is.na(lat_num[i - 1]) && !is.na(lon_num[i - 1])) {
            btw_col[i] <- geosphere::bearing(
              c(lon_num[i - 1], lat_num[i - 1]),
              c(lon_num[i], lat_num[i])
            ) %% 360
          }
        }
      }
      course_df$`BTW (°)` <- round(btw_col, 0)

      # True wind at each mark, taken from the NMEA fixes nearest the mark in
      # both space and time. A mark can be passed more than once, so we walk the
      # marks in leg order and only search fixes at or after the previous mark's
      # rounding time. This monotonic-time constraint assigns each rounding to
      # the correct leg instead of collapsing repeats onto one pass.
      #
      # TWD: prefer the H5000's calculated true wind direction (twd_deg, from the
      # MWD sentence) when present -- it is fully calibrated and needs no COG
      # derivation. When MWD is absent, fall back to deriving true wind from
      # apparent wind by vector-subtracting boat motion:
      #   W_true = W_apparent + V_boat   (as "blowing-toward" velocity vectors)
      # using COG + SOG (ground-referenced), with wind_type == "True" fixes used
      # as-is. TWS is the mean fix true-wind speed; the derived-TWD fallback is
      # gated on mean SOG since COG (the heading proxy) is unreliable at low speed.
      MIN_SOG_FOR_TWD <- 2  # knots
      tws_col <- rep(NA_real_, nrow(course_df))
      twd_col <- rep(NA_real_, nrow(course_df))
      # Per-leg STW polar performance: mean of the per-fix observed-minus-target
      # speed delta over the fixes sailed on that leg (prior mark -> this mark).
      polar_col <- rep(NA_real_, nrow(course_df))
      df_track <- tryCatch(track(), error = function(e) NULL)
      wind_cols <- c("latitude", "longitude", "datetime_local",
                     "tws_knots", "twa_deg", "cog_deg", "sog_knots")
      if (!is.null(df_track) && nrow(df_track) > 0 &&
          all(wind_cols %in% names(df_track))) {
        trk <- df_track |>
          filter(!is.na(latitude), !is.na(longitude), !is.na(datetime_local)) |>
          arrange(datetime_local)
        if (nrow(trk) > 0) {
          prev_time <- min(trk$datetime_local, na.rm = TRUE)
          for (i in seq_len(nrow(course_df))) {
            if (is.na(lat_num[i]) || is.na(lon_num[i])) next
            leg_start <- prev_time
            cand <- trk |> filter(datetime_local >= prev_time)
            if (nrow(cand) == 0) next
            d <- geosphere::distHaversine(
              cbind(cand$longitude, cand$latitude),
              c(lon_num[i], lat_num[i])
            )
            j <- which.min(d)
            t_round <- cand$datetime_local[j]
            prev_time <- t_round

            # Mean STW polar performance over the leg just sailed.
            if ("Polar_Perf_STW" %in% names(trk)) {
              leg_track <- trk |>
                filter(datetime_local > leg_start, datetime_local <= t_round)
              pv <- leg_track$Polar_Perf_STW[!is.na(leg_track$Polar_Perf_STW)]
              if (length(pv) > 0) polar_col[i] <- mean(pv)
            }
            win <- cand |>
              filter(abs(as.numeric(
                difftime(datetime_local, t_round, units = "secs"))) <= 30) |>
              filter(!is.na(tws_knots), !is.na(twa_deg),
                     !is.na(cog_deg), !is.na(sog_knots))
            if (nrow(win) == 0) next

            # Per-fix true-wind vector in the compass frame (x = East, y = North).
            wt <- if ("wind_type" %in% names(win))
              as.character(win$wind_type) else rep(NA_character_, nrow(win))
            already_true <- !is.na(wt) & wt == "True"
            # Measured wind as a "blowing-toward" vector: it blows FROM
            # (COG + TWA), i.e. toward that bearing + 180.
            to_rad  <- (((win$cog_deg + win$twa_deg) + 180) %% 360) * pi / 180
            mx <- win$tws_knots * sin(to_rad)
            my <- win$tws_knots * cos(to_rad)
            # Boat velocity over ground, added only for apparent fixes.
            cog_rad <- win$cog_deg * pi / 180
            bx <- ifelse(already_true, 0, win$sog_knots * sin(cog_rad))
            by <- ifelse(already_true, 0, win$sog_knots * cos(cog_rad))
            tx <- mx + bx
            ty <- my + by

            tws_col[i] <- mean(sqrt(tx^2 + ty^2), na.rm = TRUE)

            # Prefer the H5000's calculated TWD (MWD) if present in the window.
            twd_win <- if ("twd_deg" %in% names(win))
              win$twd_deg[!is.na(win$twd_deg)] else numeric(0)
            sog_win <- mean(win$sog_knots, na.rm = TRUE)
            if (length(twd_win) > 0) {
              twd_col[i] <- (atan2(mean(sin(twd_win * pi / 180)),
                                   mean(cos(twd_win * pi / 180))) * 180 / pi) %% 360
            } else if (!is.na(sog_win) && sog_win >= MIN_SOG_FOR_TWD) {
              # Fallback: derived TWD (FROM) = bearing of mean wind vector + 180.
              twd_col[i] <- (atan2(mean(tx, na.rm = TRUE),
                                   mean(ty, na.rm = TRUE)) * 180 / pi + 180) %% 360
            }
          }
        }
      }
      course_df$`TWS (kt)` <- round(tws_col, 1)
      course_df$`TWD (°)` <- round(twd_col, 0)
      course_df$`Polar Perf` <- round(polar_col, 2)

      # TWA-M: true wind angle relative to the leg bearing (TWD - BTW),
      # normalized to (-180, 180]. Negative = wind off port, positive = starboard.
      twam_col <- ((twd_col - btw_col + 180) %% 360) - 180
      course_df$`TWA-M (°)` <- round(twam_col, 0)

      # Place BTW and TWA-M immediately right of Longitude, and the wind
      # columns immediately left of Distance.
      course_df <- course_df |>
        relocate(`BTW (°)`, `TWA-M (°)`, .after = Longitude) |>
        relocate(`TWS (kt)`, `TWD (°)`, .before = `Distance (nm)`) |>
        relocate(`Polar Perf`, .after = `Distance (nm)`)

      # Distance-weighted race polar performance: each leg's polar perf weighted
      # by that leg's distance, over legs where both are available.
      w_ok <- !is.na(polar_col) & !is.na(dist_nm)
      total_polar <- if (any(w_ok) && sum(dist_nm[w_ok]) > 0) {
        sum(polar_col[w_ok] * dist_nm[w_ok]) / sum(dist_nm[w_ok])
      } else {
        NA_real_
      }

      # Append a total row (wind columns blank; polar perf distance-weighted)
      course_df <- bind_rows(
        course_df |> mutate(`#` = as.character(`#`)),
        tibble(
          `#` = "", Leg = "Total", Mark = "",
          Latitude = "", Longitude = "",
          `TWS (kt)` = NA_real_, `TWD (°)` = NA_real_,
          `Distance (nm)` = round(sum(dist_nm, na.rm = TRUE), 2),
          `Polar Perf` = round(total_polar, 2)
        )
      )
    }

    datatable(
      course_df,
      options = list(dom = "t", ordering = FALSE, scrollX = TRUE),
      rownames = FALSE
    )
  })

  # ---- Crew table for Race Analytics tab ----
  output$race_crew_table <- renderDT({
    cal_row <- ra_selected_row()
    req(cal_row)

    race_name     <- cal_row$race
    race_date_fmt <- format(cal_row$race_date, "%m/%d/%Y")

    # Build a long-format crew table from the wide crew/position columns
    crew_names     <- character()
    crew_positions <- character()

    for (i in 1:10) {
      c_col <- paste0("crew_", i)
      p_col <- paste0("position_", i)
      if (c_col %in% names(cal_row) && !is.na(cal_row[[c_col]])) {
        crew_names     <- c(crew_names, cal_row[[c_col]])
        crew_positions <- c(crew_positions,
          if (p_col %in% names(cal_row)) cal_row[[p_col]] else NA_character_)
      }
    }

    if (length(crew_names) == 0) {
      crew_df <- tibble(`#` = integer(), Crew = character(), Position = character())
    } else {
      crew_df <- tibble(
        `#`      = seq_along(crew_names),
        Crew     = crew_names,
        Position = crew_positions
      )
    }

    datatable(
      crew_df,
      options = list(dom = "t", ordering = FALSE, scrollX = TRUE),
      rownames = FALSE
    )
  })

  # Reactive wrapper for the full track dataset. Validates that track data
  # exists before allowing downstream reactives to proceed.
  track_all <- reactive({
    df <- data_rds$track_all
    validate(need(nrow(df) > 0, "No track data in RDS file."))
    df
  })

  # Update the Race Seasons series dropdown when the season selection changes.
  # This implements the cascading filter pattern: Season → Series → Race.
  observeEvent(input$season_select, {
    req(input$season_select)
    cal <- data_rds$race_calendar
    if (input$season_select != "All") {
      cal <- cal |> filter(!is.na(season), season == input$season_select)
    }
    series_vals <- sort(unique(cal$series[!is.na(cal$series) & nzchar(cal$series)]))
    updateSelectInput(session, "season_series_select",
                      choices = c("All", series_vals))
  })
  
  # ---------- SEASON RACES: Filtered race calendar with pre-computed stats ----------
  # This reactive provides the data behind the three Race Seasons tables (season
  # summary, series summary, and individual race list). It filters the race
  # calendar by the user's season and series selections and returns one row per
  # race with all metadata and performance stats.
  #
  # PERFORMANCE NOTE: This reactive was previously the app's most expensive
  # user-triggered operation. It performed a many-to-many inner_join between
  # the full track dataset (millions of GPS points) and the race calendar to
  # compute per-race aggregate statistics (avg speed, max wind, polar perf, etc.)
  # on every filter change. That join is now eliminated — the stats are pre-
  # computed during the RDS rebuild step (see rebuild_rds_from_raw()) and stored
  # directly in race_calendar. This reactive is now a simple filter + group on
  # the small calendar dataframe (~50–200 rows), making it effectively instant.
  #
  # bindCache() keyed on the season and series inputs avoids even the trivial
  # re-filtering cost when users navigate back to a previously viewed combination.
  season_races <- reactive({
    req(input$season_select)
    cal <- data_rds$race_calendar |>
      filter(!is.na(season))

    if (input$season_select != "All") {
      cal <- cal |> filter(season == input$season_select)
    }
    if (isTruthy(input$season_series_select) && input$season_series_select != "All") {
      cal <- cal |> filter(!is.na(series), series == input$season_series_select)
    }
    if (nrow(cal) == 0) return(tibble())

    # Group by race + date to handle same-name races on different dates.
    # All columns use first() because stats are pre-computed (one value per race).
    cal |>
      group_by(race, race_date) |>
      summarise(
        season         = first(season),
        series         = first(series),
        place          = first(place),
        fleet          = first(fleet),
        length         = first(length),
        start          = min(start, na.rm = TRUE),
        end            = max(end, na.rm = TRUE),
        duration_hrs   = first(duration_hrs),
        days_on_water  = first(days_on_water),
        avg_stw        = first(avg_stw),
        max_stw        = first(max_stw),
        max_tws        = first(max_tws),
        polar_perf_stw = first(polar_perf_stw),
        nmea_count     = first(nmea_count),
        .groups = "drop"
      ) |>
      arrange(start)
  }) |> bindCache(input$season_select, input$season_series_select)
  
  # Season summary table: one row per season
  output$season_summary_table <- renderDT({
    cal_summary <- season_races()
    if (nrow(cal_summary) == 0) {
      return(datatable(tibble::tibble(Message = "No races found."),
                       options = list(dom = "t"), rownames = FALSE))
    }
    
    summary_df <- cal_summary |>
      group_by(Season = season) |>
      summarise(
        Races   = n(),
        Days    = n_distinct(as.Date(start)),
        .avg_place = {pn <- suppressWarnings(as.numeric(place)); if (all(is.na(pn))) NA_real_ else round(mean(pn, na.rm = TRUE), 1)},
        .avg_fleet = {fn <- suppressWarnings(as.numeric(fleet)); if (all(is.na(fn))) NA_real_ else round(mean(fn, na.rm = TRUE), 1)},
        NM          = round(sum(length, na.rm = TRUE), 1),
        Hours       = round(sum(duration_hrs, na.rm = TRUE), 1),
        `Avg STW`   = if (all(is.na(avg_stw))) NA_real_ else round(mean(avg_stw, na.rm = TRUE), 1),
        `Max STW`   = if (all(is.na(max_stw))) NA_real_ else round(max(max_stw, na.rm = TRUE), 1),
        `Max TWS`   = if (all(is.na(max_tws))) NA_real_ else round(max(max_tws, na.rm = TRUE), 1),
        `Polar Perf` = if (all(is.na(polar_perf_stw))) NA_real_ else round(mean(polar_perf_stw, na.rm = TRUE), 2),
        `NMEA Pts`  = sum(nmea_count),
        .groups = "drop"
      ) |>
      mutate(
        `Place / Fleet / %` = ifelse(
          is.na(.avg_place) | is.na(.avg_fleet), NA_character_,
          paste0(.avg_place, "<br>", .avg_fleet, "<br>", round(.avg_place / .avg_fleet * 100), "%")
        ),
        .before = NM
      ) |>
      select(-`.avg_place`, -`.avg_fleet`) |>
      relocate(Hours, .before = `Place / Fleet / %`)
    
    avg_cols <- c("Avg STW", "Polar Perf")
    max_cols <- c("Max STW", "Max TWS")
    sum_int_cols <- c("Races", "Days")
    
    pfp_idx <- which(names(summary_df) == "Place / Fleet / %")
    header_names <- names(summary_df)
    header_names[pfp_idx] <- "Place<br>Fleet<br>%"
    
    shared_widths <- c("50px","55px","105px","45px","60px","60px","60px","70px","75px")
    col_widths_season <- c("200px", "60px", shared_widths)
    sketch <- htmltools::withTags(table(
      class = "display",
      tags$colgroup(lapply(col_widths_season, function(w) tags$col(style = paste0("width:", w)))),
      thead(tr(lapply(header_names, function(n) th(HTML(n))))),
      tfoot(tr(lapply(header_names, function(n) th(HTML(n)))))
    ))
    
    datatable(summary_df,
              container = sketch,
              escape = -(pfp_idx),
              options = list(
                dom = "t", ordering = FALSE, autoWidth = FALSE,
                columnDefs = list(
                  list(className = "dt-right", targets = c(which(names(summary_df) == "NM") - 1, ncol(summary_df) - 1)),
                  list(className = "dt-center", targets = which(names(summary_df) == "Place / Fleet / %") - 1)
                ),
                footerCallback = DT::JS("
                  function(row, data, start, end, display) {
                    var api = this.api();
                    var ncols = api.columns().count();
                    var avgCols = ['Avg STW', 'Polar Perf'];
                    var maxCols = ['Max STW', 'Max TWS'];
                    var sumIntCols = ['Races', 'Days'];
                    $(api.column(0).footer()).html('Total');
                    for (var col = 1; col < ncols; col++) {
                      var header = $(api.column(col).header()).text();
                      if (header.indexOf('Fleet') >= 0) {
                        var places = [], fleets = [];
                        api.column(col, {page:'current'}).data().each(function(v) {
                          var el = $('<div>').html(v);
                          var txt = el.text();
                          var parts = txt.match(/[\\d.]+/g);
                          if (parts && parts.length >= 2) {
                            var p = parseFloat(parts[0]);
                            var f = parseFloat(parts[1]);
                            if (!isNaN(p) && !isNaN(f)) { places.push(p); fleets.push(f); }
                          }
                        });
                        if (places.length) {
                          var ap = (places.reduce(function(a,b){return a+b;},0)/places.length).toFixed(1);
                          var af = (fleets.reduce(function(a,b){return a+b;},0)/fleets.length).toFixed(1);
                          var pct = Math.round(ap / af * 100);
                          $(api.column(col).footer()).html(ap + '<br>' + af + '<br>' + pct + '%');
                        } else {
                          $(api.column(col).footer()).html('');
                        }
                        continue;
                      }
                      var vals = [];
                      api.column(col, {page:'current'}).data().each(function(v) {
                        var x = parseFloat(String(v).replace(/,/g,''));
                        if (!isNaN(x)) vals.push(x);
                      });
                      if (header === 'Polar Perf') {
                        $(api.column(col).footer()).html(vals.length ? (vals.reduce(function(a,b){return a+b;},0)/vals.length).toFixed(2) : '');
                      } else if (avgCols.indexOf(header) >= 0) {
                        $(api.column(col).footer()).html(vals.length ? (vals.reduce(function(a,b){return a+b;},0)/vals.length).toFixed(1) : '');
                      } else if (maxCols.indexOf(header) >= 0) {
                        $(api.column(col).footer()).html(vals.length ? Math.max.apply(null, vals).toFixed(1) : '');
                      } else if (header === 'NMEA Pts') {
                        var sum = vals.reduce(function(a,b){return a+b;},0);
                        $(api.column(col).footer()).html(vals.length ? sum.toLocaleString() : '');
                      } else if (sumIntCols.indexOf(header) >= 0) {
                        var sum = vals.reduce(function(a,b){return a+b;},0);
                        $(api.column(col).footer()).html(vals.length ? sum.toFixed(0) : '');
                      } else if (header === 'NM') {
                        var sum = vals.reduce(function(a,b){return a+b;},0);
                        $(api.column(col).footer()).html(vals.length ? sum.toFixed(1) : '').css('text-align', 'right');
                      } else {
                        var sum = vals.reduce(function(a,b){return a+b;},0);
                        $(api.column(col).footer()).html(vals.length ? sum.toFixed(1) : '');
                      }
                    }
                  }
                ")
              ),
              rownames = FALSE) |>
      formatRound(columns = "NM", digits = 1) |>
      formatRound(columns = c("Hours", "Avg STW", "Max STW", "Max TWS"), digits = 1) |>
      formatRound(columns = "Polar Perf", digits = 2) |>
      formatRound(columns = "NMEA Pts", digits = 0)
  })
  
  # Series summary table: one row per series
  output$season_series_summary_table <- renderDT({
    cal_summary <- season_races()
    if (nrow(cal_summary) == 0) {
      return(datatable(tibble::tibble(Message = "No races found."),
                       options = list(dom = "t"), rownames = FALSE))
    }
    
    summary_df <- cal_summary |>
      mutate(series_label = ifelse(is.na(series) | series == "", "(No Series)", series)) |>
      group_by(Series = series_label) |>
      summarise(
        Races   = n(),
        Days    = n_distinct(as.Date(start)),
        .avg_place = {pn <- suppressWarnings(as.numeric(place)); if (all(is.na(pn))) NA_real_ else round(mean(pn, na.rm = TRUE), 1)},
        .avg_fleet = {fn <- suppressWarnings(as.numeric(fleet)); if (all(is.na(fn))) NA_real_ else round(mean(fn, na.rm = TRUE), 1)},
        NM          = round(sum(length, na.rm = TRUE), 1),
        Hours       = round(sum(duration_hrs, na.rm = TRUE), 1),
        `Avg STW`   = if (all(is.na(avg_stw))) NA_real_ else round(mean(avg_stw, na.rm = TRUE), 1),
        `Max STW`   = if (all(is.na(max_stw))) NA_real_ else round(max(max_stw, na.rm = TRUE), 1),
        `Max TWS`   = if (all(is.na(max_tws))) NA_real_ else round(max(max_tws, na.rm = TRUE), 1),
        `Polar Perf` = if (all(is.na(polar_perf_stw))) NA_real_ else round(mean(polar_perf_stw, na.rm = TRUE), 2),
        `NMEA Pts`  = sum(nmea_count),
        .groups = "drop"
      ) |>
      mutate(
        `Place / Fleet / %` = ifelse(
          is.na(.avg_place) | is.na(.avg_fleet), NA_character_,
          paste0(.avg_place, "<br>", .avg_fleet, "<br>", round(.avg_place / .avg_fleet * 100), "%")
        ),
        .before = NM
      ) |>
      select(-`.avg_place`, -`.avg_fleet`) |>
      relocate(Hours, .before = `Place / Fleet / %`)
    
    pfp_idx <- which(names(summary_df) == "Place / Fleet / %")
    header_names <- names(summary_df)
    header_names[pfp_idx] <- "Place<br>Fleet<br>%"
    
    shared_widths <- c("50px","55px","105px","45px","60px","60px","60px","70px","75px")
    col_widths_series <- c("200px", "60px", shared_widths)
    sketch <- htmltools::withTags(table(
      class = "display",
      tags$colgroup(lapply(col_widths_series, function(w) tags$col(style = paste0("width:", w)))),
      thead(tr(lapply(header_names, function(n) th(HTML(n))))),
      tfoot(tr(lapply(header_names, function(n) th(HTML(n)))))
    ))
    
    datatable(summary_df,
              container = sketch,
              escape = -(pfp_idx),
              options = list(
                dom = "t", ordering = FALSE, autoWidth = FALSE,
                columnDefs = list(
                  list(className = "dt-right", targets = c(which(names(summary_df) == "NM") - 1, ncol(summary_df) - 1)),
                  list(className = "dt-center", targets = pfp_idx - 1)
                ),
                footerCallback = DT::JS("
                  function(row, data, start, end, display) {
                    var api = this.api();
                    var ncols = api.columns().count();
                    var avgCols = ['Avg STW', 'Polar Perf'];
                    var maxCols = ['Max STW', 'Max TWS'];
                    var sumIntCols = ['Races', 'Days'];
                    $(api.column(0).footer()).html('Total');
                    for (var col = 1; col < ncols; col++) {
                      var header = $(api.column(col).header()).text();
                      if (header.indexOf('Fleet') >= 0) {
                        var places = [], fleets = [];
                        api.column(col, {page:'current'}).data().each(function(v) {
                          var el = $('<div>').html(v);
                          var txt = el.text();
                          var parts = txt.match(/[\\d.]+/g);
                          if (parts && parts.length >= 2) {
                            var p = parseFloat(parts[0]);
                            var f = parseFloat(parts[1]);
                            if (!isNaN(p) && !isNaN(f)) { places.push(p); fleets.push(f); }
                          }
                        });
                        if (places.length) {
                          var ap = (places.reduce(function(a,b){return a+b;},0)/places.length).toFixed(1);
                          var af = (fleets.reduce(function(a,b){return a+b;},0)/fleets.length).toFixed(1);
                          var pct = Math.round(ap / af * 100);
                          $(api.column(col).footer()).html(ap + '<br>' + af + '<br>' + pct + '%');
                        } else {
                          $(api.column(col).footer()).html('');
                        }
                        continue;
                      }
                      var vals = [];
                      api.column(col, {page:'current'}).data().each(function(v) {
                        var x = parseFloat(String(v).replace(/,/g,''));
                        if (!isNaN(x)) vals.push(x);
                      });
                      if (header === 'Polar Perf') {
                        $(api.column(col).footer()).html(vals.length ? (vals.reduce(function(a,b){return a+b;},0)/vals.length).toFixed(2) : '');
                      } else if (avgCols.indexOf(header) >= 0) {
                        $(api.column(col).footer()).html(vals.length ? (vals.reduce(function(a,b){return a+b;},0)/vals.length).toFixed(1) : '');
                      } else if (maxCols.indexOf(header) >= 0) {
                        $(api.column(col).footer()).html(vals.length ? Math.max.apply(null, vals).toFixed(1) : '');
                      } else if (header === 'NMEA Pts') {
                        var sum = vals.reduce(function(a,b){return a+b;},0);
                        $(api.column(col).footer()).html(vals.length ? sum.toLocaleString() : '');
                      } else if (sumIntCols.indexOf(header) >= 0) {
                        var sum = vals.reduce(function(a,b){return a+b;},0);
                        $(api.column(col).footer()).html(vals.length ? sum.toFixed(0) : '');
                      } else if (header === 'NM') {
                        var sum = vals.reduce(function(a,b){return a+b;},0);
                        $(api.column(col).footer()).html(vals.length ? sum.toFixed(1) : '').css('text-align', 'right');
                      } else {
                        var sum = vals.reduce(function(a,b){return a+b;},0);
                        $(api.column(col).footer()).html(vals.length ? sum.toFixed(1) : '');
                      }
                    }
                  }
                ")
              ),
              rownames = FALSE) |>
      formatRound(columns = "NM", digits = 1) |>
      formatRound(columns = c("Hours", "Avg STW", "Max STW", "Max TWS"), digits = 1) |>
      formatRound(columns = "Polar Perf", digits = 2) |>
      formatRound(columns = "NMEA Pts", digits = 0)
  })
  
  output$season_table <- renderDT({
    cal_summary <- season_races()
    
    if (nrow(cal_summary) == 0) {
      return(datatable(tibble::tibble(Message = "No races found for this selection.")))
    }
    
    res <- cal_summary |>
      transmute(
        `#` = row_number(),
        Race = paste0(
          format(as.Date(start), "%m/%d/%Y"),
          ifelse(is.na(series) | series == "", "", paste0("<br>", series)),
          "<br>", race
        ),
        .place_num = suppressWarnings(as.numeric(place)),
        .fleet_num = suppressWarnings(as.numeric(fleet)),
        `Place / Fleet / %` = ifelse(
          is.na(.place_num) | is.na(.fleet_num), "",
          paste0(as.integer(.place_num), "<br>", as.integer(.fleet_num), "<br>", round(.place_num / .fleet_num * 100), "%")
        ),
        Days       = days_on_water,
        Hours      = ifelse(is.na(duration_hrs), NA_real_, round(duration_hrs, 1)),
        NM         = ifelse(is.na(length), NA_real_, round(length, 1)),
        `Avg STW`    = ifelse(is.na(avg_stw), NA_real_, round(avg_stw, 1)),
        `Max STW`    = ifelse(is.na(max_stw), NA_real_, round(max_stw, 1)),
        `Max TWS`    = ifelse(is.na(max_tws), NA_real_, round(max_tws, 1)),
        `Polar Perf` = ifelse(is.na(polar_perf_stw), NA_real_, round(polar_perf_stw, 2)),
        `NMEA Pts`   = nmea_count
      ) |>
      select(-`.place_num`, -`.fleet_num`) |>
      relocate(Days, Hours, .before = `Place / Fleet / %`)
    
    pfp_col <- which(names(res) == "Place / Fleet / %") - 1
    pfp_idx <- pfp_col + 1
    header_names <- names(res)
    header_names[pfp_idx] <- "Place<br>Fleet<br>%"
    
    shared_widths <- c("50px","55px","105px","45px","60px","60px","60px","70px","75px")
    col_widths_detail <- c("35px", "225px", shared_widths)
    sketch <- htmltools::withTags(table(
      class = "display",
      tags$colgroup(lapply(col_widths_detail, function(w) tags$col(style = paste0("width:", w)))),
      thead(tr(lapply(header_names, function(n) th(HTML(n))))),
      tfoot(tr(lapply(header_names, function(n) th(HTML(n)))))
    ))
    
    datatable(
      res,
      container = sketch,
      selection = "none",
      escape = c(-2, -pfp_idx),
      options = list(
        dom = "t",
        pageLength = nrow(res),
        autoWidth = FALSE,
        columnDefs = list(
          list(className = "dt-left", targets = 0),
          list(className = "dt-center", targets = pfp_col),
          list(className = "dt-right", targets = setdiff(seq(pfp_col, ncol(res) - 1), pfp_col))
        ),
        footerCallback = DT::JS("
          function(row, data, start, end, display) {
            var api = this.api();
            var ncols = api.columns().count();
            var avgCols = ['Avg STW', 'Polar Perf'];
            var maxCols = ['Max STW', 'Max TWS'];
            var sumIntCols = ['Days'];
            var nRaces = api.column(0, {page:'current'}).data().length;
            $(api.column(0).footer()).html(nRaces);
            $(api.column(1).footer()).html('Total');
            for (var col = 2; col < ncols; col++) {
              var header = $(api.column(col).header()).text();
              if (header.indexOf('Fleet') >= 0) {
                var places = [], fleets = [];
                api.column(col, {page:'current'}).data().each(function(v) {
                  var el = $('<div>').html(v);
                  var txt = el.text();
                  var parts = txt.match(/[\\d.]+/g);
                  if (parts && parts.length >= 2) {
                    var p = parseFloat(parts[0]);
                    var f = parseFloat(parts[1]);
                    if (!isNaN(p) && !isNaN(f)) { places.push(p); fleets.push(f); }
                  }
                });
                if (places.length) {
                  var ap = (places.reduce(function(a,b){return a+b;},0)/places.length).toFixed(1);
                  var af = (fleets.reduce(function(a,b){return a+b;},0)/fleets.length).toFixed(1);
                  var pct = Math.round(ap / af * 100);
                  $(api.column(col).footer()).html(ap + '<br>' + af + '<br>' + pct + '%');
                } else {
                  $(api.column(col).footer()).html('');
                }
                continue;
              }
              var vals = [];
              api.column(col, {page:'current'}).data().each(function(v) {
                var x = parseFloat(String(v).replace(/,/g,''));
                if (!isNaN(x)) vals.push(x);
              });
              if (header === 'Polar Perf') {
                $(api.column(col).footer()).html(vals.length ? (vals.reduce(function(a,b){return a+b;},0)/vals.length).toFixed(2) : '');
              } else if (avgCols.indexOf(header) >= 0) {
                $(api.column(col).footer()).html(vals.length ? (vals.reduce(function(a,b){return a+b;},0)/vals.length).toFixed(1) : '');
              } else if (maxCols.indexOf(header) >= 0) {
                $(api.column(col).footer()).html(vals.length ? Math.max.apply(null, vals).toFixed(1) : '');
              } else if (header === 'NMEA Pts') {
                var sum = vals.reduce(function(a,b){return a+b;},0);
                $(api.column(col).footer()).html(vals.length ? sum.toLocaleString() : '');
              } else if (sumIntCols.indexOf(header) >= 0) {
                var sum = vals.reduce(function(a,b){return a+b;},0);
                $(api.column(col).footer()).html(vals.length ? sum.toFixed(0) : '');
              } else if (header === 'NM') {
                var sum = vals.reduce(function(a,b){return a+b;},0);
                $(api.column(col).footer()).html(vals.length ? sum.toFixed(1) : '').css('text-align', 'right');
              } else {
                var sum = vals.reduce(function(a,b){return a+b;},0);
                $(api.column(col).footer()).html(vals.length ? sum.toFixed(1) : '');
              }
            }
          }
        ")
      ),
      rownames = FALSE
    ) |>
      formatRound(columns = "NM", digits = 1) |>
      formatRound(columns = c("Hours", "Avg STW", "Max STW", "Max TWS"), digits = 1) |>
      formatRound(columns = "Polar Perf", digits = 2) |>
      formatRound(columns = "NMEA Pts", digits = 0)
  })
  
  # AI Race Season Report
  output$season_narrative_card <- renderUI({
    req(input$season_select)
    
    if (input$season_select == "All") {
      # Show AI Performance Report spanning all seasons
      text <- generate_performance_narrative(data_rds$race_calendar, data_rds$track_all)
      paras <- strsplit(text, "\n\n", fixed = TRUE)[[1]]
      body_html <- paste0("<p>", htmltools::htmlEscape(paras), "</p>", collapse = "\n")
      
      return(div(
        class = "race-narrative",
        div(
          class = "race-narrative-title",
          HTML('<svg width="18" height="18" viewBox="0 0 24 24" fill="none" stroke="rgba(100,180,255,0.7)" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><path d="M12 20h9"/><path d="M16.5 3.5a2.121 2.121 0 013 3L7 19l-4 1 1-4L16.5 3.5z"/></svg>'),
          "AI Narrative - Wings Sailing Program"
        ),
        div(class = "race-narrative-body", HTML(body_html))
      ))
    }
    
    season_name <- input$season_select
    cal <- data_rds$race_calendar |>
      filter(!is.na(season), season == season_name)
    
    if (isTruthy(input$season_series_select) && input$season_series_select != "All") {
      cal <- cal |> filter(!is.na(series), series == input$season_series_select)
    }
    
    if (nrow(cal) == 0) return(NULL)
    
    text <- generate_season_narrative(season_name, cal, data_rds$track_all)
    
    paras <- strsplit(text, "\n\n", fixed = TRUE)[[1]]
    body_html <- paste0("<p>", htmltools::htmlEscape(paras), "</p>", collapse = "\n")
    
    div(
      class = "race-narrative",
      div(
        class = "race-narrative-title",
        HTML('<svg width="18" height="18" viewBox="0 0 24 24" fill="none" stroke="rgba(100,180,255,0.7)" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><path d="M12 20h9"/><path d="M16.5 3.5a2.121 2.121 0 013 3L7 19l-4 1 1-4L16.5 3.5z"/></svg>'),
        "AI Race Season Report"
      ),
      div(class = "race-narrative-body", HTML(body_html))
    )
  }) |> bindCache(input$season_select, input$season_series_select)

  # ---------- TRACK DATA: Filter to selected race ----------
  # Filters the full track dataset to only the GPS points falling within the
  # selected race's start-end time window. This filtered subset is the primary
  # data source for all Race Analytics tab outputs: the Leaflet map, the boat
  # speed time-series plot, and the polar performance tables.
  #
  # The filter operation itself is fast (O(n) scan of in-memory data), but
  # downstream renderings (map polylines, ggplot with ~100K points) can be
  # costly. bindCache() keyed on the three Race Analytics dropdown inputs
  # ensures that navigating back to a previously viewed race reuses the cached
  # filtered dataset rather than re-scanning the full track.
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
  }) |> bindCache(input$ra_season_select, input$ra_series_select, input$ra_race_select)

  # ---------- LEAFLET MAP: GPS track rendered as colored polylines ----------
  # Renders the race's GPS track on an interactive OpenStreetMap tile layer.
  # Track segments are split by local date and color-coded so multi-day races
  # (e.g., offshore events) show each day's sailing in a distinct color.
  # The map uses the Leaflet JavaScript library via the R leaflet package,
  # which communicates with the Shiny server through websockets.
  #
  # bindCache() avoids re-rendering the map widget (including re-transmitting
  # all polyline coordinates to the browser) when revisiting a race.
  output$map <- renderLeaflet({
    # Track data may be absent for a race; fetch defensively so the map can
    # still render the course marks / rhumb line when only that data exists.
    df <- tryCatch(track(), error = function(e) NULL)
    has_track <- !is.null(df) && nrow(df) > 0

    marks_pts <- ra_course_points()
    if (!is.null(marks_pts) && nrow(marks_pts) > 0) {
      marks_pts <- marks_pts |>
        filter(!is.na(lat_dd), !is.na(lon_dd)) |>
        arrange(order)
    }
    has_marks <- !is.null(marks_pts) && nrow(marks_pts) > 0

    validate(need(has_track || has_marks,
                  "No track or course data to display for selected filters."))

    m <- leaflet() |> addTiles()

    # GPS track (only when NMEA data exists), color-coded by local day
    day_labels <- character(0)
    if (has_track) {
      days       <- sort(unique(df$day_local_ui))
      day_labels <- format(days, "%Y-%m-%d")

      pal <- colorFactor(
        brewer.pal(max(3, min(9, length(days))), "Set1"),
        day_labels
      )

      for (i in seq_along(days)) {
        dfi <- df |> filter(day_local_ui == days[i]) |> arrange(datetime_local)
        m <- m |>
          addPolylines(
            data    = dfi,
            lng     = ~longitude,
            lat     = ~latitude,
            color   = pal(day_labels[i]),
            weight  = 3,
            opacity = 0.9,
            group   = day_labels[i]
          )
      }
    }

    # Course marks and rhumb-line course
    if (has_marks) {
      if (nrow(marks_pts) > 1) {
        m <- m |>
          addPolylines(
            data      = marks_pts,
            lng       = ~lon_dd,
            lat       = ~lat_dd,
            color     = "#999999",
            weight    = 1,
            opacity   = 0.8,
            group     = "Course"
          )
      }
      m <- m |>
        addCircleMarkers(
          data         = marks_pts,
          lng          = ~lon_dd,
          lat          = ~lat_dd,
          radius       = 6,
          color        = "#111111",
          weight       = 1,
          fillColor    = "#ffd400",
          fillOpacity  = 0.9,
          label        = ~mark,
          labelOptions = labelOptions(
            noHide    = TRUE,
            direction = "top",
            textsize  = "12px",
            opacity   = 0.9
          )
        )
    }

    if (has_track) {
      m <- m |>
        addLegend(
          position = "bottomright",
          colors   = pal(day_labels),
          labels   = day_labels,
          title    = "Day (local)"
        )
    }

    m
  }) |> bindCache(input$ra_season_select, input$ra_series_select, input$ra_race_select)

  # ---------- BOAT SPEED PLOT: SOG and STW time series with moving average ----------
  # Renders a ggplot2 line chart showing Speed Over Ground (SOG) and Speed
  # Through Water (STW) over time for the selected race. Two layers are
  # overlaid for each speed type:
  #   - "Raw": every GPS fix, rendered as a thin semi-transparent line to show
  #     the full instrument noise and variability.
  #   - "1-min MA": a 1-minute moving average (computed via floor_date binning
  #     and group_by mean), rendered as a thicker opaque line showing the
  #     smoothed trend.
  #
  # The SOG-STW differential reveals current effects: when SOG > STW, the boat
  # is being helped by current; when SOG < STW, it's fighting current.
  #
  # bindCache() avoids re-computing the plot (including the pivot_longer and
  # moving average calculations on potentially ~100K GPS points) when the user
  # navigates back to a previously viewed race.
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
  }) |> bindCache(input$ra_season_select, input$ra_series_select, input$ra_race_select)
  
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
        # Fold TWA to 0-180 (port/starboard symmetric)
        twa_folded = ifelse(twa_deg > 180, 360 - twa_deg, twa_deg),
        tws_idx = snap_nearest_idx(tws_knots, polar_tws_vals),
        twa_idx = snap_nearest_idx(twa_folded, polar_twa_vals),
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
  
  # ---- FILE MANAGEMENT PASSWORD GATE ----
  FILE_MGMT_PASSWORD <- "wings"
  
  output$file_mgmt_controls <- renderUI({
    if (!isTruthy(input$file_mgmt_password) ||
        input$file_mgmt_password != FILE_MGMT_PASSWORD) {
      return(tags$p("Enter the correct password to access file management controls.",
                    style = "color: rgba(232,237,246,0.45); font-size: 13px;"))
    }
    tagList(
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