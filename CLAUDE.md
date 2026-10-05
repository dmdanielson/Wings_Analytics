# Wings Analytics — instructions for Claude Code

## Project
R Shiny app analysing NMEA instrument data from the J/112E "Wings", racing out of
Davis Island Yacht Club, Tampa. Deployed to shinyapps.io.
Implementation plan: Wings_Analytics_Implementation_Plan.docx (kept by the owner).

## Layout
- app.R — UI, server and startup processing. Being broken up; see the plan.
- R/narratives.R — race, season and performance narrative generators
- R/parsing.R — NMEA sentence parsers and sync helpers
- R/build_rds.R — rebuild_rds_from_raw(): raw logs -> track_data.rds
- tests/testthat/ — snapshot tests (see Testing)
- track_data.rds — generated data, gitignored. race_narratives.rds — cached narratives.
- KNOWN_ISSUES.md — bugs deliberately deferred. Do not fix them unless asked.
- dev/ — analysis scripts, kept in the repo. dev/plots/ — PNG output.

## External data (local only, not in the repo)
- Raw VDR logs: G:/My Drive/Personal/Mike/Sailing/Data/vdr_<ISO8601>Z.txt, rotated hourly.
  Logged by OpenCPN via a Yacht Devices gateway on a B&G H5000 network.
  The archive starts around 2025-11-01. Earlier races exist only in track_data.rds.
- Race Calendar.xlsx — race metadata, crew, course marks.
  Season format "2025-2026"; seasons run June to May.
- Polars.xlsx — ORC 2025 reference polar (TWS x TWA -> target boat speed).

## Data schema — track_data.rds is a named list
- track_all: one row per GPS fix. Columns, in file order: line_index, datetime_utc,
  datetime_local, day_local, sentence, latitude, longitude, sog_knots, cog_deg,
  stw_knots, twa_deg, tws_knots, wind_type, twd_deg, polar_bsp_knots,
  Polar_Perf_STW, Polar_Perf_SOG, race, helm, headsail.
- race_calendar: one row per race segment, with pre-computed stats
  (avg_stw, max_stw, max_tws, polar_perf_stw, duration_hrs, nmea_count, and more).
- polar_ref (wide) and polar_ref_long (tws, twa, bsp_ref).
- marks_ref: mark, lat, lon — degrees-decimal-minutes strings, e.g. "27° 54.22".
Before writing code against a column, confirm it exists:
  Rscript -e "names(readRDS('track_data.rds')$track_all)"

## Running R
- All tests:  Rscript -e "testthat::test_dir('tests/testthat')"
- One file:   Rscript -e "testthat::test_file('tests/testthat/test-parsing.R')"
- Never source("app.R") — it launches the app and runs startup processing.
- Save plots to dev/plots/ as PNG. Never try to display them.

## Testing
- test-narratives.R, test-parsing.R: fast snapshot tests of pure functions.
- test-app-polar.R, test-app-season.R: shinytest2. The full suite takes ~100 s.
  They snapshot rendered <tbody> text, not images. The polar test pins the race
  "Regatta del Sol" and asserts its label, so index drift fails loudly.
- Snapshot tests must never skip. tests/testthat/setup.R sets NOT_CRAN=true.
- During a refactor, a snapshot failure means the refactor changed behaviour.

## Rules
- Never git commit, git push, or call snapshot_accept(). The user does those.
- Never write to, overwrite or delete any track_data*.rds file.
  Write new data to a new filename and say so.
- When moving code between files, move it verbatim: no renames, no reformatting,
  no "while I'm here" improvements. Improvements are separate changes.
- Show a plan before any change touching more than one file.
- Functions in R/ resolve globals (LOCAL_TZ, data_dir, paths) from the global
  environment. In tests, set them with assign(..., envir = globalenv()).
- rsconnect/ must stay committed: it holds the shinyapps.io appId.
- New code uses |>. Existing code mixes |> and %>%; do not convert it while moving.

## App structure
- Tabs: Race Seasons, Race Analytics, Boat Performance, Social, About.
- The five polar tables are under Race Analytics -> Data, not Boat Performance.
  Boat Performance holds static ORC speed-guide pages.
- The three Race Seasons tables must stay column-aligned (table-layout: fixed,
  shared column widths). Their JavaScript footer callbacks compute totals.
- Cascading filters: Season -> Series -> Race.
- IS_DEPLOYED disables raw-data rebuilds on shinyapps.io.
- Narratives are deterministic: hash-seeded phrase selection.
- The About tab has a local-only file-management panel.

## Data facts
Established from the data. If you find evidence against one, say so rather than
silently working around it.
- Heel: positive = heeled to starboard. Verified on 114k samples.
- Rudder zero offset: -2.40 deg (add +2.40). Fitted, provisional.
- Instruments uncalibrated before 2026-04-30. Boat speed read ~11.8% high then.
- MWV wind speed is in m/s. MWD wind speed is in knots.
- Cleaning: SOG > 15 kn dropped; fixes within 1,000 ft of the home dock dropped;
  isolated GPS jumps implying > 30 kn dropped; STW < 2 kn excluded from speed stats.
- Engine: Volvo Penta D1-30. Hours are entered by hand; not in the NMEA logs.
