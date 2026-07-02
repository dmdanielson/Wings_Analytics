# Wings Analytics — Project Memory

## Overview

Sailing performance analytics platform for a **J112e sailboat ("Wings")** racing out of Davis Island Yacht Club (DIYC). Built as a single-file **R Shiny** app (`app.R`, ~3,500 lines) deployed to shinyapps.io (free tier). Transforms raw NMEA GPS/instrument data into interactive dashboards with maps, speed analysis, polar performance, and AI-generated race narratives.

## Key Files

| File | Purpose |
|------|---------|
| `app.R` | Entire Shiny application (UI + server + data pipeline) |
| `track_data.rds` | Serialized processed data (~17 MB): track, calendar, polars |
| `race_narratives.rds` | Cached deterministic AI race narratives |
| `wings_analytics_documentation.qmd` | Technical documentation (Quarto source) |
| `www/` | Static assets (images, rendered HTML docs) |

## External Data Sources (local only, not in repo)

- **NMEA `.txt` files** — Raw instrument logs from OpenCPN via B&G/Yacht Devices gateway (Google Drive: `G:/My Drive/Personal/Mike/Sailing/Data`)
- **`Race Calendar.xlsx`** — Multi-sheet workbook (one sheet per season, e.g. "2024-25 Winter") with race metadata
- **`Polars.xlsx`** — J112e reference polar table (TWS × TWA → target boat speed)

## Data Schemas

### `track_data.rds` — Named list with 4 elements:

1. **`track_all`**: Main track data (one row per NMEA fix)
   - `datetime_utc`, `datetime_local`, `lat`, `lon`, `sog`, `cog`, `stw`, `twa`, `tws`, `wind_type`
   - `polar_perf_stw`, `polar_perf_sog` (observed − reference speed delta)
   - `race`, `series`, `season`, `start`, `end`

2. **`race_calendar`**: Normalized race metadata
   - `race`, `series`, `season`, `start`, `end`, `place`, `fleet`, `length` (NM)
   - `duration_hrs`, `avg_stw`, `max_stw`, `max_tws`, `polar_perf_stw`, `nmea_count`
   - `days_on_water`, `helm`, `sails`

3. **`polar_ref`**: Wide-format reference polar table (TWS columns × TWA rows)

4. **`polar_ref_long`**: Long-format polars (`tws`, `twa`, `bsp_ref`)

## App Architecture

### UI Tabs
1. **Race Seasons** — Season/series summary tables (3 DT tables with aligned columns and JS footer callbacks)
2. **Race Analytics** — Single-race deep dive: summary table, AI narrative, Leaflet map, speed plot, 5 polar sub-tabs
3. **Social** — Spotify, YouTube, swag links
4. **About** — Documentation link, GitHub link, local-only admin (rebuild narratives/RDS, password: "wings")

### Key Patterns
- **Environment detection**: `IS_DEPLOYED` flag disables raw data rebuild on shinyapps.io
- **Cascading filters**: Season → Series → Race with reactive dropdowns
- **DT footer callbacks**: Custom JavaScript computing totals/averages/maxes per column
- **Deterministic narratives**: Hash-seeded quote selection ensures same output for same race
- **Race Seasons tables**: 3 tables share columns 2–10 (Days through NMEA Pts) with `table-layout: fixed` and matching `columnDefs` widths for alignment

### NMEA Parsing Functions
- `parse_rmc_any()` — GPS position, SOG, COG, datetime
- `parse_boat_speed()` — STW from VHW sentences
- `parse_wind_mwv()` — TWA/TWS from MWV sentences
- `nearest_sync_by_line_index()` — Aligns sensor data to GPS fixes by line proximity

### Data Cleaning Rules
- SOG > 15 knots excluded (GPS noise)
- STW < 2 knots excluded (mark rounding noise)
- Track points within 1,000 ft of home dock removed
- Teleport detection: isolated jumps implying >30 knots deleted

## Conventions

- **Timezone**: UTC internally, converted to `America/New_York` for display
- **Season format**: e.g. "2024-25" (June through May)
- **Pipe**: Base R `|>` (not magrittr)
- **Dark UI theme**: Glass-morphism aesthetic (`#0b0f17` background, translucent overlays)
- **Polar performance**: Pre-2025 speed sensors were uncalibrated; narratives note this distinction

## Libraries

`shiny`, `tidyverse`, `lubridate`, `readxl`, `leaflet`, `DT`, `RColorBrewer`, `geosphere`, `htmltools`
