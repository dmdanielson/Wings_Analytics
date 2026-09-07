# Known Issues

Bugs deliberately left unfixed so that snapshot-test failures continue to mean
"the refactor changed behavior" rather than "a fix changed behavior." These
should be addressed **after** the parsing/narrative refactoring is complete, at
which point the corresponding snapshots must be regenerated intentionally.

## 1. `parse_rmc_any` two-digit year handling assumes the 2000s

**Location:** `R/parsing.R`, `parse_rmc_any()` (formerly `app.R`).

The RMC date field is `DDMMYY`. The parser builds the calendar date with a
hardcoded century prefix:

```r
date_fmt <- paste0("20", substr(date_utc, 5, 6), "-",
                   substr(date_utc, 3, 4), "-", substr(date_utc, 1, 2))
```

Every two-digit year is therefore mapped into 2000–2099. A sentence dated
`230394` (23 Mar 1994) is parsed as **2094-03-23**, not 1994-03-23. There is no
pivot-year logic.

- **Impact:** Low for this project (all real logs are post-2000), but any
  pre-2000 fix, or a garbled year field, produces a date a century in the future.
- **Pinned by:** `tests/testthat/test-parsing.R`, "parse_rmc_any extracts
  position, speed, and datetime" (fixture uses `230394` and snapshots `2094`).
- **Possible fix:** Parse the full `DDMMYY` with a proper two-digit-year window
  (e.g. `lubridate` with an explicit pivot) instead of prefixing `"20"`.

## 2. `excel_to_posix_local` numeric-serial timezone offset

**Location:** `R/parsing.R`, `excel_to_posix_local()` (formerly `app.R`).

The numeric (Excel serial) branch is:

```r
as.POSIXct(x * 86400, origin = "1899-12-30", tz = LOCAL_TZ)
```

This treats the Excel serial as a count of seconds from a UTC-based epoch and
then displays it in `LOCAL_TZ`, so the result is shifted earlier by the local
UTC offset instead of landing on local midnight:

- `45000`   -> `2023-03-14 20:00:00 EDT` (intended `2023-03-15 00:00:00` local)
- `45658.5` -> `2025-01-01 07:00:00 EST` (serial noon shown 5 h earlier)

- **Impact:** Excel serial dates are shifted 4–5 hours earlier and can roll back
  to the previous calendar day. Character/Date/POSIXct inputs are unaffected.
- **Pinned by:** `tests/testthat/test-parsing.R`, "excel_to_posix_local converts
  Excel serial numbers".
- **Possible fix:** Convert on a UTC basis and then set/force the local zone
  (e.g. build the instant with `tz = "UTC"` and `lubridate::force_tz(..., LOCAL_TZ)`),
  or use a dedicated Excel date converter.
