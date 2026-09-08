# convert_degmin converts ddmm.mmm and applies hemisphere sign

    {
      "type": "double",
      "attributes": {},
      "value": [48.1173, -122.18333333, -27.20575, 0]
    }

# convert_degmin returns NA for NA input

    {
      "type": "double",
      "attributes": {},
      "value": ["NA"]
    }

# parse_rmc_any extracts position, speed, and datetime

    {
      "type": "list",
      "attributes": {
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["line_index", "datetime_utc", "sentence", "latitude", "longitude", "sog_knots", "cog_deg", "mode_indicator"]
        },
        "row.names": {
          "type": "integer",
          "attributes": {},
          "value": [1, 2, 3]
        },
        "class": {
          "type": "character",
          "attributes": {},
          "value": ["tbl_df", "tbl", "data.frame"]
        }
      },
      "value": [
        {
          "type": "integer",
          "attributes": {},
          "value": [1, 2, 3]
        },
        {
          "type": "double",
          "attributes": {
            "class": {
              "type": "character",
              "attributes": {},
              "value": ["POSIXct", "POSIXt"]
            },
            "tzone": {
              "type": "character",
              "attributes": {},
              "value": ["UTC"]
            }
          },
          "value": [3920186119, 1718482530, "NA"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["$GPRMC", "$GPRMC", "$GPRMC"]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [48.1173, -27.20575, "NA"]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [11.51666667, -82.7613, "NA"]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [22.4, 6.5, "NA"]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [84.4, 215.7, "NA"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": [null, null, null]
        }
      ]
    }

# parse_rmc_any returns an empty typed tibble when no RMC present

    {
      "type": "list",
      "attributes": {
        "class": {
          "type": "character",
          "attributes": {},
          "value": ["tbl_df", "tbl", "data.frame"]
        },
        "row.names": {
          "type": "integer",
          "attributes": {},
          "value": []
        },
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["line_index", "datetime_utc", "sentence", "latitude", "longitude", "sog_knots", "cog_deg", "mode_indicator"]
        }
      },
      "value": [
        {
          "type": "integer",
          "attributes": {},
          "value": []
        },
        {
          "type": "double",
          "attributes": {
            "class": {
              "type": "character",
              "attributes": {},
              "value": ["POSIXct", "POSIXt"]
            },
            "tzone": {
              "type": "character",
              "attributes": {},
              "value": ["UTC"]
            }
          },
          "value": []
        },
        {
          "type": "character",
          "attributes": {},
          "value": []
        },
        {
          "type": "double",
          "attributes": {},
          "value": []
        },
        {
          "type": "double",
          "attributes": {},
          "value": []
        },
        {
          "type": "double",
          "attributes": {},
          "value": []
        },
        {
          "type": "double",
          "attributes": {},
          "value": []
        },
        {
          "type": "character",
          "attributes": {},
          "value": []
        }
      ]
    }

# parse_boat_speed extracts knots and drops empty speeds

    {
      "type": "list",
      "attributes": {
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["line_index", "sentence", "boat_speed_knots"]
        },
        "row.names": {
          "type": "integer",
          "attributes": {},
          "value": [1]
        },
        "class": {
          "type": "character",
          "attributes": {},
          "value": ["tbl_df", "tbl", "data.frame"]
        }
      },
      "value": [
        {
          "type": "integer",
          "attributes": {},
          "value": [4]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["$IIVHW"]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [6.5]
        }
      ]
    }

# parse_wind_mwv converts units and classifies wind type

    {
      "type": "list",
      "attributes": {
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["line_index", "sentence", "twa_deg", "tws_knots", "wind_type"]
        },
        "row.names": {
          "type": "integer",
          "attributes": {},
          "value": [1, 2, 3]
        },
        "class": {
          "type": "character",
          "attributes": {},
          "value": ["tbl_df", "tbl", "data.frame"]
        }
      },
      "value": [
        {
          "type": "integer",
          "attributes": {},
          "value": [6, 7, 8]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["$IIMWV", "$IIMWV", "$IIMWV"]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [45, 120, 200]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [12.5, 12.051808, 10.79914]
        },
        {
          "type": "integer",
          "attributes": {
            "levels": {
              "type": "character",
              "attributes": {},
              "value": ["True", "Apparent", "Unknown"]
            },
            "class": {
              "type": "character",
              "attributes": {},
              "value": ["factor"]
            }
          },
          "value": [1, 2, 1]
        }
      ]
    }

# parse_mwd takes true direction, falls back to magnetic, wraps mod 360

    {
      "type": "list",
      "attributes": {
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["line_index", "sentence", "twd_deg"]
        },
        "row.names": {
          "type": "integer",
          "attributes": {},
          "value": [1, 2, 3]
        },
        "class": {
          "type": "character",
          "attributes": {},
          "value": ["tbl_df", "tbl", "data.frame"]
        }
      },
      "value": [
        {
          "type": "integer",
          "attributes": {},
          "value": [11, 12, 13]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["$WIMWD", "$WIMWD", "$WIMWD"]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [235, 240, 10]
        }
      ]
    }

# snap_nearest_idx returns nearest grid index, clamped, NA-safe

    {
      "type": "integer",
      "attributes": {},
      "value": [1, 1, 2, 2, 3, 3, 5, "NA"]
    }

# excel_to_posix_local parses character dates (incl. malformed -> NA)

    {
      "type": "character",
      "attributes": {},
      "value": ["2024-06-15 14:30:00 EDT", "2024-06-15 09:05:00 EDT", "2024-06-15 00:00:00 EDT", null]
    }

# excel_to_posix_local converts Excel serial numbers

    {
      "type": "character",
      "attributes": {},
      "value": ["2023-03-14 20:00:00 EDT", "2025-01-01 07:00:00 EST"]
    }

# excel_to_posix_local handles Date and POSIXct inputs

    {
      "type": "list",
      "attributes": {
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["date", "posix"]
        }
      },
      "value": [
        {
          "type": "character",
          "attributes": {},
          "value": ["2024-06-14 20:00:00 EDT", "2024-12-24 19:00:00 EST"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["2024-06-15 14:30:00 EDT"]
        }
      ]
    }

# excel_to_posix_local returns NA for unsupported types

    {
      "type": "logical",
      "attributes": {},
      "value": [null]
    }

