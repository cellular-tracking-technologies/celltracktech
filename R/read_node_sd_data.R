#' Read Node SD Card Data
#'
#' Reads CSV files pulled directly from a CTT node's SD card and standardizes
#' column names to match the format used by the rest of this package (database
#' tables, \code{prep_data()}, \code{calculate_node_locations()}, etc.).
#'
#' Nodes write four types of files to the SD card:
#' \itemize{
#'   \item \strong{beep} — sub-GHz tag detections (\code{beep_0.csv}, \code{beep_1.csv}, ...)
#'   \item \strong{blu} — BLE tag detections (\code{blu_beep_0.csv}, \code{2p4_ghz_beep_0.csv}, ...)
#'   \item \strong{gps} — node GPS fixes (\code{gps.csv}, \code{gps_0.csv}, ...)
#'   \item \strong{health} — node health/status (\code{health_0.csv}, ...)
#' }
#'
#' The function auto-detects the file type from the filename and applies the
#' appropriate column renaming.
#'
#' @param path Character. Path to a single CSV file or a directory. If a
#'   directory, all CSV files are read recursively. The node ID is inferred from
#'   the parent folder name for each file.
#' @param node_id Character (optional). Override the node ID instead of
#'   inferring it from the folder name. Required when \code{path} points to a
#'   single file that is not inside a node-named folder.
#' @param parse_payload Logical. If \code{TRUE} (default), BLU files will have
#'   their payload parsed to extract battery voltage and temperature via
#'   \code{parseit()}.
#'
#' @returns A named list with up to four elements: \code{beep}, \code{blu},
#'   \code{gps}, \code{health}. Each is a data frame with standardized column
#'   names, or \code{NULL} if no files of that type were found.
#'
#' @details
#' The returned data frames use the same column names as the package database
#' tables, so they can be passed directly to functions like \code{prep_data()},
#' \code{calculate_node_locations()}, \code{calculate_track()}, etc.
#'
#' \strong{Beep columns:} \code{time}, \code{tag_id}, \code{tag_rssi},
#' \code{node_id}
#'
#' \strong{BLU columns:} \code{time}, \code{tag_id}, \code{tag_rssi},
#' \code{node_id}, \code{sync}, \code{product}, \code{revision},
#' \code{payload}, \code{battery_voltage_v}, \code{temperature_celsius}
#'
#' \strong{GPS columns:} \code{gps_at}, \code{node_id}, \code{latitude},
#' \code{longitude}, \code{altitude}, \code{hdop}, \code{vdop}, \code{pdop},
#' \code{navigation_mode}, \code{satellites}, \code{on_time}
#'
#' \strong{Health columns:} \code{time}, \code{node_id}, \code{up_time},
#' \code{power_ok}, \code{batt_mv}, \code{batt_temp_c}, \code{charge_mv},
#' \code{charge_ma}, \code{charge_temp_c}, \code{node_temp_c},
#' \code{energy_used_mah}, \code{sd_free}, \code{sub_ghz_det},
#' \code{ble_det}, \code{errors}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Read all files from a node folder
#' data <- read_node_sd_data("path/to/nodes/3b4c32")
#' beeps <- data$beep
#' health <- data$health
#'
#' # Read a full nodes directory (multiple node folders)
#' data <- read_node_sd_data("path/to/nodes")
#'
#' # Read a single file with explicit node ID
#' beeps <- read_node_sd_data("beep_0.csv", node_id = "3B4C32")
#' }
read_node_sd_data <- function(path, node_id = NULL, parse_payload = TRUE) {

  # collect all CSV files
  if (dir.exists(path)) {
    files <- list.files(path, pattern = "\\.csv(\\.gz)?$",
                        recursive = TRUE, full.names = TRUE)
  } else if (file.exists(path)) {
    files <- path
  } else {
    stop("Path does not exist: ", path)
  }

  if (length(files) == 0) {
    stop("No CSV files found at: ", path)
  }

  beep_list  <- list()
  blu_list   <- list()
  gps_list   <- list()
  health_list <- list()

  for (f in files) {
    fname <- basename(f)
    ftype <- detect_node_filetype(fname)

    if (is.na(ftype)) {
      message("Skipping unrecognized file: ", fname)
      next
    }

    # infer node_id from parent folder if not supplied
    nid <- node_id
    if (is.null(nid)) {
      nid <- basename(dirname(f))
    }
    nid <- toupper(nid)

    df <- tryCatch(
      suppressWarnings(
        readr::read_csv(f, na = c("NA", ""),
                        show_col_types = FALSE,
                        skip_empty_rows = TRUE)
      ),
      error = function(e) {
        message("Could not read file: ", f, " (", conditionMessage(e), ")")
        return(NULL)
      }
    )

    if (is.null(df) || nrow(df) == 0) {
      message("Skipping empty file: ", f)
      next
    }

    # remove rows with non-ASCII / invalid UTF-8 corruption (common on SD cards)
    char_cols <- names(df)[vapply(df, is.character, logical(1))]
    if (length(char_cols) > 0) {
      bad <- apply(df[, char_cols, drop = FALSE], 1, function(row) {
        any(vapply(row, function(val) {
          if (is.na(val)) return(FALSE)
          converted <- iconv(val, from = "UTF-8", to = "ASCII")
          is.na(converted)
        }, logical(1)))
      })
      if (any(bad)) {
        message("Removed ", sum(bad), " corrupted rows from ", fname)
        df <- df[!bad, ]
      }
    }
    if (nrow(df) == 0) next

    result <- switch(ftype,
      beep   = standardize_beep(df, nid),
      blu    = standardize_blu(df, nid, parse_payload),
      gps    = standardize_gps(df, nid),
      health = standardize_health(df, nid)
    )

    if (!is.null(result) && nrow(result) > 0) {
      switch(ftype,
        beep   = { beep_list[[length(beep_list) + 1]]     <- result },
        blu    = { blu_list[[length(blu_list) + 1]]        <- result },
        gps    = { gps_list[[length(gps_list) + 1]]        <- result },
        health = { health_list[[length(health_list) + 1]]  <- result }
      )
    }
  }

  out <- list(
    beep   = if (length(beep_list) > 0) dplyr::bind_rows(beep_list) else NULL,
    blu    = if (length(blu_list) > 0) dplyr::bind_rows(blu_list) else NULL,
    gps    = if (length(gps_list) > 0) dplyr::bind_rows(gps_list) else NULL,
    health = if (length(health_list) > 0) dplyr::bind_rows(health_list) else NULL
  )

  counts <- vapply(out, function(x) if (!is.null(x)) nrow(x) else 0L, integer(1))
  message("Read ", sum(counts), " total records (",
          paste(names(counts), counts, sep = ": ", collapse = ", "), ")")

  return(out)
}


# --- internal helpers --------------------------------------------------------

#' Detect node file type from filename
#' @param fname Basename of the file
#' @returns Character: "beep", "blu", "gps", "health", or NA
#' @noRd
detect_node_filetype <- function(fname) {
  fname <- tolower(fname)
  if (grepl("^blu_beep|^2p4_ghz_beep", fname)) return("blu")
  if (grepl("^beep|^434", fname))               return("beep")
  if (grepl("^gps", fname))                     return("gps")
  if (grepl("^health", fname))                  return("health")
  return(NA_character_)
}


#' Standardize beep (sub-GHz detection) data
#'
#' SD card columns: time, id, rssi
#' Package columns: time, tag_id, tag_rssi, node_id
#' @noRd
standardize_beep <- function(df, node_id) {
  # handle both lowercase SD card format and CamelCase formats
  names(df) <- tolower(names(df))

  df$time <- as.POSIXct(df$time, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

  # SD card beep files use "id" and "rssi"
  if ("id" %in% names(df)) {
    df$tag_id <- toupper(df$id)
    df$id <- NULL
  } else if ("tagid" %in% names(df)) {
    df$tag_id <- toupper(df$tagid)
    df$tagid <- NULL
  }

  if ("rssi" %in% names(df)) {
    df$tag_rssi <- as.integer(df$rssi)
    df$rssi <- NULL
  } else if ("tagrssi" %in% names(df)) {
    df$tag_rssi <- as.integer(df$tagrssi)
    df$tagrssi <- NULL
  }

  df$node_id <- toupper(node_id)

  # filter: valid timestamps, 8-char tag IDs
  df <- df[!is.na(df$time), ]
  df <- df[nchar(df$tag_id) == 8, ]

  df[, c("time", "tag_id", "tag_rssi", "node_id")]
}


#' Standardize BLU (BLE detection) data
#'
#' SD card columns: time, tag_id, sync, family, payload_version, payload, rssi
#' Package columns: time, tag_id, tag_rssi, node_id, sync, product, revision,
#'                  payload, battery_voltage_v, temperature_celsius
#' @noRd
standardize_blu <- function(df, node_id, parse_payload = TRUE) {
  names(df) <- tolower(names(df))

  df$time <- as.POSIXct(df$time, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  df$tag_id <- toupper(df$tag_id)
  df$tag_rssi <- as.integer(df$rssi)
  df$node_id <- toupper(node_id)

  # SD card uses "family" -> package uses "product"
  if ("family" %in% names(df)) {
    df$product <- as.integer(df$family)
    df$family <- NULL
  }

  # SD card uses "payload_version" -> package uses "revision"
  if ("payload_version" %in% names(df)) {
    df$revision <- as.integer(df$payload_version)
    df$payload_version <- NULL
  }

  df$sync <- as.integer(df$sync)
  df$payload <- as.character(df$payload)
  df$rssi <- NULL

  # parse payload for battery voltage and temperature
  if (parse_payload && "payload" %in% names(df)) {
    parsed <- parse_payload_vector(df$payload)
    df$battery_voltage_v <- parsed$battery_voltage_v
    df$temperature_celsius <- parsed$temperature_celsius
  } else {
    df$battery_voltage_v <- NA_real_
    df$temperature_celsius <- NA_real_
  }

  # filter
  df <- df[!is.na(df$time), ]
  df <- df[nchar(df$tag_id) == 8, ]

  df[, c("time", "tag_id", "tag_rssi", "node_id", "sync", "product",
         "revision", "payload", "battery_voltage_v", "temperature_celsius")]
}


#' Standardize GPS data
#'
#' SD card columns (v3): time, latitude, longitude, altitude, hdop, vdop, pdop, on_time
#' SD card columns (v2): Time, Latitude, Longitude, Altitude, Hdop, Vdop, Pdop,
#'                       NavigationMode, Satellites, OnTime
#' Package columns: gps_at, node_id, latitude, longitude, altitude, hdop, vdop,
#'                  pdop, navigation_mode, satellites, on_time
#' @noRd
standardize_gps <- function(df, node_id) {
  orig_names <- names(df)
  names(df) <- tolower(names(df))

  # time column -> gps_at
  df$gps_at <- as.POSIXct(df$time, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  df$time <- NULL

  df$node_id <- toupper(node_id)

  # handle CamelCase v2 columns (already lowered)
  if ("navigationmode" %in% names(df)) {
    names(df)[names(df) == "navigationmode"] <- "navigation_mode"
  }
  if ("ontime" %in% names(df)) {
    names(df)[names(df) == "ontime"] <- "on_time"
  }

  # fill missing columns
  if (!"navigation_mode" %in% names(df)) df$navigation_mode <- NA_integer_
  if (!"satellites" %in% names(df))      df$satellites <- NA_real_
  if (!"on_time" %in% names(df))         df$on_time <- NA_real_

  df <- df[!is.na(df$gps_at), ]
  df <- df[!is.na(df$latitude), ]

  df[, c("gps_at", "node_id", "latitude", "longitude", "altitude",
         "hdop", "vdop", "pdop", "navigation_mode", "satellites", "on_time")]
}


#' Standardize health data
#'
#' SD card columns (v3 node): time, up_time, power_ok, batt_mv, batt_temp_c,
#'   charge_mv, charge_ma, charge_temp_c, node_temp_c, energy_used_mah,
#'   sd_free, sub_ghz_det, ble_det, errors
#' Package columns: same as above plus node_id
#'
#' Also handles the legacy "434_det" / "blu_det" column names.
#' @noRd
standardize_health <- function(df, node_id) {
  names(df) <- tolower(names(df))

  df$time <- as.POSIXct(df$time, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  df$node_id <- toupper(node_id)

  # handle alternate detection column names
  if (!"sub_ghz_det" %in% names(df) && "434_det" %in% names(df)) {
    names(df)[names(df) == "434_det"] <- "sub_ghz_det"
  }
  if (!"ble_det" %in% names(df) && "blu_det" %in% names(df)) {
    names(df)[names(df) == "blu_det"] <- "ble_det"
  }

  df <- df[!is.na(df$time), ]

  expected <- c("time", "node_id", "up_time", "power_ok", "batt_mv",
                "batt_temp_c", "charge_mv", "charge_ma", "charge_temp_c",
                "node_temp_c", "energy_used_mah", "sd_free",
                "sub_ghz_det", "ble_det", "errors")

  # fill any missing columns with NA
  for (col in expected) {
    if (!col %in% names(df)) df[[col]] <- NA
  }

  df[, expected]
}


#' Vectorized payload parsing for BLU data
#'
#' Extracts battery voltage and temperature from hex payload strings without
#' row-by-row iteration, making it much faster on large files.
#' @param payloads Character vector of hex payload strings
#' @returns A data frame with columns battery_voltage_v and temperature_celsius
#' @noRd
parse_payload_vector <- function(payloads) {
  n <- length(payloads)
  batt <- rep(NA_real_, n)
  temp <- rep(NA_real_, n)

  valid <- !is.na(payloads) & nchar(payloads) > 3

  if (any(valid)) {
    p <- payloads[valid]
    # extract first 4-char hex word (battery mV) and second (temperature)
    # parseit() uses readBin with little-endian signed int16:
    # hex "4007" -> raw bytes 0x40, 0x07 -> little-endian int16 = 0x0740 = 1856
    hex_batt <- substr(p, 1, 4)
    hex_temp <- substr(p, 5, 8)

    # little-endian: low byte is first pair, high byte is second pair
    lo_b <- strtoi(substr(hex_batt, 1, 2), 16L)
    hi_b <- strtoi(substr(hex_batt, 3, 4), 16L)
    raw_batt <- hi_b * 256L + lo_b

    lo_t <- strtoi(substr(hex_temp, 1, 2), 16L)
    hi_t <- strtoi(substr(hex_temp, 3, 4), 16L)
    raw_temp <- hi_t * 256L + lo_t

    # handle signed 16-bit (temperature can be negative)
    raw_batt <- ifelse(raw_batt > 32767L, raw_batt - 65536L, raw_batt)
    raw_temp <- ifelse(raw_temp > 32767L, raw_temp - 65536L, raw_temp)

    batt[valid] <- raw_batt / 1000
    temp[valid] <- raw_temp / 100
  }

  data.frame(battery_voltage_v = batt, temperature_celsius = temp)
}
