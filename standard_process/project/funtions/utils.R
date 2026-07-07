## functions/utils.R
## Shared helper functions — provider-agnostic.
## Sourced by ingest_<provider>.R and any shared function that needs them.
## Never contains provider-specific logic.

# =============================================================================
# STRING HELPERS
# =============================================================================

#' Strip all non-numeric characters from a string.
nosigns <- function(x) gsub("[^0-9]", "", as.character(x))

#' Remove leading zeros from a string/number.
remove_leading_zeros <- function(x) sub("^0+", "", as.character(x))

#' Trim edges, strip non-alphanumeric, uppercase — used for join key normalisation.
normalize_key <- function(x) {
  x_clean <- gsub("^[^[:alnum:]]+|[^[:alnum:]]+$", "", trimws(as.character(x)))
  toupper(x_clean)
}

#' Replace comma decimal separator with dot and coerce to numeric.
#' R default is "." — all raw data normalised here so downstream code
#' can call as.numeric() safely on any column.
parse_decimal_comma <- function(x) as.numeric(gsub(",", ".", as.character(x)))

# =============================================================================
# DATE CONVERSION HELPER (shared internal)
# =============================================================================

#' From a Date vector compute standard yyyyww and yyyymm strings.
#' Used internally by all parse_date_* functions below.
#' @param d  Date vector
#' @return list(week = character "yyyyww", month = character "yyyymm")
.date_to_std <- function(d) {
  list(
    week  = paste0(lubridate::isoyear(d), sprintf("%02d", lubridate::isoweek(d))),
    month = format(d, "%Y%m")
  )
}

# =============================================================================
# DATE PARSING FUNCTIONS
## Each function follows the same contract:
##   Input : raw character vector(s) from the data file
##   Output: list(week = character "yyyyww", month = character "yyyymm")
##
## ingest_<provider>.R calls the appropriate function based on what the
## provider's raw file contains. Adding a new format = adding one function here.
# =============================================================================

#' TWO separate columns: year (yyyy) + ISO week number (w or ww).
#' Used by: Combase (POS_Start_Receipt_Date_Jahr + POS_Woche_des_Jahres)
#'
#' Strategy: reconstruct the Monday of that ISO week, derive month from it.
#' ISO 8601 guarantees Jan 4 always falls in week 1.
#' @param year_vec  Character/numeric vector e.g. c("2026", "2026")
#' @param week_vec  Character/numeric vector e.g. c("3", "12")
parse_date_year_week <- function(year_vec, week_vec) {
  year      <- as.integer(year_vec)
  week      <- as.integer(week_vec)
  jan4      <- as.Date(paste0(year, "-01-04"))
  monday_w1 <- jan4 - (as.integer(format(jan4, "%u")) - 1L)
  d         <- monday_w1 + (week - 1L) * 7L
  .date_to_std(d)
}

#' Single column: ddmmyyyy (exactly 8 digits, no separators).
#' @param date_vec  Character vector e.g. c("15012026", "22062026")
parse_date_ddmmyyyy <- function(date_vec) {
  .date_to_std(as.Date(as.character(date_vec), format = "%d%m%Y"))
}

#' Single column: wwyyyy (6 digits — ISO week number then year).
#' @param date_vec  Character vector e.g. c("032026", "122026")
parse_date_wwyyyy <- function(date_vec) {
  s    <- sprintf("%06s", as.character(date_vec))  # ensure leading zero on week
  week <- as.integer(substr(s, 1, 2))
  year <- as.integer(substr(s, 3, 6))
  parse_date_year_week(year, week)                 # reuse year+week logic
}

#' Single column: ISO date string YYYY-MM-DD.
#' @param date_vec  Character vector e.g. c("2026-01-15", "2026-06-22")
parse_date_iso <- function(date_vec) {
  .date_to_std(as.Date(as.character(date_vec), format = "%Y-%m-%d"))
}

#' Single column: yyyymmdd (8 digits, year-first — common in DWH exports).
#' @param date_vec  Character vector e.g. c("20260115", "20260622")
parse_date_yyyymmdd <- function(date_vec) {
  .date_to_std(as.Date(as.character(date_vec), format = "%Y%m%d"))
}
