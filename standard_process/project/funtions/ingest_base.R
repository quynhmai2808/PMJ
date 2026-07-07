## functions/ingest_combase.R
## Provider-specific ingest script for Combase.
## Responsibilities:
##   1. Define all Combase-specific config (paths, patterns, column mapping)
##   2. Discover + read raw files (meta_df + read_df logic)
##   3. Standardise output into the shared contract consumed by all shared functions
##
## To onboard a new provider: copy this file as ingest_<provider>.R,
## update SECTION 1 (config) and SECTION 3 (standardise) only.
## SECTION 2 (generic read logic) can be reused as-is in most cases.
##
## Output contract (list returned by ingest()):
##   $data : data.frame with fixed column names (see SECTION 3)
##   $meta : data.frame with file-level QC metadata (used only by summary_df)

# =============================================================================
# SECTION 1 — COMBASE CONFIG (provider-specific, change this per provider)
# =============================================================================

# --- Provider identity & paths ---
.PROVIDER       <- "Combase"
.PATHORG        <- paste0("/qa/data1/PMI/Archive/", .PROVIDER, "/")
.PATHRES        <- paste0("/qa/data1/PMI/Results/", .PROVIDER, "/")
.PATHPMI        <- "/qa/data1/PMI/Archive/PMI_Dateien/"

# --- PMI reference file stamps ---
.PMG_STAMP      <- "20260622"
.MISSING_STAMP  <- "20260617"

# --- File discovery: 2 filename patterns for Combase ---
# Pattern 1: YYYY_MM_DD_HHMM_COMBASE.csv  (timestamped, used for QC only)
# Pattern 2: DD_MM_YYYY_COMBASE.csv        (date-only, used for fact data)
.PATTERN_1      <- "^\\d{4}_\\d{2}_\\d{2}_\\d{4}_COMBASE.csv$"
.PATTERN_2      <- "^\\d{2}_\\d{2}_\\d{4}_COMBASE.csv$"

.DATE_EXTRACT_1 <- function(fn) sub("^\\d+_", "", sub("_COMBASE\\.csv$", "", fn))
.DATE_EXTRACT_2 <- function(fn) sub("_COMBASE\\.csv$", "", fn)
.DATE_FORMAT    <- "%d_%m_%Y"

# --- CSV format ---
.SEP      <- ";"
.ENCODING <- "UTF-8"
.QUOTE    <- ""

# --- Raw column names in Combase CSV ---
# These are the only Combase-specific names — everything below maps to
# standard output names.
.COL_YEAR     <- "POS_Start_Receipt_Date_Jahr"   # year (for date parsing)
.COL_WEEK     <- "POS_Woche_des_Jahres"          # ISO week number
.COL_STORE    <- "POS_Store"
.COL_QTY      <- "POS_POS_Quantity"
.COL_REVENUE  <- "POS_Amount_Tendered"
.COL_NETTO    <- "POS_Amount_Netto"
.COL_EAN      <- "POS_Barcode_No"
.COL_TEXT     <- "Bezeichnung"
.COL_VKE      <- "VKE"                           # Combase-specific, not in standard output
.COL_WGRCODE  <- "Warengruppecode"
.COL_WGR      <- "Warengruppe"
# Combase does NOT have a raw einzelpreis column — will be set to NA

# =============================================================================
# SECTION 2 — GENERIC READ LOGIC (reusable across providers)
# =============================================================================
# Shared helpers (nosigns, parse_date_*, etc.) live in utils.R — sourced once
# here so ingest_<provider>.R files are self-contained when source()d from main.
source("functions/utils.R")

#' Discover files matching a pattern and build a metadata table.
.build_meta <- function(path, pattern, date_extract_fn, date_format) {

  file_paths <- list.files(path = path, pattern = pattern, full.names = TRUE)

  if (length(file_paths) == 0) {
    return(data.frame(
      file_path  = character(0), file_name  = character(0),
      file_date  = as.Date(character(0)), year = integer(0),
      week       = character(0), day = character(0), wday = integer(0),
      start_week = as.Date(character(0)), end_week = as.Date(character(0)),
      size_bytes = numeric(0), size_kb = numeric(0),
      stringsAsFactors = FALSE
    ))
  }

  data.frame(file_path = file_paths, file_name = basename(file_paths),
             stringsAsFactors = FALSE) %>%
    dplyr::mutate(
      file_date  = as.Date(date_extract_fn(file_name), format = date_format),
      year       = lubridate::isoyear(file_date),
      week       = paste0(year, format(file_date, "%V")),
      day        = weekdays(file_date),
      wday       = as.integer(format(file_date, "%u")),
      start_week = file_date - (wday - 1L),
      end_week   = file_date + (7L - wday),
      size_bytes = file.info(file_path)$size,
      size_kb    = round(size_bytes / 1024, 2)
    ) %>%
    dplyr::arrange(week)
}

#' Parse date from two separate year + ISO week columns.
#' Returns list(week = "yyyyww", month = "yyyymm"), vectorised.
.parse_date_year_week <- function(year_vec, week_vec) {
  year      <- as.integer(year_vec)
  week      <- as.integer(week_vec)
  jan4      <- as.Date(paste0(year, "-01-04"))
  monday_w1 <- jan4 - (as.integer(format(jan4, "%u")) - 1L)
  d         <- monday_w1 + (week - 1L) * 7L
  list(
    week  = paste0(lubridate::isoyear(d), sprintf("%02d", lubridate::isoweek(d))),
    month = format(d, "%Y%m")
  )
}

#' Read all files in a meta table, attach week_in_data + week_match_flag.
#' All columns read as character; comma decimals normalised to dot.
.read_raw <- function(meta, sep, encoding, quote,
                       year_col, week_col) {

  if (nrow(meta) == 0) return(list())

  lapply(seq_len(nrow(meta)), function(i) {

    df <- read.csv(meta$file_path[i], sep = sep, fileEncoding = encoding,
                   quote = quote, fill = TRUE, colClasses = "character")

    # Normalise decimal separator globally once
    df <- df %>%
      dplyr::mutate(dplyr::across(dplyr::everything(), ~ gsub(",", ".", .x)))

    parsed <- .parse_date_year_week(df[[year_col]], df[[week_col]])

    df %>%
      dplyr::mutate(
        file_name       = meta$file_name[i],
        file_date       = as.character(meta$file_date[i]),
        year_file       = as.character(meta$year[i]),
        week_file       = as.character(meta$week[i]),
        size_kb         = as.character(meta$size_kb[i]),
        week_in_data    = parsed$week,
        month_in_data   = parsed$month,
        week_match_flag = (parsed$week == as.character(meta$week[i]))
      )
  }) %>%
    stats::setNames(meta$file_name)
}

# =============================================================================
# SECTION 3 — STANDARDISE TO OUTPUT CONTRACT (provider-specific mapping)
# =============================================================================

#' Map Combase raw columns -> standard output column names.
#' This is the ONLY place Combase raw column names appear outside SECTION 1.
#'
#' Standard output columns (fixed contract for all shared functions):
#'   week_in_data, month_in_data, store_id, EAN, product_text,
#'   wgr_code, wgr_text, sales_units, sales_revenue, einzelpreis
#'
#' Combase-specific columns (VKE, raw year/week, etc.) are kept in
#' $meta and $raw for QC/debug but are NOT part of $data.
.standardise <- function(raw_df) {
  raw_df %>%
    dplyr::transmute(
      # --- Standard date columns (already computed in .read_raw) ---
      week_in_data  = week_in_data,
      month_in_data = month_in_data,

      # --- Standard dimension columns ---
      store_id     = .data[[.COL_STORE]],
      EAN          = nosigns(.data[[.COL_EAN]]),         # strip non-numeric
      product_text = .data[[.COL_TEXT]],
      wgr_code     = .data[[.COL_WGRCODE]],
      wgr_text     = .data[[.COL_WGR]],

      # --- Standard measure columns ---
      sales_units   = as.numeric(.data[[.COL_QTY]]),
      sales_revenue = as.numeric(.data[[.COL_REVENUE]]),

      # --- einzelpreis: Combase has no raw price column, compute from data ---
      # Set to NA here; calculate_price will be derived in factdata_final()
      # from sales_revenue / sales_units for ALL providers consistently.
      einzelpreis   = NA_real_
    )
}

# =============================================================================
# MAIN INGEST FUNCTION — called by main_combase.R
# =============================================================================

#' Run the full Combase ingest and return the standard output contract.
#'
#' @return list(
#'   data       = standardised data.frame (all shared functions use this),
#'   meta       = file-level metadata     (summary_df uses this for QC),
#'   raw        = raw data list before standardisation (debug / audit),
#'   paths      = list(PATHRES, PATHPMI, PMG_STAMP, MISSING_STAMP),
#'   provider   = provider name string
#' )
ingest <- function() {

  # 1. Discover files
  meta_1 <- .build_meta(.PATHORG, .PATTERN_1, .DATE_EXTRACT_1, .DATE_FORMAT)
  meta_2 <- .build_meta(.PATHORG, .PATTERN_2, .DATE_EXTRACT_2, .DATE_FORMAT)

  # 2. Read raw files
  raw_list_1 <- .read_raw(meta_1, .SEP, .ENCODING, .QUOTE, .COL_YEAR, .COL_WEEK)
  raw_list_2 <- .read_raw(meta_2, .SEP, .ENCODING, .QUOTE, .COL_YEAR, .COL_WEEK)

  # 3. Bind into single raw data.frame per list
  raw_df_1 <- dplyr::bind_rows(raw_list_1)
  raw_df_2 <- dplyr::bind_rows(raw_list_2)

  # 4. Standardise to output contract
  # data_list_2 is used for the fact table (date-accurate filenames)
  # data_list_1 is kept for QC summary only — still standardised for consistency
  std_data_1 <- .standardise(raw_df_1)
  std_data_2 <- .standardise(raw_df_2)

  # 5. Build unified meta table (both sources, tagged by source)
  meta_combined <- dplyr::bind_rows(
    dplyr::mutate(meta_1, source = "list_1"),
    dplyr::mutate(meta_2, source = "list_2")
  )

  # 6. Return standard contract
  list(
    data     = std_data_2,          # fact data: shared functions use this
    data_qc  = std_data_1,          # QC-only data (list_1)
    meta     = meta_combined,        # file-level metadata for summary_df
    raw      = list(                 # audit trail — raw before standardisation
      list_1 = raw_list_1,
      list_2 = raw_list_2
    ),
    paths    = list(                 # passed through to pmg_process + reports
      PATHRES        = .PATHRES,
      PATHPMI        = .PATHPMI,
      PMG_STAMP      = .PMG_STAMP,
      MISSING_STAMP  = .MISSING_STAMP
    ),
    provider = .PROVIDER
  )
}
