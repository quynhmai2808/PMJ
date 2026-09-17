## funtions/ingest_COMBASE.R
## Provider-specific ingest for Combase — reads TWO CSV file types per week,
## standardises the fact file (pattern 2) into the shared output contract.
##
## File type 1 (timestamped): YYYY_MM_DD_HHMM_COMBASE.csv
##   - Delivered for QC reference only — NOT used for fact data
##   - Kept in $raw$pattern_1 for audit trail
##
## File type 2 (date-only):   DD_MM_YYYY_COMBASE.csv
##   - Used for fact data → $data (input to all shared functions)
##   - Date parsed from two data columns: year + ISO week number
##
## Columns in both CSV types:
##   POS_Start_Receipt_Date_Jahr  — year (integer)
##   POS_Woche_des_Jahres         — ISO week number
##   POS_Store                    — store identifier
##   POS_POS_Quantity             — sales units
##   POS_Amount_Tendered          — revenue (gross)
##   POS_Barcode_No               — EAN / barcode
##   Bezeichnung                  — product description
##   Warengruppecode              — WGR code (separate column)
##   Warengruppe                  — WGR text (separate column)
##
## Note: Combase has separate Warengruppecode + Warengruppe columns
## (no split needed — unlike DTV "1:0 | Zigaretten").

# =============================================================================
# SECTION 1 — COMBASE CONFIG
# =============================================================================

source("/qa/data1/PMI/Code/R_profiling_framework/data_pipeline/functions/utils.R")

# --- Provider identity & paths ---
.PROVIDER  <- "Combase"
.PATHORG   <- paste0("/qa/data1/PMI/Archive/", .PROVIDER, "/")
.PATHRES   <- paste0("/qa/data1/PMI/Results/", .PROVIDER, "/")
.PATHPMI   <- "/qa/data1/PMI/Archive/PMI_Dateien/"

# Auto-detect most recent PMI reference files — no manual update needed
.detect_stamp <- function(path, pattern) {
  files  <- list.files(path, pattern = pattern, full.names = FALSE)
  if (length(files) == 0)
    stop(sprintf("No file matching '%s' found in %s", pattern, path))
  stamps <- regmatches(files, regexpr("\\d{8}", files))
  max(stamps)
}
.PMG_STAMP     <- .detect_stamp(.PATHPMI, "^pmg_product_\\d{8}\\.csv$")
.MISSING_STAMP <- .detect_stamp(.PATHPMI, "^missing_ean_\\d{8}\\.csv$")

# --- File patterns ---
# Pattern 1: YYYY_MM_DD_HHMM_COMBASE.csv  (timestamped — QC reference only)
# Pattern 2: DD_MM_YYYY_COMBASE.csv        (date-only   — fact data → $data)
.PATTERN_1 <- "^\\d{4}_\\d{2}_\\d{2}_\\d{4}_COMBASE\\.csv$"
.PATTERN_2 <- "^\\d{2}_\\d{2}_\\d{4}_COMBASE\\.csv$"

# --- Date from filename helpers (for meta_df only — NOT used for week_in_data) ---
# week_in_data is ALWAYS derived from data columns (year + ISO week)
.DATE_FROM_FILENAME_1 <- function(fn) {
  # YYYY_MM_DD_HHMM_COMBASE.csv → strip timestamp prefix and COMBASE suffix
  date_str <- sub("^\\d+_", "", sub("_COMBASE\\.csv$", "", fn))
  as.Date(date_str, format = "%d_%m_%Y")
}
.DATE_FROM_FILENAME_2 <- function(fn) {
  # DD_MM_YYYY_COMBASE.csv
  date_str <- sub("_COMBASE\\.csv$", "", fn)
  as.Date(date_str, format = "%d_%m_%Y")
}

# --- CSV format ---
.SEP      <- ";"
.ENCODING <- "UTF-8"
.QUOTE    <- ""

# --- Raw column names ---
.COL_YEAR    <- "POS_Start_Receipt_Date_Jahr"  # year for date parsing
.COL_WEEK    <- "POS_Woche_des_Jahres"          # ISO week number
.COL_STORE   <- "POS_Store"
.COL_QTY     <- "POS_POS_Quantity"
.COL_REVENUE <- "POS_Amount_Tendered"
.COL_EAN     <- "POS_Barcode_No"
.COL_TEXT    <- "Bezeichnung"
.COL_WGRCODE <- "Warengruppecode"               # separate code column (no split needed)
.COL_WGR     <- "Warengruppe"                   # separate text column (no split needed)

# --- Tobacco / Accessory groups ---
# Combase has separate Warengruppecode + Warengruppe — match on wgr_text directly
.TOBACCO_GROUPS <- c(
  "Tabak", "Zigaretten", "Zigaretten & Co", "Zigarettenhülsen",
  "Feinschnitt-Tabak", "Cigarillos / Cigarren", "Zigarren/Zigarillos",
  "Pfeifentabak", "Kautabak", "Kau-/Schnupftabak", "Schnupftabak",
  "And.Rauchart son.Mat", "IQOS", "Heat-not-burn",
  "Devices (z.B. IQOS & VEEV)", "Devices (z.B. IQOS, VEEV One)",
  "Pods", "Disposables"
)
.ACCESSORY_GROUPS <- c(
  "Zigarettenpapier", "Raucherbedarf", "Raucherbedarf (Zubehör)",
  "Sonst.Raucherbedarf", "Taschenfeuerzeuge"
)

# =============================================================================
# SECTION 2 — GENERIC HELPERS  (same structure as DTV)
# =============================================================================

#' Discover files matching pattern → build metadata table.
.build_meta <- function(path, pattern, date_from_fn) {
  file_paths <- list.files(path, pattern = pattern,
                            full.names = TRUE, ignore.case = TRUE)
  if (length(file_paths) == 0) {
    message(sprintf("WARNING: No files found in %s matching '%s'", path, pattern))
    return(data.frame(
      file_path = character(0), file_name = character(0),
      file_date = as.Date(character(0)), year = integer(0),
      week = character(0), size_kb = numeric(0),
      stringsAsFactors = FALSE
    ))
  }
  data.frame(file_path = file_paths, file_name = basename(file_paths),
              stringsAsFactors = FALSE) %>%
    dplyr::mutate(
      file_date  = as.Date(sapply(file_name, date_from_fn)),
      year       = lubridate::isoyear(file_date),
      week       = paste0(year, format(file_date, "%V")),
      size_bytes = file.info(file_path)$size,
      size_kb    = round(size_bytes / 1024, 2)
    ) %>%
    dplyr::arrange(file_date)
}

#' Parse date from Combase year + ISO week integer columns.
#' Returns list(week = "yyyyww", month = "yyyymm"), vectorised.
#' (DTV equivalent: .parse_date_iso / .parse_date_ddmmyyyy)
.parse_date <- function(year_vec, week_vec) {
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

#' Keep store_id as raw value — no parsing applied.
#' (Same as DTV .parse_store_id)
.parse_store_id <- function(store_vec) {
  as.character(store_vec)
}

#' Standardise one Combase CSV data.frame → output contract.
#'
#' Combase difference vs DTV:
#'   - No .parse_wgr() needed — wgr_code and wgr_text are separate raw columns
#'   - Date parsed from two integer columns (year + ISO week)
#'   - Decimal comma → dot normalisation applied before parsing
#'   - week_file = meta week (weekly delivery) not month (monthly delivery)
#'
#' Output columns (fixed contract — identical to DTV .standardise output):
#'   week_in_data, month_in_data, week_file, store_id, EAN, product_text,
#'   wgr_code, wgr_text, sales_units, sales_revenue, einzelpreis,
#'   file_name, size_kb
.standardise <- function(raw_df, file_name_val, size_kb_val, week_file_val) {

  # Log empty EAN rows — keep all rows for QC detection (same as DTV)
  n_null_ean <- sum(is.na(raw_df[[.COL_EAN]]) | raw_df[[.COL_EAN]] == "")
  if (n_null_ean > 0)
    message(sprintf("  [%s] %s: %d rows with empty EAN → flagged by ean_quality_detail()",
      .PROVIDER, file_name_val, n_null_ean))

  parsed <- .parse_date(raw_df[[.COL_YEAR]], raw_df[[.COL_WEEK]])

  raw_df %>%
    dplyr::transmute(
      week_in_data  = parsed$week,
      month_in_data = parsed$month,
      week_file     = week_file_val,          # from filename (weekly delivery)
      store_id      = .parse_store_id(.data[[.COL_STORE]]),
      EAN           = nosigns(as.character(.data[[.COL_EAN]])),
      product_text  = as.character(.data[[.COL_TEXT]]),
      wgr_code      = as.character(.data[[.COL_WGRCODE]]),  # separate col — no split
      wgr_text      = as.character(.data[[.COL_WGR]]),      # separate col — no split
      sales_units   = as.numeric(.data[[.COL_QTY]]),
      sales_revenue = as.numeric(.data[[.COL_REVENUE]]),
      einzelpreis   = NA_real_,               # no raw price col in Combase
      file_name     = file_name_val,
      size_kb       = as.character(size_kb_val)
    )
}

# =============================================================================
# SECTION 3 — DATASET-SPECIFIC READ
# (DTV equivalent: .read_reemtsma / .read_auswertung)
# =============================================================================

#' Read pattern-2 CSV files (fact data) → list of standardised data.frames.
#' Applies decimal normalisation (comma → dot) before standardising.
.read_pattern2 <- function(meta) {
  if (nrow(meta) == 0) return(list())
  lapply(seq_len(nrow(meta)), function(i) {
    message(sprintf("  [Combase] Reading: %s (%.0f KB)",
      meta$file_name[i], meta$size_kb[i]))
    raw <- read.csv(
      meta$file_path[i], sep = .SEP, fileEncoding = .ENCODING,
      quote = .QUOTE, fill = TRUE, colClasses = "character"
    )
    # Normalise decimal separator (German comma → dot) globally
    raw <- raw %>%
      dplyr::mutate(dplyr::across(dplyr::everything(), ~ gsub(",", ".", .x)))

    .standardise(raw,
      file_name_val = meta$file_name[i],
      size_kb_val   = meta$size_kb[i],
      week_file_val = meta$week[i]     # weekly delivery → week from filename
    )
  }) %>% stats::setNames(meta$file_name)
}

#' Read pattern-1 CSV files (timestamped — QC reference only).
#' Returns raw data.frames — NOT standardised, NOT used in $data.
.read_pattern1 <- function(meta) {
  if (nrow(meta) == 0) return(list())
  lapply(seq_len(nrow(meta)), function(i) {
    message(sprintf("  [Combase/QC] Reading: %s", meta$file_name[i]))
    raw <- read.csv(
      meta$file_path[i], sep = .SEP, fileEncoding = .ENCODING,
      quote = .QUOTE, fill = TRUE, colClasses = "character"
    )
    raw %>%
      dplyr::mutate(dplyr::across(dplyr::everything(), ~ gsub(",", ".", .x)))
  }) %>% stats::setNames(meta$file_name)
}

# =============================================================================
# MAIN INGEST FUNCTION
# =============================================================================

#' Full Combase ingest: discover → read → standardise → return contract.
#'
#' Only pattern-2 files (DD_MM_YYYY_COMBASE.csv) are standardised into $data.
#' Pattern-1 files (timestamped) are kept in $raw$pattern_1 for audit only.
#'
#' (DTV equivalent: reads Reemtsma + Auswertung separately, combines into $data)
#'
#' @return list(
#'   data             = combined standardised data.frame  ← all shared functions use this
#'   data_pattern1    = raw pattern-1 data.frames         ← QC/audit only
#'   meta_pattern1    = file metadata for pattern-1 files
#'   meta_pattern2    = file metadata for pattern-2 files
#'   paths            = list(PATHRES, PATHPMI, PMG_STAMP, MISSING_STAMP)
#'   provider         = "Combase"
#'   tobacco_groups   = .TOBACCO_GROUPS  ← summary_df + mix_price_check()
#'   accessory_groups = .ACCESSORY_GROUPS
#' )
ingest <- function() {

  # 1. Discover files (both patterns)
  meta_1 <- .build_meta(.PATHORG, .PATTERN_1, .DATE_FROM_FILENAME_1)
  meta_2 <- .build_meta(.PATHORG, .PATTERN_2, .DATE_FROM_FILENAME_2)

  if (nrow(meta_1) == 0 && nrow(meta_2) == 0)
    stop(sprintf("No Combase files found in %s. Check .PATHORG and file patterns.", .PATHORG))

  message(sprintf("  [%s] Pattern 1 (QC): %d file(s) | Pattern 2 (fact): %d file(s)",
    .PROVIDER, nrow(meta_1), nrow(meta_2)))

  # 2. Read + standardise pattern-2 (fact data)
  std_list_2 <- .read_pattern2(meta_2)

  # 3. Read pattern-1 raw (audit/QC only — not standardised)
  raw_list_1 <- .read_pattern1(meta_1)

  # 4. Combine pattern-2 into unified data.frame
  std_data <- dplyr::bind_rows(std_list_2) %>%
    dplyr::arrange(week_in_data, store_id, EAN)

  message(sprintf(
    "  [%s] %s rows | %d weeks | %d stores | %d EANs",
    .PROVIDER,
    format(nrow(std_data), big.mark = ","),
    dplyr::n_distinct(std_data$week_in_data),
    dplyr::n_distinct(std_data$store_id),
    dplyr::n_distinct(std_data$EAN)
  ))

  list(
    data          = std_data,       # ← single combined output for pipeline
    data_pattern1 = raw_list_1,     # ← QC/audit: timestamped raw files
    meta_pattern1 = meta_1,
    meta_pattern2 = meta_2,
    paths = list(
      PATHRES       = .PATHRES,
      PATHPMI       = .PATHPMI,
      PMG_STAMP     = .PMG_STAMP,
      MISSING_STAMP = .MISSING_STAMP
    ),
    provider         = .PROVIDER,
    tobacco_groups   = .TOBACCO_GROUPS,
    accessory_groups = .ACCESSORY_GROUPS
  )
}
