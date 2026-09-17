## funtions/ingest_EuroTrade.R
## Provider: EuroTrade (Circana data)
## File format: XLSX, 1 sheet "Sheet01", monthly delivery
## Filename: Circana_PMI_Absätze+Umsätze_gesamt_MMYYYY.xlsx
## Periode column: YYYYMM integer (monthly aggregate — no daily date)
## ─────────────────────────────────────────────────────────────────────────────

source("/qa/data1/PMI/Code/R_profiling_framework/data_pipeline/functions/utils.R")

# =============================================================================
# SECTION 1 — PROVIDER CONFIG
# =============================================================================

.PROVIDER <- "EUROTRADE"

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

# Filename: Circana_PMI_Absätze+Umsätze_gesamt_MMYYYY.xlsx
# Note: "+" and "ä/ü" in filename — use loose pattern
.PATTERN <- "^Circana_PMI.*gesamt_\\d{6}\\.xlsx$"

# Parse MMYYYY from filename → first day of month as Date
.DATE_FROM_FILENAME <- function(fn) {
  m <- regmatches(fn, regexpr("\\d{6}(?=\\.xlsx$)", fn, perl = TRUE))
  if (length(m) == 0) return(as.Date(NA))
  mm   <- substr(m, 1, 2)
  yyyy <- substr(m, 3, 6)
  as.Date(paste0(yyyy, "-", mm, "-01"))
}

.FILE_FORMAT <- "xlsx"
.SHEET       <- "Sheet01"

# 10 columns: Periode, KSt, Hersteller, Artikel Name, Marke,
#              StabileNr, EAN, WarenGrp, Umsatz brutto, Absatz
.COL_TYPES <- c(
  "text",     # Periode    (YYYYMM integer → read as text, parse manually)
  "text",     # KSt        (store: "004 - Newspoint")
  "skip",     # Hersteller (manufacturer — not needed)
  "text",     # Artikel Name
  "skip",     # Marke      (brand — not needed)
  "skip",     # StabileNr  (internal stable number — not needed)
  "text",     # EAN
  "text",     # WarenGrp
  "text",  # Umsatz brutto
  "text"   # Absatz
)

# Column names after skipping
.COL_DATE    <- "Periode"       # YYYYMM string e.g. "202512"
.COL_STORE   <- "KSt"          # "004 - Newspoint"
.COL_EAN     <- "EAN"
.COL_TEXT    <- "Artikel Name"
.COL_WGR     <- "WarenGrp"     # "04000 - Zigaretten versteuert"
.COL_QTY     <- "Absatz"
.COL_REV     <- "Umsatz brutto"
.COL_WGRCODE <- NULL

# Periode is YYYYMM → custom format (not a daily date)
.DATE_FORMAT <- "yyyymm"   # handled by custom .parse_date below

# Monthly delivery: week_file = month (yyyymm)
.DELIVERY <- "monthly"

# WarenGrp: "04000 - Zigaretten versteuert" → split on " - "
# wgr_code = "04000", wgr_text = "Zigaretten versteuert"
.PARSE_WGR <- TRUE

# After split, wgr_text values:
.TOBACCO_GROUPS <- c(
  "Zigaretten versteuert",
  "Feinschnitt versteuert",
  "Pfeifentabak versteuert",
  "Tabak Sticks versteuert",
  "Kräutermischung Sticks versteuert",
  "Liquids/Verdampfer versteuert",
  "E-Zigarette versteuert"
)

.ACCESSORY_GROUPS <- c(
  "Raucherbedarf"
)

# =============================================================================
# SECTION 2 — GENERIC HELPERS
# =============================================================================

# Returns a dataframe of file properties
.build_meta <- function(path, pattern, date_from_fn) {
  file_paths <- list.files(path, pattern = pattern,
                            full.names = TRUE, ignore.case = TRUE)
  if (length(file_paths) == 0) {
    message(sprintf("WARNING: No files found in %s matching '%s'", path, pattern))
    return(data.frame(
      file_path = character(0), 
      file_name = character(0),
      file_date = as.Date(character(0)), 
      year = integer(0),
      week = character(0), 
      size_kb = numeric(0),
      stringsAsFactors = FALSE
    ))
  }
  data.frame(file_path = file_paths,
             file_name = basename(file_paths),
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

.parse_store_id <- function(store_vec) {
  # "004 - Newspoint" → keep raw, extract leading code if needed later
  as.character(store_vec)
}

# EuroTrade: split "04000 - Zigaretten versteuert" on " - "
# wgr_code = "04000", wgr_text = "Zigaretten versteuert"
.parse_wgr <- function(wgr_vec) {
  s       <- trimws(as.character(wgr_vec))
  has_sep <- grepl(" - ", s, fixed = TRUE)
  list(
    wgr_code = ifelse(has_sep, trimws(sub(" -.*$",   "", s)), NA_character_),
    wgr_text = ifelse(has_sep, trimws(sub("^[^-]+ - ", "", s)), s)
  )
}

# EuroTrade date: Periode = YYYYMM integer (monthly aggregate)
# → No daily date available → week_in_data derived from first day of month
# → week_file = month (yyyymm) since .DELIVERY == "monthly"
.parse_date <- function(date_vec, year_vec = NULL) {
  # Periode: "202512" → first day of month → ISO week
  yyyymm <- trimws(as.character(date_vec))
  yyyy   <- substr(yyyymm, 1, 4)
  mm     <- substr(yyyymm, 5, 6)
  d      <- as.Date(paste0(yyyy, "-", mm, "-01"))
  parse_date_iso(format(d, "%Y-%m-%d"))
}

# =============================================================================
# SECTION 3 — READ LOGIC
# =============================================================================

.read_file <- function(file_path) {
  raw <- readxl::read_excel(
    file_path,
    sheet     = .SHEET,
    col_types = .COL_TYPES
  )
  # After skipping cols, rename to expected names
  # Remaining cols (after skip): Periode, KSt, Artikel Name, EAN, WarenGrp,
  #                               Umsatz brutto, Absatz
  # readxl drops skipped cols automatically
  raw
}

# =============================================================================
# SECTION 4 — STANDARDISE TO OUTPUT CONTRACT
# =============================================================================

.standardise <- function(raw_df, file_name_val, size_kb_val) {
  
  parsed <- .parse_date(raw_df[[.COL_DATE]])
  store  <- .parse_store_id(raw_df[[.COL_STORE]])
  wgr    <- .parse_wgr(raw_df[[.COL_WGR]])
  
  # Monthly: week_file = month (yyyymm) — all rows in file share same month
  week_file_out <- parsed$month
  
  # EuroTrade is monthly aggregate — no daily date in data.
  # Periode = yyyymm only → week_in_data set to month (yyyymm) as well.
  # summary_df_single() groups by week_in_data — for EuroTrade this means
  # 1 row per month in the QC summary, which is the correct granularity.
  raw_df %>%
    dplyr::transmute(
      week_in_data  = parsed$month,   # monthly data: use month as grouping key
      month_in_data = parsed$month,
      week_file     = week_file_out,
      store_id      = store,
      EAN           = nosigns(as.character(.data[[.COL_EAN]])),
      product_text  = as.character(.data[[.COL_TEXT]]),
      wgr_code      = wgr$wgr_code,
      wgr_text      = wgr$wgr_text,
      sales_units   = as.numeric(gsub(",", ".", .data[[.COL_QTY]])),
      sales_revenue = as.numeric(gsub(",", ".", .data[[.COL_REV]])),
      einzelpreis   = NA_real_,
      file_name     = file_name_val,
      size_kb       = as.character(size_kb_val)
    )
}

# =============================================================================
# MAIN INGEST FUNCTION
# =============================================================================

#' Load EuroTrade data. Default: all files in .PATHORG.
#' To load only 2025 backdata:
#' ingested <- ingest(year_filter = 2025)
#'
#' @param year_filter integer or NULL — filter files by year in filename
ingest <- function(year_filter = NULL) {

  # Call section 2 build_meta()
  meta <- .build_meta(.PATHORG, .PATTERN, .DATE_FROM_FILENAME)

  if (!is.null(year_filter)) {
    meta <- dplyr::filter(meta,
      as.integer(format(file_date, "%Y")) == year_filter)
    message(sprintf("  [%s] year_filter=%d → %d file(s)",
      .PROVIDER, year_filter, nrow(meta)))
  }

  if (nrow(meta) == 0)
    stop(sprintf("No files found for '%s'. Check .PATHORG and .PATTERN.", .PROVIDER))

  message(sprintf("  [%s] %d file(s) found", .PROVIDER, nrow(meta)))

  # Call section 3 - read logic
  raw_list <- lapply(seq_len(nrow(meta)), function(i) {
    message(sprintf("  Reading: %s (%.0f KB)", meta$file_name[i], meta$size_kb[i]))
    .read_file(meta$file_path[i])
  })
  names(raw_list) <- meta$file_name
  
  
  # Call section 3 - STANDARDISE TO OUTPUT CONTRACT - Use as standrad input data structure
  std_list <- lapply(seq_len(nrow(meta)), function(i) {
    .standardise(raw_list[[i]],
                 file_name_val = meta$file_name[i],
                 size_kb_val   = meta$size_kb[i])
  })

  std_data <- dplyr::bind_rows(std_list) %>%
    dplyr::arrange(month_in_data, store_id, EAN)

  message(sprintf(
    "  [%s] %s rows | %d months | %d stores | %d EANs",
    .PROVIDER,
    format(nrow(std_data), big.mark = ","),
    dplyr::n_distinct(std_data$month_in_data),
    dplyr::n_distinct(std_data$store_id),
    dplyr::n_distinct(std_data$EAN)
  ))

  list(
    data             = std_data,
    meta             = meta,
    raw              = raw_list,
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

# =============================================================================
# EUROTRADE-SPECIFIC: RAW SALES VOLUME SUMMARY (before PMI match)
# =============================================================================

# WGR classification for EuroTrade report types
# Used by build_raw_sales_summary() below
.CIG_GROUPS <- c(
  "Zigaretten versteuert",
  "Feinschnitt versteuert",
  "Tabak Sticks versteuert",
  "Kräutermischung Sticks versteuert"
)

.OTP_GROUPS <- c(
  "Feinschnitt versteuert"
)

.ECIG_GROUPS <- c(
  "E-Zigarette versteuert",
  "Liquids/Verdampfer versteuert"
)

#' Summarise raw Sales Volume from factdata by report type and month.
#' EuroTrade-specific — uses wgr_text to classify rows.
#' No PMI join required — run directly after factdata_final().
#' Note: 9961/9962/9963 split not possible without PMI PACKAGETYPE.
#'       All ECIG WGR rows grouped as "ECIG_EUROTRADE" in raw summary.
#'
#' @param fact_df  output of factdata_final()
#' @return data.frame: Measure | Report | one column per month (yyyymm), desc
build_raw_sales_summary <- function(fact_df) {
  
  provider_up <- toupper(.PROVIDER)
  
  classify <- function(wgr) {
    dplyr::case_when(
      wgr %in% .OTP_GROUPS  ~ "OTP",
      wgr %in% .CIG_GROUPS  ~ "CIG",
      wgr %in% .ECIG_GROUPS ~ "ECIG",
      TRUE                   ~ "OTHER"
    )
  }
  
  fact_df <- fact_df %>%
    dplyr::mutate(report_type = classify(wgr_text))
  
  build_row <- function(df, report_label) {
    df %>%
      dplyr::group_by(Month) %>%   # factdata_final() already names this column "Month"
      dplyr::summarise(Sales = sum(Sales, na.rm = TRUE), .groups = "drop") %>%
      dplyr::mutate(
        Measure = "Sales Volume",
        Report  = paste0(report_label, "_", provider_up)
      )
  }
  
  long <- dplyr::bind_rows(
    build_row(fact_df,                                          "PMG"),
    build_row(dplyr::filter(fact_df, report_type == "CIG"),    "CIG"),
    build_row(dplyr::filter(fact_df, report_type == "OTP"),    "OTP"),
    build_row(dplyr::filter(fact_df, report_type == "ECIG"), "ECIG")
  )
  
  # Pivot wide: months descending as columns
  months <- sort(unique(long$Month), decreasing = TRUE)
  
  long %>%
    dplyr::select(Measure, Report, Month, Sales) %>%
    tidyr::pivot_wider(
      names_from  = Month,
      values_from = Sales,
      values_fill = 0
    ) %>%
    dplyr::select(Measure, Report, dplyr::all_of(months))
}


# =============================================================================
# EUROTRADE-SPECIFIC: SALES VOLUME SUMMARY (after PMI match)
# =============================================================================

#' Summarise Sales Volume from df_final (after PMI match) by report type and month.
#' Uses PMI attributes — requires match_with_pmi() + add_sales_factors() to have run.
#' Mirrors build_all_reports() filter logic from reports.R exactly.
#' Includes MISSING_ITEM (kein match rows) as additional report type.
#'
#' @param df_final  output of add_sales_factors(match_with_pmi(fact_df, pmi_file))
#' @return data.frame: Measure | Report | one column per month (yyyymm), desc
build_sales_summary_post_pmi <- function(df_final) {
  
  provider_up <- toupper(.PROVIDER)
  
  build_row <- function(df, report_label) {
    df %>%
      dplyr::group_by(Month) %>%
      dplyr::summarise(Sales = sum(Sales, na.rm = TRUE), .groups = "drop") %>%
      dplyr::mutate(
        Measure = "Sales Volume",
        Report  = paste0(report_label, "_", provider_up)
      )
  }
  
  long <- dplyr::bind_rows(
    # PMG — PRODUCER == "PMG"
    build_row(
      dplyr::filter(df_final, PRODUCER == "PMG"),
      "PMG"
    ),
    # CIG — Cigarette + HTP Sticks
    build_row(
      dplyr::filter(df_final,
                    PRODUCTTYPE == "Cigarette" | LENGTHTYPE == "RRP HTP STICKS" | LENGTHTYPE == "HNP STICKS",
                    !is.na(EAN)),
      "CIG"
    ),
    # OTP — Finecut
    build_row(
      dplyr::filter(df_final, PRODUCTTYPE == "Finecut", !is.na(EAN)),
      "OTP"
    ),
    # 9961 — ECIG PODS / BOTTLE / CAPS
    build_row(
      dplyr::filter(df_final,
                    grepl("ECigarette", DISTRIBUTIONCATEGORY),
                    PACKAGETYPE %in% c("BOTTLE", "PODS", "CAPS"),
                    !is.na(EAN)),
      "9961"
    ),
    # 9962 — ECIG Box
    build_row(
      dplyr::filter(df_final,
                    grepl("ECigarette", DISTRIBUTIONCATEGORY),
                    PACKAGETYPE == "Box",
                    !is.na(EAN)),
      "9962"
    ),
    # 9963 — ECIG KIT
    build_row(
      dplyr::filter(df_final,
                    grepl("ECigarette", DISTRIBUTIONCATEGORY),
                    PACKAGETYPE == "KIT",
                    !is.na(EAN)),
      "9963"
    ),
    # MISSING_ITEM — no PMI match
    build_row(
      dplyr::filter(df_final, match_pmi == "kein match"),
      "MISSING_ITEM"
    )
  )
  
  # Pivot wide: months descending as columns
  months <- sort(unique(long$Month), decreasing = TRUE)
  
  long %>%
    dplyr::select(Measure, Report, Month, Sales) %>%
    tidyr::pivot_wider(
      names_from  = Month,
      values_from = Sales,
      values_fill = 0
    ) %>%
    dplyr::select(Measure, Report, dplyr::all_of(months))
}


#' Extract distinct CIG EANs per month with raw wgr_text from provider.
#' Uses same CIG filter as build_sales_summary_post_pmi().
#'
#' @param df_joined  output of match_with_pmi(fact_df, pmi_file)
#' @return data.frame: Month | EAN | wgr_text | product_text | Sales
extract_cig_ean_by_month <- function(df_joined) {
  
  df_joined %>%
    dplyr::filter(
      PRODUCTTYPE == "Cigarette" |
        LENGTHTYPE  == "RRP HTP STICKS" |
        LENGTHTYPE  == "HNP STICKS"
    ) %>%
    dplyr::group_by(Month, Retailer, EAN, wgr_text, product_text) %>%
    dplyr::summarise(
      Total_Sales_Units = sum(Sales,   na.rm = TRUE),
      Total_Revenue     = sum(Revenue, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::arrange(Month, dplyr::desc(Total_Sales_Units))
}
