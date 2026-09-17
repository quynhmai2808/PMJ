## funtions/ingest_DTV.R
## Provider-specific ingest for DTV — reads TWO raw dataset types per month,
## standardises each independently, then combines into one output.
##
## Dataset 1 (Reemtsma): yyyy_Monat_Reemtsma_Abverkauf_sent_gdp_complett.xlsx
##   - Sheet: "Sheet"
##   - Cols: Filiale, Datum(datetime), Lief.-Art.-Nr., Warengruppe, Artikelnummer,
##           Bezeichnung, Menge, Umsatz, Column1(formula — skip)
##   - Store format: "1 | 848 - DTV Hamm"  → kept as raw value
##   - Date: Excel datetime → parse_date_iso
##   - Warengruppe: "1:0 | Zigaretten"    → split on " | "
##   - EAN: string
##
## Dataset 2 (Auswertung): Auswertung_yyyy_mm_YYYYMMDD_HHmmss.xlsx
##   - Sheet: "Auswertung"
##   - Cols: Filialname, Datum(string dd.mm.yyyy), Lieferantenartikelnummer,
##           Warengruppe, Artikelnummer, Bezeichnung, Menge, Umsatz
##   - Store format: "200032 - Eitorf"     → kept as raw value
##   - Date: string "01.06.2026"           → parse_date_ddmmyyyy
##   - Warengruppe: "1 | Zigaretten"       → kept as raw value
##   - EAN: integer → nosigns()
##
## Both datasets are standardised to the same output contract then combined.

# =============================================================================
# SECTION 1 — DTV CONFIG
# =============================================================================

source("/qa/data1/PMI/Code/R_profiling_framework/data_pipeline/functions/utils.R")

.PROVIDER  <- "DTV"
.PATHORG   <- "/qa/data1/PMI/Original/DTV/"
.PATHRES   <- "/qa/data1/PMI/Results/DTV/"
.PATHPMI   <- "/qa/data1/PMI/Archive/PMI_Dateien/"

# Auto-detect the most recent PMI reference files by date stamp in filename.
# Format: pmg_product_YYYYMMDD.csv  /  missing_ean_YYYYMMDD.csv
# No manual update needed when new files are added to PATHPMI.
.detect_stamp <- function(path, pattern) {
  files <- list.files(path, pattern = pattern, full.names = FALSE)
  if (length(files) == 0) stop(sprintf("No file matching '%s' found in %s", pattern, path))
  # Extract YYYYMMDD stamp and return the largest (= most recent)
  stamps <- regmatches(files, regexpr("\\d{8}", files))
  max(stamps)
}
.PMG_STAMP     <- .detect_stamp(.PATHPMI, "^pmg_product_\\d{8}\\.csv$")
.MISSING_STAMP <- .detect_stamp(.PATHPMI, "^missing_ean_\\d{8}\\.csv$")

# --- File patterns ---
# Dataset 1: yyyy_Monat_Reemtsma_Abverkauf_sent_gdp_complett.xlsx
# .PATTERN_REEMTSMA  <- "^\\d{4}_.*Reemtsma.*Abverkauf.*\\.xlsx$"
.PATTERN_REEMTSMA   <- "Reemtsma.*\\.xlsx$"
# Dataset 2: Auswertung_yyyy_mm_YYYYMMDD_HHmmss.xlsx
.PATTERN_AUSWERTUNG <- "Auswertung.*\\.xlsx$"
# .PATTERN_AUSWERTUNG <- "^Auswertung_\\d{4}_\\d{2}_.*\\.xlsx$"

# --- Date from filename helper (for meta_df only, not used for week_in_data) ---
.DATE_REEMTSMA <- function(fn) {
  month_map <- c("Januar"="01","Februar"="02","März"="03","April"="04",
                 "Mai"="05","Juni"="06","Juli"="07","August"="08",
                 "September"="09","Oktober"="10","November"="11","Dezember"="12")
  year       <- sub("^(\\d{4})_.*",       "\\1", fn)
  month_name <- sub("^\\d{4}_([^_]+)_.*", "\\1", fn)
  month_num  <- month_map[month_name]
  if (is.na(month_num)) return(as.Date(NA))
  as.Date(paste0(year, "-", month_num, "-01"))
}

.DATE_AUSWERTUNG <- function(fn) {
  # Auswertung_2026_06_20260701_070006.xlsx → 2026-06-01
  year  <- sub("^Auswertung_(\\d{4})_.*", "\\1", fn)
  month <- sub("^Auswertung_\\d{4}_(\\d{2})_.*", "\\1", fn)
  as.Date(paste0(year, "-", month, "-01"))
}

# --- Column names per dataset type ---
.R_STORE   <- "Filiale"
.R_DATE    <- "Datum"
.R_LIEF_NR <- "Lief.-Art.-Nr."
.R_WGR     <- "Warengruppe"      
.R_EAN     <- "Artikelnummer"    
.R_TEXT    <- "Bezeichnung"      
.R_QTY     <- "Menge"           
.R_REV     <- "Umsatz"           


.A_STORE   <- "Filialname"
.A_DATE    <- "Datum"
.A_LIEF_NR <- "Lieferantenartikelnummer"
.A_WGR     <- "Warengruppe"
.A_EAN     <- "Artikelnummer"
.A_TEXT    <- "Bezeichnung"
.A_QTY     <- "Menge"
.A_REV     <- "Umsatz"

# --- Tobacco / Accessory groups ---
# wgr_text is now the PARSED text part after splitting on " | "
# (e.g. "1:0 | Zigaretten" → wgr_text = "Zigaretten")
# Both Reemtsma and Auswertung share the same text values after parsing.
.TOBACCO_GROUPS <- c(
  "Zigaretten",
  "Feinschnitt",
  "Zigarren",
  "E-Zigarette",
  "RBA",
  "ECO-Cig",
  "Heat not Burn",
  "E-Zig./Liquid",
  "Pfeifentab."
)
.ACCESSORY_GROUPS <- c(
  "Handelsmarken"
)

# =============================================================================
# SECTION 2 — GENERIC HELPERS (shared logic)
# =============================================================================

#' Build file metadata table for a given pattern.
.build_meta <- function(path, pattern, date_from_fn) {
  file_paths <- list.files(path=path, pattern=pattern,
                            full.names=TRUE, ignore.case=TRUE)
  if (length(file_paths)==0) {
    message(sprintf("WARNING: No files found in %s matching '%s'", path, pattern))
    return(data.frame(file_path=character(0), file_name=character(0),
                      file_date=as.Date(character(0)), year=integer(0),
                      week=character(0), 
                      size_kb=numeric(0),
                      stringsAsFactors=FALSE))
  }
  data.frame(file_path=file_paths, file_name=basename(file_paths),
              stringsAsFactors=FALSE) %>%
    dplyr::mutate(
      file_date  = as.Date(sapply(file_name, date_from_fn)),
      year       = lubridate::isoyear(file_date),
      week       = paste0(year, format(file_date, "%V")),
      size_bytes = file.info(file_path)$size,
      size_kb    = round(size_bytes/1024, 2)
    ) %>%
    dplyr::arrange(file_date)
}

#' Keep store raw value as-is — no parsing applied.
#' "1 | 848 - DTV Hamm" → "1 | 848 - DTV Hamm"
#' "200032 - Eitorf"    → "200032 - Eitorf"
.parse_store_id <- function(store_vec) {
  as.character(store_vec)
}

#' Split Warengruppe on " | " into wgr_code and wgr_text.
#' "1:0 | Zigaretten" → wgr_code="1:0",  wgr_text="Zigaretten"
#' "1 | Zigaretten"   → wgr_code="1",    wgr_text="Zigaretten"
#' "Zigaretten"       → wgr_code=NA,     wgr_text="Zigaretten"
#' TOBACCO_GROUPS matches on wgr_text (after split).
.parse_wgr <- function(wgr_vec) {
  s       <- trimws(as.character(wgr_vec))
  has_sep <- grepl(" | ", s, fixed = TRUE)
  
  list(
    wgr_code = ifelse(has_sep, trimws(sub(" \\|.*$",  "", s)), NA_character_),
    wgr_text = ifelse(has_sep, trimws(sub("^.*\\| ", "", s)), s)
  )
}

#' Common standardise step — same contract for both dataset types.
#' @param raw_df    data.frame already read
#' @param store_col raw column name for store
#' @param date_col  raw column name for date
#' @param ean_col   raw column name for EAN
#' @param text_col  raw column name for product text
#' @param qty_col   raw column name for quantity
#' @param rev_col   raw column name for revenue
#' @param wgr_col   raw column name for Warengruppe
#' @param date_parser_fn  function(date_vec) → list(week, month)
#' @param file_name_val   filename string (for metadata)
#' @param size_kb_val     file size (for metadata)
.standardise <- function(raw_df, store_col, date_col, ean_col, text_col,
                          qty_col, rev_col, wgr_col,
                          date_parser_fn, file_name_val, size_kb_val){
                           
  parsed  <- date_parser_fn(raw_df[[date_col]])
  store   <- .parse_store_id(raw_df[[store_col]])
  wgr     <- .parse_wgr(raw_df[[wgr_col]])

  # Compute weekday from the actual date object (before conversion to yyyyww).
  # date_parser_fn is called internally — we need the Date object directly.
  # For Reemtsma: Datum is Excel datetime → as.Date()
  # For Auswertung: Datum is string "dd.mm.yyyy" → parse manually
  actual_date <- tryCatch(
    as.Date(raw_df[[date_col]]),                              # Reemtsma path
    error = function(e) {
      as.Date(gsub("\\.", "-",
                   sub("^(\\d{2})\\.(\\d{2})\\.(\\d{4})$", "\\3-\\2-\\1",
                       as.character(raw_df[[date_col]]))), format = "%Y-%m-%d")
    }
  )
  weekday_vec <- weekdays(actual_date)   # e.g. "Monday", "Montag"
  
  raw_df %>%
    dplyr::transmute(
      week_in_data  = parsed$week,
      month_in_data = parsed$month,
      store_id      = store,
      EAN           = nosigns(as.character(.data[[ean_col]])),
      product_text  = as.character(.data[[text_col]]),
      wgr_code      = wgr$wgr_code,
      wgr_text      = wgr$wgr_text,
      sales_units   = as.numeric(gsub(",", ".", .data[[qty_col]])),
      sales_revenue = as.numeric(gsub(",", ".", .data[[rev_col]])),
      einzelpreis   = NA_real_,
      # file metadata for summary_df
      file_name       = file_name_val,
      size_kb         = as.character(size_kb_val),
      # DTV delivers monthly files → week_file = month (yyyymm)
      # For weekly files, change to: parsed$week
      week_file = parsed$month,
      Date = actual_date,
      weekday = weekday_vec 
    )
}

# =============================================================================
# SECTION 3 — DATASET-SPECIFIC READ + STANDARDISE
# =============================================================================

#' Read and standardise Reemtsma files (yyyy_Monat_Reemtsma_...).
#' - Date: Excel datetime → format to ISO → parse_date_iso()
 
.read_reemtsma <- function(meta) {
  if (nrow(meta)==0) return(list())
  lapply(seq_len(nrow(meta)), function(i) {
    message(sprintf("  [Reemtsma] Reading: %s", meta$file_name[i]))
    raw <- readxl::read_excel(
      meta$file_path[i],
      sheet     = 1,
      col_types = c("text","date","text","text","text","text","text","text")
    )
    date_parser <- function(date_vec) {
      parse_date_iso(format(as.Date(date_vec), "%Y-%m-%d"))
    }
    .standardise(raw,
                 store_col      = .R_STORE,
                 date_col       = .R_DATE,
                 ean_col        = .R_EAN,
                 text_col       = .R_TEXT,
                 qty_col        = .R_QTY,
                 rev_col        = .R_REV,
                 wgr_col        = .R_WGR,
                 date_parser_fn = date_parser,
                 file_name_val  = meta$file_name[i],
                 size_kb_val    = meta$size_kb[i])
  }) %>% stats::setNames(meta$file_name)
}

#' Read and standardise Auswertung files (Auswertung_yyyy_mm_...).
#' - Date: string "dd.mm.yyyy" → parse_date_ddmmyyyy()
#' - EAN: integer → nosigns() in .standardise handles this
#' - Store: "200032 - Eitorf" → .parse_store_id extracts leading digits
.read_auswertung <- function(meta) {
  if (nrow(meta)==0) return(list())
  lapply(seq_len(nrow(meta)), function(i) {
    message(sprintf("  [Auswertung] Reading: %s", meta$file_name[i]))
    raw <- readxl::read_excel(
      meta$file_path[i],
      sheet     = 1,
      col_types = c("text","text","text","text","text","text","text","text")
    )
    date_parser <- function(date_vec) {
      # Convert "dd.mm.yyyy" → parse_date_ddmmyyyy expects "ddmmyyyy"
      date_clean <- gsub("\\.", "", as.character(date_vec))
      parse_date_ddmmyyyy(date_clean)
    }
    .standardise(raw,
                 store_col      = .A_STORE,
                 date_col       = .A_DATE,
                 ean_col        = .A_EAN,
                 text_col       = .A_TEXT,
                 qty_col        = .A_QTY,
                 rev_col        = .A_REV,
                 wgr_col        = .A_WGR,
                 date_parser_fn = date_parser,
                 file_name_val  = meta$file_name[i],
                 size_kb_val    = meta$size_kb[i])
  }) %>% stats::setNames(meta$file_name)
}

# =============================================================================
# MAIN INGEST FUNCTION
# =============================================================================

#' Full DTV ingest: reads both dataset types, standardises separately,
#' combines into one unified data.frame.
#'
#' @return list(
#'   data             = combined standardised data.frame  ← used by all shared functions
#'   data_reemtsma    = standardised Reemtsma only        ← audit trail
#'   data_auswertung  = standardised Auswertung only      ← audit trail
#'   meta_reemtsma    = file metadata Reemtsma
#'   meta_auswertung  = file metadata Auswertung
#'   paths            = list(PATHRES, PATHPMI, PMG_STAMP, MISSING_STAMP)
#'   provider         = "DTV"
#'   tobacco_groups   = .TOBACCO_GROUPS
#'   accessory_groups = .ACCESSORY_GROUPS
#' )
#' @param year_filter integer or NULL — filter files by year in filename
ingest <- function(year_filter = NULL) {

  # Call section 2 build_meta()
  # 1. Discover files
  meta_r <- .build_meta(.PATHORG, .PATTERN_REEMTSMA,  .DATE_REEMTSMA)
  meta_a <- .build_meta(.PATHORG, .PATTERN_AUSWERTUNG, .DATE_AUSWERTUNG)
  
  if (!is.null(year_filter)) {
    meta_r <- dplyr::filter(meta_r,
      as.integer(format(file_date, "%Y")) == year_filter)
    message(sprintf("  [%s] year_filter=%d → %d file(s)",
      .PROVIDER, year_filter, nrow(meta_r)))
    
    meta_a <- dplyr::filter(meta_a,
      as.integer(format(file_date, "%Y")) == year_filter)
    message(sprintf("  [%s] year_filter=%d → %d file(s)",
      .PROVIDER, year_filter, nrow(meta_a)))
  }


  if (nrow(meta_r)==0 && nrow(meta_a)==0) {
    stop("No DTV files found. Check .PATHORG and file patterns.")
  }

  # 2. Read + standardise each dataset type separately
  raw_r <- .read_reemtsma(meta_r)
  raw_a <- .read_auswertung(meta_a)

  std_r <- dplyr::bind_rows(raw_r)
  std_a <- dplyr::bind_rows(raw_a)

  # 3. Combine — single unified data.frame for all downstream shared functions
  # factdata_final(), summary_df_merged(), mix_price_check() all receive this
  std_all <- dplyr::bind_rows(std_r, std_a) %>%
    dplyr::arrange(week_in_data, store_id, EAN)

  message(sprintf(
    "  DTV ingest complete: %s Reemtsma rows + %s Auswertung rows = %s total",
    format(nrow(std_r), big.mark=","),
    format(nrow(std_a), big.mark=","),
    format(nrow(std_all), big.mark=",")
  ))

  list(
    data             = std_all,       # ← single combined output for pipeline
    data_reemtsma    = std_r,         # ← audit: Reemtsma only
    data_auswertung  = std_a,         # ← audit: Auswertung only
    meta_reemtsma    = meta_r,
    meta_auswertung  = meta_a,
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
# DTV-SPECIFIC: RAW SALES VOLUME SUMMARY (before PMI match)
# =============================================================================

# WGR classification for DTV report types
# Used by build_raw_sales_summary() below
.CIG_GROUPS <- c(
  "Zigaretten",
  "Heat not Burn"
  )

.OTP_GROUPS <- c(
  "Feinschnitt", 
  "Zigarren", 
  "Pfeifentab."
)

.ECIG_GROUPS <- c(
  "E-Zigarette", 
  "RBA", 
  "ECO-Cig", 
  "E-Zig./Liquid"
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
      dplyr::group_by(Date) %>%  
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
  weeks <- sort(unique(long$Date), decreasing = TRUE)
  
  long %>%
    dplyr::select(Measure, Report, Date, Sales) %>%
    tidyr::pivot_wider(
      names_from  = Date,
      values_from = Sales,
      values_fill = 0
    ) %>%
    dplyr::select(Measure, Report, dplyr::all_of(weeks))
}


# =============================================================================
# DTV-SPECIFIC: SALES VOLUME SUMMARY (after PMI match)
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
      dplyr::group_by(Date) %>%
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
    # MISSING_ITEM — no PMI match and exclude wgr with specific conditions
    # aggregate across ALL weeks first → distinct EAN
    # No week split: same EAN appearing in multiple weeks counts only once
    build_row(
      dplyr::filter(df_final, match_pmi == "kein match", 
                    !wgr_text  %in% c("Pfeifentab.", "Zigarren", "Zigarrenn", "RBA")) %>%
      dplyr::distinct(Date, EAN, .keep_all = TRUE),
      "MISSING_ITEM"
    )
  )
  
  # Pivot wide: months descending as columns
  weeks <- sort(unique(long$Date), decreasing = TRUE)
  
  long %>%
    dplyr::select(Measure, Report, Date, Sales) %>%
    tidyr::pivot_wider(
      names_from  = Date,
      values_from = Sales,
      values_fill = 0
    ) %>%
    dplyr::select(Measure, Report, dplyr::all_of(weeks))
}
