# =============================================================================
# standardize_decimal.R
#
# Purpose: Standardize a numeric column into clean
# R numeric values, regardless of how the value was originally stored:
#   - plain numeric (Excel display-format only, no transformation needed)
#   - character with "," as decimal separator (DE/EU style: "9,90" -> 9.90)
#   - character with "." as decimal separator (US/UK style: "9.90" -> 9.90)
#   - character with both thousand + decimal separators:
#         "2.745,00" (DE) -> 2745.00
#         "2,745.00" (US) -> 2745.00
#
# Logic: whichever separator ("," or ".") appears LAST in the string is
# treated as the decimal separator; the other one (if present) is treated
# as the thousand separator and removed.
#
# Use case: data is manually entered by the provider and format consistency
# is not guaranteed -> needs cleaning before entering the ETL pipeline.
# =============================================================================

library(dplyr)
library(stringr)

#' Standardize a single value (character or numeric) into numeric
#'
#' @param v input value (scalar)
#' @return numeric, or NA if the value could not be parsed (warning issued)
.parse_one_decimal <- function(v) {
  if (is.na(v) || v == "") return(NA_real_)

  v <- trimws(v)
  has_comma <- str_detect(v, ",")
  has_dot   <- str_detect(v, "\\.")

  if (has_comma && has_dot) {
    # Both separators present -> the one appearing last is the decimal separator
    last_comma <- max(str_locate_all(v, ",")[[1]][, 1])
    last_dot   <- max(str_locate_all(v, "\\.")[[1]][, 1])

    if (last_comma > last_dot) {
      # DE style: "." = thousand, "," = decimal  -> "2.745,00"
      v <- str_remove_all(v, "\\.")
      v <- str_replace(v, ",", ".")
    } else {
      # US style: "," = thousand, "." = decimal  -> "2,745.00"
      v <- str_remove_all(v, ",")
    }

  } else if (has_comma) {
    # Only "," present -> distinguish decimal comma vs. thousand comma
    # Heuristic: if the part after the last "," has <= 2 digits -> decimal
    parts <- str_split(v, ",")[[1]]
    if (length(parts) == 2 && nchar(parts[2]) <= 2) {
      v <- str_replace(v, ",", ".")   # decimal: "9,90" -> "9.90"
    } else {
      v <- str_remove_all(v, ",")     # thousand: "9,900" -> "9900"
    }

  } else if (has_dot) {
    # Only "." present -> distinguish decimal dot vs. thousand dot
    parts <- str_split(v, "\\.")[[1]]
    if (length(parts) == 2 && nchar(parts[2]) <= 2) {
      # decimal dot, keep as is: "9.90" -> 9.90
    } else {
      v <- str_remove_all(v, "\\.")   # thousand dot: "9.900" -> "9900"
    }
  }
  # If no separator is present -> plain integer, keep as is

  suppressWarnings(as.numeric(v))
}

#' Standardize a decimal-number column regardless of its original format (vectorized)
#'
#' @param x vector (numeric or character; may be mixed type after read_excel)
#' @return standardized numeric vector
standardize_decimal <- function(x) {
  if (is.numeric(x)) return(as.numeric(x))

  x_chr <- as.character(x)
  result <- vapply(x_chr, .parse_one_decimal, numeric(1), USE.NAMES = FALSE)

  # QC: warn if any values failed to parse (newly created NA, not original NA)
  na_new <- is.na(result) & !is.na(x_chr) & x_chr != ""
  if (any(na_new)) {
    warning(sprintf(
      "standardize_decimal(): %d value(s) could not be parsed, e.g.: %s",
      sum(na_new),
      paste(head(x_chr[na_new], 5), collapse = ", ")
    ))
  }

  result
}

#' Apply standardize_decimal() to one or more columns in a dataframe
#'
#' @param df input dataframe
#' @param cols column name(s) (character vector) to standardize, e.g. c("Umsatz brutto", "Absatz")
#' @return dataframe with the specified columns standardized to numeric
standardize_decimal_cols <- function(df, cols) {
  df %>%
    mutate(across(all_of(cols), standardize_decimal))
}

# =============================================================================
# USAGE EXAMPLE
# =============================================================================
# --- 1. Discover source files ---
PATHORG  <- "/qa/data1/PMI/Archive/EuroTrade/"
PATTERN  <- "^Circana_PMI.*gesamt_\\d{6}\\.xlsx$"

files <- list_source_files(PATHORG, PATTERN)
files

# --- 2. Read + standardize (works for 1 file or many — files may have length 1) ---
df_all <- purrr::map_dfr(files, function(f) {
  readxl::read_excel(f, sheet = "Sheet01") %>%
    standardize_decimal_cols(cols = c("Umsatz brutto", "Absatz")) %>%
    mutate(source_file = basename(f))   # keep provenance for QC/debugging
})

# Quick QC after standardizing:
summary(df_all$`Umsatz brutto`)
sum(is.na(df_all$`Umsatz brutto`))   # should be 0 (or match original NA count)
#
# NOTE: for a single known file, skip step 1 and pass it directly:
# files <- "/qa/data1/PMI/Archive/EuroTrade/Circana_PMI_..._092025.xlsx"
