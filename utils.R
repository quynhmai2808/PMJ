## functions/utils.R
## Generic helper functions, provider-agnostic.

#' Strip all non-numeric characters from a string
nosigns <- function(variable) {
  gsub("[^0-9]", "", variable)
}

#' Remove leading zeros from a string/number
remove_leading_zeros <- function(x) {
  sub("^0+", "", as.character(x))
}

#' Trim, strip non-alphanumeric edges, and uppercase a key for matching
normalize_key <- function(x) {
  x_chr   <- as.character(x)
  x_trim  <- trimws(x_chr)
  x_clean <- gsub("^[^[:alnum:]]+|[^[:alnum:]]+$", "", x_trim)
  toupper(x_clean)
}

#' Convert a "1.234,56"-style or "1234,56"-style string to numeric
#' Combase uses comma as decimal separator; this is provider-specific logic
#' isolated here so other providers can swap in their own parser.
parse_decimal_comma <- function(x) {
  as.numeric(gsub(",", ".", x))
}
