## funtions/factdata_final.R
## Build the aggregated fact table for PMI matching and reports.
## Shared across ALL providers — consumes only standard column names.
##
## Input:  ingest()$data — single combined standardised data.frame.
##         Works regardless of how many source files ingest() read
##         (1 file/week, 1 file/month, 2 file types combined, etc.)
##
## Output: one row per (Date, Month, Retailer, EAN, product_text, wgr_code,
##         wgr_text) with summed Sales, Revenue, and derived calculate_price.
##
## No rows are filtered — zero qty, negative qty (returns), and NA rows are
## all passed through so PMI matching covers the full dataset.
## calculate_price uses if_else(Sales != 0) to handle zero-qty rows safely.

#' Aggregate standardised data to weekly EAN-level fact table.
#'
#' @param std_data  ingest()$data — standardised data.frame.
#' @return data.frame:
#'   Date (yyyyww), Month (yyyymm), Retailer, EAN,
#'   product_text, wgr_code, wgr_text,
#'   Sales, Revenue, calculate_price
factdata_final <- function(std_data) {
  
  std_data %>%
    dplyr::group_by(
      Date     = week_in_data,
      Month    = month_in_data,
      Retailer = store_id,
      EAN,
      product_text,
      wgr_code,
      wgr_text
    ) %>%
    dplyr::summarise(
      Sales   = sum(sales_units,   na.rm = TRUE),
      Revenue = sum(sales_revenue, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      calculate_price = dplyr::if_else(
        !is.na(Sales) & Sales != 0,
        round(Revenue / Sales, 2),
        NA_real_
      )
    ) %>%
    dplyr::arrange(Date, Retailer, EAN)
}
