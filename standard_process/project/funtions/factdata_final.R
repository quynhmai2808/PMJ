## functions/factdata_final.R
## Build the aggregated fact table for PMI matching and reports.
## Shared across all providers — consumes only standard column names.
##
## Input : ingest()$data  (standardised, from data_list_2 / fact-accurate source)
## Output: one row per Date/Month/Retailer/EAN/product attributes,
##         with Sales, Revenue, calculate_price in standard names.

#' Aggregate standardised data to weekly EAN-level fact table.
#'
#' calculate_price is ALWAYS computed from Revenue/Sales regardless of
#' whether einzelpreis exists in the source — ensures mix-price QC is
#' consistent across all providers.
#'
#' @param std_data  ingest()$data — standardised data.frame.
#' @return data.frame with columns:
#'   Date, Month, Retailer, EAN, product_text, wgr_code, wgr_text,
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
        Sales > 0,
        round(Revenue / Sales, 2),
        NA_real_
      )
    )
}
