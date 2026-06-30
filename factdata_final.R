## functions/factdata_final.R
## Build the aggregated fact table used downstream for PMI matching/reports.
## Uses ONLY data_list_2 as input (date-accurate filenames). data_list_1 is
## not merged in here — it only feeds the QC summary_df.

#' Select the standard set of raw columns needed downstream.
#' @param data_list_raw  bind_rows() of a read_df() list (data_list_2).
#' @param cols           provider_cols.
select_factdata_cols <- function(data_list_raw, cols) {
  data_list_raw %>%
    dplyr::select(
      dplyr::all_of(c(cols$year, cols$week, cols$store, cols$qty, cols$amount,
                       cols$amount_netto, cols$ean, cols$text, cols$vke,
                       cols$wgr_code, cols$wgr)),
      file_date, week_in_data
    )
}

#' Aggregate to one row per file_date/Date/Retailer/EAN/Text/VKE/WGR/Month,
#' summing Sales and Revenue and recomputing calculate_price.
#'
#' @param factdata_df  Output of select_factdata_cols() for data_list_2.
#' @param cols         provider_cols.
factdata_final <- function(factdata_df, cols) {
  factdata_df %>%
    dplyr::mutate(
      Retailer = .data[[cols$store]],
      Date     = week_in_data,
      Month    = format(as.Date(file_date), "%Y%m"),
      EAN      = .data[[cols$ean]]
    ) %>%
    dplyr::group_by(
      file_date, Date, Retailer, EAN,
      .data[[cols$text]], .data[[cols$vke]],
      .data[[cols$wgr_code]], .data[[cols$wgr]],
      Month
    ) %>%
    dplyr::summarise(
      Sales           = sum(parse_decimal_comma(.data[[cols$qty]])),
      Revenue         = sum(parse_decimal_comma(.data[[cols$amount]])),
      calculate_price = round(Revenue / Sales, 2),
      .groups = "drop"
    ) %>%
    dplyr::rename(
      Bezeichnung    = !!cols$text,
      VKE            = !!cols$vke,
      Warengruppecode = !!cols$wgr_code,
      Warengruppe    = !!cols$wgr
    )
}
