## functions/mix_price.R
## Detect EANs sold at multiple inconsistent prices (e.g. pack vs bundle
## mixed under the same barcode) and classify by tobacco type.

#' Add calculate_price column to a raw combined df.
add_calculate_price <- function(df, cols) {
  df %>%
    dplyr::mutate(
      Sales           = parse_decimal_comma(.data[[cols$qty]]),
      Revenue         = parse_decimal_comma(.data[[cols$amount]]),
      calculate_price = ifelse(Sales > 0, round(Revenue / Sales, 2), NA)
    )
}

#' Classify Warengruppe into Tobacco / Accessory / Non-Tobacco.
classify_tobacco_type <- function(warengruppe, tobacco_groups, accessory_groups) {
  dplyr::case_when(
    warengruppe %in% tobacco_groups   ~ "Tobacco",
    warengruppe %in% accessory_groups ~ "Accessory",
    TRUE ~ "Non-Tobacco"
  )
}

#' Find EANs with multiple distinct prices and pull all their raw rows,
#' restricted to tobacco items, with a configurable mix-price business rule
#' (ratio + absolute price gap).
#'
#' @param df_all  Combined raw data (all files), with provider raw columns.
#' @param cols    provider_cols.
#' @param ratio_min  Minimum max/min price ratio to flag as real mix price.
#' @param diff_min   Minimum absolute price difference to flag.
#' @return list(summary = mix_price_summary, raw = final_mix_price_lst)
mix_price_check <- function(df_all, cols, tobacco_groups, accessory_groups,
                             ratio_min = 2, diff_min = 5) {

  df_all <- add_calculate_price(df_all, cols)

  mix_price_ean <- df_all %>%
    dplyr::filter(!is.na(calculate_price)) %>%
    dplyr::group_by(.data[[cols$ean]]) %>%
    dplyr::summarise(n_price = dplyr::n_distinct(calculate_price), .groups = "drop") %>%
    dplyr::filter(n_price > 1)

  mix_price_cases_all <- df_all %>%
    dplyr::filter(.data[[cols$ean]] %in% mix_price_ean[[cols$ean]]) %>%
    dplyr::filter(!is.na(.data[[cols$ean]]) & calculate_price != 0) %>%
    dplyr::select(
      dplyr::all_of(c(cols$year, cols$week, cols$store, cols$qty, cols$amount,
                       cols$amount_netto, cols$ean, cols$text, cols$vke,
                       cols$wgr_code, cols$wgr)),
      calculate_price
    ) %>%
    dplyr::mutate(
      tobacco_type = classify_tobacco_type(.data[[cols$wgr]], tobacco_groups, accessory_groups)
    )

  mix_price_tobacco <- mix_price_cases_all %>%
    dplyr::filter(tobacco_type == "Tobacco")

  mix_price_summary <- mix_price_tobacco %>%
    dplyr::group_by(EAN = .data[[cols$ean]]) %>%
    dplyr::summarise(
      min_price     = min(calculate_price, na.rm = TRUE),
      max_price     = max(calculate_price, na.rm = TRUE),
      n_price       = dplyr::n_distinct(calculate_price),
      prices        = paste(sort(unique(calculate_price)), collapse = ", "),
      total_revenue = sum(parse_decimal_comma(.data[[cols$amount]]), na.rm = TRUE),
      ratio         = round(max_price / min_price, 2),
      price_diff    = max_price - min_price,
      n_records     = dplyr::n(),
      .groups = "drop"
    ) %>%
    dplyr::filter(
      n_price > 1,
      ratio > ratio_min,
      price_diff > diff_min
    ) %>%
    dplyr::arrange(EAN)

  final_mix_price_lst <- mix_price_tobacco %>%
    dplyr::filter(
      .data[[cols$ean]] %in% mix_price_summary$EAN,
      !is.na(.data[[cols$ean]]),
      !is.na(calculate_price),
      calculate_price != 0
    ) %>%
    dplyr::arrange(.data[[cols$ean]])

  list(summary = mix_price_summary, raw = final_mix_price_lst)
}
