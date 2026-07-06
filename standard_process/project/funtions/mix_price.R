## functions/mix_price.R
## Mix-price detection — shared across all providers.
## Consumes standard column names from ingest() output contract.

#' Classify product category from wgr_text.
#' tobacco_groups and accessory_groups come from ingest config (provider-specific
#' values passed in — different providers may have different category names).
classify_tobacco_type <- function(wgr_text, tobacco_groups, accessory_groups) {
  dplyr::case_when(
    wgr_text %in% tobacco_groups   ~ "Tobacco",
    wgr_text %in% accessory_groups ~ "Accessory",
    TRUE                           ~ "Non-Tobacco"
  )
}

#' Detect EANs sold at multiple inconsistent prices across all files.
#'
#' @param std_data        ingest()$data — standardised data.frame.
#' @param tobacco_groups  character vector from ingest config.
#' @param accessory_groups character vector from ingest config.
#' @param ratio_min       Min max/min price ratio to flag (default 2).
#' @param diff_min        Min absolute price gap to flag (default 5).
#' @return list(summary = mix_price_summary df, raw = raw rows of flagged EANs)
mix_price_check <- function(std_data, tobacco_groups, accessory_groups,
                             ratio_min = 2, diff_min = 5) {

  # Compute calculate_price at row level for detection
  df <- std_data %>%
    dplyr::mutate(
      calc_price = dplyr::if_else(
        sales_units > 0,
        round(sales_revenue / sales_units, 2),
        NA_real_
      ),
      tobacco_type = classify_tobacco_type(wgr_text, tobacco_groups, accessory_groups)
    )

  # EANs with more than one distinct price (any category)
  multi_price_eans <- df %>%
    dplyr::filter(!is.na(calc_price)) %>%
    dplyr::group_by(EAN) %>%
    dplyr::summarise(n_price = dplyr::n_distinct(calc_price), .groups = "drop") %>%
    dplyr::filter(n_price > 1) %>%
    dplyr::pull(EAN)

  # Restrict to tobacco items only
  tobacco_df <- df %>%
    dplyr::filter(EAN %in% multi_price_eans, tobacco_type == "Tobacco",
                  !is.na(EAN), !is.na(calc_price), calc_price != 0)

  # Summarise and apply business rule filters
  mix_price_summary <- tobacco_df %>%
    dplyr::group_by(EAN) %>%
    dplyr::summarise(
      min_price     = min(calc_price, na.rm = TRUE),
      max_price     = max(calc_price, na.rm = TRUE),
      n_price       = dplyr::n_distinct(calc_price),
      prices        = paste(sort(unique(calc_price)), collapse = ", "),
      total_revenue = sum(sales_revenue, na.rm = TRUE),
      ratio         = round(max_price / min_price, 2),
      price_diff    = max_price - min_price,
      n_records     = dplyr::n(),
      .groups = "drop"
    ) %>%
    dplyr::filter(n_price > 1, ratio > ratio_min, price_diff > diff_min) %>%
    dplyr::arrange(EAN)

  # Raw rows for flagged EANs
  raw_flagged <- tobacco_df %>%
    dplyr::filter(EAN %in% mix_price_summary$EAN) %>%
    dplyr::arrange(EAN)

  list(summary = mix_price_summary, raw = raw_flagged)
}
