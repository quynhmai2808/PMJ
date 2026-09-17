## funtions/mix_price.R
## Mix-price detection — shared across all providers.
## Consumes standard column names from ingest() output contract.

#' Classify product category from wgr_text.
classify_tobacco_type <- function(wgr_text, tobacco_groups, accessory_groups) {
  dplyr::case_when(
    wgr_text %in% tobacco_groups   ~ "Tobacco",
    wgr_text %in% accessory_groups ~ "Accessory",
    TRUE                           ~ "Non-Tobacco"
  )
}

#' Detect EANs sold at multiple inconsistent prices within Tobacco category.
#'
#' Logic:
#'   1. Compute calc_price = revenue / qty (qty > 0 only)
#'   2. Classify rows via tobacco_groups → tobacco_type
#'   3. Keep only Tobacco rows with valid calc_price > 0
#'   4. Find EANs with >1 distinct price
#'   5. Apply business rules: ratio > ratio_min AND price_diff > diff_min
#'
#' Diagnostic messages printed at each step for debugging.
#'
#' @param std_data         ingest()$data
#' @param tobacco_groups   character vector from ingest()$tobacco_groups
#' @param accessory_groups character vector from ingest()$accessory_groups
#' @param ratio_min        min max/min price ratio to flag (default 2)
#' @param diff_min         min absolute price gap in € to flag (default 5)
#' @return list(summary, raw)
mix_price_check <- function(std_data, tobacco_groups, accessory_groups,
                            ratio_min = 2, diff_min = 5) {

  message(sprintf("  mix_price_check: %s total rows | tobacco_groups: %s",
    format(nrow(std_data), big.mark = ","),
    paste(tobacco_groups, collapse = ", ")
  ))

  # Step 1: calc_price + classify
  df <- std_data %>%
    dplyr::mutate(
      calc_price = dplyr::if_else(
        !is.na(sales_units) & sales_units > 0,
        round(sales_revenue / sales_units, 2),
        NA_real_
      ),
      tobacco_type = classify_tobacco_type(wgr_text, tobacco_groups, accessory_groups)
    )

  type_counts <- table(df$tobacco_type)
  message(sprintf("  Classification: %s",
    paste(names(type_counts), type_counts, sep = "=", collapse = " | ")
  ))

  # Step 2: tobacco rows with valid price
  tobacco_df <- df %>%
    dplyr::filter(
      tobacco_type == "Tobacco",
      !is.na(EAN), EAN != "",
      !is.na(calc_price),
      calc_price > 0
    )

  message(sprintf("  Tobacco rows with calc_price > 0: %s",
    format(nrow(tobacco_df), big.mark = ",")))

  if (nrow(tobacco_df) == 0) {
    message("  → No tobacco rows found. Check TOBACCO_GROUPS matches wgr_text values.")
    message(sprintf("  → wgr_text sample: %s",
      paste(head(unique(df$wgr_text), 8), collapse = " | ")))
    return(list(summary = data.frame(), raw = data.frame()))
  }

  # Step 3: EANs with >1 distinct price within tobacco rows
  multi_price_eans <- tobacco_df %>%
    dplyr::group_by(EAN) %>%
    dplyr::summarise(n = dplyr::n_distinct(calc_price), .groups = "drop") %>%
    dplyr::filter(n > 1) %>%
    dplyr::pull(EAN)

  message(sprintf("  EANs with >1 price (before business rules): %d",
    length(multi_price_eans)))

  tobacco_df <- dplyr::filter(tobacco_df, EAN %in% multi_price_eans)

  if (nrow(tobacco_df) == 0) {
    message("  → No multi-price EANs found in tobacco rows.")
    return(list(summary = data.frame(), raw = data.frame()))
  }

  # Step 4: summarise + apply business rules
  # mix_price_summary <- tobacco_df %>%
  #   dplyr::group_by(EAN) %>%
  #   dplyr::summarise(
  #     min_price     = min(calc_price),
  #     max_price     = max(calc_price),
  #     n_price       = dplyr::n_distinct(calc_price),
  #     prices        = paste(sort(unique(calc_price)), collapse = ", "),
  #     total_revenue = sum(sales_revenue, na.rm = TRUE),
  #     ratio         = round(max_price / min_price, 2),
  #     price_diff    = max_price - min_price,
  #     n_records     = dplyr::n(),
  #     .groups       = "drop"
  #   ) %>%
  #   dplyr::filter(n_price > 1, ratio > ratio_min, price_diff > diff_min) %>%
  #   dplyr::arrange(EAN)
  # 
  # message(sprintf(
  #   "  After business rules (ratio>%s, diff>%s): %d EANs flagged",
  #   ratio_min, diff_min, nrow(mix_price_summary)
  # ))
  
  # Step 4 - Alternative: Adding rule 
  # Same Store + Same EAN + Same product text + >1 distinct price + ratio > 2 + diff > 5
  mix_price_summary <- tobacco_df %>%
    dplyr::group_by(
      week_in_data,
      store_id,
      EAN
    ) %>%
    dplyr::summarise(
      min_price     = min(calc_price),
      max_price     = max(calc_price),
      n_price       = dplyr::n_distinct(calc_price),
      prices        = paste(sort(unique(calc_price)), collapse = ", "),
      total_revenue = sum(sales_revenue, na.rm = TRUE),
      ratio         = round(max_price / min_price, 2),
      price_diff    = max_price - min_price,
      n_records     = dplyr::n(),
      .groups       = "drop"
    ) %>%
    # Exclude EANs where min_price = 0:
    # price = 0 means revenue = 0 with sales > 0 → likely promotional items,
    # already flagged by zero_rev_pos_qty in summary_df. Not a mix-price issue.
    dplyr::filter(n_price > 1, min_price > 0,
                  ratio > ratio_min, price_diff > diff_min) %>%
    dplyr::arrange(EAN)

message(sprintf(
  "  After business rules (ratio>%s, diff>%s): %d EANs flagged",
  ratio_min, diff_min, nrow(mix_price_summary)
))

  # Step 5: raw flagged rows
  # raw_flagged <- tobacco_df %>%
  #   dplyr::filter(EAN %in% mix_price_summary$EAN) %>%
  #   dplyr::arrange(EAN)
  
  # Step 5 - Alternativ: raw flagged rows
  raw_flagged <- tobacco_df %>%
    dplyr::inner_join(
      mix_price_summary %>%
        dplyr::select(
          week_in_data,
          store_id,
          EAN
        ),
      by = c(
        "week_in_data",
        "store_id",
        "EAN"
      )
    )

  list(summary = mix_price_summary, raw = raw_flagged)
}
