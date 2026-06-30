## functions/summary_df.R
## Per-file QC statistics. Run separately for data_list_1 and data_list_2
## (different raw structures), then merged into a single review table.

#' Compute QC summary stats for one list of raw data.frames.
#'
#' @param data_list  Named list of data.frames (output of read_df()).
#' @param meta       Corresponding meta_df() table (same source as data_list).
#' @param cols       provider_cols list from config.R.
#' @param source_tag Label identifying which source produced this list
#'                    (e.g. "data_list_1" / "data_list_2"), kept as a column
#'                    so the merged table stays traceable.
#' @return One row per file with QC counts and flags.
summary_df_single <- function(data_list, meta, cols, source_tag) {

  if (length(data_list) == 0) return(data.frame())

  lapply(seq_len(length(data_list)), function(i) {

    df <- data_list[[i]]

    EAN  <- as.character(df[[cols$ean]])
    Text <- df[[cols$text]]

    Sales   <- parse_decimal_comma(df[[cols$qty]])
    Revenue <- parse_decimal_comma(df[[cols$amount]])

    calculate_price <- ifelse(Sales > 0, round(Revenue / Sales, 2), NA)
    df$calculate_price <- calculate_price

    # 1. Duplicate EAN with multiple distinct texts
    dup_ean_text <- df %>%
      dplyr::group_by(.data[[cols$ean]]) %>%
      dplyr::summarise(n_text = dplyr::n_distinct(.data[[cols$text]]), .groups = "drop") %>%
      dplyr::filter(n_text > 1) %>%
      nrow()

    # 2. EAN empty
    ean_empty <- sum(is.na(EAN) | trimws(EAN) == "" | EAN == "0")

    # 3. EAN non-numeric
    ean_non_numeric <- sum(!grepl("^[0-9]+$", EAN) & !is.na(EAN))

    # 4. EAN length error
    ean_length_error <- sum(nchar(EAN) > 14, na.rm = TRUE)

    # 5. Quantity has decimals
    qty_decimal <- sum(!is.na(Sales) & Sales %% 1 != 0)

    # 6. Mix price EAN count
    mix_price_ean <- df %>%
      dplyr::filter(!is.na(calculate_price)) %>%
      dplyr::group_by(.data[[cols$ean]]) %>%
      dplyr::summarise(n_price = dplyr::n_distinct(calculate_price, na.rm = TRUE), .groups = "drop") %>%
      dplyr::filter(n_price > 1)

    mix_price_ean_count <- nrow(mix_price_ean)

    # 7. Week consistency check (already computed per-row in read_df as week_match_flag)
    week_distinct_count <- dplyr::n_distinct(df$week_in_data)
    week_mismatch_count <- sum(!as.logical(df$week_match_flag), na.rm = TRUE)
    week_match_flag_all <- week_mismatch_count == 0 & week_distinct_count == 1

    data.frame(
      source        = source_tag,
      file_name     = meta$file_name[i],
      file_date     = meta$file_date[i],
      file_day      = meta$day[i],
      year_file     = meta$year[i],
      week_file     = meta$week[i],
      start_day     = meta$start_week[i],
      end_day       = meta$end_week[i],
      size_kb       = meta$size_kb[i],

      records = nrow(df),
      n_ean   = dplyr::n_distinct(EAN),
      n_store = dplyr::n_distinct(df[[cols$store]]),

      total_sales   = sum(Sales, na.rm = TRUE),
      total_revenue = sum(Revenue, na.rm = TRUE),

      dup_ean_multi_text  = dup_ean_text,
      ean_empty_count     = ean_empty,
      ean_non_numeric     = ean_non_numeric,
      ean_length_error    = ean_length_error,
      mix_price_ean_count = mix_price_ean_count,
      qty_decimal_count   = qty_decimal,
      week_distinct_count = week_distinct_count,
      week_mismatch_count = week_mismatch_count,
      week_match_flag     = week_match_flag_all,

      stringsAsFactors = FALSE
    )
  }) %>% dplyr::bind_rows()
}

#' Run summary_df_single() over both source lists and merge into one table.
#' data_list_1 and data_list_2 have different raw structures (list_1 has one
#' extra column), but summary_df_single() only touches abstract `cols` fields
#' plus row counts, so the differing raw structure doesn't block merging —
#' the `source` column keeps provenance visible for review.
summary_df_merged <- function(data_list_1, meta_1, data_list_2, meta_2, cols) {
  dplyr::bind_rows(
    summary_df_single(data_list_1, meta_1, cols, source_tag = "data_list_1"),
    summary_df_single(data_list_2, meta_2, cols, source_tag = "data_list_2")
  )
}
