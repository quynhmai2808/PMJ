## functions/summary_df.R
## QC summary statistics — shared across all providers.
## Consumes only standard column names from ingest() output contract.
## Uses $meta for file-level info (file_name, week_file, size_kb).

#' Compute per-file QC stats for one standardised data.frame.
#'
#' @param std_data   Standardised data.frame (ingest()$data or $data_qc).
#'                   Must contain standard columns: EAN, product_text,
#'                   store_id, sales_units, sales_revenue,
#'                   week_in_data, week_match_flag, file_name, size_kb, week_file.
#' @param source_tag Label for this source (e.g. "list_1" / "list_2").
#' @return One row per file with QC counts and flags.
summary_df_single <- function(std_data, source_tag) {
  
  if (nrow(std_data) == 0) return(data.frame())
  
  # std_data still carries file_name / week_file / size_kb from .read_raw()
  # so we can split by file without needing meta separately
  file_groups <- split(std_data, std_data$file_name)
  
  lapply(file_groups, function(df) {
    
    EAN     <- as.character(df$EAN)
    Sales   <- df$sales_units
    Revenue <- df$sales_revenue
    
    calc_price <- ifelse(Sales > 0, round(Revenue / Sales, 2), NA_real_)
    
    # 1. EAN with multiple distinct product texts
    dup_ean_text <- df %>%
      dplyr::group_by(EAN) %>%
      dplyr::summarise(n_text = dplyr::n_distinct(product_text), .groups = "drop") %>%
      dplyr::filter(n_text > 1) %>%
      nrow()
    
    # 2. EAN empty / zero
    ean_empty <- sum(is.na(EAN) | trimws(EAN) == "" | EAN == "0")
    
    # 3. EAN non-numeric
    ean_non_numeric <- sum(!grepl("^[0-9]+$", EAN) & !is.na(EAN))
    
    # 4. EAN length > 14 digits
    ean_length_error <- sum(nchar(EAN) > 14, na.rm = TRUE)
    
    # 5. Quantity has decimals (should be whole units)
    qty_decimal <- sum(!is.na(Sales) & Sales %% 1 != 0)
    
    # 6. EANs sold at multiple distinct calculated prices
    mix_price_count <- data.frame(EAN, calc_price) %>%
      dplyr::filter(!is.na(calc_price)) %>%
      dplyr::group_by(EAN) %>%
      dplyr::summarise(n_price = dplyr::n_distinct(calc_price), .groups = "drop") %>%
      dplyr::filter(n_price > 1) %>%
      nrow()
    
    # 7. Week consistency: data week vs filename week
    week_distinct   <- dplyr::n_distinct(df$week_in_data)
    week_mismatch   <- sum(!as.logical(df$week_match_flag), na.rm = TRUE)
    week_match_ok   <- week_mismatch == 0 & week_distinct == 1
    
    data.frame(
      source              = source_tag,
      file_name           = df$file_name[1],
      week_file           = df$week_file[1],
      size_kb             = df$size_kb[1],
      records             = nrow(df),
      n_ean               = dplyr::n_distinct(EAN),
      n_store             = dplyr::n_distinct(df$store_id),
      total_sales         = sum(Sales, na.rm = TRUE),
      total_revenue       = sum(Revenue, na.rm = TRUE),
      dup_ean_multi_text  = dup_ean_text,
      ean_empty_count     = ean_empty,
      ean_non_numeric     = ean_non_numeric,
      ean_length_error    = ean_length_error,
      mix_price_ean_count = mix_price_count,
      qty_decimal_count   = qty_decimal,
      week_distinct_count = week_distinct,
      week_mismatch_count = week_mismatch,
      week_match_flag     = week_match_ok,
      stringsAsFactors    = FALSE
    )
  }) %>%
    dplyr::bind_rows()
}

#' Run summary over both data sources and merge into one review table.
#' @param ingested  Output of ingest().
summary_df_merged <- function(ingested) {
  dplyr::bind_rows(
    summary_df_single(ingested$data_qc, source_tag = "list_1"),
    summary_df_single(ingested$data,    source_tag = "list_2")
  )
}
