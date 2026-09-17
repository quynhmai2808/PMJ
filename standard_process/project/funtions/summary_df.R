## funtions/summary_df.R
## QC summary statistics — shared across ALL providers.
## Input: std_data = ingest()$data (standard output contract)
## Standard columns required: week_in_data, month_in_data, week_file,
##   store_id, EAN, product_text, wgr_text, sales_units, sales_revenue, file_name

# =============================================================================
# CORE: summary_df_single() + summary_df_merged()
# =============================================================================

#' Compute QC statistics grouped by week_in_data.
#' One output row per ISO week regardless of provider delivery pattern.
#'
#' Output columns (18):
#'   week_in_data, week_file, month_in_data,
#'   records, n_store, n_ean, avg_price, total_sales, total_revenue,
#'   ean_empty_count, ean_non_numeric, ean_length_error,
#'   zero_qty_count,     # qty == 0
#'   zero_rev_pos_qty,   # sum of qty where qty > 0 but rev == 0
#'   neg_qty_count,      # qty < 0               <- outlier (returns)
#'   decimal_qty_count,  # qty > 0 & fractional  <- outlier (invalid)
#'   mix_price_ean_count, multi_text_ean_count
summary_df_single <- function(std_data, tobacco_groups = character(0)) {
  
  if (nrow(std_data) == 0) return(data.frame())
  
  dplyr::bind_rows(lapply(
    split(std_data, std_data$week_in_data),
    function(df) {
      
      EAN     <- as.character(df$EAN)
      Sales   <- df$sales_units
      Revenue <- df$sales_revenue
      
      # avg_price: median of per-EAN medians (positive rows only)
      calc_price <- ifelse(Sales > 0, round(Revenue / Sales, 2), NA_real_)
      ean_medians <- tapply(calc_price, EAN, function(x) {
        x <- x[!is.na(x) & x > 0]
        if (length(x) == 0) NA_real_ else median(x)
      })
      avg_price <- round(median(ean_medians, na.rm = TRUE), 2)
      
      # EAN quality
      ean_empty        <- sum(is.na(EAN) | trimws(EAN) == "" | EAN == "0")
      ean_non_numeric  <- sum(!grepl("^[0-9]+$", EAN) & !is.na(EAN) & EAN != "")
      ean_length_error <- sum(nchar(EAN) > 14, na.rm = TRUE)
      
      # Sales outliers
      zero_qty_count    <- sum(!is.na(Sales) & Sales == 0)
      zero_rev_pos_qty  <- sum(Sales[!is.na(Sales) & Sales > 0 &
                                       !is.na(Revenue) & Revenue == 0], na.rm = TRUE)
      neg_qty_count     <- sum(!is.na(Sales) & Sales < 0)
      decimal_qty_count <- sum(!is.na(Sales) & Sales > 0 & Sales %% 1 != 0)
      
      # Mix price: count of EANs consistent with mix_price_detail() rules.
      # FIX: calc_price_col is mutated INTO df as a column — not the top-level
      # vector calc_price — so dplyr::filter() references the correct column.
      # Rules: min_p > 0 (exclude promo), ratio > 2, price_diff > 5
      mix_price_ean_count <- df %>%
        dplyr::mutate(
          calc_price_col = dplyr::if_else(
            !is.na(sales_units) & sales_units > 0,
            round(sales_revenue / sales_units, 2),
            NA_real_
          )
        ) %>%
        dplyr::filter(
          wgr_text %in% tobacco_groups,
          !is.na(calc_price_col),
          calc_price_col > 0
        ) %>%
        dplyr::group_by(week_in_data, store_id, EAN) %>%
        dplyr::summarise(
          n     = dplyr::n_distinct(calc_price_col),
          min_p = min(calc_price_col, na.rm = TRUE),
          max_p = max(calc_price_col, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        dplyr::filter(
          is.finite(min_p), is.finite(max_p),
          n > 1,
          min_p > 0,
          max_p / min_p > 2,
          max_p - min_p > 5
        ) %>%
        dplyr::distinct(EAN) %>%
        nrow()
      
      # Multi-text: EANs with >1 distinct product_text AND >1 distinct wgr_text
      # Group by same week + store_id + EAN — consistent with dup_product_detail()
      multi_text_ean_count <- df %>%
        dplyr::filter(
          !is.na(EAN), EAN != "",
          !is.na(product_text), product_text != "",
          !is.na(wgr_text),     wgr_text != ""
        ) %>%
        dplyr::group_by(week_in_data, store_id, EAN) %>%
        dplyr::summarise(
          n_text = dplyr::n_distinct(product_text),
          n_wgr  = dplyr::n_distinct(wgr_text),
          .groups = "drop"
        ) %>%
        dplyr::filter(n_text > 1, n_wgr > 1) %>%
        dplyr::distinct(EAN) %>%
        nrow()
      
      data.frame(
        week_in_data         = df$week_in_data[1],
        week_file            = df$week_file[1],
        month_in_data        = df$month_in_data[1],
        records              = nrow(df),
        n_store              = dplyr::n_distinct(df$store_id),
        n_ean                = dplyr::n_distinct(EAN),
        avg_price            = avg_price,
        total_sales          = round(as.numeric(sum(Sales, na.rm = TRUE)), 2),
        total_revenue        = round(as.numeric(sum(Revenue, na.rm = TRUE)), 2),
        ean_empty_count      = ean_empty,
        ean_non_numeric      = ean_non_numeric,
        ean_length_error     = ean_length_error,
        zero_qty_count       = zero_qty_count,
        zero_rev_pos_qty     = zero_rev_pos_qty,
        neg_qty_count        = neg_qty_count,
        decimal_qty_count    = decimal_qty_count,
        mix_price_ean_count  = mix_price_ean_count,
        multi_text_ean_count = multi_text_ean_count,
        stringsAsFactors     = FALSE
      )
    }
  )) %>%
    dplyr::arrange(week_in_data)
}

#' Run QC summary on ingest() output.
summary_df_merged <- function(ingested, extra = NULL) {
  std_data <- ingested$data
  if (!is.null(extra)) std_data <- dplyr::bind_rows(std_data, extra)
  summary_df_single(std_data, tobacco_groups = ingested$tobacco_groups)
}

# =============================================================================
# DETAIL TABLE FUNCTIONS
# =============================================================================

ean_quality_detail <- function(std_data) {
  
  classify_type <- function(ean) {
    dplyr::case_when(
      is.na(ean) | trimws(ean) == "" | ean == "0"  ~ "empty / zero",
      !grepl("^[0-9]+$", ean)                       ~ "non-numeric",
      nchar(ean) > 14                               ~ paste0(nchar(ean), " digits (too long)"),
      !nchar(ean) %in% c(8,12,13,14)               ~ paste0(nchar(ean), " digits (non-standard)"),
      TRUE                                          ~ NA_character_
    )
  }
  
  std_data %>%
    dplyr::mutate(
      EAN_char = as.character(EAN),
      ean_type = classify_type(EAN_char)
    ) %>%
    dplyr::filter(!is.na(ean_type)) %>%
    dplyr::group_by(EAN_char, ean_type) %>%
    dplyr::summarise(
      ean_length          = nchar(EAN_char[1]),
      n_rows              = dplyr::n(),
      product_text_sample = dplyr::first(product_text),
      wgr_text_sample     = dplyr::first(wgr_text),
      .groups = "drop"
    ) %>%
    dplyr::arrange(ean_type, dplyr::desc(n_rows)) %>%
    dplyr::rename(EAN = EAN_char, flag = ean_type) %>%
    dplyr::select(EAN, ean_length, flag, n_rows,
                  product_text_sample, wgr_text_sample)
}

sales_outlier_detail <- function(std_data) {
  
  std_data %>%
    dplyr::filter(
      (!is.na(sales_units) & !is.na(sales_revenue)) &
        (
          (sales_units > 0  & sales_revenue == 0) |
            (sales_units < 0)                        |
            (sales_units > 0  & sales_units %% 1 != 0)
        )
    ) %>%
    dplyr::mutate(
      outlier_flag = dplyr::case_when(
        sales_units > 0 & sales_revenue == 0     ~ "zero_rev_pos_qty",
        sales_units < 0                          ~ "neg_qty",
        sales_units > 0 & sales_units %% 1 != 0 ~ "fractional_qty",
        TRUE                                     ~ NA_character_
      )
    ) %>%
    dplyr::arrange(outlier_flag, week_in_data, dplyr::desc(abs(sales_units))) %>%
    dplyr::select(outlier_flag, week_in_data, file_name, store_id,
                  EAN, product_text, wgr_text, sales_units, sales_revenue)
}

#' [MIX_PRICE]: Top N EANs with largest price spread.
#' Same 3 business rules as mix_price_ean_count and mix_price_check():
#'   min_price > 0, ratio > ratio_min (default 2), price_diff > diff_min (default 5)
mix_price_detail <- function(std_data, top_n = 10, ratio_min = 2, diff_min = 5) {
  
  # FIX: filter calc_price != 0 AND !is.na(calc_price) before grouping
  # to prevent empty groups from reaching min()/max() → Inf/-Inf warning
  detail <- std_data %>%
    dplyr::filter(
      !is.na(EAN), EAN != "",
      sales_units > 0,
      !is.na(sales_revenue)
    ) %>%
    dplyr::mutate(
      calc_price = round(sales_revenue / sales_units, 2)
    ) %>%
    dplyr::filter(!is.na(calc_price) & calc_price > 0)
  
  # Step 1: find flagged (week + store + EAN) combinations
  mix_price_summary <- detail %>%
    dplyr::group_by(week_in_data, store_id, EAN) %>%
    dplyr::summarise(
      n_prices   = dplyr::n_distinct(calc_price),
      min_price  = min(calc_price, na.rm = TRUE),
      max_price  = max(calc_price, na.rm = TRUE),
      price_diff = round(max_price - min_price, 2),
      ratio      = round(max_price / min_price, 2),
      .groups    = "drop"
    ) %>%
    dplyr::filter(
      is.finite(min_price), is.finite(max_price),
      n_prices > 1,
      min_price > 0,
      ratio > ratio_min,
      price_diff > diff_min
    )
  
  if (nrow(mix_price_summary) == 0) return(data.frame())
  
  # Step 2: semi_join back to get source (file_name) info
  detail %>%
    dplyr::semi_join(
      mix_price_summary,
      by = c("week_in_data", "store_id", "EAN")
    ) %>%
    dplyr::group_by(week = week_in_data, store_id, EAN) %>%
    dplyr::summarise(
      product_text = paste(unique(product_text), collapse = " | "),
      wgr_text     = paste(unique(wgr_text),     collapse = " | "),
      source       = paste(unique(file_name),     collapse = " | "),
      n_prices     = dplyr::n_distinct(calc_price),
      min_price    = min(calc_price, na.rm = TRUE),
      max_price    = max(calc_price, na.rm = TRUE),
      price_diff   = round(max_price - min_price, 2),
      ratio        = round(max_price / min_price, 2),
      .groups      = "drop"
    ) %>%
    dplyr::filter(is.finite(min_price), is.finite(max_price)) %>%
    dplyr::arrange(dplyr::desc(price_diff), dplyr::desc(ratio)) %>%
    dplyr::slice_head(n = top_n)
}

dup_product_detail <- function(std_data) {
  # Logic: 1 EAN with >1 distinct product_text AND >1 distinct wgr_text
  # → same EAN classified into different product categories (data quality issue)
  std_data %>%
    dplyr::filter(
      !is.na(EAN), EAN != "",
      !is.na(product_text), product_text != "",
      !is.na(wgr_text),     wgr_text != ""
    ) %>%
    dplyr::group_by(week = week_in_data, store_id, EAN) %>%
    dplyr::summarise(
      n_text    = dplyr::n_distinct(product_text),
      n_wgr     = dplyr::n_distinct(wgr_text),
      text_list = paste(sort(unique(product_text)), collapse = " | "),
      wgr_list  = paste(sort(unique(wgr_text)),     collapse = " | "),
      .groups   = "drop"
    ) %>%
    dplyr::filter(n_text > 1, n_wgr > 1) %>%
    dplyr::arrange(dplyr::desc(n_wgr), dplyr::desc(n_text), week, store_id) %>%
    dplyr::select(week, store_id, EAN, n_wgr, wgr_list, n_text, text_list)
}

wgr_distribution <- function(std_data) {
  
  # total <- nrow(std_data)
  # 
  # std_data %>%
  #   dplyr::filter(!is.na(wgr_text), wgr_text != "") %>%
  #   dplyr::count(wgr_text, name = "row_count") %>%
  #   dplyr::arrange(dplyr::desc(row_count)) %>%
  #   dplyr::mutate(share_pct = round(row_count / total * 100, 1)) %>%
  #   dplyr::select(wgr_text, row_count, share_pct)
  
  total_revenue <- sum(std_data$sales_revenue, na.rm = TRUE)
  
  std_data %>%
    dplyr::filter(!is.na(wgr_text), wgr_text != "") %>%
    dplyr::group_by(wgr_text) %>%
    dplyr::summarise(
      row_count         = dplyr::n(),
      total_revenue_wgr = round(sum(sales_revenue, na.rm = TRUE), 2),
      .groups           = "drop"
    ) %>%
    dplyr::arrange(dplyr::desc(total_revenue_wgr)) %>%
    dplyr::mutate(
      share_pct = round(total_revenue_wgr / total_revenue * 100, 1)
    ) %>%
    dplyr::select(wgr_text, row_count, total_revenue_wgr, share_pct)
  
}

#' Call all 5 detail functions and return as a named list.
#' Allows inspection of each table before writing.
#' @param std_data  ingest()$data
#' @return named list: ean_quality, sales_outlier, mix_price, dup_product, wgr_dist
build_detail_tables <- function(std_data) {
  list(
    ean_quality   = ean_quality_detail(std_data),
    sales_outlier = sales_outlier_detail(std_data),
    mix_price     = mix_price_detail(std_data),
    dup_product   = dup_product_detail(std_data),
    wgr_dist      = wgr_distribution(std_data)
  )
}

export_detail_csv <- function(std_data, out_path) {
  
  detail_tables <- build_detail_tables(std_data)
  
  write_section <- function(con, tag, df) {
    writeLines(tag, con)
    if (nrow(df) > 0) {
      write.csv2(df, con, row.names = FALSE, quote = FALSE)
    } else {
      writeLines("(no data)", con)
    }
    writeLines("", con)
  }
  
  con <- file(out_path, open = "w", encoding = "UTF-8")
  on.exit(close(con))
  
  write_section(con, "[EAN_QUALITY]",   ean_quality_detail(std_data))
  write_section(con, "[SALES_OUTLIER]", sales_outlier_detail(std_data))
  write_section(con, "[MIX_PRICE]",     mix_price_detail(std_data))
  write_section(con, "[DUP_PRODUCT]",   dup_product_detail(std_data))
  write_section(con, "[WGR_DIST]",      wgr_distribution(std_data))
  
  message(sprintf("  QC detail exported: %s", out_path))
}
