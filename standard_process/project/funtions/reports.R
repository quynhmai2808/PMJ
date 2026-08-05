## functions/reports.R
## Build and write the 7 output reports — shared across all providers.
## Consumes factdata_final() output (standard column names) + PMI master.

#' Join fact table to PMI product master, add match flag.
match_with_pmi <- function(fact_df, pmi_file) {
  fact_df %>%
    dplyr::left_join(pmi_file, by = "EAN") %>%
    dplyr::mutate(
      match_pmi = dplyr::if_else(is.na(PRODUCTNAME), "kein match", "match")
    )
}

#' Flag rows where PMI price or EANTYPE disagrees with calculate_price.
#' Returns list(checked, price_issues, price_issues_ean).
mix_price_qc_check <- function(df_final,
                               price_pack_min, price_pack_max,
                               price_bundle_min, price_bundle_max,
                               price_threshold) {
  checked <- df_final %>%
    dplyr::mutate(
      PRICE       = as.numeric(PRICE),
      check_price = dplyr::if_else(abs(PRICE - calculate_price) < 0.01, "y", "n"),
      
      expected_type = dplyr::case_when(
        calculate_price >= price_pack_min   & calculate_price <= price_pack_max   ~ "P",
        calculate_price >= price_bundle_min & calculate_price <= price_bundle_max ~ "B",
        TRUE ~ "UNKNOWN"
      ),
      type_mismatch_flag = (expected_type != EANTYPE & expected_type != "UNKNOWN"),
      price_type_flag = dplyr::case_when(
        EANTYPE == "P" & calculate_price > price_threshold ~ "P_as_Bundle",
        EANTYPE == "B" & calculate_price < price_threshold ~ "B_as_Pack",
        TRUE ~ "OK"
      )
    )
  
  price_issues <- checked %>%
    dplyr::filter(match_pmi == "match",
                  type_mismatch_flag == TRUE | price_type_flag != "OK")
  
  list(
    checked          = checked,
    price_issues     = price_issues,
    price_issues_ean = dplyr::pull(dplyr::distinct(price_issues, EAN), EAN)
  )
}

#' Revenue impact: split matched EANs into Issue vs No_Issue.
build_revenue_summary <- function(df_final, price_issues_ean) {
  df_final %>%
    dplyr::filter(match_pmi == "match") %>%
    dplyr::group_by(EAN) %>%
    dplyr::summarise(Total_Sales   = sum(Sales,   na.rm = TRUE),
                     Total_revenue = sum(Revenue, na.rm = TRUE), .groups = "drop") %>%
    dplyr::mutate(price_issue_flag = ifelse(EAN %in% price_issues_ean, "Issue", "No_Issue")) %>%
    dplyr::group_by(price_issue_flag) %>%
    dplyr::summarise(Total_Sales   = sum(Total_Sales),
                     Total_revenue = sum(Total_revenue), .groups = "drop") %>%
    dplyr::mutate(pct_Revenue = Total_revenue / sum(Total_revenue) * 100,
                  pct_Sales   = Total_Sales   / sum(Total_Sales)   * 100)
}

#' Add sales-factor-derived columns: Total Sticks, Total G.
add_sales_factors <- function(df_final) {
  df_final %>%
    dplyr::mutate(
      ITEMSPERBUNDLE       = as.numeric(ITEMSPERBUNDLE),
      ITEMSPERPACK         = as.numeric(ITEMSPERPACK),
      OTP_BUNDLE_CONTENT_G = as.numeric(OTP_BUNDLE_CONTENT_G),
      OTP_PACK_CONTENT_G   = as.numeric(OTP_PACK_CONTENT_G),
      salesfactor          = ifelse(EANTYPE == "B", ITEMSPERBUNDLE, ITEMSPERPACK),
      `Total Sticks`       = Sales * salesfactor,
      salesfactor_otp      = ifelse(EANTYPE == "B", OTP_BUNDLE_CONTENT_G, OTP_PACK_CONTENT_G),
      `Total G`            = Sales * salesfactor_otp
    )
}

# --- Individual report builders (all use standard column names) -------------

build_pmi_output <- function(df, provider) {
  df %>%
    dplyr::filter(PRODUCER == "PMG") %>%
    dplyr::mutate(FileName = paste0(toupper(provider), "_PMG_", Date),
                  `Retail Store` = Retailer) %>%
    dplyr::group_by(FileName, Date, `Retail Store`, PRODUCTNUMBER, PRODUCTNAME, Month) %>%
    dplyr::summarise(`Total Sales`   = floor(sum(Sales)),
                     `Total Sticks`  = floor(sum(`Total Sticks`)),
                     `Total Revenue` = floor(sum(Revenue)), .groups = "drop") %>%
    dplyr::rename(Artikel = PRODUCTNAME) %>%
    dplyr::select(FileName, Date, `Retail Store`, PRODUCTNUMBER, Artikel,
                  `Total Sales`, `Total Sticks`, `Total Revenue`, Month)
}

build_cig_output <- function(df, provider) {
  df %>%
    dplyr::filter((PRODUCTTYPE == "Cigarette" | LENGTHTYPE == "RRP HTP STICKS") & !is.na(EAN)) %>%
    dplyr::mutate(FileName = paste0(toupper(provider), "_CIG_OUTLET_", Date),
                  `Retail Store` = Retailer) %>%
    dplyr::group_by(FileName, Date, `Retail Store`, Month) %>%
    dplyr::summarise(`Total Sticks`  = floor(sum(`Total Sticks`)),
                     `Total Revenue` = floor(sum(Revenue)), .groups = "drop") %>%
    dplyr::select(FileName, Date, `Retail Store`, `Total Sticks`, `Total Revenue`, Month)
}

build_ecig_output <- function(df, provider, suffix, pkg_filter_fn) {
  df %>%
    dplyr::filter(grepl("ECigarette", DISTRIBUTIONCATEGORY), !is.na(EAN),
                  pkg_filter_fn(PACKAGETYPE)) %>%
    dplyr::mutate(FileName = paste0(toupper(provider), "_ECIG_OUTLET_", suffix, "_", Date),
                  `Retail Store` = Retailer) %>%
    dplyr::group_by(FileName, Date, `Retail Store`, Month) %>%
    dplyr::summarise(`Total Sales`   = floor(sum(Sales)),
                     `Total Revenue` = floor(sum(Revenue)), .groups = "drop") %>%
    dplyr::select(FileName, Date, `Retail Store`, `Total Sales`, `Total Revenue`, Month)
}

build_otp_output <- function(df, provider) {
  df %>%
    dplyr::filter(PRODUCTTYPE == "Finecut", !is.na(EAN)) %>%
    dplyr::mutate(FileName = paste0(toupper(provider), "_OTP_OUTLET_", Date),
                  `Retail Store` = Retailer) %>%
    dplyr::group_by(FileName, Date, `Retail Store`, Month) %>%
    dplyr::summarise(`Total G`       = floor(sum(`Total G`)),
                     `Total Revenue` = floor(sum(Revenue)), .groups = "drop") %>%
    dplyr::select(FileName, Date, `Retail Store`, `Total G`, `Total Revenue`, Month)
}

#' Missing items: EANs in fact table with no PMI match.
#' Uses standard column names: product_text -> Bezeichnung, wgr_text -> Warengruppe
build_missingitems_output <- function(df, provider) {
  df %>%
    dplyr::filter(match_pmi == "kein match") %>%
    dplyr::mutate(FileName    = paste0(toupper(provider), "_MISSING_ITEMS_", Date),
                  RetailerOut = toupper(provider)) %>%
    dplyr::group_by(FileName, Date, RetailerOut, EAN, product_text, wgr_text, Month) %>%
    dplyr::summarise(`Total Sales`   = floor(sum(Sales)),
                     `Total Revenue` = floor(sum(Revenue)), .groups = "drop") %>%
    dplyr::rename(Retailer    = RetailerOut,
                  Bezeichnung = product_text,
                  Warengruppe = wgr_text) %>%
    dplyr::select(FileName, Date, Retailer, GTIN = EAN, Bezeichnung, Warengruppe,
                  `Total Sales`, `Total Revenue`) %>%
    dplyr::arrange(dplyr::desc(`Total Sales`))
}

#' Build all 7 reports in one call.
build_all_reports <- function(df_final_sales, provider) {
  list(
    PMG           = build_pmi_output(df_final_sales, provider),
    CIG           = build_cig_output(df_final_sales, provider),
    OTP           = build_otp_output(df_final_sales, provider),
    ecig_9961     = build_ecig_output(df_final_sales, provider, "9961",
                                      function(pt) pt %in% c("BOTTLE", "PODS", "CAPS")),
    ecig_9962     = build_ecig_output(df_final_sales, provider, "9962",
                                      function(pt) pt == "Box"),
    ecig_9963     = build_ecig_output(df_final_sales, provider, "9963",
                                      function(pt) pt == "KIT"),
    MISSING_ITEMS = build_missingitems_output(df_final_sales, provider)
  )
}

#' Write each report to one CSV per Date per category.
write_reports <- function(all_outputs, path_res) {
  for (cat_name in names(all_outputs)) {
    df <- all_outputs[[cat_name]]
    if (nrow(df) == 0) next
    for (report_df in split(df, df$Date)) {
      write.csv2(
        report_df,
        file.path(path_res, paste0(unique(report_df$FileName), ".csv")),
        row.names = FALSE, quote = FALSE
      )
    }
  }
}
