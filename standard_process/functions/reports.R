## functions/reports.R
## Match factdata_final against the PMI/PMG master, run mix-price QC on the
## matched data, compute sales factors, and produce the 6 output reports
## (PMG, CIG, OTP, 3x ECIG, MISSING_ITEMS), one CSV per category per Date.

#' Left-join factdata_final to the PMI product master.
match_with_pmi <- function(factdata_final_df, pmi_file) {
  factdata_final_df %>%
    dplyr::left_join(pmi_file, by = "EAN") %>%
    dplyr::mutate(match_pmi1 = dplyr::case_when(
      is.na(PRODUCTNAME) ~ "kein match",
      !is.na(PRODUCTNAME) ~ "match"
    ))
}

#' QC check: flag rows where PMI master PRICE disagrees with calculated
#' price, or where EANTYPE (pack/bundle) looks wrong given the price level.
#'
#' @return list(checked = full df with flags, price_issues = matched rows
#'         with a flagged mismatch, price_issues_ean = distinct EANs flagged)
mix_price_qc_check <- function(df_final, price_pack_min, price_pack_max,
                                price_bundle_min, price_bundle_max,
                                price_threshold) {

  checked <- df_final %>%
    dplyr::mutate(
      PRICE = as.numeric(PRICE),
      check_price = ifelse(abs(PRICE - calculate_price) < 0.01, "y", "n"),

      expected_type = dplyr::case_when(
        calculate_price >= price_pack_min   & calculate_price <= price_pack_max   ~ "P",
        calculate_price >= price_bundle_min & calculate_price <= price_bundle_max ~ "B",
        TRUE ~ "UNKNOWN"
      ),

      type_mismatch_flag = expected_type != EANTYPE & expected_type != "UNKNOWN",

      price_type_flag = dplyr::case_when(
        EANTYPE == "P" & calculate_price > price_threshold ~ "P_as_Bundle",
        EANTYPE == "B" & calculate_price < price_threshold ~ "B_as_Pack",
        TRUE ~ "OK"
      )
    ) %>%
    dplyr::select(
      file_date, Date, Month, Retailer, EAN, Bezeichnung, Warengruppecode, Warengruppe,
      EANTYPE, expected_type, PRODUCTNUMBER, Sales, Revenue, calculate_price, PRICE,
      check_price, type_mismatch_flag, price_type_flag, PACKSPERBUNDLE, ITEMSPERPACK,
      dplyr::everything()
    )

  price_issues <- checked %>%
    dplyr::filter(match_pmi1 == "match") %>%
    dplyr::filter(type_mismatch_flag == TRUE | price_type_flag != "OK")

  price_issues_ean <- price_issues %>%
    dplyr::distinct(EAN) %>%
    dplyr::pull(EAN)

  list(checked = checked, price_issues = price_issues, price_issues_ean = price_issues_ean)
}

#' Pull raw transaction rows for the flagged EANs (for the PMI_PRICE_ISSUES report).
build_pmi_price_issues_list <- function(factdata_df_2, price_issues_ean, pmi_file, cols) {
  factdata_df_2 %>%
    dplyr::filter(.data[[cols$ean]] %in% price_issues_ean, !is.na(.data[[cols$ean]])) %>%
    dplyr::arrange(.data[[cols$ean]]) %>%
    dplyr::left_join(pmi_file, by = stats::setNames("EAN", cols$ean)) %>%
    dplyr::select(
      dplyr::all_of(c(cols$year, cols$week, cols$store, cols$qty, cols$amount,
                       cols$amount_netto, cols$ean, cols$text, cols$vke,
                       cols$wgr_code, cols$wgr)),
      EANTYPE, PACKSPERBUNDLE, ITEMSPERPACK, ITEMSPERBUNDLE
    )
}

#' Revenue impact summary of price issues vs no-issues.
build_revenue_summary <- function(df_final, price_issues_ean) {
  revenue_per_ean <- df_final %>%
    dplyr::filter(match_pmi1 == "match") %>%
    dplyr::group_by(EAN) %>%
    dplyr::summarise(
      Total_Sales   = sum(Sales, na.rm = TRUE),
      Total_revenue = sum(Revenue, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(price_issue_flag = ifelse(EAN %in% price_issues_ean, "Issue", "No_Issue"))

  revenue_per_ean %>%
    dplyr::group_by(price_issue_flag) %>%
    dplyr::summarise(
      Total_Sales   = sum(Total_Sales),
      Total_revenue = sum(Total_revenue),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      pct_Revenue = (Total_revenue / sum(Total_revenue)) * 100,
      pct_Sales   = (Total_Sales / sum(Total_Sales)) * 100
    )
}

#' Add sales-factor-derived columns: Total Sticks, Total G.
add_sales_factors <- function(df_final) {
  df_final %>%
    dplyr::mutate(
      ITEMSPERBUNDLE = as.numeric(ITEMSPERBUNDLE),
      ITEMSPERPACK   = as.numeric(ITEMSPERPACK),
      salesfactor    = ifelse(EANTYPE == "B", ITEMSPERBUNDLE, ITEMSPERPACK),
      `Total Sticks` = as.numeric(Sales) * salesfactor,

      OTP_BUNDLE_CONTENT_G = as.numeric(OTP_BUNDLE_CONTENT_G),
      OTP_PACK_CONTENT_G   = as.numeric(OTP_PACK_CONTENT_G),
      salesfactor_otp = ifelse(EANTYPE == "B", OTP_BUNDLE_CONTENT_G, OTP_PACK_CONTENT_G),
      `Total G` = as.numeric(Sales) * salesfactor_otp
    )
}

# --- Individual report builders -------------------------------------------

build_pmi_output <- function(df_final_sales, foldername_data) {
  df_final_sales %>%
    dplyr::filter(PRODUCER == "PMG") %>%
    dplyr::mutate(
      FileName = paste0(toupper(foldername_data), "_PMG_", Date),
      `Retail Store` = Retailer
    ) %>%
    dplyr::group_by(FileName, Date, `Retail Store`, PRODUCTNUMBER, PRODUCTNAME, Month) %>%
    dplyr::summarise(
      `Total Sales`   = floor(sum(as.numeric(Sales))),
      `Total Sticks`  = floor(sum(`Total Sticks`)),
      `Total Revenue` = floor(sum(Revenue)),
      .groups = "drop"
    ) %>%
    dplyr::rename(Artikel = PRODUCTNAME) %>%
    dplyr::select(FileName, Date, `Retail Store`, PRODUCTNUMBER, Artikel,
                  `Total Sales`, `Total Sticks`, `Total Revenue`, Month)
}

build_cig_output <- function(df_final_sales, foldername_data) {
  df_final_sales %>%
    dplyr::filter((PRODUCTTYPE == "Cigarette" | LENGTHTYPE == "RRP HTP STICKS") & !is.na(EAN)) %>%
    dplyr::mutate(
      FileName = paste0(toupper(foldername_data), "_CIG_OUTLET_", Date),
      `Retail Store` = Retailer
    ) %>%
    dplyr::group_by(FileName, Date, `Retail Store`, Month) %>%
    dplyr::summarise(
      `Total Sticks`  = floor(sum(`Total Sticks`)),
      `Total Revenue` = floor(sum(Revenue)),
      .groups = "drop"
    ) %>%
    dplyr::select(FileName, Date, `Retail Store`, `Total Sticks`, `Total Revenue`, Month)
}

build_ecig_output <- function(df_final_sales, foldername_data, suffix, package_filter) {
  df_final_sales %>%
    dplyr::filter(
      grepl("ECigarette", DISTRIBUTIONCATEGORY) & !is.na(EAN) &
        package_filter(PACKAGETYPE)
    ) %>%
    dplyr::mutate(
      FileName = paste0(toupper(foldername_data), "_ECIG_OUTLET_", suffix, "_", Date),
      `Retail Store` = Retailer
    ) %>%
    dplyr::group_by(FileName, Date, `Retail Store`, Month) %>%
    dplyr::summarise(
      `Total Sales`   = floor(sum(as.numeric(Sales))),
      `Total Revenue` = floor(sum(Revenue)),
      .groups = "drop"
    ) %>%
    dplyr::select(FileName, Date, `Retail Store`, `Total Sales`, `Total Revenue`, Month)
}

build_otp_output <- function(df_final_sales, foldername_data) {
  df_final_sales %>%
    dplyr::filter(PRODUCTTYPE == "Finecut" & !is.na(EAN)) %>%
    dplyr::mutate(
      FileName = paste0(toupper(foldername_data), "_OTP_OUTLET_", Date),
      `Retail Store` = Retailer
    ) %>%
    dplyr::group_by(FileName, Date, `Retail Store`, Month) %>%
    dplyr::summarise(
      `Total G`       = floor(sum(`Total G`)),
      `Total Revenue` = floor(sum(Revenue)),
      .groups = "drop"
    ) %>%
    dplyr::select(FileName, Date, `Retail Store`, `Total G`, `Total Revenue`, Month)
}

#' Items in factdata_final that did NOT match the PMI master.
#' Fixed from original: WGR -> Warengruppe, Artikelbezeichnung -> Bezeichnung.
build_missingitems_output <- function(df_final_sales, foldername_data) {
  df_final_sales %>%
    dplyr::filter(match_pmi1 == "kein match") %>%
    dplyr::mutate(
      FileName = paste0(toupper(foldername_data), "_MISSING_ITEMS_", Date),
      Retailer = toupper(foldername_data)
    ) %>%
    dplyr::group_by(FileName, Date, Retailer, EAN, Bezeichnung, Warengruppe, Month) %>%
    dplyr::summarise(
      `Total Sales`   = floor(sum(Sales)),
      `Total Revenue` = floor(sum(Revenue)),
      .groups = "drop"
    ) %>%
    dplyr::select(FileName, Date, Retailer, GTIN = EAN, Bezeichnung, Warengruppe,
                  `Total Sales`, `Total Revenue`) %>%
    dplyr::arrange(dplyr::desc(`Total Sales`))
}

#' Build all 7 report data.frames (6 original + missing items) in one call.
build_all_reports <- function(df_final_sales, foldername_data) {
  list(
    PMG           = build_pmi_output(df_final_sales, foldername_data),
    CIG           = build_cig_output(df_final_sales, foldername_data),
    OTP           = build_otp_output(df_final_sales, foldername_data),
    ecig_9961     = build_ecig_output(df_final_sales, foldername_data, "9961",
                                       function(pt) pt %in% c("BOTTLE", "PODS", "CAPS")),
    ecig_9962     = build_ecig_output(df_final_sales, foldername_data, "9962",
                                       function(pt) pt == "Box"),
    ecig_9963     = build_ecig_output(df_final_sales, foldername_data, "9963",
                                       function(pt) pt == "KIT"),
    MISSING_ITEMS = build_missingitems_output(df_final_sales, foldername_data)
  )
}

#' Write every report to one CSV per Date per category into PATHRES.
write_reports <- function(all_outputs, path_res) {
  for (cat_name in names(all_outputs)) {
    df <- all_outputs[[cat_name]]
    if (nrow(df) == 0) next

    split_list <- split(df, df$Date)
    for (dt in names(split_list)) {
      report_df <- split_list[[dt]]
      write.csv2(
        report_df,
        file.path(path_res, paste0(unique(report_df$FileName), ".csv")),
        row.names = FALSE, quote = FALSE
      )
    }
  }
}
