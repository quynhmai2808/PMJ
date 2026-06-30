## main.R
## Orchestrates the full Combase ETL pipeline using the modular functions in
## functions/. To run for a different data provider: write a new config.R
## (different paths + provider_cols mapping) and source it instead.

rm(list = ls())
st    <- Sys.time()
runid <- format(st, "%y%m%d")
gc()
options("width" = 200)

library(dplyr)
library(lubridate)
library(tidyverse)
library(readr)
library(readxl)
library(stringr)
library(stringi)
library(openxlsx)

# --- Load config + functions ---
source("config.R")
source("functions/utils.R")
source("functions/meta_df.R")
source("functions/read_df.R")
source("functions/summary_df.R")
source("functions/mix_price.R")
source("functions/factdata_final.R")
source("functions/pmg_process.R")
source("functions/reports.R")

cols <- provider_cols

# =========================
# 1. DISCOVER FILES
# =========================

meta_1 <- meta_df(PATHORG, file_pattern_1, date_extract_1, date_format_in_filename)
meta_2 <- meta_df(PATHORG, file_pattern_2, date_extract_2, date_format_in_filename)

# =========================
# 2. READ FILES
# =========================

data_list_1 <- read_df(meta_1, cols, sep = csv_sep, quote = csv_quote)
data_list_2 <- read_df(meta_2, cols, sep = csv_sep, quote = csv_quote)

# =========================
# 3. QC SUMMARY (merged across both sources)
# =========================

summary_df_all <- summary_df_merged(data_list_1, meta_1, data_list_2, meta_2, cols)

summary_file <- file.path(PATHRES, paste0(toupper(foldername_data), "_QC_SUMMARY.csv"))
write.csv2(summary_df_all, summary_file, row.names = FALSE, quote = FALSE)

# =========================
# 4. MIX PRICE EXTRACTION (across all raw data, both sources)
# =========================

df_all_raw <- dplyr::bind_rows(data_list_1, data_list_2)

mix_price_result <- mix_price_check(
  df_all_raw, cols, tobacco_groups, accessory_groups,
  ratio_min = mix_price_ratio_min, diff_min = mix_price_diff_min
)

rawdata_mix_price_file <- file.path(PATHRES, paste0(toupper(foldername_data), "_RAW_DATA_MIX_PRICE.csv"))
write.csv2(mix_price_result$raw, rawdata_mix_price_file, row.names = FALSE, quote = FALSE)

# =========================
# 5. BUILD FACT DATA (data_list_2 ONLY - no merge with data_list_1)
# =========================

factdata_raw_2 <- dplyr::bind_rows(data_list_2)
factdata_df_2  <- select_factdata_cols(factdata_raw_2, cols)

factdata_final_df <- factdata_final(factdata_df_2, cols)

# =========================
# 6. PMG / PMI MASTER DATA
# =========================

pmg <- pmg_process(PATHPMI, pmg_product_stamp, missing_ean_stamp, codierung_PMI)
pmi_file <- pmg$pmi_file

# =========================
# 7. MATCH WITH PMI + MIX PRICE QC
# =========================

df_final <- match_with_pmi(factdata_final_df, pmi_file)

qc <- mix_price_qc_check(
  df_final,
  price_pack_min, price_pack_max,
  price_bundle_min, price_bundle_max,
  price_threshold_pack_bundle
)

pmi_mix_price_lst <- build_pmi_price_issues_list(
  factdata_df_2, qc$price_issues_ean, pmi_file, cols
)

price_issues_file <- file.path(PATHRES, paste0(toupper(foldername_data), "_PMI_PRICE_ISSUES.csv"))
write.csv2(pmi_mix_price_lst, price_issues_file, row.names = FALSE, quote = FALSE)

revenue_summary <- build_revenue_summary(df_final, qc$price_issues_ean)

# =========================
# 8. SALES FACTORS
# =========================

df_final_sales <- add_sales_factors(df_final)

# =========================
# 9. BUILD + WRITE 7 REPORTS (PMG, CIG, OTP, 3x ECIG, MISSING_ITEMS)
# =========================

all_outputs <- build_all_reports(df_final_sales, foldername_data)
write_reports(all_outputs, PATHRES)

message(sprintf("Combase pipeline finished in %.1f sec.", as.numeric(Sys.time() - st, units = "secs")))

