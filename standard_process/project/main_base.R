## main_combase.R
## Orchestrates the full Combase ETL pipeline.
## To run for a different provider: copy this file as main_<provider>.R
## and change ONLY the source() call on line 1 of SECTION 1.

rm(list = ls())
st    <- Sys.time()
runid <- format(st, "%y%m%d")
gc()
options("width" = 200)

library(dplyr)
library(lubridate)
library(stringr)

# =============================================================================
# SECTION 1 — PROVIDER SELECTION (only line that changes per provider)
# =============================================================================
source("functions/ingest_combase.R")   # swap to ingest_provider2.R for next provider

# --- Shared functions (never change) ---
source("functions/summary_df.R")
source("functions/mix_price.R")
source("functions/factdata_final.R")
source("functions/pmg_process.R")
source("functions/reports.R")

# Business rule thresholds (could also live inside ingest_<provider>.R if
# they need to differ per provider — kept here for visibility)
PRICE_PACK_MIN          <- 3
PRICE_PACK_MAX          <- 25
PRICE_BUNDLE_MIN        <- 46
PRICE_BUNDLE_MAX        <- 100
PRICE_THRESHOLD         <- 30
MIX_PRICE_RATIO_MIN     <- 2
MIX_PRICE_DIFF_MIN      <- 5

TOBACCO_GROUPS <- c(
  "Tabak", "Zigaretten", "Zigaretten & Co", "Cigarillos / Cigarren",
  "Pfeifentabak", "Kautabak", "Schnupftabak", "IQOS", "Heat-not-burn"
)
ACCESSORY_GROUPS <- c(
  "Zigarettenpapier", "Raucherbedarf", "Sonst.Raucherbedarf", "Taschenfeuerzeuge"
)

# =============================================================================
# SECTION 2 — PIPELINE (identical for every provider)
# =============================================================================

# 1. Ingest + standardise
message("[1/7] Ingesting raw data...")
ingested <- ingest()

# 2. QC summary
message("[2/7] Building QC summary...")
summary_all <- summary_df_merged(ingested)
write.csv2(summary_all,
           file.path(ingested$paths$PATHRES,
                     paste0(toupper(ingested$provider), "_QC_SUMMARY.csv")),
           row.names = FALSE, quote = FALSE)

# 3. Mix price check (across all raw data — both lists via $data + $data_qc)
message("[3/7] Checking mix prices...")
all_raw_std <- dplyr::bind_rows(ingested$data_qc, ingested$data)
mix_result  <- mix_price_check(all_raw_std, TOBACCO_GROUPS, ACCESSORY_GROUPS,
                                ratio_min = MIX_PRICE_RATIO_MIN,
                                diff_min  = MIX_PRICE_DIFF_MIN)
write.csv2(mix_result$raw,
           file.path(ingested$paths$PATHRES,
                     paste0(toupper(ingested$provider), "_RAW_DATA_MIX_PRICE.csv")),
           row.names = FALSE, quote = FALSE)

# 4. Fact table (data_list_2 only — already in ingested$data)
message("[4/7] Building fact table...")
fact_df <- factdata_final(ingested$data)

# 5. PMI master
message("[5/7] Loading PMI master...")
pmg <- pmg_process(ingested$paths$PATHPMI,
                   ingested$paths$PMG_STAMP,
                   ingested$paths$MISSING_STAMP)

# 6. PMI match + price QC
message("[6/7] Matching with PMI + price QC...")
df_final <- match_with_pmi(fact_df, pmg$pmi_file)

qc <- mix_price_qc_check(df_final,
                          PRICE_PACK_MIN, PRICE_PACK_MAX,
                          PRICE_BUNDLE_MIN, PRICE_BUNDLE_MAX,
                          PRICE_THRESHOLD)

# Write PMI price issues (raw rows from ingested$data for flagged EANs)
pmi_price_issues <- ingested$data %>%
  dplyr::filter(EAN %in% qc$price_issues_ean, !is.na(EAN)) %>%
  dplyr::left_join(pmg$pmi_file, by = "EAN") %>%
  dplyr::select(week_in_data, month_in_data, store_id, sales_units, sales_revenue,
                EAN, product_text, wgr_code, wgr_text,
                EANTYPE, PACKSPERBUNDLE, ITEMSPERPACK, ITEMSPERBUNDLE)

write.csv2(pmi_price_issues,
           file.path(ingested$paths$PATHRES,
                     paste0(toupper(ingested$provider), "_PMI_PRICE_ISSUES.csv")),
           row.names = FALSE, quote = FALSE)

revenue_summary <- build_revenue_summary(df_final, qc$price_issues_ean)

# 7. Sales factors + reports
message("[7/7] Building reports...")
df_final_sales <- add_sales_factors(df_final)
all_outputs    <- build_all_reports(df_final_sales, ingested$provider)
write_reports(all_outputs, ingested$paths$PATHRES)

message(sprintf("Done in %.1f sec.", as.numeric(Sys.time() - st, units = "secs")))

