## main.R
## The full PMI standard process pipeline — Works across different providers.
## To run for a different provider: change ONLY the source() call in SECTION 1.
##
## ── PART 1: PMI PROCESS ──────────────────────────────────────────────────────
##   Ingest → Fact table → PMI master → Match → Sales factors → Reports
##   Goal: produce final delivery CSV files for PMI client
##
## ── PART 2: QC ───────────────────────────────────────────────────────────────
##   All quality checks and diagnostics — runs after Part 1
##   Goal: validate raw data and output before sending to client

rm(list = ls())
st    <- Sys.time()
runid <- format(st, "%y%m%d")
gc()
options("width" = 200)

library(dplyr)
library(lubridate)
library(stringr)
library(readxl)
library(tidyr)

# =============================================================================
# SECTION 1 — PROVIDER SELECTION (only this line changes per provider)
# =============================================================================
source("/qa/data1/PMI/Code/R_profiling_framework/data_pipeline/ingest_DTV.R")
# source("/qa/data1/PMI/Code/R_profiling_framework/data_pipeline/ingest_Eurotrade.R")

# --- Shared functions (never change across providers) ---
source("/qa/data1/PMI/Code/R_profiling_framework/data_pipeline/functions/summary_df.R")
source("/qa/data1/PMI/Code/R_profiling_framework/data_pipeline/functions/mix_price.R")
source("/qa/data1/PMI/Code/R_profiling_framework/data_pipeline/functions/factdata_final.R")
source("/qa/data1/PMI/Code/R_profiling_framework/data_pipeline/functions/pmg_process.R")
source("/qa/data1/PMI/Code/R_profiling_framework/data_pipeline/functions/reports.R")

# --- Business rule thresholds ---
PRICE_PACK_MIN      <- 3
PRICE_PACK_MAX      <- 25
PRICE_BUNDLE_MIN    <- 46
PRICE_BUNDLE_MAX    <- 100
PRICE_THRESHOLD     <- 30
MIX_PRICE_RATIO_MIN <- 2
MIX_PRICE_DIFF_MIN  <- 5

# =============================================================================
# PART 1 — PMI PROCESS
# Core pipeline: ingest → fact table → PMI master → PMI match → delivery reports
# All objects produced here are also used as input for Part 2 QC checks.
# =============================================================================

# Step 1: Ingest + standardise
# ingest_*provider*.R returns ingested$data (combined, standardised) + provider-specific
# config (tobacco_groups, accessory_groups, paths, provider name)
message("[1/7] Ingesting raw data...")
ingested <- ingest()
# ingested <- ingest(year_filter = 2025) # if specific year required

# Step 2: Fact table — aggregate to EAN × store × week level
# factdata_final.R consumes ingested$data directly — single combined
# standardised data.frame regardless of how many source files were read
message("[4/7] Building fact table...")
fact_df <- factdata_final(ingested$data)

# Step 3: PMI master — load PMG + missing_ean reference files
message("[5/7] Loading PMI master...")
pmg <- pmg_process(ingested$paths$PATHPMI,
                   ingested$paths$PMG_STAMP,
                   ingested$paths$MISSING_STAMP)

# Step 4: PMI match — left join fact_df with PMI master on EAN
message("[6/7] Matching with PMI + price QC...")
df_final <- match_with_pmi(fact_df, pmg$pmi_file)

# Step 5: Sales factors + all reports → write delivery CSV files
message("[7/7] Building reports...")
df_final_sales <- add_sales_factors(df_final)

all_outputs    <- build_all_reports(df_final_sales, ingested$provider)
write_reports(all_outputs, ingested$paths$PATHRES)

message("Part 1 complete — reports written to: ", ingested$paths$PATHRES)

# ============================================================================
# QC SESSION 
# =============================================================================
# PART 2 — QC
# Quality checks and diagnostics.
# Input:  ingested, fact_df, df_final (all produced in Part 1)
# Output: CSV files + summaries for QC Checklist and Analyse Overview
# =============================================================================

# ── QC 1: Weekly QC summary ──────────────────────────────────────────────────
# Purpose : aggregated KPIs per ISO week — records, stores, EANs, price,
#           revenue, and all counts (zero rev, neg qty, mix price, etc.)
# Input   : ingested$data (standardised raw data, all rows)
# Output  : PROVIDER_QC_SUMMARY.csv → paste into QC Checklist -> Data_Summary
message("[2/7] Building QC summary...")
summary_all <- summary_df_merged(ingested) # Called from summary_df.R

write.csv2(summary_all,
           file.path(ingested$paths$PATHRES,
                     paste0(toupper(ingested$provider), "_QC_SUMMARY.csv")),
           row.names = FALSE, quote = FALSE)

# ── QC 2: Detail tables ───────────────────────────────────────────────────────
# Purpose : row-level detail for each type — EAN quality issues,
#           sales outliers, mix price candidates, duplicate products, WGR dist.
# Input   : ingested$data (standardised raw data)
# Output  : PROVIDER_QC_DETAIL.csv with 5 tagged sections →
#           paste into QC Checklist -> Data_Detail
#           [EAN_QUALITY] [SALES_OUTLIER] [MIX_PRICE] [DUP_PRODUCT] [WGR_DIST]
message("[QC 2] Building QC detail tables...")
# Called from summary_df.R
detail_tables <- build_detail_tables(ingested$data)

message("[2b] Exporting QC detail...")
export_detail_csv(ingested$data, 
                  out_path = file.path(ingested$paths$PATHRES, 
                                       paste0(toupper(ingested$provider), "_QC_DETAIL.csv")))

# ── QC 3: Mix price check ─────────────────────────────────────────────────────
# Purpose : Detect EANs sold at inconsistent prices within tobacco category.
#           Same business rules as mix_price_ean_count in QC 1:
#           Condition: ratio > MIX_PRICE_RATIO_MIN AND price_diff > MIX_PRICE_DIFF_MIN.
#           For multi-source providers (e.g. DTV), the source column identifies
#           which file caused the price gap (Reemtsma vs Auswertung).
# Input   : ingested$data + ingested$tobacco_groups + ingested$accessory_groups
# Output  : PROVIDER_RAW_DATA_MIX_PRICE.csv — flagged EAN-level rows
message("[QC 3] Checking mix prices...")

# Called from mix_price.R
mix_result <- mix_price_check(
  ingested$data,
  ingested$tobacco_groups,
  ingested$accessory_groups,
  ratio_min = MIX_PRICE_RATIO_MIN,
  diff_min  = MIX_PRICE_DIFF_MIN
)

write.csv2(mix_result$raw,
           file.path(ingested$paths$PATHRES,
                     paste0(toupper(ingested$provider), "_RAW_DATA_MIX_PRICE.csv")),
           row.names = FALSE, quote = FALSE)

# ── QC 4: Raw sales summary — before PMI match ───────────────────────────────
# Purpose : compare sales volume by report type using provider WGR taxonomy.
#           Runs BEFORE PMI match so no PMI attributes available.
#           WGR-based classification defined in ingest_<PROVIDER>.R.
#           Use to verify: are the numbers consistent with prior months?
#           If a drop/spike exists here → root cause is in raw provider data,
#           not in our pipeline processing.
# Input   : fact_df (output of factdata_final)
# Output  : printed pivot table — Measure × Report × Week
message("[QC 4] Raw sales summary (before PMI)...")
raw_summary <- build_raw_sales_summary(fact_df) # Called from ingest_*provider*.R

# ── QC 5: Post-PMI sales summary — after PMI match ───────────────────────────
# Purpose : compare sales volume by report type using PMI attributes.
#           Runs AFTER PMI match — uses PRODUCTTYPE, PACKAGETYPE, LENGTHTYPE
#           to split into CIG / OTP / 9961 / 9962 / 9963 / MISSING_ITEM.
#           Figures are higher than QC 4 because salesfactor is NOT applied here
#           (raw Sales units only — no sticks multiplication).
#           Compare QC 4 vs QC 5: difference = EANs not matched with PMI master.
# Input   : df_final (output of match_with_pmi)
# Output  : printed pivot table — Measure × Report × Week
message("[QC 5] Post-PMI sales summary...")
pmi_summary <- build_sales_summary_post_pmi(df_final) # Called from ingest_*provider*.R

write.csv2(pmi_summary,
           file.path(ingested$paths$PATHRES,
                     paste0(toupper(ingested$provider), "_QC_REPORT_PMI.csv")),
           row.names = FALSE, quote = FALSE)

# ── QC 6: Price tolerance analysis ───────────────────────────────────────────
# Purpose : determine optimal PRICE_TOLERANCE before running price QC (QC 7).
#           Analyses distribution of |calculate_price (raw) − PMI PRICE| on matched
#           rows. Run once per provider or after PMI master update.
#           After confirming: update PRICE_TOLERANCE in Section 1 and comment
#           out these 2 lines.
# Input   : df_final (matched rows with both calculate_price and PMI PRICE)
# Output  : printed percentile table + recommendation

message("[QC 6] Analysing price tolerance...")
price_tol <- analyse_price_tolerance(df_final)
print(price_tol$distribution)
# → update PRICE_TOLERANCE <- price_tol$recommendation, then comment out above

# ── QC 7: After matching PMI price QC ────────────────────────────────────────────────────────
# Purpose : flag EANs where calculate_price disagrees with PMI reference price.
#           2-step logic:
#             Step 1 — if |calculate_price − PMI PRICE| < PRICE_TOLERANCE → OK
#             Step 2 — else derive expected EANTYPE from price range and flag
#                      mismatches (P_as_Bundle / B_as_Pack)
#            → investigate root cause before acting on these flags.
# Input   : df_final · PRICE_* thresholds + PRICE_TOLERANCE
# Output  : PROVIDER_PMI_PRICE_ISSUES.csv + printed revenue_summary
message("[QC 7] PMI price QC...")

# Called from report.R
pmi_price_qc <- pmi_price_qc_check(df_final,
                          PRICE_PACK_MIN, PRICE_PACK_MAX,
                          PRICE_BUNDLE_MIN, PRICE_BUNDLE_MAX,
                          PRICE_THRESHOLD, PRICE_TOLERANCE)

pmi_price_issues <- ingested$data %>%
  dplyr::filter(EAN %in% pmi_price_qc$price_issues_ean, !is.na(EAN)) %>%
  dplyr::left_join(pmg$pmi_file, by = "EAN") %>%
  dplyr::select(week_in_data, month_in_data, store_id,
                sales_units, sales_revenue,
                EAN, product_text, wgr_code, wgr_text,
                EANTYPE, PACKSPERBUNDLE, ITEMSPERPACK, ITEMSPERBUNDLE)

write.csv2(pmi_price_issues,
           file.path(ingested$paths$PATHRES,
                     paste0(toupper(ingested$provider), "_PMI_PRICE_ISSUES.csv")),
           row.names = FALSE, quote = FALSE)

# Called from report.R
revenue_summary <- build_revenue_summary(df_final, pmi_price_qc$price_issues_ean)

message(sprintf("Done in %.1f sec.",
                as.numeric(Sys.time() - st, units = "secs")))













