## config.R
## Central configuration: paths + provider-specific column mapping.
## To onboard a new data provider: copy this file, change `provider_cols`
## to match that provider's raw CSV headers, and point main.R at the new config.

# --- Provider identity ---
foldername_data <- "Combase"
retailer_name   <- "Combase"   # same string today, kept separate because
                                # foldername_data drives paths/file patterns,
                                # retailer_name drives output naming/labels

# --- Paths ---
PATHORG <- paste0("/qa/data1/PMI/Archive/", foldername_data, "/")
PATHRES <- paste0("/qa/data1/PMI/Results/", retailer_name, "/")
PATHPMI <- "/qa/data1/PMI/Archive/PMI_Dateien/"

# --- PMI reference file date stamps (kept as variables, not hardcoded in logic) ---
pmg_product_stamp  <- "20260622"
missing_ean_stamp  <- "20260617"

# --- File discovery patterns (provider-specific) ---
# file_pattern_1: filenames with a leading timestamp, e.g. 2026_06_22_1530_COMBASE.csv
# file_pattern_2: filenames with just a date, e.g.        22_06_2026_COMBASE.csv
file_pattern_1 <- "^\\d{4}_\\d{2}_\\d{2}_\\d{4}_COMBASE.csv$"
file_pattern_2 <- "^\\d{2}_\\d{2}_\\d{4}_COMBASE.csv$"

# date_regex_strip_1/2: regex used to pull the date string out of the filename
date_extract_1 <- function(file_name) sub("^\\d+_", "", sub("_COMBASE\\.csv$", "", file_name))
date_extract_2 <- function(file_name) sub("_COMBASE\\.csv$", "", file_name)
date_format_in_filename <- "%d_%m_%Y"

# --- Raw column mapping (THIS is what changes per provider) ---
# Map abstract field names -> actual raw CSV column names for Combase.
provider_cols <- list(
  year          = "POS_Start_Receipt_Date_Jahr",
  week          = "POS_Woche_des_Jahres",
  store         = "POS_Store",
  qty           = "POS_POS_Quantity",
  amount        = "POS_Amount_Tendered",
  amount_netto  = "POS_Amount_Netto",
  ean           = "POS_Barcode_No",
  text          = "Bezeichnung",
  vke           = "VKE",
  wgr_code      = "Warengruppecode",
  wgr           = "Warengruppe"
)

# --- CSV read options ---
csv_sep        <- ";"
csv_quote      <- ""
codierung_PMI  <- "UTF-8"

# --- Business rule thresholds (mix price / pack-bundle classification) ---
price_pack_min   <- 3
price_pack_max   <- 25
price_bundle_min <- 46
price_bundle_max <- 100
price_threshold_pack_bundle <- 30
mix_price_ratio_min <- 2
mix_price_diff_min  <- 5

tobacco_groups <- c(
  "Tabak", "Zigaretten", "Zigaretten & Co", "Cigarillos / Cigarren",
  "Pfeifentabak", "Kautabak", "Schnupftabak", "IQOS", "Heat-not-burn"
)
accessory_groups <- c(
  "Zigarettenpapier", "Raucherbedarf", "Sonst.Raucherbedarf", "Taschenfeuerzeuge"
)
