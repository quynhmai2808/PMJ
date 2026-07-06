## functions/pmg_process.R
## Locate, read, and prepare the PMI/PMG product master file, enriched with
## the missing-EAN supplement. Provider-agnostic (same PMI master for all).

#' Find the dated PMI reference files on disk.
find_pmi_files <- function(path_pmi, pmg_stamp, missing_ean_stamp) {
  pmg_product_files <- dir(path_pmi, pattern = "pmg_product", full.names = TRUE)
  pmg_product <- pmg_product_files[stringr::str_detect(basename(pmg_product_files), pmg_stamp)]

  missing_ean_files <- dir(path_pmi, pattern = "missing_ean", full.names = TRUE)
  missing_ean <- missing_ean_files[stringr::str_detect(basename(missing_ean_files), missing_ean_stamp)]

  list(pmg_product = pmg_product, missing_ean = missing_ean)
}

#' Read the PMG product master file, clean EAN, drop duplicate EANs.
read_pmg_product <- function(pmg_product_path, encoding = "UTF-8") {
  pmi_file_raw <- read.csv(
    pmg_product_path,
    sep = ";",
    colClasses = rep("character", 49),
    na.strings = c("NULL"),
    fileEncoding = encoding
  )

  pmi_file_raw %>%
    dplyr::mutate(EAN = nosigns(EAN)) %>%
    dplyr::mutate(EAN = as.character(as.numeric(EAN))) %>%
    dplyr::distinct(EAN, .keep_all = TRUE)  # QC: drop dup EANs created by nosigns()
}

#' Read the missing-EAN supplement file, restricted to ALL_RETAILER rows.
read_missing_eans <- function(missing_ean_path, encoding = "UTF-8") {
  read.csv(
    missing_ean_path,
    sep = ";",
    colClasses = rep("character", 11),
    na.strings = c("NULL"),
    fileEncoding = encoding
  ) %>%
    dplyr::distinct(WHOLESALER, INTERNAL_ARTICLE_NR, .keep_all = TRUE) %>%
    dplyr::filter(WHOLESALER == "ALL_RETAILER") %>%
    dplyr::arrange(WHOLESALER) %>%
    dplyr::mutate(
      EAN = nosigns(EAN),
      EAN = as.character(as.numeric(EAN)),
      INTERNAL_ARTICLE_NR = remove_leading_zeros(INTERNAL_ARTICLE_NR)
    ) %>%
    dplyr::mutate(ist_duplikat = duplicated(INTERNAL_ARTICLE_NR)) %>%
    dplyr::filter(!(ist_duplikat & WHOLESALER != "ALL_RETAILER")) %>%
    dplyr::select(-ist_duplikat)
}

#' Join missing_eans onto the PMG product master via INTERNAL_ARTICLE_NR,
#' producing extra rows keyed by the internal article number (acting as a
#' substitute EAN) for items not already present under their real EAN.
build_missing_ean_join <- function(pmi_file_raw, missing_eans) {

  common_values <- intersect(pmi_file_raw$EAN, missing_eans$INTERNAL_ARTICLE_NR)
  items_without_dup <- missing_eans %>%
    dplyr::filter(!INTERNAL_ARTICLE_NR %in% common_values)

  pmi_cols <- c(
    "EANTYPE","PRODUCTNUMBER","VALIDFROM","VALIDTO","DWHPRODUCTID","PRODUCTSTATUS",
    "PRODUCTNAME","SHORTCODE","PRODUCTGROUPL1","PRODUCTGROUPL2","OTP_PACK_CONTENT_G",
    "OTP_BUNDLE_CONTENT_G","ITEMSPERPACK","ITEMSPERBUNDLE","PACKSPERBUNDLE",
    "UNITOFMEASUREFACTORGRAM","UNITOFMEASUREFACTORPIECE","MASBRAND","PRODUCER",
    "DISTRIBUTOR","BRANDFAMILY","PRODUCTGROUPFINECUT","COUNTRY","PRODUCTTYPE",
    "PACKAGETYPE","OFFERTYPE","FILTERTYPE","FLAVOURPRODUCTGROUP","FLAVOURBLEND",
    "FLAVOURIMAGE","FLAVOURMENTHOL","PRICEGROUP","LENGTHMM","NICOTINE","CONDENSATE",
    "CARBONMONOXIDE","CODING","TIPPING","TASTEINTL","LENGTHTYPE","DISTRIBUTIONCATEGORY",
    "THICKNESS","PACKSIZE","PACKTYPE","GIMSBRANDDIFF","GIMSPACKTYPE","GIMSLENGTH","PRICE"
  )

  items_without_dup %>%
    dplyr::inner_join(pmi_file_raw, by = c("EAN" = "EAN")) %>%
    dplyr::rename(EAN_OLD = EAN, EAN = INTERNAL_ARTICLE_NR) %>%
    dplyr::select("EAN", "EANTYPE", "EAN_OLD", dplyr::all_of(pmi_cols[-1]))
}

#' Full PMG process: discover files, read, join, return enriched master +
#' two QC checks for bundles/packs missing key fields.
#'
#' @return list(pmi_file, check_QC_bundle, check_QC_pack)
pmg_process <- function(path_pmi, pmg_stamp, missing_ean_stamp, encoding = "UTF-8") {

  files <- find_pmi_files(path_pmi, pmg_stamp, missing_ean_stamp)

  pmi_file_raw <- read_pmg_product(files$pmg_product, encoding)
  missing_eans <- read_missing_eans(files$missing_ean, encoding)

  ean_join <- build_missing_ean_join(pmi_file_raw, missing_eans)

  pmi_file <- dplyr::bind_rows(pmi_file_raw, ean_join) %>%
    dplyr::mutate(EAN_fuer_Filter = EAN)

  check_QC_bundle <- pmi_file %>%
    dplyr::filter(EANTYPE == "B" & (ITEMSPERBUNDLE == "" | OTP_BUNDLE_CONTENT_G == "")) %>%
    dplyr::filter(PRODUCER == "PMG" | PRODUCTTYPE == "Cigarette" |
                    LENGTHTYPE == "RRP HTP STICKS" | PRODUCTTYPE == "Finecut")

  check_QC_pack <- pmi_file %>%
    dplyr::filter(EANTYPE == "P" & (ITEMSPERPACK == "" | OTP_PACK_CONTENT_G == "")) %>%
    dplyr::filter(PRODUCER == "PMG" | PRODUCTTYPE == "Cigarette" |
                    LENGTHTYPE == "RRP HTP STICKS" | PRODUCTTYPE == "Finecut")

  list(pmi_file = pmi_file, check_QC_bundle = check_QC_bundle, check_QC_pack = check_QC_pack)
}
