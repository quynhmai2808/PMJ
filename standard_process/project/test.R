# library(dplyr)
# library(lubridate)
library(tidyr)
library(ISOweek)
library(timeDate)
install.packages("timeDate")

# --- Muster dataframe ---
# df <- tibble::tribble(
#   ~week_in_data, ~weekday,   ~Date,
#   "202614",      "Wednesday","2026-04-01",
#   "202614",      "Thursday", "2026-04-02",
#   "202614",      "Saturday", "2026-04-04",
#   "202615",      "Tuesday",  "2026-04-07",
#   "202615",      "Wednesday","2026-04-08",
#   "202615",      "Thursday", "2026-04-09",
#   "202615",      "Friday",   "2026-04-10",
#   "202615",      "Saturday", "2026-04-11"
# ) %>%
#   mutate(Date = as.Date(Date))

# --- 1. Split year + iso week from week_in_data ---

weekday_DTV_all <- ingested$data %>%
  select(week_in_data,weekday,Date)


weekday_DTV_all  <- weekday_DTV_all  %>%
  mutate(
    year_part = as.integer(substr(week_in_data, 1, 4)),
    week_part = as.integer(substr(week_in_data, 5, 6))
  )

# --- 2. full 7 days (Mon-Sun) for each week_in_data ---
week_keys <- weekday_DTV_all %>%
  distinct(week_in_data, year_part, week_part)

full_week_grid <- week_keys %>%
  rowwise() %>%
  mutate(
    monday = ISOweek::ISOweek2date(sprintf("%d-W%02d-1", year_part, week_part))
  ) %>%
  ungroup() %>%
  tidyr::uncount(7, .id = "offset") %>%
  mutate(
    Date = monday + (offset - 1),
    weekday = weekdays(Date)
  ) %>%
  select(week_in_data, Date, weekday)

# --- 3. Finding missing days ---
missing_days <- full_week_grid %>%
  anti_join(weekday_DTV_all, by = c("week_in_data", "Date")) %>%
  arrange(week_in_data, Date)

source("/qa/data1/PMI/Code/R_profiling_framework/data_pipeline/functions/classify_day.R")
missing_days <- missing_days %>%
  mutate(day_type = classify_day(Date, bundesland = "ALL"))  # replace "ALL" -> "NW", "BY"... for specific bundesland

write.csv2(missing_days,
           file.path(ingested$paths$PATHRES,
                     paste0(toupper(ingested$provider), "_MISSING_RAW_DAILY_DATA.csv")),
           row.names = FALSE, quote = FALSE)

###########################################
# IMPACT SALES RELEVANT WGR vs IRRELEVANT WGR
###########################################
relevant_wgr_sales <- df_final_sales %>%
  dplyr::filter(match_pmi == "match") %>%
  dplyr::filter(!(wgr_text=="Pfeifentab." | wgr_text=="Zigarren" | wgr_text== "Zigarrenn" | wgr_text=="RBA"))

all_matched_wgr_sales <- df_final_sales %>%
  dplyr::filter(match_pmi == "match")

compare <- bind_rows(
  all_matched_wgr_sales %>% summarise(Version = "Matched_full_WGR",  Total_Sales = sum(Sales), Total_Revenue = sum(Revenue), n_EAN = n_distinct(EAN)),
  relevant_wgr_sales %>% summarise(Version = "Matched_relevant_WGR", Total_Sales = sum(Sales), Total_Revenue = sum(Revenue), n_EAN = n_distinct(EAN))
) %>%
  mutate(irrelevant_wgr_Sales = first(Total_Sales) - Total_Sales,
         irrelevant_wgr_Revenue = first(Total_Revenue) - Total_Revenue,
         irrelevant_wgr_Revenue_pct   = round(irrelevant_wgr_Revenue / first(Total_Revenue) * 100, 1))

###########################################
# DTV - MISSING ITEMS PROCESS
###########################################
missing_items_output <- all_outputs[["MISSING_ITEMS"]]

missing_items_output_filtered <- missing_items_output %>%
  filter(!(Warengruppe=="Pfeifentab." | Warengruppe=="Zigarren" | Warengruppe== "Zigarrenn" | Warengruppe=="RBA"))
