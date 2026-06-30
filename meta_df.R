## functions/meta_df.R
## Discover raw files for a given pattern and attach week/date metadata.
## Generalized over the original meta_df_1 / meta_df_2 blocks.

#' Build a metadata data.frame for files matching a pattern in `path`.
#'
#' @param path             Directory to search.
#' @param pattern          Regex filename pattern (e.g. config$file_pattern_1).
#' @param date_extract_fn  Function(file_name) -> date string to be parsed.
#' @param date_format      Format string for as.Date() on the extracted date string.
#' @return data.frame with columns: file_path, file_name, file_date, year, week,
#'         day, wday, start_week, end_week, size_bytes, size_kb
meta_df <- function(path, pattern, date_extract_fn, date_format) {

  file_paths <- list.files(path = path, pattern = pattern, full.names = TRUE)

  if (length(file_paths) == 0) {
    return(data.frame(
      file_path = character(0), file_name = character(0),
      file_date = as.Date(character(0)), year = integer(0), week = character(0),
      day = character(0), wday = integer(0),
      start_week = as.Date(character(0)), end_week = as.Date(character(0)),
      size_bytes = numeric(0), size_kb = numeric(0),
      stringsAsFactors = FALSE
    ))
  }

  df <- data.frame(
    file_path = file_paths,
    file_name = basename(file_paths),
    stringsAsFactors = FALSE
  )

  df %>%
    dplyr::mutate(
      file_date  = as.Date(date_extract_fn(file_name), format = date_format),

      year  = lubridate::isoyear(file_date),
      week  = paste0(year, format(file_date, "%V")),
      day   = weekdays(file_date),

      wday       = as.integer(format(file_date, "%u")),
      start_week = file_date - (wday - 1),
      end_week   = file_date + (7 - wday),

      size_bytes = file.info(file_path)$size,
      size_kb    = round(size_bytes / 1024, 2)
    ) %>%
    dplyr::arrange(week)
}
