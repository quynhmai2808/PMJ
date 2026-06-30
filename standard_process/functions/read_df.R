## functions/read_df.R
## Read raw CSV files listed in a meta_df table, attach file-level metadata
## and a row-level week-match QC flag. Generalized over data_list_1/2.

#' Read each file referenced in `meta` and return a named list of data.frames.
#'
#' @param meta      Output of meta_df().
#' @param cols      provider_cols list from config.R (needs $year, $week).
#' @param sep       CSV separator.
#' @param quote     CSV quote character.
#' @return Named list of data.frames (names = file_name), each with extra
#'         columns: file_name, file_date, year_file, week_file, day,
#'         start_week, end_week, size_kb, week_in_data, week_match_flag.
#'         All columns coerced to character (preserves original behavior).
read_df <- function(meta, cols, sep = ";", quote = "") {

  if (nrow(meta) == 0) return(list())

  data_list <- lapply(seq_len(nrow(meta)), function(i) {

    df <- read.csv(meta$file_path[i], sep = sep, fill = TRUE, quote = quote)

    df %>%
      dplyr::mutate(
        file_name  = meta$file_name[i],
        file_date  = as.character(meta$file_date[i]),
        year_file  = as.character(meta$year[i]),
        week_file  = as.character(meta$week[i]),
        day        = as.character(meta$day[i]),
        start_week = as.character(meta$start_week[i]),
        end_week   = as.character(meta$end_week[i]),
        size_kb    = as.character(meta$size_kb[i]),

        # QC: does the week encoded in the data match the week derived
        # from the filename?
        week_in_data = paste0(
          as.integer(.data[[cols$year]]),
          sprintf("%02d", as.integer(.data[[cols$week]]))
        ),
        week_match_flag = week_in_data == week_file
      ) %>%
      dplyr::mutate(dplyr::across(dplyr::everything(), as.character))
  })

  names(data_list) <- meta$file_name
  data_list
}
