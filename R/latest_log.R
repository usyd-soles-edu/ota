# Internal function to read the two latest log files for a given unit
#' @importFrom readr read_csv cols col_date col_character col_integer
.latest_pair <- function(unit, logs_dir) {
  # List existing log files for this unit
  log_files <- list.files(logs_dir, pattern = paste0(unit, "-.*\\.csv$"), full.names = TRUE)
  
  if (length(log_files) == 0) {
    return(NULL)
  }
  
  # Sort log files by name (timestamp)
  sorted_logs <- sort(log_files)
  n <- length(sorted_logs)
  
  # Read the latest log file
  latest_df <- readr::read_csv(
    sorted_logs[n],
    col_types = readr::cols(
      date = readr::col_date(),
      day = readr::col_character(),
      start = readr::col_character(),
      end = readr::col_character(),
      location = readr::col_character(),
      name = readr::col_character(),
      role = readr::col_character(),
      week = readr::col_integer()
    ),
    show_col_types = FALSE
  )
  
  if (n == 1) {
    return(list(latest = latest_df, previous = NULL))
  }
  
  # Read the previous log file
  previous_df <- readr::read_csv(
    sorted_logs[n - 1],
    col_types = readr::cols(
      date = readr::col_date(),
      day = readr::col_character(),
      start = readr::col_character(),
      end = readr::col_character(),
      location = readr::col_character(),
      name = readr::col_character(),
      role = readr::col_character(),
      week = readr::col_integer()
    ),
    show_col_types = FALSE
  )
  
  return(list(latest = latest_df, previous = previous_df))
}
