#' Read the Two Latest Log Files for a Unit
#'
#' An internal helper function that retrieves and reads the two most recent
#' timestamped log files for a given unit. Used exclusively by \code{\link{compare}()} to
#' detect changes between successive snapshots.
#'
#' @param unit A character string identifying the unit (e.g., "biol1007").
#' @param logs_dir The directory path where log CSV files are stored.
#'
#' @return A list with elements:
#'   \item{latest}{Dataframe of the most recent log file}
#'   \item{previous}{Dataframe of the second-most recent log file}
#'
#'   Returns \code{NULL} if fewer than 2 log files exist for the unit. This
#'   ensures \code{compare()} can only execute when there are two snapshots
#'   to compare.
#'
#' @details
#' Log files are identified by the pattern \code{<unit>-*.csv} in the
#' logs directory. Files are sorted by name (which corresponds to timestamp
#' due to YYYY-MM-DD-HHmmss naming convention) to identify the latest two.
#'
#' This function is specifically designed for \code{\link{compare}()} and
#' requires at least 2 log files to exist. For checking if current data
#' matches the latest log file, \code{\link{snapshot}()} has its own internal
#' comparison logic and does not use this function.
#'
#' @importFrom readr read_csv cols col_date col_character col_integer
#' @keywords internal
.latest_pair <- function(unit, logs_dir) {
  # List existing log files for this unit
  log_files <- list.files(
    logs_dir,
    pattern = paste0(unit, "-.*\\.csv$"),
    full.names = TRUE
  )

  if (length(log_files) < 2) {
    return(NULL)
  }

  # Sort log files by name (timestamp)
  sorted_logs <- sort(log_files)
  n <- length(sorted_logs)

  # Read the two latest log files
  latest_log <- sorted_logs[n]
  previous_log <- sorted_logs[n - 1]

  latest_df <- readr::read_csv(
    latest_log,
    col_types = readr::cols(
      date = readr::col_date(),
      day = readr::col_character(),
      start = readr::col_character(),
      end = readr::col_character(),
      location = readr::col_character(),
      name = readr::col_character(),
      role = readr::col_character(),
      week = readr::col_integer(),
      index = readr::col_character(),
      paycode = readr::col_character()
    ),
    show_col_types = FALSE
  )

  previous_df <- readr::read_csv(
    previous_log,
    col_types = readr::cols(
      date = readr::col_date(),
      day = readr::col_character(),
      start = readr::col_character(),
      end = readr::col_character(),
      location = readr::col_character(),
      name = readr::col_character(),
      role = readr::col_character(),
      week = readr::col_integer(),
      index = readr::col_character(),
      paycode = readr::col_character()
    ),
    show_col_types = FALSE
  )

  return(list(latest = latest_df, previous = previous_df))
}
