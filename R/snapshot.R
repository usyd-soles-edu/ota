#' Create a Timestamped Snapshot of the Roster
#'
#' Saves the current roster to a timestamped CSV file in the logs directory.
#' This function is used to record roster states over time for comparison and
#' change tracking. By default, snapshots are only saved if there are differences 
#' from the latest existing snapshot.
#'
#' @param df A dataframe with roster data, typically returned by \code{\link{roster}()}.
#'   Must have attributes \code{file_path} and \code{unit} set.
#'
#' @param force Logical. If \code{FALSE} (default), only saves a snapshot if the 
#'   current roster differs from the latest snapshot. If \code{TRUE}, always saves 
#'   a new snapshot regardless of changes.
#'
#' @return Invisibly returns the input dataframe with an attribute `snapshot_created`
#'   (logical) indicating whether a new snapshot was created (TRUE) or skipped due to 
#'   no changes (FALSE), allowing for function chaining and downstream decisions.
#'
#' @details
#' The function creates a \code{logs} subdirectory in the same directory as
#' the source roster file. CSV files are named with the pattern:
#' \code{<unit>-YYYY-MM-DD-HHmmss.csv}
#'
#' Before saving, the function applies \code{\link{mutate_tutor_roles}()} to
#' mark the first tutor role per person-week as "Tutor" and subsequent tutor
#' roles in the same week as "Tutor (repeat)".
#'
#' If the current roster is identical to the latest snapshot and \code{force = FALSE},
#' no new snapshot is created and a message is displayed. When \code{force = TRUE},
#' a snapshot is always created even if no changes are detected.
#'
#' @examples
#' \dontrun{
#' # Load roster and create snapshot (only if changed)
#' df <- roster("path/to/roster.xlsx", unit = "biol1007")
#' snapshot(df)
#'
#' # Force snapshot creation regardless of changes
#' snapshot(df, force = TRUE)
#' }
#'
#' @importFrom dplyr anti_join arrange group_by mutate row_number if_else
#' @importFrom readr read_csv cols col_date col_character col_integer
#' @export
snapshot <- function(df, force = FALSE) {
  # Extract the file path from the dataframe attribute
  file_path <- attr(df, "file_path")
  unit <- attr(df, "unit")
  
  if (is.null(file_path)) {
    stop("No file_path attribute found in the dataframe. Make sure the dataframe comes from roster().")
  }
  
  # Get the directory of the original file
  dir_path <- dirname(file_path)
  
  # Create logs folder in that directory
  logs_dir <- file.path(dir_path, "logs")
  dir.create(logs_dir, showWarnings = FALSE, recursive = TRUE)
  
  # Apply tutor role mutation (mark repeats) - do this early for comparison
  df <- mutate_tutor_roles(df)
  
  # If force = FALSE, check if there are differences before saving
  if (!force) {
    # Read the latest log file if it exists
    log_files <- list.files(logs_dir, pattern = paste0(unit, "-.*\\.csv$"), full.names = TRUE)
    
    if (length(log_files) > 0) {
      # Read the most recent log file
      latest_log <- sort(log_files)[length(log_files)]
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
          index = readr::col_character()
        ),
        show_col_types = FALSE
      )
      
      # Check if current df is identical to the latest log using anti_join
      diff1 <- dplyr::anti_join(df, latest_df, by = names(df))
      diff2 <- dplyr::anti_join(latest_df, df, by = names(df))
      
      if (nrow(diff1) == 0 && nrow(diff2) == 0) {
        message("No changes detected, skipping snapshot.")
        # Mark that no snapshot was created and restore attributes before returning
        attr(df, "file_path") <- file_path
        attr(df, "unit") <- unit
        attr(df, "snapshot_created") <- FALSE
        return(invisible(df))
      }
    }
  }
  
  # Create timestamp for filename: yyyy-mm-dd-hms.csv
  timestamp <- format(Sys.time(), "%Y-%m-%d-%H%M%S")
  csv_file <- paste0(unit, "-", timestamp, ".csv")
  csv_path <- file.path(logs_dir, csv_file)
  
  # Arrange the dataframe by name, date, start
  df <- df %>% dplyr::arrange(name, date, start)
  
  # Save the dataframe to CSV
  write.csv(df, csv_path, row.names = FALSE)
  
  message("Snapshot saved to: ", csv_path)
  
  # Restore attributes to the dataframe, marking that snapshot was created, and return invisibly
  attr(df, "file_path") <- file_path
  attr(df, "unit") <- unit
  attr(df, "snapshot_created") <- TRUE
  
  invisible(df)
}
