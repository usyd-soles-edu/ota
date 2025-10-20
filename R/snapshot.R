# Snapshot function to save roster output to a timestamped CSV in a logs folder
#' @importFrom readr read_csv cols col_date col_character col_integer
#' @importFrom dplyr anti_join
snapshot <- function(df) {
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
  
  # List existing log files for this unit
  log_files <- list.files(logs_dir, pattern = paste0(unit, "-.*\\.csv$"), full.names = TRUE)
  
  if (length(log_files) > 0) {
    # Get the latest log file (assuming filenames are sortable by timestamp)
    latest_log <- sort(log_files)[length(log_files)]
    
    # Read the latest log file
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
        week = readr::col_integer()
      ),
      show_col_types = FALSE
    )
    
    # Check if current df is identical to the latest log
    diff1 <- dplyr::anti_join(df, latest_df, by = names(df))
    diff2 <- dplyr::anti_join(latest_df, df, by = names(df))
    
    if (nrow(diff1) == 0 && nrow(diff2) == 0) {
      message("No changes detected, skipping snapshot.")
      return(invisible(df))
    }
  }
  
  # Create timestamp for filename: yyyy-mm-dd-hms.csv
  timestamp <- format(Sys.time(), "%Y-%m-%d-%H%M%S")
  csv_file <- paste0(unit, "-", timestamp, ".csv")
  csv_path <- file.path(logs_dir, csv_file)
  
  # Save the dataframe to CSV
  write.csv(df, csv_path, row.names = FALSE)
  
  message("Snapshot saved to: ", csv_path)
  
  # Return the dataframe invisibly for potential chaining
  invisible(df)
}
