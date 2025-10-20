# Snapshot function to save roster output to a timestamped CSV in a logs folder
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
