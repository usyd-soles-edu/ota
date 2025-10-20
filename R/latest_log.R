# Internal function to find the latest CSV file in a folder based on timestamp in filename
.latest_log <- function(folder_path) {
  # List all CSV files in the folder
  csv_files <- list.files(folder_path, pattern = "\\.csv$", full.names = TRUE)
  
  if (length(csv_files) == 0) {
    stop("No CSV files found in the specified folder.")
  }
  
  # Function to extract timestamp from filename
  extract_timestamp <- function(file_path) {
    # Get the basename
    filename <- basename(file_path)
    # Remove .csv extension
    name_without_ext <- sub("\\.csv$", "", filename)
    # Split by '-'
    parts <- strsplit(name_without_ext, "-")[[1]]
    if (length(parts) == 4) {
      # Format: yyyy-mm-dd-hhmmss
      timestamp_parts <- parts
    } else if (length(parts) == 5) {
      # Format: prefix-yyyy-mm-dd-hhmmss
      timestamp_parts <- parts[2:5]
    } else {
      stop("Filename does not match expected format: ", filename)
    }
    timestamp_str <- paste(timestamp_parts, collapse = "-")
    # Convert to POSIXct
    as.POSIXct(timestamp_str, format = "%Y-%m-%d-%H%M%S")
  }
  
  # Extract timestamps for all files
  timestamps <- sapply(csv_files, extract_timestamp)
  
  # Find the index of the latest timestamp
  latest_index <- which.max(timestamps)
  
  # Return the path of the latest file
  csv_files[latest_index]
}
