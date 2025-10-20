test_that("latest_log function returns the path of the latest CSV file", {
  # Source the function
  source("../../R/latest_log.R")

  # Create a temporary directory for testing
  temp_dir <- tempdir()
  logs_dir <- file.path(temp_dir, "test_logs")
  dir.create(logs_dir, recursive = TRUE)

  # Create mock CSV files with timestamps
  # Format: prefix-yyyy-mm-dd-hhmmss.csv
  mock_files <- c(
    "log-2023-10-01-120000.csv",
    "log-2023-10-02-130000.csv",
    "log-2023-10-03-140000.csv"
  )

  # Create the files
  for (file in mock_files) {
    file_path <- file.path(logs_dir, file)
    write.csv(data.frame(x = 1:5), file_path, row.names = FALSE)
  }

  # Call the function
  result <- .latest_log(logs_dir)

  # Check that result is a character string
  expect_type(result, "character")

  # Check that it's a full path
  expect_true(file.exists(result))

  # Check that it's the latest file (log-2023-10-03-140000.csv)
  expect_equal(basename(result), "log-2023-10-03-140000.csv")

  # Clean up
  unlink(logs_dir, recursive = TRUE)
})