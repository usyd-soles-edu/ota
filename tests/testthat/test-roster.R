test_that("roster function processes Excel file correctly", {
  # Path to the test Excel file
  file_path <- "../data/baseline.xlsx"

  # Call the function with unit
  result <- roster(file_path, "biol1007")

  # Check that result is a dataframe
  expect_s3_class(result, "data.frame")

  # Check column names
  expected_cols <- c("date", "day", "start", "end", "location", "name", "role", "week", "activity")
  expect_equal(names(result), expected_cols)

  # Check that date is Date class
  expect_s3_class(result$date, "Date")

  # Check that role is either "Tutor" or "Demonstrator"
  expect_true(all(result$role %in% c("Tutor", "Demonstrator")))

  # Check that location is numeric (as string)
  expect_type(result$location, "character")
  expect_true(all(grepl("^\\d+$", result$location)))

  # Check that name is character and not NA
  expect_type(result$name, "character")
  expect_true(all(!is.na(result$name)))

  # Check that day is a valid day abbreviation
  valid_days <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
  expect_true(all(result$day %in% valid_days))

  # Check that start and end are time strings
  expect_type(result$start, "character")
  expect_type(result$end, "character")
  expect_true(all(grepl("^\\d{2}:\\d{2}$", result$start)))
  expect_true(all(grepl("^\\d{2}:\\d{2}$", result$end)))

  # Check that week is numeric
  expect_type(result$week, "integer")

  # Check that activity is character
  expect_type(result$activity, "character")

  # Clean up created log files
  logs_dir <- file.path(dirname(file_path), "logs")
  if (dir.exists(logs_dir)) {
    unlink(logs_dir, recursive = TRUE)
  }
})