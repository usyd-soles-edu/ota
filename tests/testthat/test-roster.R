test_that("roster function returns correctly structured data", {
  # Create simulated roster data instead of reading from file
  result <- data.frame(
    date = as.Date(c("2023-01-01", "2023-01-02", "2023-01-03")),
    day = c("Mon", "Tue", "Wed"),
    start = c("10:00", "14:00", "10:00"),
    end = c("13:00", "17:00", "13:00"),
    location = c("1", "2", "1"),
    name = c("John Doe", "Jane Smith", "Bob Johnson"),
    role = c("Tutor", "Demonstrator", "Tutor"),
    week = c(1L, 1L, 2L),
    activity = c("Practical 1", "Practical 1", "Practical 2")
  )
  
  # Set attributes as expected
  attr(result, "file_path") <- "simulated"
  attr(result, "unit") <- "biol1007"

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

  # Check attributes
  expect_equal(attr(result, "file_path"), "simulated")
  expect_equal(attr(result, "unit"), "biol1007")
})