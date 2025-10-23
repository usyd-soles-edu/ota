#' Process Roster Data for BIOL1007
#'
#' This function handles roster processing specific to BIOL1007 unit.
#'
#' @param file_path Path to the Excel file containing the roster data.
#' @return A dataframe in long format with columns: index, date, day, start, end, location, name, role, week.
#' @importFrom readxl read_excel
#' @importFrom dplyr %>% select all_of filter mutate across where na_if
#'   case_when
#' @importFrom tidyr fill pivot_longer
#' @importFrom stringr str_extract str_replace str_detect str_starts
roster_biol1007 <- function(file_path) {
  lgr::lgr$info("Starting to process BIOL1007 roster data.")
  # For now, using the same logic as default
  # TODO: Customize for BIOL1007 specific requirements
  df <- suppressMessages(read_excel(file_path, sheet = 1, skip = 4))
  lgr::lgr$info("Loaded roster data from the file.")

  # Validate that this is the correct roster file
  required_cols <- c("Week", "Practical", "Date", "Session")
  if (!all(required_cols %in% names(df))) {
    stop("Invalid roster file: missing ",
         paste(setdiff(required_cols, names(df)), collapse = ", "))
  }
  if (!any(str_starts(names(df), "Sup-"))) {
    stop("Invalid roster file: no 'Sup-' columns found.")
  }
  lgr::lgr$info("Checked that the file has the required format.")

  # Fill down Week and Practical columns
  df <- df %>%
    fill(Week, Practical, .direction = "down")
  lgr::lgr$debug("Filled in missing values for Week and Practical columns.")

  # Find the first column starting with "Sup-"
  sup_cols <- which(str_starts(names(df), "Sup-"))
  if (length(sup_cols) > 0) {
    start_col <- min(sup_cols)
    # Find columns from start_col onwards that start with "Sup-" or "Demo-"
    relevant_cols <- start_col:ncol(df)
    keep_cols <- relevant_cols[str_starts(names(df)[relevant_cols], "Sup-") |
                                 str_starts(names(df)[relevant_cols], "Demo-")]
    # If there's a column that doesn't, stop before it
    if (length(keep_cols) < length(relevant_cols)) {
      # Find the first non-matching
      non_match <- min(relevant_cols[!relevant_cols %in% keep_cols])
      keep_cols <- keep_cols[keep_cols < non_match]
    }
    # Keep columns up to the last relevant
    df <- df %>% select(1:(start_col - 1), all_of(keep_cols))
  }

  # Remove rows where Date is NA
  df <- df %>% filter(!is.na(Date))
  lgr::lgr$debug("Removed rows with missing dates.")

  # Pivot the Sup and Demo columns
  df_long <- df %>%
    pivot_longer(
      cols = starts_with(c("Sup-", "Demo-")),
      names_to = "role_loc",
      values_to = "name",
      values_drop_na = TRUE
    ) %>%
    mutate(
      index = row_number(),
      role = str_extract(role_loc, "^(Sup|Demo)"),
      role = case_when(
        role == "Sup" ~ "Tutor",
        role == "Demo" ~ "Demonstrator",
        TRUE ~ role
      ),
      location = str_extract(role_loc, "(?<=-)\\d+"),
      date = as.Date(Date),
      day = str_extract(Session, "^\\w+"),
      start = case_when(
        str_detect(Session, "10-1pm") ~ "10:00",
        str_detect(Session, "2-5pm") ~ "14:00",
        TRUE ~ NA_character_
      ),
      end = case_when(
        str_detect(Session, "10-1pm") ~ "13:00",
        str_detect(Session, "2-5pm") ~ "17:00",
        TRUE ~ NA_character_
      ),
      week = as.integer(Week)
    ) %>%
    select(index, date, day, start, end, location, name, role, week)
  lgr::lgr$info("Reformatted the data into the required structure.")

  # Attach the file path and unit as attributes to the returned dataframe
  attr(df_long, "file_path") <- file_path
  attr(df_long, "unit") <- "biol1007"
  lgr::lgr$info("Finished processing BIOL1007 roster data.")

  return(df_long)
}
