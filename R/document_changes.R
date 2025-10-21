# Document changes function to provide plain English documentation of additions, removals, and replacements
#' @importFrom dplyr inner_join anti_join select mutate pull filter arrange bind_rows
#' @importFrom lubridate day month year
#' @importFrom cli style_strikethrough style_bold ansi_nchar ansi_strip col_green col_red
#' @export
document_changes <- function(compare_result) {
  changes <- compare_result$changes
  df <- compare_result$df
  
  if (is.null(changes)) {
    message("No changes to document.")
    return(invisible(df))
  }
  
  additions <- changes$additions
  removals <- changes$removals
  replacements <- changes$replacements
  
  # Calculate totals
  total_changes <- nrow(additions) + nrow(removals) + nrow(replacements)
  n_additions <- nrow(additions)
  n_removals <- nrow(removals)
  n_replacements <- nrow(replacements)
  
  # Print header
  cli::cat_line("\nRoster Changes Summary")
  cli::cat_line("======================")
  cli::cat_line(sprintf("Total changes: %2d  | Additions: %2d  | Removals: %2d  | Replacements: %2d", 
                        total_changes, n_additions, n_removals, n_replacements))
  cli::cat_line()
  
  # Function to format activity
  format_activity <- function(row) {
    week_str <- sprintf("Wk%d", row$week)
    day_abbr <- substr(row$day, 1, 3)  # Mon, Tue, etc.
    
    # Convert times to 12-hour format
    convert_time <- function(time_str) {
      hour <- as.integer(substr(time_str, 1, 2))
      if (hour == 0) "12am" else if (hour < 12) paste0(hour, "am") else if (hour == 12) "12pm" else paste0(hour - 12, "pm")
    }
    
    start_conv <- convert_time(row$start)
    end_conv <- convert_time(row$end)
    
    sprintf("%s - Lab %s %s %s-%s", week_str, row$location, day_abbr, start_conv, end_conv)
  }
  
  # Collect all changes into one data frame
  all_changes <- dplyr::bind_rows(
    additions %>% dplyr::mutate(type = "Addition", details = sprintf("%s (%s)", cli::style_bold(cli::col_green(name)), role)),
    removals %>% dplyr::mutate(type = "Removal", details = sprintf("%s (%s)", cli::style_bold(cli::col_red(name)), role)),
    replacements %>% dplyr::mutate(type = "Replacement", details = sprintf("%s → %s (%s)", cli::style_strikethrough(cli::col_red(name_removed)), cli::style_bold(cli::col_green(name_added)), role))
  ) %>% dplyr::arrange(date)
  
  if (nrow(all_changes) == 0) {
    cli::cat_line("No changes detected.")
    return(invisible(df))
  }
  
  # Compute formatted rows
  rows <- lapply(seq_len(nrow(all_changes)), function(i) {
    row <- all_changes[i, ]
    date_str <- format(row$date, "%Y-%m-%d")
    activity <- format_activity(row)
    list(date = date_str, type = row$type, details = row$details, activity = activity)
  })
  
  # Compute max widths based on visible characters
  col_widths <- c(
    Date = max(sapply(rows, function(r) nchar(r$date))),
    Type = max(sapply(rows, function(r) nchar(r$type))),
    Details = max(sapply(rows, function(r) nchar(cli::ansi_strip(r$details)))),
    Activity = max(sapply(rows, function(r) nchar(cli::ansi_strip(r$activity))))
  )
  
  # Ensure minimum widths
  col_widths <- pmax(col_widths, c(Date = 10, Type = 12, Details = 20, Activity = 15))
  
  # Function to pad strings properly accounting for ANSI codes
  pad_string <- function(text, width) {
    visible_text <- cli::ansi_strip(text)
    padding_needed <- width - nchar(visible_text)
    if (padding_needed > 0) {
      return(paste0(text, strrep(" ", padding_needed)))
    } else {
      return(text)
    }
  }
  
  # Print header
  header <- paste0(
    pad_string("Date", col_widths["Date"]), " | ",
    pad_string("Type", col_widths["Type"]), " | ",
    pad_string("Details", col_widths["Details"]), " | ",
    pad_string("Activity", col_widths["Activity"])
  )
  cli::cat_line(header)
  
  # Print separator
  sep <- paste0(
    strrep("-", col_widths["Date"]), "-|-",
    strrep("-", col_widths["Type"]), "-|-",
    strrep("-", col_widths["Details"]), "-|-",
    strrep("-", col_widths["Activity"])
  )
  cli::cat_line(sep)
  
  # Print rows
  for (r in rows) {
    row_str <- paste0(
      pad_string(r$date, col_widths["Date"]), " | ",
      pad_string(r$type, col_widths["Type"]), " | ",
      pad_string(r$details, col_widths["Details"]), " | ",
      pad_string(r$activity, col_widths["Activity"])
    )
    cli::cat_line(row_str)
  }
  
  # Return the dataframe invisibly
  invisible(df)
}