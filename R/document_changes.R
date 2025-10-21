#' @title Document Roster Changes
#' @description 
#' This function takes the result from a comparison and prints a formatted,
#' color-coded summary of changes including additions (in green), removals 
#' (in red), replacements (showing old crossed-out and new in green), and
#' paycode changes (showing role transitions). The output is organized by date 
#' with a formatted table layout and includes helpful emoji indicators.
#' 
#' @param compare_result A list returned by \code{\link{compare}()}, containing
#'   changes (a list of additions, removals, replacements, and paycode_changes) 
#'   and the original dataframe.
#' 
#' @return Invisibly returns the original dataframe, allowing for function chaining.
#' 
#' @details
#' The function displays changes in a formatted table with the following columns:
#' \itemize{
#'   \item{Date}{The date of the roster change}
#'   \item{Type}{The type of change (Addition, Removal, Replacement, or Paycode change)}
#'   \item{Details}{Person name(s) and role(s) with emoji indicator, with color coding}
#'   \item{Activity}{Week, day, time, and lab location}
#' }
#' 
#' Emoji indicators:
#' \itemize{
#'   \item{🟢}{Addition - new person added to a slot}
#'   \item{🔴}{Removal - person removed from a slot}
#'   \item{🔄}{Replacement - person replaced by another in the same slot}
#'   \item{💰}{Paycode change - person's role changed in the same slot (e.g., Tutor (repeat) → Tutor)}
#' }
#'
#' Color coding: additions are shown in green, removals in red, replacements
#' show the removed name struck through in red and the new name in bold green.
#' Paycode changes show the person's name in bold green with the old role in red
#' and the new role in green.
#' 
#' @examples
#' \dontrun{
#' # Load, snapshot, and compare rosters
#' df <- roster("path/to/roster.xlsx", unit = "biol1007")
#' snapshot(df)
#' # ... make changes to the Excel file ...
#' df_new <- roster("path/to/roster.xlsx", unit = "biol1007")
#' snapshot(df_new)
#' result <- compare(df_new)
#' document_changes(result)  # Displays formatted summary
#' }
#'
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
  paycode_changes <- changes$paycode_changes
  
  # Calculate totals
  total_changes <- nrow(additions) + nrow(removals) + nrow(replacements)
  n_additions <- nrow(additions)
  n_removals <- nrow(removals)
  n_replacements <- nrow(replacements)
  n_paycode_changes <- if (is.null(paycode_changes)) 0 else nrow(paycode_changes)
  
  # Print header
  cli::cat_line("\nRoster Changes Summary")
  cli::cat_line("======================")
  cli::cat_line(sprintf("Total changes: %2d  | Additions: %2d  | Removals: %2d  | Replacements: %2d  | Paycode changes: %2d", 
                        total_changes, n_additions, n_removals, n_replacements, n_paycode_changes))
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
  change_list <- list(
    additions %>% dplyr::mutate(type = "Addition", details = sprintf("%s (%s)", cli::style_bold(cli::col_green(name)), role)),
    removals %>% dplyr::mutate(type = "Removal", details = sprintf("%s (%s)", cli::style_bold(cli::col_red(name)), role)),
    replacements %>% dplyr::mutate(type = "Replacement", details = sprintf("%s → %s (%s)", cli::style_strikethrough(cli::col_red(name_removed)), cli::style_bold(cli::col_green(name_added)), role))
  )
  
  # Add paycode changes if they exist
  if (!is.null(paycode_changes) && nrow(paycode_changes) > 0) {
    paycode_changes_formatted <- paycode_changes %>%
      dplyr::mutate(
        type = "Paycode change",
        details = sprintf("%s (%s → %s)", cli::style_bold(cli::col_green(name)), 
                         cli::style_strikethrough(cli::col_red(role_prev)), cli::style_bold(cli::col_green(role_latest))),
        role = role_latest
      ) %>%
      dplyr::select(date, day, start, end, location, name, week, type, details, role)
    change_list <- append(change_list, list(paycode_changes_formatted))
  }
  
  all_changes <- dplyr::bind_rows(change_list) %>% dplyr::arrange(date)
  
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

# Suppress R CMD check notes for non-standard evaluation variables
utils::globalVariables(c("name", "role", "name_removed", "name_added", "role_prev", "role_latest", "location", "week", "type", "details", "date", "day", "start", "end"))
