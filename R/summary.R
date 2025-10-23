#' @title Summarize Roster Changes
#' @description S3 method for summarising roster changes.
#' @param object A `document_changes` object from [compare()].
#' @param HTML Logical. If TRUE, generates HTML summary and saves to `logs/html/`.
#' @param ... Additional arguments (ignored).
#' @return Invisibly returns the input dataframe.
#' @importFrom dplyr bind_rows arrange select mutate
#' @importFrom cli style_strikethrough style_bold ansi_nchar ansi_strip col_green col_red cat_line
#' @export
summary.document_changes <- function(object, HTML = FALSE, ...) {
  compare_result <- object
  changes <- compare_result$changes
  df <- compare_result$df

  if (is.null(changes)) {
    return(invisible(df))
  }

  # Extract unit from dataframe attributes
  unit <- attr(df, "unit")
  if (is.null(unit)) {
    unit <- "unknown"
  }

  additions <- changes$additions
  removals <- changes$removals
  replacements <- changes$replacements
  swaps <- changes$swaps
  paycode_changes <- changes$paycode_changes

  # Calculate totals
  total_changes <- nrow(additions) + nrow(removals) + nrow(replacements) + 
    (if (is.null(swaps)) 0 else nrow(swaps))
  n_additions <- nrow(additions)
  n_removals <- nrow(removals)
  n_replacements <- nrow(replacements)
  n_swaps <- if (is.null(swaps)) 0 else nrow(swaps)
  n_paycode_changes <- if (is.null(paycode_changes))
    0
  else
    nrow(paycode_changes)

  # Function to format activity (shared with HTML generation)
  format_activity <- function(row) {
    week_str <- sprintf("Wk%d", row$week)
    day_abbr <- substr(row$day, 1, 3)  # Mon, Tue, etc.
    start_conv <- convert_time_12h(row$start)
    end_conv <- convert_time_12h(row$end)
    sprintf("%s - Lab %s %s %s-%s", week_str, row$location, day_abbr, start_conv, end_conv)
  }

  # Collect all changes into one data frame
  change_list <- list(
    additions %>% dplyr::mutate(
      type = "Addition",
      details = sprintf("%s - %s", cli::style_bold(cli::col_green(name)), role)
    ),
    removals %>% dplyr::mutate(
      type = "Removal",
      details = sprintf("%s - %s", cli::style_bold(cli::col_red(name)), role)
    ),
    replacements %>% dplyr::mutate(
      type = "Replacement",
      details = sprintf(
        "%s → %s - %s",
        cli::style_strikethrough(cli::col_red(name_removed)),
        cli::style_bold(cli::col_green(name_added)),
        role_added
      ),
      role = role_added
    ) %>%
      dplyr::select(date, day, start, end, location, name = name_added, role, week, type, details)
  )

  # Add swaps if they exist
  if (!is.null(swaps) && nrow(swaps) > 0) {
    # For swaps, we show both people involved
    swaps_formatted <- swaps %>%
      dplyr::mutate(
        type = "Swap",
        details = sprintf(
          "%s ↔ %s (roles: %s ↔ %s)",
          cli::style_bold(cli::col_blue(name_removed)),
          cli::style_bold(cli::col_blue(name_added)),
          role_removed,
          role_added
        ),
        role = role_added,
        name = paste(name_removed, "↔", name_added)
      ) %>%
      dplyr::select(date, day, start, end, location, name, role, week, type, details, index)
    change_list <- append(change_list, list(swaps_formatted))
  }

  # Add paycode changes if they exist
  if (!is.null(paycode_changes) && nrow(paycode_changes) > 0) {
    paycode_changes_formatted <- paycode_changes %>%
      dplyr::mutate(
        type = "Paycode change",
        details = sprintf(
          "%s - %s → %s",
          cli::style_bold(cli::col_green(name)),
          cli::style_strikethrough(cli::col_red(role_prev)),
          cli::style_bold(cli::col_green(role_latest))
        ),
        role = role_latest
      ) %>%
      dplyr::select(date,
                    day,
                    start,
                    end,
                    location,
                    name,
                    role,
                    week,
                    type,
                    details,
                    index)
    change_list <- append(change_list, list(paycode_changes_formatted))
  }

  all_changes <- dplyr::bind_rows(change_list) %>% dplyr::arrange(date)

  if (nrow(all_changes) == 0) {
    cli::cat_line("\nRoster Changes Summary")
    cli::cat_line("======================")
    cli::cat_line("No changes detected.")
    return(invisible(df))
  }

  # Compute formatted rows
  rows <- lapply(seq_len(nrow(all_changes)), function(i) {
    row <- all_changes[i, ]
    date_str <- format(row$date, "%Y-%m-%d")
    activity <- format_activity(row)
    list(
      date = date_str,
      type = row$type,
      details = row$details,
      activity = activity,
      row_data = row
    )
  })

  # Compute max widths based on visible characters
  col_widths <- c(
    Date = max(sapply(rows, function(r)
      nchar(r$date))),
    Type = max(sapply(rows, function(r)
      nchar(r$type))),
    Details = max(sapply(rows, function(r)
      nchar(cli::ansi_strip(r$details)))),
    Activity = max(sapply(rows, function(r)
      nchar(cli::ansi_strip(r$activity))))
  )

  # Ensure minimum widths
  col_widths <- pmax(col_widths, c(
    Date = 10,
    Type = 12,
    Details = 20,
    Activity = 15
  ))

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

  # Print console output
  cli::cat_line("\nRoster Changes Summary")
  cli::cat_line("======================")
  cli::cat_line(
    sprintf(
      "Total changes: %2d  | Additions: %2d  | Removals: %2d  | Replacements: %2d  | Swaps: %2d  | Paycode changes: %2d",
      total_changes,
      n_additions,
      n_removals,
      n_replacements,
      n_swaps,
      n_paycode_changes
    )
  )
  cli::cat_line()

  # Print header
  header <- paste0(
    pad_string("Date", col_widths["Date"]),
    " | ",
    pad_string("Type", col_widths["Type"]),
    " | ",
    pad_string("Details", col_widths["Details"]),
    " | ",
    pad_string("Activity", col_widths["Activity"])
  )
  cli::cat_line(header)

  # Print separator
  sep <- paste0(
    strrep("-", col_widths["Date"]),
    "-|-",
    strrep("-", col_widths["Type"]),
    "-|-",
    strrep("-", col_widths["Details"]),
    "-|-",
    strrep("-", col_widths["Activity"])
  )
  cli::cat_line(sep)

  # Print rows
  for (r in rows) {
    row_str <- paste0(
      pad_string(r$date, col_widths["Date"]),
      " | ",
      pad_string(r$type, col_widths["Type"]),
      " | ",
      pad_string(r$details, col_widths["Details"]),
      " | ",
      pad_string(r$activity, col_widths["Activity"])
    )
    cli::cat_line(row_str)
  }
  cli::cat_line()

  # Generate HTML if requested
  if (HTML) {
    file_path <- attr(df, "file_path")
    html_file <- generate_html_summary(
      all_changes,
      unit,
      total_changes,
      n_additions,
      n_removals,
      n_replacements,
      n_swaps,
      n_paycode_changes,
      file_path
    )
    cli::cat_line(cli::col_green(sprintf("✓ HTML summary saved: %s", html_file)))
    cli::cat_line()

    # Open in RStudio viewer pane
    if (rlang::is_installed("rstudioapi") &&
        rstudioapi::isAvailable()) {
      rstudioapi::viewer(html_file)
    } else {
      # Fallback to browser if RStudio not available
      utils::browseURL(html_file)
    }
  }

  # Return the dataframe invisibly
  invisible(df)
}

# Helper function to generate HTML with table only
generate_html_summary <- function(all_changes,
                                  unit,
                                  total_changes,
                                  n_additions,
                                  n_removals,
                                  n_replacements,
                                  n_swaps,
                                  n_paycode_changes,
                                  file_path = NULL) {
  timestamp <- format(Sys.time(), "%Y-%m-%d-%H%M%S")
  output_dir <- if (!is.null(file_path) && file_path != "") dirname(file_path) else "logs/html"
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  
  filename <- sprintf("%s-changes-%s.html", unit, timestamp)
  filepath <- file.path(output_dir, filename)

  # Build table rows
  table_rows <- c("<tbody>")
  for (i in seq_len(nrow(all_changes))) {
    row <- all_changes[i, ]
    date_str <- format(row$date, "%Y-%m-%d")
    week_str <- sprintf("Wk%d", row$week)
    day_abbr <- substr(row$day, 1, 3)
    start_conv <- convert_time_12h(row$start)
    end_conv <- convert_time_12h(row$end)
    activity <- sprintf("%s - Lab %s %s %s-%s", week_str, row$location, day_abbr, start_conv, end_conv)
    details_html <- format_details_html(row$type, row$details)
    
    table_rows <- c(table_rows, sprintf(
      "<tr><td>%s</td><td>%s</td><td>%s</td><td>%s</td></tr>",
      date_str, row$type, details_html, activity
    ))
  }
  table_rows <- c(table_rows, "</tbody>")

  # Simple HTML with table only
  html <- c(
    "<!DOCTYPE html>",
    "<html><head><meta charset=\"UTF-8\">",
    "<style>",
    "table { border-collapse: collapse; width: 100%; }",
    "th, td { border: 1px solid #ddd; padding: 8px; text-align: left; }",
    "th { background: #333; color: #fff; }",
    "tr:nth-child(even) { background: #f9f9f9; }",
    ".text-success { color: #16a34a; font-weight: bold; }",
    ".text-danger { color: #ef4444; font-weight: bold; }",
    ".text-info { color: #3b82f6; font-weight: bold; }",
    ".strikethrough { text-decoration: line-through; }",
    "</style>",
    "</head><body>",
    "<table>",
    "<thead><tr><th>Date</th><th>Type</th><th>Details</th><th>Activity</th></tr></thead>",
    table_rows,
    "</table>",
    "</body></html>"
  )

  writeLines(html, filepath)
  filepath
}

# Helper function to format details text with HTML styling
format_details_html <- function(type, details_text) {
  # Strip ANSI codes first
  clean_text <- cli::ansi_strip(details_text)
  
  if (type == "Addition") {
    # Format: Name - Role
    return(sprintf("<span class=\"text-success\">%s</span>", clean_text))
  } else if (type == "Removal") {
    # Format: Name - Role
    return(sprintf("<span class=\"text-danger\">%s</span>", clean_text))
  } else if (type == "Replacement") {
    # Format: OldName → NewName - Role
    # Use regex to split on arrow and extract components
    arrow_pattern <- "^(.+?) → (.+?) - (.+)$"
    
    if (grepl(arrow_pattern, clean_text)) {
      old_name <- sub(arrow_pattern, "\\1", clean_text)
      new_name <- sub(arrow_pattern, "\\2", clean_text)
      role <- sub(arrow_pattern, "\\3", clean_text)
      
      return(sprintf(
        "<span class=\"strikethrough text-danger\">%s</span> → <span class=\"text-success\">%s</span> - %s",
        old_name, new_name, role
      ))
    }
    return(sprintf("<span class=\"text-warning\">%s</span>", clean_text))
  } else if (type == "Paycode change") {
    # Format: Name - OldRole → NewRole - all in blue with strikethrough on old role
    paycode_pattern <- "^(.+?) - (.+?) → (.+)$"
    
    if (grepl(paycode_pattern, clean_text)) {
      name <- sub(paycode_pattern, "\\1", clean_text)
      old_role <- sub(paycode_pattern, "\\2", clean_text)
      new_role <- sub(paycode_pattern, "\\3", clean_text)
      
      return(sprintf(
        "<span class=\"text-info\">%s - <span class=\"strikethrough\">%s</span> → %s</span>",
        name, old_role, new_role
      ))
    }
    return(sprintf("<span class=\"text-info\">%s</span>", clean_text))
  }
  
  return(clean_text)
}

# Utility: Convert 24h time to 12h format
convert_time_12h <- function(time_str) {
  hour <- as.integer(substr(time_str, 1, 2))
  if (hour == 0) "12am"
  else if (hour < 12) paste0(hour, "am")
  else if (hour == 12) "12pm"
  else paste0(hour - 12, "pm")
}

# Suppress R CMD check notes for non-standard evaluation variables
utils::globalVariables(
  c(
    "name",
    "role",
    "name_removed",
    "name_added",
    "role_prev",
    "role_latest",
    "role_added",
    "role_removed",
    "location",
    "week",
    "type",
    "details",
    "date",
    "day",
    "start",
    "end",
    "index",
    "n"
  )
)
