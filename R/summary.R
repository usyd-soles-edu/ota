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
  paycode_changes <- changes$paycode_changes

  # Calculate totals
  total_changes <- nrow(additions) + nrow(removals) + nrow(replacements)
  n_additions <- nrow(additions)
  n_removals <- nrow(removals)
  n_replacements <- nrow(replacements)
  n_paycode_changes <- if (is.null(paycode_changes))
    0
  else
    nrow(paycode_changes)

  # Function to format activity
  format_activity <- function(row) {
    week_str <- sprintf("Wk%d", row$week)
    day_abbr <- substr(row$day, 1, 3)  # Mon, Tue, etc.

    # Convert times to 12-hour format
    convert_time <- function(time_str) {
      hour <- as.integer(substr(time_str, 1, 2))
      if (hour == 0)
        "12am"
      else if (hour < 12)
        paste0(hour, "am")
      else if (hour == 12)
        "12pm"
      else
        paste0(hour - 12, "pm")
    }

    start_conv <- convert_time(row$start)
    end_conv <- convert_time(row$end)

    sprintf("%s - Lab %s %s %s-%s",
            week_str,
            row$location,
            day_abbr,
            start_conv,
            end_conv)
  }

  # Collect all changes into one data frame
  change_list <- list(
    additions %>% dplyr::mutate(
      type = "Addition",
      details = sprintf("%s (%s)", cli::style_bold(cli::col_green(name)), role)
    ),
    removals %>% dplyr::mutate(
      type = "Removal",
      details = sprintf("%s (%s)", cli::style_bold(cli::col_red(name)), role)
    ),
    replacements %>% dplyr::mutate(
      type = "Replacement",
      details = sprintf(
        "%s → %s (%s)",
        cli::style_strikethrough(cli::col_red(name_removed)),
        cli::style_bold(cli::col_green(name_added)),
        role
      )
    )
  )

  # Add paycode changes if they exist
  if (!is.null(paycode_changes) && nrow(paycode_changes) > 0) {
    paycode_changes_formatted <- paycode_changes %>%
      dplyr::mutate(
        type = "Paycode change",
        details = sprintf(
          "%s (%s → %s)",
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
                    week,
                    type,
                    details,
                    role)
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
      "Total changes: %2d  | Additions: %2d  | Removals: %2d  | Replacements: %2d  | Paycode changes: %2d",
      total_changes,
      n_additions,
      n_removals,
      n_replacements,
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

# Helper function to generate HTML
generate_html_summary <- function(all_changes,
                                  unit,
                                  total_changes,
                                  n_additions,
                                  n_removals,
                                  n_replacements,
                                  n_paycode_changes,
                                  file_path = NULL) {
  # Create timestamp
  timestamp <- format(Sys.time(), "%Y-%m-%d-%H%M%S")

  # Determine output directory
  if (!is.null(file_path) && file_path != "") {
    # Use same directory as source file
    output_dir <- dirname(file_path)
  } else {
    # Fallback to logs/html
    output_dir <- "logs/html"
  }
  
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  filename <- sprintf("%s-changes-%s.html", unit, timestamp)
  filepath <- file.path(output_dir, filename)

  # Start HTML document with basic styling
  html_parts <- c(
    "<!DOCTYPE html>",
    "<html>",
    "<head>",
    "<meta charset=\"UTF-8\">",
    "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">",
    "<title>Roster Changes Summary</title>",
    "<style>",
    "  body { font-family: Arial, sans-serif; margin: 0; padding: 16px; background-color: #ffffff; }",
    "  .container { max-width: 900px; margin: 0 auto; }",
    "  h1 { font-size: 22px; color: #333333; margin: 0 0 16px 0; }",
    "  .summary-stats { margin-bottom: 24px; }",
    "  .stats-row { display: flex; gap: 24px; flex-wrap: wrap; }",
    "  .stat-item { min-width: 120px; }",
    "  .stat-label { font-size: 12px; color: #666666; text-transform: uppercase; font-weight: bold; }",
    "  .stat-value { font-size: 24px; font-weight: bold; color: #333333; margin-top: 4px; }",
    "  .stat-additions { color: #16a34a; }",
    "  .stat-removals { color: #ef4444; }",
    "  .stat-replacements { color: #f59e0b; }",
    "  .stat-paycode { color: #3b82f6; }",
    "  table { width: 100%; border-collapse: collapse; margin-top: 0; }",
    "  th { text-align: left; font-size: 12px; font-weight: bold; color: #ffffff; background-color: #333333; padding: 10px; border: 1px solid #333333; }",
    "  td { padding: 10px; border: 1px solid #dddddd; font-size: 13px; }",
    "  tr:nth-child(even) { background-color: #f9f9f9; }",
    "  .text-success { color: #16a34a; font-weight: bold; }",
    "  .text-danger { color: #ef4444; font-weight: bold; }",
    "  .text-warning { color: #f59e0b; font-weight: bold; }",
    "  .text-info { color: #3b82f6; font-weight: bold; }",
    "  .strikethrough { text-decoration: line-through; }",
    "  .footer { font-size: 11px; color: #999999; margin-top: 20px; padding-top: 12px; border-top: 1px solid #dddddd; }",
    "</style>",
    "</head>",
    "<body>",
    "<div class=\"container\">",
    sprintf("<h1>Roster Changes Summary - %s</h1>", toupper(unit)),
    "<div class=\"summary-stats\">",
    "<div class=\"stats-row\">"
  )

  # Add summary statistics
  html_parts <- c(
    html_parts,
    sprintf("<div class=\"stat-item\"><div class=\"stat-label\">Total Changes</div><div class=\"stat-value\">%d</div></div>", total_changes),
    sprintf("<div class=\"stat-item\"><div class=\"stat-label\">Additions</div><div class=\"stat-value stat-additions\">%d</div></div>", n_additions),
    sprintf("<div class=\"stat-item\"><div class=\"stat-label\">Removals</div><div class=\"stat-value stat-removals\">%d</div></div>", n_removals),
    sprintf("<div class=\"stat-item\"><div class=\"stat-label\">Replacements</div><div class=\"stat-value stat-replacements\">%d</div></div>", n_replacements),
    sprintf("<div class=\"stat-item\"><div class=\"stat-label\">Paycode Changes</div><div class=\"stat-value stat-paycode\">%d</div></div>", n_paycode_changes),
    "</div>",
    "</div>",
    "<table>",
    "<thead><tr><th>Date</th><th>Type</th><th>Details</th><th>Activity</th></tr></thead>",
    "<tbody>"
  )

  # Add table rows
  for (i in seq_len(nrow(all_changes))) {
    row <- all_changes[i, ]
    date_str <- format(row$date, "%Y-%m-%d")

    # Format activity
    week_str <- sprintf("Wk%d", row$week)
    day_abbr <- substr(row$day, 1, 3)
    convert_time <- function(time_str) {
      hour <- as.integer(substr(time_str, 1, 2))
      if (hour == 0)
        "12am"
      else if (hour < 12)
        paste0(hour, "am")
      else if (hour == 12)
        "12pm"
      else
        paste0(hour - 12, "pm")
    }
    start_conv <- convert_time(row$start)
    end_conv <- convert_time(row$end)
    activity <- sprintf("%s - Lab %s %s %s-%s",
                        week_str,
                        row$location,
                        day_abbr,
                        start_conv,
                        end_conv)

    # Determine type badge class
    
    # Format details with HTML styling
    details_html <- format_details_html(row$type, row$details)

    html_row <- sprintf(
      "<tr><td>%s</td><td>%s</td><td>%s</td><td>%s</td></tr>",
      date_str,
      row$type,
      details_html,
      activity
    )

    html_parts <- c(html_parts, html_row)
  }

  # Close HTML document
  html_parts <- c(
    html_parts,
    "</tbody>",
    "</table>",
    sprintf(
      "<div class=\"footer\">Generated: %s | Unit: %s</div>",
      format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      toupper(unit)
    ),
    "</div>",
    "</body>",
    "</html>"
  )

  # Write HTML file
  writeLines(html_parts, filepath)

  # Return full file path
  return(filepath)
}

# Helper function to format details text with HTML styling
format_details_html <- function(type, details_text) {
  # Strip ANSI codes first
  clean_text <- cli::ansi_strip(details_text)
  
  if (type == "Addition") {
    # Format: Name (Role) - green and bold
    return(sprintf("<span class=\"text-success\">%s</span>", clean_text))
  } else if (type == "Removal") {
    # Format: Name (Role) - red and bold
    return(sprintf("<span class=\"text-danger\">%s</span>", clean_text))
  } else if (type == "Replacement") {
    # Format: OldName → NewName (Role)
    # Use regex to split on arrow and extract components
    arrow_pattern <- "^(.+?) → (.+?) \\((.+)\\)$"
    
    if (grepl(arrow_pattern, clean_text)) {
      old_name <- sub(arrow_pattern, "\\1", clean_text)
      new_name <- sub(arrow_pattern, "\\2", clean_text)
      role <- sub(arrow_pattern, "\\3", clean_text)
      
      return(sprintf(
        "<span class=\"strikethrough text-danger\">%s</span> → <span class=\"text-success\">%s</span> <span style=\"color: #666666;\">(%s)</span>",
        old_name, new_name, role
      ))
    }
    return(sprintf("<span class=\"text-warning\">%s</span>", clean_text))
  } else if (type == "Paycode change") {
    # Format: Name (Role1 → Role2) - colored
    return(sprintf("<span class=\"text-info\">%s</span>", clean_text))
  }
  
  return(clean_text)
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
    "location",
    "week",
    "type",
    "details",
    "date",
    "day",
    "start",
    "end"
  )
)
