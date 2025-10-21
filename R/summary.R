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

  # Start HTML document
  html_parts <- c(
    "<!DOCTYPE html>",
    "<html>",
    "<head>",
    "<meta charset=\"UTF-8\">",
    "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">",
    "<title>Roster Changes Summary</title>",
    "<style>",
    "  body { font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif; margin: 0; padding: 16px; background-color: #f9fafb; }",
    "  .container { max-width: 900px; margin: 0 auto; background-color: white; border-radius: 6px; box-shadow: 0 1px 3px rgba(0,0,0,0.1); padding: 20px; }",
    "  h1 { margin: 0 0 8px 0; font-size: 20px; color: #1f2937; }",
    "  .summary-header { display: grid; grid-template-columns: repeat(auto-fit, minmax(150px, 1fr)); gap: 12px; margin-bottom: 20px; padding-bottom: 16px; border-bottom: 1px solid #e5e7eb; }",
    "  .summary-stat { }",
    "  .stat-label { font-size: 12px; color: #6b7280; font-weight: 600; text-transform: uppercase; }",
    "  .stat-value { font-size: 24px; font-weight: bold; color: #1f2937; margin-top: 4px; }",
    "  .stat-value.additions { color: #22c55e; }",
    "  .stat-value.removals { color: #ef4444; }",
    "  .stat-value.replacements { color: #f59e0b; }",
    "  .stat-value.paycode { color: #3b82f6; }",
    "  table { width: 100%; border-collapse: collapse; margin-top: 12px; }",
    "  th { text-align: left; font-size: 12px; font-weight: 600; color: #6b7280; text-transform: uppercase; border-bottom: 2px solid #e5e7eb; padding: 8px 12px; }",
    "  td { padding: 10px 12px; border-bottom: 1px solid #f3f4f6; font-size: 13px; }",
    "  tr:hover { background-color: #f9fafb; }",
    "  .type-badge { display: inline-block; padding: 4px 8px; border-radius: 4px; font-size: 11px; font-weight: 600; color: white; text-align: center; min-width: 100px; }",
    "  .type-addition { background-color: #22c55e; }",
    "  .type-removal { background-color: #ef4444; }",
    "  .type-replacement { background-color: #f59e0b; }",
    "  .type-paycode { background-color: #3b82f6; }",
    "  .details { font-weight: 500; }",
    "  .activity { color: #6b7280; font-size: 12px; }",
    "  .timestamp { font-size: 11px; color: #9ca3af; margin-top: 16px; padding-top: 12px; border-top: 1px solid #e5e7eb; }",
    "</style>",
    "</head>",
    "<body>",
    "<div class=\"container\">",
    sprintf("<h1>Roster Changes Summary - %s</h1>", unit),
    "<div class=\"summary-header\">",
    sprintf(
      "<div class=\"summary-stat\"><div class=\"stat-label\">Total Changes</div><div class=\"stat-value\">%d</div></div>",
      total_changes
    ),
    sprintf(
      "<div class=\"summary-stat\"><div class=\"stat-label\">Additions</div><div class=\"stat-value additions\">%d</div></div>",
      n_additions
    ),
    sprintf(
      "<div class=\"summary-stat\"><div class=\"stat-label\">Removals</div><div class=\"stat-value removals\">%d</div></div>",
      n_removals
    ),
    sprintf(
      "<div class=\"summary-stat\"><div class=\"stat-label\">Replacements</div><div class=\"stat-value replacements\">%d</div></div>",
      n_replacements
    ),
    sprintf(
      "<div class=\"summary-stat\"><div class=\"stat-label\">Paycode Changes</div><div class=\"stat-value paycode\">%d</div></div>",
      n_paycode_changes
    ),
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

    # Format details based on type
    type_lower <- tolower(gsub(" ", "-", row$type))
    type_badge_class <- paste0("type-", type_lower)

    # Extract details - strip ANSI codes if present
    details <- cli::ansi_strip(row$details)

    html_row <- sprintf(
      "<tr><td>%s</td><td><span class=\"type-badge %s\">%s</span></td><td class=\"details\">%s</td><td class=\"activity\">%s</td></tr>",
      date_str,
      type_badge_class,
      row$type,
      details,
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
      "<div class=\"timestamp\">Generated: %s | Unit: %s</div>",
      format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      unit
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
