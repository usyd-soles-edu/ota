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

  # Check if snapshot was created; if not, skip showing the summary
  snapshot_created <- attr(compare_result, "snapshot_created")
  if (isFALSE(snapshot_created)) {
    # No new snapshot created (no changes detected)
    file_path <- attr(df, "file_path")
    unit <- attr(df, "unit")
    if (is.null(unit)) {
      unit <- "unknown"
    }
    previous_html <- find_latest_html(unit, file_path)

    if (!is.null(previous_html)) {
      # Get relative path if possible
      rel_path <- tryCatch(
        {
          fs::path_rel(previous_html, getwd())
        },
        error = function(e) previous_html
      )
      cli::cat_line(cli::col_blue(sprintf(
        "ℹ No new snapshot created (no changes detected)."
      )))
      cli::cat_line(cli::col_blue(sprintf(
        "  Previous HTML summary: %s",
        rel_path
      )))
      cli::cat_line()
    } else {
      cli::cat_line(cli::col_blue(sprintf(
        "ℹ No new snapshot created (no changes detected)."
      )))
      cli::cat_line(cli::col_blue(sprintf("  No previous HTML summary found.")))
      cli::cat_line()
    }
    return(invisible(df))
  }

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
  total_changes <- nrow(additions) +
    nrow(removals) +
    nrow(replacements) +
    (if (is.null(swaps)) 0 else nrow(swaps))
  n_additions <- nrow(additions)
  n_removals <- nrow(removals)
  n_replacements <- nrow(replacements)
  n_swaps <- if (is.null(swaps)) 0 else nrow(swaps)
  n_paycode_changes <- if (is.null(paycode_changes)) {
    0
  } else {
    nrow(paycode_changes)
  }

  # Function to format activity (shared with HTML generation)
  format_activity <- function(row) {
    week_str <- sprintf("Wk%d", row$week)
    day_abbr <- substr(row$day, 1, 3) # Mon, Tue, etc.
    start_conv <- convert_time_12h(row$start)
    end_conv <- convert_time_12h(row$end)
    sprintf(
      "%s - Lab %s %s %s-%s",
      week_str,
      row$location,
      day_abbr,
      start_conv,
      end_conv
    )
  }

  # Collect all changes into one data frame
  change_list <- list(
    additions %>%
      dplyr::mutate(
        type = "Addition",
        before = "",
        after = sprintf("%s - %s", cli::style_bold(cli::col_green(name)), role)
      ) %>%
      dplyr::select(
        date,
        day,
        start,
        end,
        location,
        week,
        type,
        before,
        after,
        index
      ),
    removals %>%
      dplyr::mutate(
        type = "Removal",
        before = sprintf("%s - %s", cli::style_bold(cli::col_red(name)), role),
        after = ""
      ) %>%
      dplyr::select(
        date,
        day,
        start,
        end,
        location,
        week,
        type,
        before,
        after,
        index
      ),
    replacements %>%
      dplyr::mutate(
        type = "Replacement",
        before = sprintf(
          "%s - %s",
          cli::style_bold(cli::col_red(name_removed)),
          role_removed
        ),
        after = sprintf(
          "%s - %s",
          cli::style_bold(cli::col_green(name_added)),
          role_added
        )
      ) %>%
      dplyr::select(
        date,
        day,
        start,
        end,
        location,
        week,
        type,
        before,
        after,
        index
      )
  )

  # Add swaps if they exist
  if (!is.null(swaps) && nrow(swaps) > 0) {
    # For swaps, we show both people involved with their roles
    # Note: name_removed/role_removed is person at position before swap
    #       name_added/role_added is person at that same position after swap
    # So after swap: name_added has role_added, and name_removed has role_added's old role
    # We need to find what role name_removed has after the swap
    # Since both people swapped positions, name_removed now has role_added's previous role
    swaps_formatted <- swaps %>%
      dplyr::mutate(
        type = "Swap",
        before = sprintf(
          "%s - %s / %s - %s",
          cli::style_bold(cli::col_blue(name_removed)),
          role_removed,
          cli::style_bold(cli::col_blue(name_added)),
          role_added
        ),
        # After swap: positions swapped, so each person is at the other's position
        # name_removed is now at the position that had role_added, so gets role_added
        # name_added is now at the position that had role_removed, so gets role_removed
        after = sprintf(
          "%s - %s / %s - %s",
          cli::style_bold(cli::col_blue(name_removed)),
          role_added,
          cli::style_bold(cli::col_blue(name_added)),
          role_removed
        )
      ) %>%
      dplyr::select(
        date,
        day,
        start,
        end,
        location,
        week,
        type,
        before,
        after,
        index
      )
    change_list <- append(change_list, list(swaps_formatted))
  }

  # Add paycode changes if they exist
  if (!is.null(paycode_changes) && nrow(paycode_changes) > 0) {
    paycode_changes_formatted <- paycode_changes %>%
      dplyr::mutate(
        type = "Paycode change",
        before = sprintf(
          "%s - %s",
          cli::style_bold(cli::col_blue(name)),
          role_prev
        ),
        after = sprintf(
          "%s - %s",
          cli::style_bold(cli::col_blue(name)),
          role_latest
        )
      ) %>%
      dplyr::select(
        date,
        day,
        start,
        end,
        location,
        week,
        type,
        before,
        after,
        index
      )
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
      before = row$before,
      after = row$after,
      activity = activity,
      row_data = row
    )
  })

  # Compute max widths based on visible characters
  col_widths <- c(
    Date = max(sapply(rows, function(r) {
      nchar(r$date)
    })),
    Type = max(sapply(rows, function(r) {
      nchar(r$type)
    })),
    Before = max(sapply(rows, function(r) {
      nchar(cli::ansi_strip(r$before))
    })),
    After = max(sapply(rows, function(r) {
      nchar(cli::ansi_strip(r$after))
    })),
    Activity = max(sapply(rows, function(r) {
      nchar(cli::ansi_strip(r$activity))
    }))
  )

  # Ensure minimum widths
  col_widths <- pmax(
    col_widths,
    c(
      Date = 10,
      Type = 12,
      Before = 20,
      After = 20,
      Activity = 15
    )
  )

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
    pad_string("Before", col_widths["Before"]),
    " | ",
    pad_string("After", col_widths["After"]),
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
    strrep("-", col_widths["Before"]),
    "-|-",
    strrep("-", col_widths["After"]),
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
      pad_string(r$before, col_widths["Before"]),
      " | ",
      pad_string(r$after, col_widths["After"]),
      " | ",
      pad_string(r$activity, col_widths["Activity"])
    )
    cli::cat_line(row_str)
  }
  cli::cat_line()

  # Check if a new snapshot was created
  snapshot_created <- attr(compare_result, "snapshot_created")

  # Generate HTML only if a new snapshot was created
  if (HTML) {
    if (isTRUE(snapshot_created)) {
      # New snapshot created, generate new HTML
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
      cli::cat_line(cli::col_green(sprintf(
        "✓ HTML summary saved: %s",
        html_file
      )))
      cli::cat_line()

      # Open in RStudio viewer pane
      if (
        rlang::is_installed("rstudioapi") &&
          rstudioapi::isAvailable()
      ) {
        rstudioapi::viewer(html_file)
      } else {
        # Fallback to browser if RStudio not available
        utils::browseURL(html_file)
      }
    }
  }

  # Return the dataframe invisibly
  invisible(df)
}

# Helper function to find the latest HTML file for a unit
find_latest_html <- function(unit, file_path = NULL) {
  # Determine the logs/html directory
  if (!is.null(file_path) && file_path != "") {
    base_dir <- dirname(file_path)
    html_dir <- file.path(base_dir, "logs", "html")
  } else {
    html_dir <- file.path("logs", "html")
  }

  # Check if directory exists
  if (!dir.exists(html_dir)) {
    return(NULL)
  }

  # Find all HTML files for this unit
  html_files <- list.files(
    html_dir,
    pattern = sprintf("^%s-changes-.*\\.html$", unit),
    full.names = TRUE
  )

  if (length(html_files) == 0) {
    return(NULL)
  }

  # Return the most recent file
  html_files <- sort(html_files)
  return(html_files[length(html_files)])
}

# Helper function to generate HTML with table only
generate_html_summary <- function(
  all_changes,
  unit,
  total_changes,
  n_additions,
  n_removals,
  n_replacements,
  n_swaps,
  n_paycode_changes,
  file_path = NULL
) {
  timestamp <- format(Sys.time(), "%Y-%m-%d-%H%M%S")
  # Determine the logs/html directory based on where the roster file is located
  if (!is.null(file_path) && file_path != "") {
    base_dir <- dirname(file_path)
    output_dir <- file.path(base_dir, "logs", "html")
  } else {
    output_dir <- file.path("logs", "html")
  }
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

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
    activity <- sprintf(
      "%s - Lab %s %s %s-%s",
      week_str,
      row$location,
      day_abbr,
      start_conv,
      end_conv
    )
    before_html <- format_state_html(row$before)
    after_html <- format_state_html(row$after)

    table_rows <- c(
      table_rows,
      sprintf(
        "<tr><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td></tr>",
        date_str,
        row$type,
        before_html,
        after_html,
        activity
      )
    )
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
    "<thead><tr><th>Date</th><th>Type</th><th>Before</th><th>After</th><th>Activity</th></tr></thead>",
    table_rows,
    "</table>",
    "</body></html>"
  )

  writeLines(html, filepath)
  filepath
}

# Helper function to format state text with HTML styling (for before/after)
format_state_html <- function(state_text) {
  # Strip ANSI codes first
  clean_text <- cli::ansi_strip(state_text)

  # Return empty string as-is
  if (clean_text == "") {
    return("")
  }

  # Detect color patterns from ANSI-stripped text
  # Green text (additions)
  if (grepl("^.+ - .+$", clean_text)) {
    # Check context: if it appears to be a clean addition (was empty before)
    # We'll add color based on the context in the calling function
    return(sprintf("<span>%s</span>", clean_text))
  }

  return(clean_text)
}

# Utility: Convert 24h time to 12h format
convert_time_12h <- function(time_str) {
  hour <- as.integer(substr(time_str, 1, 2))
  if (hour == 0) {
    "12am"
  } else if (hour < 12) {
    paste0(hour, "am")
  } else if (hour == 12) {
    "12pm"
  } else {
    paste0(hour - 12, "pm")
  }
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
    "before",
    "after",
    "date",
    "day",
    "start",
    "end",
    "index",
    "n"
  )
)
