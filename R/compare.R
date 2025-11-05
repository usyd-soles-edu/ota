#' Compare Roster Changes Between Latest Log Files
#'
#' Detects additions, removals, swaps, and replacements between the two latest
#' roster log files for a given unit. This function reads the previous and
#' latest snapshots and compares them to identify what has changed.
#'
#' The `index` column (e.g., "Sup-1_row5") tracks the original cell position
#' before the pivot, allowing easy detection of what happened at each position.
#'
#' @param df A dataframe with roster data, typically returned by \code{\link{roster}()}.
#'   Must have attributes \code{unit} and \code{file_path} set.
#'
#' @return A list containing:
#'   \item{changes}{A list with elements:
#'     \itemize{
#'       \item{additions}{New staff in positions that were previously empty (name was NA)}
#'       \item{removals}{Staff removed (names become NA) or positions left empty}
#'       \item{replacements}{Same position (index) but different person}
#'       \item{swaps}{Two staff members swap positions in the same lab/time slot}
#'       \item{paycode_changes}{Same person, same position, but different role/paycode}
#'     }
#'   }
#'   \item{df}{The input dataframe}
#'
#' @details
#' Change detection is now simplified by leveraging the `index` column which
#' uniquely identifies each roster cell position. Both the latest and previous
#' log files are processed with \code{\link{mutate_tutor_roles}()} before
#' comparison to ensure consistent tutor role labeling.
#'
#' This function is typically followed by \code{\link{summary}()} to display
#' the detected changes in a formatted table or HTML report.
#'
#' @examples
#' \dontrun{
#' # Read, compare, and summarize rosters
#' df <- roster("path/to/roster.xlsx", unit = "biol1007")
#' snapshot(df)  # Save current state
#' # ... later, after roster is updated ...
#' df_new <- roster("path/to/roster.xlsx", unit = "biol1007")
#' snapshot(df_new)  # Save new state
#' result <- compare(df_new)
#' summary(result, HTML = TRUE)  # Display changes and generate HTML report
#' }
#'
#' @importFrom dplyr left_join anti_join select mutate filter
#' @keywords internal
#' @export
#' @importFrom rlang .data
compare <- function(df) {
  # Extract unit and logs directory from the dataframe attributes
  unit <- attr(df, "unit")
  file_path <- attr(df, "file_path")
  logs_dir <- file.path(dirname(file_path), "logs")

  # Get the two latest log files
  latest_pair <- .latest_pair(unit, logs_dir)

  if (is.null(latest_pair)) {
    message("No log files exist for comparison.")
    return(list(changes = NULL, df = df))
  }

  if (is.null(latest_pair$previous)) {
    message("Only one log file exists, nothing to compare yet.")
    return(list(changes = NULL, df = df))
  }

  # Apply tutor role mutation to both versions for consistent comparison
  latest_mutated <- mutate_tutor_roles(latest_pair$latest)
  previous_mutated <- mutate_tutor_roles(latest_pair$previous)

  # Check if index column exists; if not, fall back to old method
  if (
    !("index" %in% names(latest_mutated)) ||
      !("index" %in% names(previous_mutated))
  ) {
    warning("Index column not found. Using fallback comparison method.")
    return(.compare_legacy(df, latest_mutated, previous_mutated))
  }

  # New simplified comparison using index column
  # Join previous and latest on index to see what changed at each position
  comparison <- dplyr::left_join(
    previous_mutated %>%
      dplyr::rename(name_prev = name, role_prev = role) %>%
      dplyr::mutate(
        name_prev = dplyr::if_else(name_prev == ".", NA_character_, name_prev)
      ),
    latest_mutated %>%
      dplyr::rename(name_latest = name, role_latest = role) %>%
      dplyr::mutate(
        name_latest = dplyr::if_else(
          name_latest == ".",
          NA_character_,
          name_latest
        )
      ),
    by = c("index", "date", "day", "start", "end", "location", "week")
  )

  # Identify changes by comparing names at each index
  # Treat "." placeholder as NA (empty slot)
  additions <- comparison %>%
    dplyr::filter(is.na(name_prev) & !is.na(name_latest)) %>%
    dplyr::select(
      date,
      day,
      start,
      end,
      location,
      name = name_latest,
      role = role_latest,
      week,
      index
    )

  removals <- comparison %>%
    dplyr::filter(!is.na(name_prev) & is.na(name_latest)) %>%
    dplyr::select(
      date,
      day,
      start,
      end,
      location,
      name = name_prev,
      role = role_prev,
      week,
      index
    )

  # Replacements: same position but different person (excluding placeholder dot)
  replacements <- comparison %>%
    dplyr::filter(
      !is.na(name_prev) & !is.na(name_latest) & name_prev != name_latest
    ) %>%
    dplyr::select(
      date,
      day,
      start,
      end,
      location,
      name_removed = name_prev,
      role_removed = role_prev,
      name_added = name_latest,
      role_added = role_latest,
      week,
      index
    )

  # Detect swaps: two people exchange positions (indices) in the same lab/session
  # A swap occurs when:
  # - At index X: person A was at index X, now person B is at index X
  # - At index Y: person B was at index Y, now person A is at index Y
  # (regardless of role changes)
  swaps <- NULL
  replaced_indices <- c() # Track indices involved in swaps for removal from replacements

  if (nrow(replacements) > 0) {
    # Self-join to find pairs of replacements where names and indices are swapped
    n_repl <- nrow(replacements)
    for (i in 1:(n_repl - 1)) {
      for (j in (i + 1):n_repl) {
        r1 <- replacements[i, ]
        r2 <- replacements[j, ]

        # Check if this is a swap:
        # Person A was at index X, now person B is there (r1)
        # Person B was at index Y, now person A is there (r2)
        if (
          r1$name_removed == r2$name_added &&
            r2$name_removed == r1$name_added &&
            r1$date == r2$date &&
            r1$day == r2$day &&
            r1$start == r2$start &&
            r1$end == r2$end &&
            r1$location == r2$location &&
            r1$week == r2$week
        ) {
          # This is a swap!
          # Create a record that captures the swap correctly:
          # r1$name_removed (Anita) had r1$role_removed (Demonstrator)
          # r2$name_removed (Shahnoosh) had r2$role_removed (Tutor)
          # After swap:
          # r1$name_removed (Anita) has r2$role_removed (Tutor)
          # r2$name_removed (Shahnoosh) has r1$role_removed (Demonstrator)
          swap_record <- r1
          swap_record$role_added <- r2$role_removed # Anita's new role is Shahnoosh's old role

          if (is.null(swaps)) {
            swaps <- swap_record
          } else {
            swaps <- dplyr::bind_rows(swaps, swap_record)
          }
          replaced_indices <- c(replaced_indices, i, j)
        }
      }
    }

    # Remove swapped replacements from the replacements list (in reverse order to avoid index shifting)
    if (length(replaced_indices) > 0) {
      replaced_indices <- sort(unique(replaced_indices), decreasing = TRUE)
      for (idx in replaced_indices) {
        replacements <- replacements[-idx, ]
      }
    }
  }

  # Detect paycode changes: same position, same person, but different role
  paycode_changes <- comparison %>%
    dplyr::filter(
      !is.na(name_prev) &
        !is.na(name_latest) &
        name_prev == name_latest &
        role_prev != role_latest
    ) %>%
    dplyr::select(
      date,
      day,
      start,
      end,
      location,
      name = name_prev,
      week,
      index,
      role_prev,
      role_latest
    )

  if (nrow(paycode_changes) == 0) {
    paycode_changes <- NULL
  }

  # Return the changes and df
  changes <- list(
    additions = additions,
    removals = removals,
    replacements = replacements,
    swaps = swaps,
    paycode_changes = paycode_changes
  )

  result <- list(changes = changes, df = df)
  class(result) <- c("document_changes", "list")

  # Preserve the snapshot_created attribute if it exists
  if (!is.null(attr(df, "snapshot_created"))) {
    attr(result, "snapshot_created") <- attr(df, "snapshot_created")
  }

  return(result)
}

# Legacy comparison method for backwards compatibility
.compare_legacy <- function(df, latest_mutated, previous_mutated) {
  # Find common columns between the two versions
  common_join_cols <- intersect(names(latest_mutated), names(previous_mutated))

  # Detect additions: rows in latest but not in previous
  additions <- dplyr::anti_join(
    latest_mutated,
    previous_mutated,
    by = common_join_cols
  )

  # Detect removals: rows in previous but not in latest
  removals <- dplyr::anti_join(
    previous_mutated,
    latest_mutated,
    by = common_join_cols
  )

  # Detect replacements: slots where a person was removed and another added
  common_cols <- c("date", "day", "start", "end", "location", "role", "week")
  replacements <- dplyr::inner_join(
    removals,
    additions,
    by = common_cols,
    suffix = c("_removed", "_added")
  )

  # Remove replacements from additions and removals
  if (nrow(replacements) > 0) {
    add_remove_join_cols <- intersect(names(additions), common_cols)
    replacements_add <- dplyr::select(
      replacements,
      all_of(common_cols),
      name = name_added
    )
    replacements_rem <- dplyr::select(
      replacements,
      all_of(common_cols),
      name = name_removed
    )
    additions <- dplyr::anti_join(
      additions,
      replacements_add,
      by = add_remove_join_cols
    )
    removals <- dplyr::anti_join(
      removals,
      replacements_rem,
      by = add_remove_join_cols
    )
  }

  changes <- list(
    additions = additions,
    removals = removals,
    replacements = replacements,
    swaps = NULL,
    paycode_changes = NULL
  )

  result <- list(changes = changes, df = df)
  class(result) <- c("document_changes", "list")
  return(result)
}

# Suppress R CMD check notes for non-standard evaluation variables
utils::globalVariables(c(
  "name_added",
  "name_removed",
  "role",
  "paycode",
  "role_prev",
  "role_latest",
  "paycode_prev",
  "paycode_latest",
  "name_prev",
  "name_latest",
  "index",
  "n"
))
