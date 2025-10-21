#' Compare Roster Changes Between Latest Log Files
#'
#' Detects additions, removals, and replacements between the two latest
#' roster log files for a given unit. This function reads the previous and
#' latest snapshots and compares them to identify what has changed.
#'
#' @param df A dataframe with roster data, typically returned by \code{\link{roster}()}.
#'   Must have attributes \code{unit} and \code{file_path} set.
#'
#' @return A list containing:
#'   \item{changes}{A list with elements:
#'     \itemize{
#'       \item{additions}{Rows in latest log not in previous log (new assignments)}
#'       \item{removals}{Rows in previous log not in latest log (removed assignments)}
#'       \item{replacements}{Slots where a person was replaced by another}
#'     }
#'   }
#'   \item{df}{The input dataframe}
#'
#' @details
#' Both the latest and previous log files are processed with 
#' \code{\link{mutate_tutor_roles}()} before comparison to ensure consistent
#' tutor role labeling.
#'
#' Replacements are detected by matching on common columns (date, day, start, 
#' end, location, role, week) where different names appear between versions.
#' These are then removed from the additions and removals lists.
#'
#' @examples
#' \dontrun{
#' # Read and compare rosters
#' df <- roster("path/to/roster.xlsx", unit = "biol1007")
#' snapshot(df)  # Save current state
#' # ... later, after roster is updated ...
#' df_new <- roster("path/to/roster.xlsx", unit = "biol1007")
#' snapshot(df_new)  # Save new state
#' result <- compare(df_new)
#' }
#'
#' @importFrom dplyr inner_join anti_join select
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
  
  # Find common columns between the two versions for comparison
  # This handles cases where one version has paycode and the other doesn't
  common_join_cols <- intersect(names(latest_mutated), names(previous_mutated))
  
  # Detect additions: rows in latest but not in previous
  additions <- dplyr::anti_join(latest_mutated, previous_mutated, by = common_join_cols)
  
  # Detect removals: rows in previous but not in latest
  removals <- dplyr::anti_join(previous_mutated, latest_mutated, by = common_join_cols)
  
  # Detect replacements: slots where a person was removed and another added
  # Use only the subset of columns that exclude paycode (if present) for replacement detection
  common_cols <- c("date", "day", "start", "end", "location", "role", "week")
  replacements <- dplyr::inner_join(removals, additions, by = common_cols, suffix = c("_removed", "_added"))
  
  # Remove replacements from additions and removals to get pure additions/removals
  if (nrow(replacements) > 0) {
    # Get the common columns between additions/removals (excluding paycode if present)
    add_remove_join_cols <- intersect(names(additions), common_cols)
    
    replacements_add <- dplyr::select(replacements, all_of(common_cols), name = name_added)
    replacements_rem <- dplyr::select(replacements, all_of(common_cols), name = name_removed)
    
    # Use only common columns for anti_join to handle paycode differences
    additions <- dplyr::anti_join(additions, replacements_add, by = add_remove_join_cols)
    removals <- dplyr::anti_join(removals, replacements_rem, by = add_remove_join_cols)
  }
  
  # Detect paycode changes: same person, same slot, but different role/paycode
  # This happens when staff change their tutorial position (e.g., drop first tutorial → becomes Tutor instead of Tutor (repeat))
  paycode_changes <- NULL
  if ("paycode" %in% names(latest_mutated) && "paycode" %in% names(previous_mutated)) {
    # Find rows with same date, day, start, end, location, name, week but different role
    slot_cols <- c("date", "day", "start", "end", "location", "name", "week")
    slot_matches <- dplyr::inner_join(
      previous_mutated %>% dplyr::select(all_of(slot_cols), role_prev = role, paycode_prev = paycode),
      latest_mutated %>% dplyr::select(all_of(slot_cols), role_latest = role, paycode_latest = paycode),
      by = slot_cols
    )
    
    # Filter to only rows where role/paycode actually changed
    if (nrow(slot_matches) > 0) {
      paycode_changes <- slot_matches %>%
        dplyr::filter(role_prev != role_latest) %>%
        dplyr::select(all_of(slot_cols), role_prev, role_latest, paycode_prev, paycode_latest)
      
      if (nrow(paycode_changes) == 0) {
        paycode_changes <- NULL
      } else {
        # Remove paycode changes from additions and removals since they're already tracked
        # Only use common columns for the anti_join to handle paycode column differences
        slot_cols_with_role <- c("date", "day", "start", "end", "location", "name", "week", "role")
        
        removals_to_remove <- paycode_changes %>%
          dplyr::select(all_of(slot_cols)) %>%
          dplyr::mutate(role = paycode_changes$role_prev)
        
        additions_to_remove <- paycode_changes %>%
          dplyr::select(all_of(slot_cols)) %>%
          dplyr::mutate(role = paycode_changes$role_latest)
        
        removals <- dplyr::anti_join(removals, removals_to_remove, by = slot_cols_with_role)
        additions <- dplyr::anti_join(additions, additions_to_remove, by = slot_cols_with_role)
      }
    }
  }
  
  # Return the changes and df
  changes <- list(
    additions = additions,
    removals = removals,
    replacements = replacements,
    paycode_changes = paycode_changes
  )
  
  return(list(changes = changes, df = df))
}

# Suppress R CMD check notes for non-standard evaluation variables
utils::globalVariables(c("name_added", "name_removed", "role", "paycode", "role_prev", "role_latest", "paycode_prev", "paycode_latest"))
