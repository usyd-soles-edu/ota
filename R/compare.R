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
  
  # Detect additions: rows in latest but not in previous
  additions <- dplyr::anti_join(latest_mutated, previous_mutated, by = names(latest_mutated))
  
  # Detect removals: rows in previous but not in latest
  removals <- dplyr::anti_join(previous_mutated, latest_mutated, by = names(previous_mutated))
  
  # Detect replacements: slots where a person was removed and another added
  common_cols <- c("date", "day", "start", "end", "location", "role", "week")
  replacements <- dplyr::inner_join(removals, additions, by = common_cols, suffix = c("_removed", "_added"))
  
  # Remove replacements from additions and removals to get pure additions/removals
  if (nrow(replacements) > 0) {
    replacements_add <- dplyr::select(replacements, all_of(common_cols), name = name_added)
    replacements_rem <- dplyr::select(replacements, all_of(common_cols), name = name_removed)
    
    additions <- dplyr::anti_join(additions, replacements_add, by = names(additions))
    removals <- dplyr::anti_join(removals, replacements_rem, by = names(removals))
  }
  
  # Return the changes and df
  changes <- list(
    additions = additions,
    removals = removals,
    replacements = replacements
  )
  
  return(list(changes = changes, df = df))
}
