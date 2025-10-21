# Compare function to detect additions, removals, and replacements between the two latest log files
#' @importFrom dplyr inner_join anti_join select
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
  
  # Detect additions: rows in latest but not in previous
  additions <- dplyr::anti_join(latest_pair$latest, latest_pair$previous, by = names(latest_pair$latest))
  
  # Detect removals: rows in previous but not in latest
  removals <- dplyr::anti_join(latest_pair$previous, latest_pair$latest, by = names(latest_pair$previous))
  
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
