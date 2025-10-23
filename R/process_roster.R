#' Process Roster Pipeline
#'
#' This function combines the common roster processing pipeline into a single
#' function call: loading roster, creating snapshot, optionally adding paycodes,
#' comparing changes, and generating summary reports.
#'
#' @param file_path Path to the Excel file containing the roster data.
#' @param unit The unit identifier (e.g., "BIOL1007", "BIOL2022", case-insensitive).
#' @param add_paycodes Logical. If TRUE (default), adds paycode information to the roster.
#' @param compare Logical. If TRUE (default), compares the current roster with the previous snapshot.
#' @param summary Logical. If TRUE (default), generates a summary of changes. Only applies if compare is TRUE.
#' @param HTML Logical. If TRUE (default), generates HTML summary report. Only applies if summary is TRUE.
#' @param force_snapshot Logical. If TRUE, forces snapshot creation even if no changes detected. Default FALSE.
#' @return If compare is TRUE, returns the comparison result list. Otherwise, returns the processed dataframe.
#' @export
process_roster <- function(file_path, unit, add_paycodes = TRUE, compare = TRUE, summary = TRUE, HTML = TRUE, force_snapshot = FALSE) {
  # Load roster
  df <- roster(file_path, unit)

  # Create snapshot and capture whether a new snapshot was created
  df <- snapshot(df, force = force_snapshot)
  snapshot_created <- attr(df, "snapshot_created")

  # Optionally add paycodes
  if (add_paycodes) {
    df <- add_paycodes(df)
  }

  # Optionally compare and summarize
  if (compare) {
    result <- compare(df)
    # Pass the snapshot_created flag to the result
    attr(result, "snapshot_created") <- snapshot_created
    if (summary) {
      summary(result, HTML = HTML)
    }
    return(invisible(result))
  } else {
    return(invisible(df))
  }
}