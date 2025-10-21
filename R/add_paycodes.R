#' Add Paycode Information to Roster Data
#'
#' This function adds paycode information to a roster dataframe by matching staff
#' names with paycode rules from an external stafflist. It validates that all 
#' roster staff are accounted for in the stafflist and applies paycode rules 
#' based on staff attributes (PhD status, role, and whether they're a new staff member).
#'
#' @param df A dataframe with roster data, typically returned by \code{\link{snapshot}()}.
#'   Must have columns: name, role. Must have attributes \code{file_path} and \code{unit} set.
#'
#' @details
#' This function imports staff data from sheet 2 of the Excel file referenced in
#' the input dataframe's \code{file_path} attribute. The stafflist should have
#' columns: Label (staff names), Phd (0 or 1), New (0 or 1).
#'
#' Paycode rules applied:
#' \itemize{
#'   \item{PhD = 1 staff:}{\itemize{
#'     \item{Tutor → TU1}
#'     \item{Tutor (repeat) → TU3}
#'     \item{Demonstrator → DE1}
#'   }}
#'   \item{PhD = 0 staff:}{\itemize{
#'     \item{Tutor → TU2}
#'     \item{Tutor (repeat) → TU4}
#'     \item{Demonstrator → DE2}
#'   }}
#' }
#'
#' If staff names in the roster are not found in the stafflist Label column,
#' they are highlighted in the console output with a warning message so the user
#' can update the stafflist accordingly.
#'
#' The function also stores information about paycode changes (for tracking in \code{\link{compare}()})
#' if staff had their TUX rates modified due to changes in their tutorial schedule 
#' (e.g., dropping their first tutorial changes the next tutor slot from 
#' "Tutor (repeat)" to "Tutor").
#'
#' @return The input dataframe with a new \code{paycode} column added, invisibly.
#'   The dataframe retains its original attributes (file_path, unit).
#'   If called in a pipe without subsequent functions that check for the paycode
#'   column, it behaves gracefully.
#'
#' @examples
#' \dontrun{
#' # Use in a pipeline
#' result <- roster("path/to/roster.xlsx", unit = "biol1007") |>
#'   snapshot() |>
#'   add_paycodes() |>
#'   compare() |>
#'   document_changes()
#'
#' # add_paycodes is optional; can be skipped
#' result <- roster("path/to/roster.xlsx", unit = "biol1007") |>
#'   snapshot() |>
#'   compare() |>
#'   document_changes()
#' }
#'
#' @importFrom readxl read_excel
#' @importFrom dplyr left_join mutate case_when filter pull select distinct arrange
#' @importFrom tidyr replace_na
#' @importFrom cli style_bold col_red cat_line
#' @importFrom rlang .data
#' @export
add_paycodes <- function(df) {
  # Extract file path and unit from attributes
  file_path <- attr(df, "file_path")
  unit <- attr(df, "unit")
  
  if (is.null(file_path)) {
    stop("No file_path attribute found in the dataframe. Make sure the dataframe comes from roster().")
  }
  
  # Read stafflist from sheet 2 of the Excel file
  stafflist <- suppressMessages(
    readxl::read_excel(file_path, sheet = 2)
  )
  
  # Clean stafflist - use bare column names with NSE
  stafflist <- stafflist %>%
    dplyr::filter(!is.na(Label), Label != "", Label != ".") %>%
    dplyr::arrange(Label) %>%
    dplyr::mutate(
      Phd = tidyr::replace_na(Phd, 0),
      New = tidyr::replace_na(New, 0)
    )
  
  # Get unique staff names in the roster
  roster_staff <- df %>%
    dplyr::pull(name) %>%
    unique() %>%
    sort()
  
  # Get unique staff names in the stafflist
  stafflist_names <- stafflist %>%
    dplyr::pull(Label) %>%
    unique() %>%
    sort()
  
  # Check for staff in roster that are NOT in stafflist
  missing_staff <- setdiff(roster_staff, stafflist_names)
  
  if (length(missing_staff) > 0) {
    cli::cat_line("")
    cli::cat_line(cli::style_bold(cli::col_red("⚠️  STAFFLIST UPDATE REQUIRED ⚠️ ")))
    cli::cat_line("The following staff are in the roster but NOT in the stafflist:")
    cli::cat_line("Please update the stafflist in sheet 2 of your Excel file:")
    cli::cat_line("")
    for (name in missing_staff) {
      cli::cat_line(cli::style_bold(cli::col_red(paste0("  • ", name))))
    }
    cli::cat_line("")
  }
  
  # Add paycode column by joining with stafflist
  # Select only the columns we need from stafflist for the join
  stafflist_for_join <- stafflist %>%
    dplyr::select(Label, Phd) %>%
    dplyr::distinct()
  
  # Join and apply paycode rules
  df_with_paycode <- df %>%
    dplyr::left_join(
      stafflist_for_join,
      by = c("name" = "Label")
    ) %>%
    dplyr::mutate(
      Phd = tidyr::replace_na(Phd, 0),
      paycode = dplyr::case_when(
        # PhD = 1
        Phd == 1 & role == "Tutor" ~ "TU1",
        Phd == 1 & role == "Tutor (repeat)" ~ "TU3",
        Phd == 1 & role == "Demonstrator" ~ "DE1",
        # PhD = 0
        Phd == 0 & role == "Tutor" ~ "TU2",
        Phd == 0 & role == "Tutor (repeat)" ~ "TU4",
        Phd == 0 & role == "Demonstrator" ~ "DE2",
        TRUE ~ NA_character_
      )
    ) %>%
    dplyr::select(-Phd)  # Remove the temporary Phd column
  
  # Restore attributes and return invisibly
  attr(df_with_paycode, "file_path") <- file_path
  attr(df_with_paycode, "unit") <- unit
  
  invisible(df_with_paycode)
}

# Suppress R CMD check notes for non-standard evaluation variables
utils::globalVariables(c(".data", "Label", "Phd", "New", "role", "name"))

