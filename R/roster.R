#' Process Roster Data from Excel File
#'
#' This function reads a roster Excel file, cleans and transforms it into a
#' long-format dataframe with columns: date, day, start, end, location, name,
#' role. Different units have different processing logic.
#'
#' @param file_path Path to the Excel file containing the roster data.
#' @param unit The unit identifier (e.g., "BIOL1007", "BIOL2022", case-insensitive). Required.
#' @return A dataframe in long format with roster assignments.
#' @importFrom readxl read_excel
#' @importFrom dplyr %>% select all_of filter mutate across where na_if
#'   case_when
#' @importFrom tidyr fill pivot_longer
#' @importFrom stringr str_extract str_replace str_detect str_starts
#' @export
roster <- function(file_path, unit) {
  # Convert unit to lowercase for case-insensitive matching
  unit_lower <- tolower(unit)
  
  if (unit_lower == "biol1007") {
    return(roster_biol1007(file_path))
  } else if (unit_lower == "biol2022") {
    return(roster_biol2022(file_path))
  } else {
    stop("Unknown unit: ", unit, ". Supported units: 'BIOL1007', 'BIOL2022' (case-insensitive)")
  }
}
