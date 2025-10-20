#' Process Roster Data from Excel File
#'
#' This function reads a roster Excel file, cleans and transforms it into a
#' long-format dataframe with columns: date, day, start, end, location, name,
#' role. Different units have different processing logic.
#'
#' @param file_path Path to the Excel file containing the roster data.
#' @param unit The unit identifier (e.g., "biol1007", "biol2022"). Required.
#' @return A dataframe in long format with roster assignments.
#' @importFrom readxl read_excel
#' @importFrom dplyr %>% select all_of filter mutate across where na_if
#'   case_when
#' @importFrom tidyr fill pivot_longer
#' @importFrom stringr str_extract str_replace str_detect str_starts
#' @export
roster <- function(file_path, unit) {
  if (unit == "biol1007") {
    return(roster_biol1007(file_path))
  } else if (unit == "biol2022") {
    return(roster_biol2022(file_path))
  } else {
    stop("Unknown unit: ", unit, ". Supported units: 'biol1007', 'biol2022'")
  }
}
