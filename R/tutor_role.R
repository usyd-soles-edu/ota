#' Mutate Tutor Roles to Mark Repeats
#'
#' Marks the first tutor role for each person in each week as "Tutor" and
#' all subsequent tutor roles within that week as "Tutor (repeat)". All other
#' roles (e.g., Demonstrator) are left unchanged.
#'
#' @param df A dataframe with columns: name, week, role
#'
#' @return The input dataframe with the role column updated according to the
#'   tutor repeat logic.
#'
#' @details
#' This function groups by name and week, then for rows where role == "Tutor",
#' marks the first occurrence as "Tutor" and subsequent ones as "Tutor (repeat)".
#' Other roles are preserved as-is.
#'
#' @examples
#' \dontrun{
#' df <- roster("path/to/roster.xlsx", unit = "biol1007")
#' df_mutated <- mutate_tutor_roles(df)
#' }
#'
#' @importFrom dplyr group_by mutate row_number case_when arrange cumsum
#' @keywords internal
mutate_tutor_roles <- function(df) {
  df %>%
    dplyr::arrange(date, start) %>%
    dplyr::group_by(name, week) %>%
    dplyr::mutate(
      tutor_count = cumsum(role == "Tutor"),
      role = dplyr::case_when(
        role == "Tutor" & tutor_count == 1 ~ "Tutor",
        role == "Tutor" & tutor_count > 1 ~ "Tutor (repeat)",
        TRUE ~ role
      )
    ) %>%
    dplyr::select(-tutor_count) %>%
    dplyr::ungroup()
}

# Suppress R CMD check notes for non-standard evaluation variables
utils::globalVariables(c("tutor_count"))
