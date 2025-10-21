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
#' @importFrom dplyr group_by mutate row_number case_when
#' @keywords internal
mutate_tutor_roles <- function(df) {
  df %>%
    dplyr::group_by(name, week) %>%
    dplyr::mutate(
      role = dplyr::case_when(
        role == "Tutor" & dplyr::row_number() == 1 ~ "Tutor",
        role == "Tutor" & dplyr::row_number() > 1 ~ "Tutor (repeat)",
        TRUE ~ role
      )
    ) %>%
    dplyr::ungroup()
}
