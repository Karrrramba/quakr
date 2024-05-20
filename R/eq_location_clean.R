#' Extracts location information
#'
#' Helper function that splits the location name into 'country' and 'location'.
#' International earthquake events are assigned to each country individually.
#' Transforms the location names to title case.
#'
#' @param data A data frame with column names with underscores (`_`) instead of
#' whitespaces and lower letter case.
#'
#' @importFrom dplyr across filter mutate
#' @importFrom stringr str_to_title str_to_upper
#' @importFrom tidyr separate_longer_delim separate_wider_delim
#' @importFrom magrittr  %>%
#'
#' @returns A data frame.
#'
#' @examples
#' data(world)
#' world %>%
#'   janitor::clean_names(.) %>%
#'   eq_location_clean() %>%
#'   dplyr::select(location, country) %>%
#'   head()
#'
#' @export

eq_location_clean <- function(data){

    data <- data %>%
      dplyr::filter(!is.na(location_name)) %>%
      tidyr::separate_longer_delim(., location_name, delim = ";") %>%
      tidyr::separate_wider_delim(., location_name,
                                  names = c("country", "location"),
                                  delim = ": ",
                                  cols_remove = TRUE,
                                  too_few = "align_start",
                                  too_many = "merge"
      ) %>%
      dplyr::mutate(
        location = stringr::str_to_title(location),
        country = stringr::str_to_upper(country),
        dplyr::across(c(location, country), ~gsub('"', '', trimws(.))),
      )

    data
}
