#' Extracts Location Information
#'
#' Helper function that splits the location name into 'country' and 'location'.
#' International earthquake events are assigned to each country individually.
#' Transforms the location names to title case.
#'
#' @param data A data frame with column names with underscores (`_`) instead of
#' whitespaces and lower letter case
#'
#' @importFrom dplyr across mutate
#' @importFrom stringr str_to_title
#' @importFrom tidyr separate_longer_delim separate_wider_delim
#'
#' @returns A data frame.
#'
#' @examples
#' data(mexico)
#' #uses `clean_names()` from the `janitor` package to tidy up column names
#' mexico %>%
#'   janitor::clean_names(.) %>%
#'   eq_location_clean()
#'
#' @export

eq_location_clean <- function(data){

    data <- data %>%
      tidyr::separate_longer_delim(., location_name, delim = ";") %>%
      tidyr::separate_wider_delim(., location_name,
                                  names = c("country", "location"),
                                  delim = ": ",
                                  cols_remove = TRUE,
                                  too_few = "align_start",
                                  too_many = "merge"
      ) %>%
      mutate(
        location = stringr::str_to_title(.),
        across(c(location, country), ~gsub('"', '', trimws(location))),
      )

    data
}


