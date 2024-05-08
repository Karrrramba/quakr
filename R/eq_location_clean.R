#' Extracts Location Information
#'
#' Helper function that splits the location name into 'country' and 'location'.
#' Transforms the location names to title case.
#'
#' @param data A data frame.
#'
#' @importFrom dplyr mutate select
#' @importFrom stringr str_extract str_remove str_to_title
#'
#' @examples
#' # example code
#'
#'
#' @export

eq_location_clean <- function(data){

  if (toupper("LOCATION_NAME") %in% names(data)) {
    data <- data %>%
      dplyr::mutate(
        COUNTRY = stringr::str_extract(LOCATION_NAME, "(^[a-zA-Z]+)"),
        LOCATION = stringr::str_remove(LOCATION_NAME, "(^[a-zA-Z]+)\\W+"),
        LOCATION = stringr::str_to_title(LOCATION)) %>%
      dplyr::select(!LOCATION_NAME)
  }

  data
}
