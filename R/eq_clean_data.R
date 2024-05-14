#' Formats Raw NCEI Data
#'
#' Performs various data formatting tasks: cleans and capitalizes column names,
#' merges columns 'year', 'mo' and 'dy' to create the 'date' column,
#' splits 'country name' to create 'country' and 'location' columns,
#' and removes the respective original columns.
#'
#' @param data A data frame with raw NCEI earthquake data.
#'
#' @importFrom dplyr filter mutate relocate rename_with select
#' @importFrom janitor clean_names
#' @importFrom lubridate make_date
#'
#' @examples
#' # example code
#'
#'
#' @export
eq_clean_data <- function(data) {
  data <- data %>%
    janitor::clean_names(.) %>%
    dplyr::rename_with(toupper) %>%
    dplyr::filter(!is.na(LOCATION_NAME)) %>%
    eq_location_clean(.) %>%
    dplyr::mutate(DATE = lubridate::make_date(YEAR, MO, DY)) %>%
    dplyr::relocate(DATE, .before = LOCATION) %>%
    dplyr::relocate(c(COUNTRY, LOCATION), .before = LATITUDE) %>%
    dplyr::select(!c(YEAR, MO, DY)) %>%
    dplyr::filter(!is.na(DATE))

  data
}
