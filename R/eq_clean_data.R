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
#' data(mexico)
#' mexico %>%
#'   eq_clean_data()
#'
#' @export
eq_clean_data <- function(data) {
  data <- data %>%
    janitor::clean_names(.) %>%
    dplyr::filter(!is.na(location_name)) %>%
    eq_location_clean(.) %>%
    dplyr::mutate(date = lubridate::make_date(year, mo, dy)) %>%
    dplyr::relocate(date, .before = location) %>%
    dplyr::relocate(c(country, location), .before = latitude) %>%
    dplyr::select(!c(year, mo, dy)) %>%
    dplyr::filter(!is.na(date))

  data
}
