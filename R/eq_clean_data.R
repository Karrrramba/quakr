#' Formats raw NCEI data for visualizations
#'
#' Performs various data formatting tasks: cleans and capitalizes column names,
#' creates the 'date' as well as 'country' and 'location' columns,
#' and removes the respective original columns. Observations without complete date
#' information are removed.
#'
#' @param data A data frame with raw NCEI earthquake data.
#'
#' @returns A data frame.
#'
#' @importFrom dplyr filter mutate relocate rename_with select
#' @importFrom janitor clean_names
#' @importFrom lubridate make_date
#'
#' @examples
#' mexico %>%
#'   eq_clean_data()
#'
#' @export
eq_clean_data <- function(data) {
  data <- data %>%
    janitor::clean_names(.) %>%
    eq_location_clean(.) %>%
    dplyr::mutate(date = lubridate::make_date(year, mo, dy)) %>%
    dplyr::relocate(c(country, location, date), .before = latitude) %>%
    dplyr::select(!c(year, mo, dy)) %>%
    dplyr::filter(!is.na(date))

  data
}
