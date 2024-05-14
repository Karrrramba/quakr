#' Extracts Location Information
#'
#' Helper function that splits the location name into 'country' and 'location'.
#' Transforms the location names to title case.
#'
#' @param data A data frame.
#'
#' @importFrom dplyr mutate select
#' @importFrom stringr  str_to_title
#' @importFrom tidyr separate_longer_delim separate_wider_delim
#'
#' @examples
#' # example code
#'
#'
#' @export

eq_location_clean <- function(data){

    data <- data %>%
      tidyr::separate_longer_delim(location_name, delim = ";") %>%
      tidyr::separate_wider_delim(location_name, names = c("country", "location"),
                                  delim = ":", cols_remove = TRUE,
                                  too_few = "align_start", too_many = "merge") %>%
      mutate(location = stringr::str_to_title(location))
      dplyr::select(!location_name)

  data
}
