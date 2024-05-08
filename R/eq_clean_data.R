eq_clean_data <- function(data) {
  df <- data %>%
    janitor::clean_names(.) %>%
    dplyr::rename_with(stringr::str_to_upper) %>%
    dplyr::filter(!is.na(LOCATION_NAME)) %>%
    eq_location_clean(.) %>%
    dplyr::mutate(DATE = lubridate::make_date(YEAR, MO, DY)) %>%
    dplyr::relocate(DATE, .before = HR) %>%
    dplyr::relocate(c(COUNTRY, LOCATION), .before = LOCATION_NAME) %>%
    dplyr::select(!c(YEAR, MO, DY))

  df
}
