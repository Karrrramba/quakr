eq_df <- readr::read_tsv("data/earthquakes-2024-04-29_16-19-32_+0200.tsv",
                 col_select = !1)

eq_clean_data <- function(df) {
  df <- df %>%
    janitor::clean_names(.) %>%
    dplyr::rename_with(stringr::str_to_upper()) %>%
    dplyr::filter(!is.na(LOCATION_NAME)) %>%
    dplyr::mutate(
      Date = lubridate::make_date(YEAR, MO, DY),
      COUNTRY = as.factor(stringr::str_extract(LOCATION_NAME, "(^[a-zA-Z]+)")),
      LOCATION = stringr::str_remove(LOCATION_NAME, "(^[a-zA-Z]+)\\W+"),
      LOCATION = stringr::str_to_title(LOCATION)
      ) %>%
    dplyr::relocate(DATE, .before = HR) %>%
    dplyr::relocate(c(COUNTRY, LOCATION), .before = LOCATION_NAME) %>%
    dplyr::select(!c(LOCATION_NAME))

  df
}

eq_location_clean <- function(df){
  df <- data %>%
    mutate(Country = stringr::str_extract(Location_name, "(^[a-zA-Z]+)"),
           Location = stringr::str_remove(Location_name, "(^[a-zA-Z]+)\\W+"),
           Location = stringr::str_to_title(Location))

  df

}


eq_clean <-  eq_clean_data(eq_df)

