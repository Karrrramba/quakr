eq_df <- readr::read_tsv("data/earthquakes-2024-04-29_16-19-32_+0200.tsv",
                 col_select = !1)



eq_location_clean <- function(data){
  df <- data %>%
    dplyr::mutate(COUNTRY = stringr::str_extract(LOCATION_NAME, "(^[a-zA-Z]+)"),
           LOCATION = stringr::str_remove(LOCATION_NAME, "(^[a-zA-Z]+)\\W+"),
           LOCATION = stringr::str_to_title(LOCATION)) %>%
    dplyr::select(!LOCATION_NAME)

  df
}

eq_clean <-  eq_clean_data(eq_df)

