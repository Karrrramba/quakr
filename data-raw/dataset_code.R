#  raw data
eq_df <- readr::read_tsv("data-raw/earthquakes-2024-04-29_16-19-32_+0200.tsv",
                                                  col_select = !1)

# complete cases for magnitude, coordinates and year
earthquakes <- eq_df %>%
  filter(if_all(c(Year, Mag, Latitude, Longitude), ~!is.na(.)))
  save(earthquakes, file = "data/earthquakes.rda")

southamerica <- eq_df %>%
  select(c(`Location Name`, Year, Mo, Dy, Latitude, Longitude, `Total Deaths`, Mag)) %>%
  filter(str_detect(`Location Name`, "ARGENTINA|CHILE|BRAZIL|
                                         ECUADOR|PERU|PARAGUAY|
                                         BOLIVIA|COLOMBIA|VENEZUELA|
                                         GUYANA")) %>%
  filter(if_all(c('Total Deaths', Mag), ~!is.na(.)))

mexico <- eq_df %>%
  select(c(`Location Name`, Year, Mo, Dy, Latitude, Longitude, `Total Deaths`, Mag)) %>%
  tidyr::separate_longer_delim(., 'Location Name', delim = ";") %>%
  filter(str_detect(`Location Name`, "MEXICO") &
           Year >= 1900) %>%
  filter(if_all(c('Total Deaths', Mag), ~!is.na(.))) %>%
  filter(!str_detect(`Location Name`, 'PERU'))
