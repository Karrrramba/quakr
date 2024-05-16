library(dplyr)
library(readr)
library(tidyr)


raw <- readr::read_tsv("data-raw/earthquakes-2024-04-29_16-19-32_+0200.tsv",
                       col_select = !1)

world %>%

mexico <- raw %>%
  select(c(`Location Name`, Year, Mo, Dy, Latitude, Longitude, `Total Deaths`, Mag)) %>%
  tidyr::separate_longer_delim(., 'Location Name', delim = ";") %>%
  filter(str_detect(`Location Name`, "MEXICO") &
           Year >= 1900) %>%
  filter(if_all(c('Total Deaths', Mag), ~!is.na(.))) %>%
  filter(!str_detect(`Location Name`, 'PERU'))

write_csv(mexico, "data-raw/mexico.csv")
save(mexico, file = "data-raw/mexico.rda")
usethis::use_data(mexico, overwrite = TRUE)
