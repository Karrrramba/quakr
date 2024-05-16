library(dplyr)
library(readr)
library(tidyr)

southamerica <- eq_df %>%
  select(c(`Location Name`, Year, Mo, Dy, Latitude, Longitude, `Total Deaths`, Mag)) %>%
  filter(str_detect(`Location Name`, "ARGENTINA|CHILE|BRAZIL|
                                         ECUADOR|PERU|PARAGUAY|
                                         BOLIVIA|COLOMBIA|VENEZUELA|
                                         GUYANA")) %>%
  filter(if_all(c('Total Deaths', Mag), ~!is.na(.)))

write_csv(southamerica, "data-raw/southamerica.csv")
save(southamerica, file = "data-raw/southamerica.rda")
usethis::use_data(mexico, overwrite = TRUE)
