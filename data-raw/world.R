library(dplyr)
library(readr)
library(tidyr)

raw <- readr::read_tsv("data-raw/earthquakes-2024-04-29_16-19-32_+0200.tsv",
                         col_select = !1)
world <- raw %>%
  filter(if_all(c(Year, Mag, Latitude, Longitude, 'Location Name'), ~!is.na(.))) %>%
  separate_longer_delim(., 'Location Name', delim = ";") %>%

write_csv(world, "data-raw/world.csv")
save(world, file = "data-raw/world.rda")
