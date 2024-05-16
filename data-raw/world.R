library(dplyr)
library(readr)

raw <- readr::read_tsv("data-raw/earthquakes-2024-04-29_16-19-32_+0200.tsv",
                         col_select = !1)
world <- raw %>%
  filter(if_all(c(Year, Mag, Latitude, Longitude), ~!is.na(.)))

write_csv(world, "data-raw/world.csv")
save(world, file = "data-raw/world.rda")
usethis::use_data(DATASET, overwrite = TRUE)
