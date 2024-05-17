library(dplyr)
library(readr)
library(stringr)


mexico <- world%>%
  select(c(`Location Name`, Year, Mo, Dy, Latitude, Longitude, `Total Deaths`, Mag)) %>%
  filter(str_detect(`Location Name`, "MEXICO") &
           Year >= 1900) %>%
  filter(!str_detect(`Location Name`, 'PERU'))

write_csv(mexico, "data-raw/mexico.csv")
save(mexico, file = "data-raw/mexico.rda")
