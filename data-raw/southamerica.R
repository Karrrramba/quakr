library(dplyr)
library(readr)
library(stringr)

southamerica <- world %>%
  select(c(`Location Name`, Year, Mo, Dy, Latitude, Longitude, `Total Deaths`, Mag)) %>%
  filter(str_detect(`Location Name`,
                    "ARGENTINA|BOLIVIA|BRAZIL|CHILE|COLOMBIA|ECUADOR|GUYANA|
                    PARAGUAY|PERU|URUGUAY|VENEZUELA"
                    )
         )

write_csv(southamerica, "data-raw/southamerica.csv")
save(southamerica, file = "data-raw/southamerica.rda")
