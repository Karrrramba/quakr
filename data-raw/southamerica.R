library(quakr)

southamerica <- world %>%
  eq_clean_data() %>%
  select(c(country, location, date, longitude, latitude, mag, total_deaths)) %>%
  filter(country %in%  c("ARGENTINA", "BOLIVIA", "BRAZIL", "CHILE", "COLOMBIA",
                         "ECUADOR", "GUYANA",  "PARAGUAY", "PERU", "URUGUAY", "VENEZUELA")
         & year(date) >= 1900) %>%
  na.omit()


write_csv(southamerica, "data-raw/southamerica.csv")
save(southamerica, file = "data-raw/southamerica.rda")
