library(quakr)

mexico <- world %>%
  eq_clean_data() %>%
  filter(country == "MEXICO" & year(date) >= 1900) %>%
  select(c(country, location, date, longitude, latitude, mag, total_deaths)) %>%
  na.omit()

write_csv(mexico, "data-raw/mexico.csv")
save(mexico, file = "data-raw/mexico.rda")
