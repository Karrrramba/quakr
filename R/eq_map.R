library(leaflet)

eq_map <- function(data, annot_col) {

  p <- leaflet::leaflet() %>%
    leaflet::addTiles() %>%
    leaflet::addCircles(data = data,
               popup = ~ str2lang(annot_col),
               lng =  ~ LONGITUDE,
               lat = ~ LATITUDE,
               radius = ~ MAG * 5000,
               color = ~ "darkred",
               fillOpacity = 0.4,
               weight = 1,
               )
  p
}

eq_clean %>%
  filter(COUNTRY == "MEXICO" & lubridate::year(DATE) > 2000) %>%
  eq_map(annot_col = "DATE")
