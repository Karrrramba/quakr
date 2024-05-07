library(leaflet)
library(tidyverse)
library(rlang)
eq_map <- function(data, annot_col) {

  p <- leaflet::leaflet() %>%
    leaflet::addTiles() %>%
    leaflet::addCircles(data = data,
               # popup = ~ paste("<b> Date: </b>", annot_col),
               # popup = ~ !!annot_col,
               # popup = ~ as.character(.data[[annot_col]]),
               # popup = ~ paste0("<b>", .data[[annot_col]], "</b>"),
               label = paste("Date:", data[[annot_col]]),
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
