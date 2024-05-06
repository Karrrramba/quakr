library(ggmap)
library(leaflet)

#
# API key setup video: https://youtu.be/6jUSyI6x3xg

eq_map <- function(data, annot_col) {

  label_col <- as.character(annot_col)
  print(label_col)

  p <- leaflet() %>%
    addTiles() %>%
    addCircles(data = data,
               popup = ~ Date,
               lng =  ~ Longitude,
               lat = ~ Latitude,
               radius = ~ Mag * 10000,
               color = ~ Deaths,
               fillOpacity = 0.2
               )
  p
}

mexico %>% eq_map_leaf(annot_col = "Date")

mexico <- eq_clean %>%
  filter(Country == "MEXICO" & !is.na(Date) & !is.na(Latitude) & !is.na(Longitude))

m <- get_map(c(left = min(mexico$Longitude) - 10, bottom = min(mexico$Latitude) - 3,
          right = max(mexico$Longitude) + 10, top = max(mexico$Latitude) + 3),
        source = "stadia", maptype = "stamen_toner_lite", zoom = 5) %>%
  ggmap(extent = "device") +
  geom_point(data = mexico, aes(x = Longitude, y = Latitude, color = Deaths, size = Mag, alpha = 0.3))
m

leaflet() %>%
  addTiles() %>%
  addCircles(data = mexico,
             label = ~ Date,
             lng =  ~ Longitude,
             lat = ~ Latitude,
             radius = ~ 50000,
             color = ~ Deaths,
             fillOpacity = 0.4
  )
