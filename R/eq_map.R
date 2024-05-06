library(ggmap)

#
# API key setup video: https://youtu.be/6jUSyI6x3xg

eq_map <- function(data, annot_col, na.rm = FALSE){

  left_margin = min(data$Longitude) - 10
  right_margin = max(data$Longitude) + 10
  bottom_margin = min(data$Latitude) - 3
  top_margin = max(data$Latitude) + 3

  p <- ggmap::get_map(c(
    left = left_margin , bottom = bottom_margin,
    right = right_margin , top = left_margin
    ),
    source = "stadia", maptype = "stamen_toner_lite", zoom = 5
    ) %>%
    ggmap::ggmap(extent = "device") +
    ggplot2::geom_point(data = data, aes(x = Longitude,
                                y = Latitude,
                                color = Deaths,
                                size = Mag,
                                alpha = 0.25)) +
    ggplot2::geom_text(data = data, aes(x = Longitude,
                               y = Latitude,
                               label = {{ annot_col }}
                               # label = as.character(.data[[annot_col]])
                               )
              )

  p
}

mexico <- eq_clean %>%
  filter(Country == "MEXICO" & !is.na(Date) & !is.na(Latitude) & !is.na(Longitude))

m <- get_map(c(left = min(mexico$Longitude) - 10, bottom = min(mexico$Latitude) - 3,
          right = max(mexico$Longitude) + 10, top = max(mexico$Latitude) + 3),
        source = "stadia", maptype = "stamen_toner_lite", zoom = 5) %>%
  ggmap(extent = "device") +
  geom_point(data = mexico, aes(x = Longitude, y = Latitude, color = Deaths, size = Mag, alpha = 0.3))
m

