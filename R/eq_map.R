library(ggmap)
# 
# API key setup video: https://youtu.be/6jUSyI6x3xg

eq_map <- function(data, annot_col, ...){
  
  ggplot(data, aes(x = data$Longitude,
                   y = data$Latitude,
                   ...)) +
    geom_point(aes(color = data$Mag)) +
    annotate(
      "text",
      
    )
    geom_label(label = annot_col)
                   
}

mexico <- eq_clean %>% 
  filter(Country == "MEXICO" & !is.na(Date) & !is.na(Latitude) & !is.na(Longitude))

get_map(c(left = min(mexico$Longitude) - 5, bottom = min(mexico$Latitude) - 3, 
          right = max(mexico$Longitude) + 3, top = max(mexico$Latitude) + 3), 
        source = "stadia", maptype = "stamen_toner_lite", zoom = 6) %>%
  ggmap(extent = "device")
