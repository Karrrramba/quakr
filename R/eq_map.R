#' Draw a Map with Earthquake Locations
#'
#' @description
#' \code{eq_map} plots the locations of earthquake events on a geographical map
#'   of a given country.
#'
#' @param data A data frame containing columns for "country",
#'   "magnitude", "longitude" and "latitude".
#'
#' @param annot_col A vector of values which will be used for labels.
#'
#' @retrurns An interactive leaflet map
#'
#' @importFrom leaflet leaflet addProviderTiles addCircles
#'
#' @examples
#' m <- as.data.frame(
#' "COUNTRY" = "MEXICO",
#' "LOCATION" = c(
#')
#' )
#'
#' @export

eq_map <- function(data, annot_col) {

  leaflet::leaflet() %>%
    leaflet::addProviderTiles("Esri.WorldTopoMap") %>%
    leaflet::addCircles(data = data,
               popup = ~ data[[annot_col]],
               lng =  ~ LONGITUDE,
               lat = ~ LATITUDE,
               radius = ~ MAG * 5000,
               color = ~ "darkred",
               fillOpacity = 0.4,
               weight = 1,
               )

}

eq_clean %>%
  filter(COUNTRY == "MEXICO" & lubridate::year(DATE) > 2000) %>%
  eq_map(annot_col = "DATE")
  slice(1:5)

