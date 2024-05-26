#' Draws a Map with Earthquake Locations
#'
#' @description
#' \code{eq_map} plots the locations of earthquake events on a geographical map
#'     of one or multiple countries. Earthquake locations are plotted as
#'     red circles with labels and size scaled based on magnitude.
#'
#' @param data A data frame containing columns for "country",
#'     "magnitude", "longitude" and "latitude". The latter two must be values in [deg].
#' @param annot_col A vector of values which will be used for labels.
#' @param scaling_factor A numeric value which scales the point size.
#'     Defaults to 1.
#' @param color Character value specifying the color of the Earthquake locations.
#'     Defaults to "darkred".
#' @param provider Name of the map provider. See: https://leaflet-extras.github.io/leaflet-providers/preview/) for available options.
#'     Defaults to "Esri.WorldTopoMap".
#'
#' @returns An interactive leaflet map.
#'
#' @importFrom leaflet leaflet addProviderTiles addCircles
#'
#' @examples
#' library(dplyr)
#' # Plot earthquake events for a single country
#' southamerica %>%
#'   dplyr::filter(country == "CHILE" & lubridate::year(date) >= 1970) %>%
#'   eq_map(annot_col = "date", scaling_factor = 5)
#'
#' # Plot earthquake events for multiple countries
#'southamerica %>%
#'   dplyr::filter(lubridate::year(date) > 2000) %>%
#'   eq_map(annot_col = "mag", scaling_factor = 5)
#'
#' @export

eq_map <- function(data, annot_col, scaling_factor = 1, color = "darkred", provider = "Esri.WorldTopoMap") {

  leaflet::leaflet() %>%
    leaflet::addProviderTiles(provider) %>%
    leaflet::addCircles(data = data,
               popup = ~ data[[annot_col]],
               lng =  ~ longitude,
               lat = ~ latitude,
               radius = ~ mag * (scaling_factor * 1000),
               color = ~ color,
               fillOpacity = 0.4,
               weight = 1,
               )

}
