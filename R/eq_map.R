#' Draws a Map with Earthquake Locations
#'
#' @description
#' \code{eq_map} plots the locations of earthquake events on a geographical map
#'   of one or multiple countries. Earthquake locations are plotted as
#'   red circles with labels and size scaled based on magnitude.
#'
#' @param data A data frame containing columns for "country",
#'   "magnitude", "longitude" and "latitude". The latter two must be values in [deg].
#'
#' @param annot_col A vector of values which will be used for labels.
#' @param scaling_factor A numeric value which scales the point size.
#'   Defaults to 1.
#'
#' @retrurns An interactive leaflet map.
#'
#' @importFrom leaflet leaflet addProviderTiles addCircles
#'
#' @examples
#' # Plot eartquake events for a single country
#' data(mexico)
#' mexico %>%
#'   eq_clean_data(.) %>%
#'   filter(year(date) > 2000) %>%
#'   eq_map(annot_col = "date")
#'
#' # Ploteathquake events for multiple countries
#' data(southamerica)
#'
#' southamerica %>%
#'   eq_clean_data(.) %>%
#'   filter(year(date) > 2000) %>%
#'   eq_map(annot_col = "mag", scaling_factor = 5)
#'
#' @export

eq_map <- function(data, annot_col, scaling_factor = 1) {

  leaflet::leaflet() %>%
    leaflet::addProviderTiles("Esri.WorldTopoMap") %>%
    leaflet::addCircles(data = data,
               popup = ~ data[[annot_col]],
               lng =  ~ longitude,
               lat = ~ latitude,
               radius = ~ mag * (scaling_factor * 1000),
               color = ~ "darkred",
               fillOpacity = 0.4,
               weight = 1,
               )

}
