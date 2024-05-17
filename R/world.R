#' NCEI eartquake data
#'
#' A subset of data from the National Centers for Environmental Information
#' earthquake database.
#'
#' @source <https://www.ngdc.noaa.gov/hazel/view/hazards/earthquake/search>
#' @format ## `world`
#' A data frame with 4646 rows and 38 columns:
#' \describe{
#'   \item{Year, Mo, Dy, Hr, Min, Sec}{Date and time. Negative 'Year' values indicate dates BCE.}
#'   \item{Location Name}{Country and location. Given as COUNTYRY: LOCATION. International events are separated by ';'}
#'   \item{Latitude, Longitude}{Location coordinates. Negative values represent locations west of the prime meridian, south of the equator resp.}
#'   \item{Focal Depth (km)}{Depth of the earthquake given in kilometers.}
#'   ...
#' }
"world"
