#' NCEI earthquake data
#'
#' A subset of data from the National Centers for Environmental Information
#' earthquake database.
#'
#' @source <https://www.ngdc.noaa.gov/hazel/view/hazards/earthquake/search>
#' @format ## `world`
#' A data frame with 4841 rows and 26 columns:
#' \describe{
#'   \item{Year, Mo, Dy, Hr, Min, Sec}{Date and time. Negative 'Year' values indicate dates BCE.}
#'   \item{Location Name}{Country and location. Given as COUNTYRY: LOCATION. International events are separated by ';'.}
#'   \item{Latitude, Longitude}{Location coordinates. Negative values represent locations west of the prime meridian, south of the equator resp.}
#'   \item{Tsu}{Depth of the earthquake given in kilometers.}
#'   \item{Vol}{Additional information if the earthquake was associated with a volcanic eruption.}
#'   \item{Focal Depth (km)}{Depth of the earthquake given in kilometers.}
#'   \item{Mag}{Primary earthquake magnitude.}
#'   \item{MMI (Int)}{Modified Mercalli Intensity (Int). For an interpretation of values see Wood and Neumann, 1931.}
#'   \item{Deaths, Total Deaths}{Number of deaths from earthquake and secondary effects, resp.}
#'   \item{Missing, Total Missing}{Number of missing persons from earthquake and secondary effects, resp.}
#'   \item{Injuries, Total Injuries}{Number of injured from earthquake and secondary effects, resp.}
#'   \item{Damage ($Mil), Total Damage ($Mil)}{Damages as U.S. dollars from earthquake and secondary effects, resp. Values should be multiplied by 1,000,000 to obtain the actual dollar amount.}
#'   \item{Houses Destroyed, Total Houses Destroyed}{Number of damaged housing from earthquake and secondary effects, resp.}
#'   \item{Houses Damaged, Total Houses Damaged}{Number of destroyed housing from earthquake and secondary effects, resp.}
#' }
#' @export
"world"
