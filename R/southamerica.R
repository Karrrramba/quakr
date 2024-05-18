#' NCEI earthquake data
#'
#' A subset of data from the National Centers for Environmental Information
#' earthquake database from 05/2024. The dataset contains earthquake data
#' for the south american continent starting from 1900 and has been filtered
#' for complete observations.
#'
#' @source <https://www.ngdc.noaa.gov/hazel/view/hazards/earthquake/search>
#' @format ## `southamerica`
#' A data frame with 4163 rows and 7 columns:
#' \describe{
#'   \item{country, location}{Geopolitical location given as conuntry and state,province, city or cardinal direction (N, S, W ,E).}
#'   \item{longitude, latitude}{Location coordinates. Negative values represent locations west of the prime meridian, south of the equator resp.}
#'   \item{date}{Date given as %Y-%m-%d.}
#'   \item{mag}{Primary earthquake magnitude.}
#'   \item{total_deaths}{Number of deaths from earthquake and its secondary effects}
#' }
"southamerica"
