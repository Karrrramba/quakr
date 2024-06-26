% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/eq_location_clean.R
\name{eq_location_clean}
\alias{eq_location_clean}
\title{Extracts location information}
\usage{
eq_location_clean(data)
}
\arguments{
\item{data}{A data frame with column names with underscores (`_`) instead of
whitespaces and lower letter case.}
}
\value{
A data frame.
}
\description{
Helper function that splits the location name into 'country' and 'location'.
International earthquake events are assigned to each country individually.
Transforms the location names to title case.
}
\examples{
world \%>\%
  janitor::clean_names(.) \%>\%
  eq_location_clean() \%>\%
  dplyr::select(location, country) \%>\%
  head()

}
