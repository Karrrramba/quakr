% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/eq_clean_data.R
\name{eq_clean_data}
\alias{eq_clean_data}
\title{Formats raw NCEI data for visualizations}
\usage{
eq_clean_data(data)
}
\arguments{
\item{data}{A data frame with raw NCEI earthquake data.}
}
\value{
A data frame.
}
\description{
Performs various data formatting tasks: cleans and capitalizes column names,
creates the 'date' as well as 'country' and 'location' columns,
and removes the respective original columns. Observations without complete date
information are removed.
}
\examples{
world \%>\%
  eq_clean_data() \%>\%
  dplyr::select(date, country, location) \%>\%
  head()

}
