% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/jggGeom.R
\name{eq_create_label}
\alias{eq_create_label}
\title{eq_create_label}
\usage{
eq_create_label(dt)
}
\arguments{
\item{dt}{A data table containing NOAA Earthquake data cleaned}
}
\value{
An interactive map displaying earthquate location for a given country with custom popup.
}
\description{
A function to generate a custom popup box for a selected earthquake showing location,
magnitude and total deaths.
}
\examples{
\dontrun{
 eq_create_label(dt)) \%>\% eq_map(annot_col="DATE")
}

}
