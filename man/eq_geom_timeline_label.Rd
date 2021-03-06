% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/jggWrappers.R
\name{eq_geom_timeline_label}
\alias{eq_geom_timeline_label}
\title{eq_geom_timeline_label}
\usage{
eq_geom_timeline_label(df, nmin = 1, nmax = lubridate::year(Sys.Date()),
  n_max = 5, countries = "")
}
\arguments{
\item{df}{A data table containing NOAA Earthquake processed}

\item{nmin}{minimum year as numeric}

\item{nmax}{maximum year as numeric}

\item{n_max}{Integer value to control number of labels per group to show}

\item{countries}{Vector of countries to filter}
}
\value{
A ggplot2 graphical object displaying timeline of earthquakes data
}
\description{
A wrapper function to simplify use of geom_timeline
}
\examples{
\dontrun{
eq_geom_timeline_label(df, n_max=5, xmin=2000, xmax=2015, countries=c("USA","CHINA"))
}
}
