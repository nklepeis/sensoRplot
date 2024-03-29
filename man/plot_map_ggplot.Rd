% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_map_ggplot.R
\name{plot_map_ggplot}
\alias{plot_map_ggplot}
\title{Plot static trajectory on a map using ggplot}
\usage{
plot_map_ggplot(
  data,
  by = "1 day",
  streets = TRUE,
  place = TRUE,
  water = TRUE,
  rail = TRUE,
  building = TRUE
)
}
\arguments{
\item{data}{a tibble of Time, Longitude, and Latitude data}
}
\description{
Plot static trajectory on a map using ggplot
}
\details{
This function plots a static map showing where one or more
sensors were located at successive times

Uses the osmdata (Open Stream Map) package and ggplot to create
the plot.
}
\author{
Neil E. Klepeis
}
