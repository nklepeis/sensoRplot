% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/time_factor.R
\name{time_factor}
\alias{time_factor}
\title{Compute an interval time factor}
\usage{
time_factor(
  times,
  interval = 1,
  units = "min",
  origin = min(times),
  level.type = "diff",
  tolerance = 1e-05
)
}
\arguments{
\item{times}{a variable containing Time}

\item{interval}{the time interval to use, 1, 5, 10, etc.}

\item{units}{the units of the interval, "mins", "secs", "hours", etc.}

\item{origin}{the origin to use in computing factors, defaults to using the
lowest time in each group (TBD: allow for specifying separate group starting times)}

\item{level.type}{format for the factor levels:  "diffs" time difference from the origin,
e.g, "0 sec", "5 sec", "10 sec"; "integers" using increasing integer values, e.g.,
1, 2, 3, 4, ...; or "breaks" the actual time value of the breaks}
}
\value{
a vector giving a factor indicating
time intervals from a defined origin
}
\description{
Compute a factor based on a time variable indicating
sequential intervals of defined duration
}
\author{
Neil Klepeis
}
