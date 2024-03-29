% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geom_timeline.R
\name{geom_timeline}
\alias{geom_timeline}
\title{Add timeline data to a ggplot}
\usage{
geom_timeline(
  data,
  cols,
  auto.y = TRUE,
  group = FALSE,
  position = "middle",
  size.timeline = 1,
  yspace = 0.05,
  col.border = "lightgray",
  size.border = 0.3,
  alpha = 0.5,
  ...
)
}
\arguments{
\item{data}{a tibble in grouped state format with Time and one or more cols containing
state groups in which states are mtually exclusive states (only one state can occur
at a time)}

\item{auto.y}{compute automatic y spacing}

\item{group}{logical, whether to collapse states in a given group to a single lane}

\item{position}{where to group the timeline, "top", "bottom", "middle"}

\item{size.timeline}{a number between 0 and 1 giving vertical extent of the
timeline relative to the vertical plot area, e.g., 0.5 for 50\% of the y extent}

\item{yspace}{y spacing between lanes in points}

\item{col.border}{color of borders of rectangles}

\item{size.border}{size of the rectangle border}

\item{alpha}{alpha value for the rectangle fill}

\item{...}{arguments to pass to the \code{geom_rect} function}
}
\description{
Add timeline data to an existing ggplot
}
\details{
TODO:  Have options to group the timeline at top, bottom, middle,
or fill the entire plot area,  maybe given actual numeric range
or "top", "bottom", "middle" and another parameter for how much of
the y scale

grobTree
textGrob

http://zevross.com/blog/2014/08/04/beautiful-plotting-in-r-a-ggplot2-cheatsheet-3/#working-with-annotation

"..using the library grid that allows you to specify the location based on scaled coordinates where 0 is low and 1 is high."
}
\author{
Neil Klepeis
}
