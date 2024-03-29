% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_regressions_plotly2.R
\name{plot_regressions_plotly2}
\alias{plot_regressions_plotly2}
\title{Plot linear regressions for sensor data streams}
\usage{
plot_regressions_plotly2(
  data,
  xsites,
  ysites,
  xvars,
  yvars,
  averaging = "10 min",
  title = NULL,
  height = 500,
  legend.inside = FALSE,
  fg = "white",
  bg = "rgba(23,103,124,1)",
  plot.bg = "gray90",
  legend.bg = "rgba(255,255,255,0.75)",
  legend.fg = "black",
  line.width = 1,
  marker.size = 8
)
}
\arguments{
\item{data}{Long-format sensor data frame for multiple streams and monitoring sites
containing a 'Time' column, index (grouping) columns, and Response and Value columns}

\item{xvars}{Response variables that are to be used as the independent (x-axis) variables}

\item{yvars}{Response variables that are to be used as the dependent (y-axis) variables}

\item{averaging}{Averaging time to use when aggregating the Values, defaults to 10 min}

\item{height}{height of the plot in pixels}

\item{fg}{foreground color for text}

\item{bg}{background color}

\item{plot.bg}{plot background color}

\item{legend.bg}{legend background color}

\item{legend.fg}{legend foreground color}

\item{line.width}{linewidth for plotted series}

\item{marker.size}{marker (symbol) size in px for plotted series}
}
\value{
a plotly object
}
\description{
Plot linear regressions for all combinations of multiple dependent and
independent variables between and within multiple monitoring sites
}
\details{
This function tags all the specified x and y data stream responses
in the passed 'data' tibble and plots all x-y combinations within and between
monitoring sites

See 'plot_regressions_subplot_plotly.R' which calls this function
to produce stacked subplots for each non-Response index variable, i.e.,
all index variables besides Response are used to facet the data into
separated plots
}
\author{
Neil Klepeis
}
