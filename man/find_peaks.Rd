% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/find_peaks.R
\name{find_peaks}
\alias{find_peaks}
\title{Find Peaks Based on pracma 'findpeaks' function}
\usage{
find_peaks(
  x,
  nups = 1,
  ndowns = nups,
  zero = "0",
  peakpat = NULL,
  minpeakheight = -Inf,
  minpeakdistance = 1,
  threshold = 0,
  sortstr = TRUE
)
}
\arguments{
\item{x}{a vector of numeric values with no NA values}

\item{nups}{min number of increasing steps before the peak, defaults to 1}

\item{ndowns}{min number of decreasing steps before the peak, defaults to 1}

\item{zero}{how to interpret succeeding steps of the same value, defaults to "0"}

\item{peakpat}{regular pattern to identify peaks, replaces nups/ndowns if specified}

\item{minpeakheight}{min height of identified peaks, defaults to -Inf}

\item{minpeakdistance}{min distance in indices peaks have to be to be counted, defaults to 1}

\item{threshold}{min value identified peaks must be above their neighboring peaks, defaults to 0}

\item{sortstr}{returned peaks sorted in decreasing order?, defaults to TRUE}
}
\value{
a dataframe defining the identified stream episodes
}
\description{
This function finds peaks in a time series
}
\details{
This is based on the findpeaks function in the pracma package
by Hans W. Borchers with a few tweaks by Neil Klepeis to make it easier
to identify intuitive sensor stream episodes with minimal user specification
of parameters
\itemize{
\item Return all peaks
\item Return NULL if no peaks are found
}
}
\author{
Hans W. Borchers and Neil Klepeis
}
