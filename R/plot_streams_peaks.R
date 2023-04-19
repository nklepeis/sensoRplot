#' @name plot_streams_peaks
#'
#' @title Plot identified peaks in a time series
#'
#' @description This function plots peaks that have bee
#' identified in a time series
#'
#' @author David Torres-Mendoza and Neil Klepeis
#'
#' @param streams a tibble of sensor data streams in standard long format
#' with Time, Response, and Value variables
#' @param peaks
#'
#' @return a ggplot object
#'
#' @details Create a ggplot of the streams superimposed with
#' identified peaks, threshold, and baseline
#'
#'----------------------------------------



plot_streams_peaks <- function(streams, peaks, baseline,
                               threshold) {

  if (NROW(streams) & NROW(peaks)) {

    ggplot(streams, aes(x=Time, y=ValueSmoothed)) +
      geom_line() +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
      scale_x_datetime(breaks = scales::pretty_breaks(n = 6)) +
      geom_point(data = peaks, aes(MaxTimestamp, PeakHeight),
                 color = "red", size=2) +
      geom_hline(yintercept=baseline, linetype="dashed",
                 color = "blue", size=1) +
      geom_hline(yintercept=threshold, linetype="dotted",
                 color = "red", size=1) +
      geom_point(data = peaks, aes(StartTimestamp, StartHeight),
                 color = "darkgreen", size=1.5,
                 shape=24, fill = "darkgreen") +
      geom_point(data = peaks, aes(EndTimestamp, EndHeight),
                 color = "darkgreen", size=1.5,
                 shape=25, fill = "darkgreen")

  } else if (NROW(streams)) {

    ggplot(streams, aes(x=Time, y=ValueSmoothed)) +
      geom_line() +
      geom_hline(yintercept=baseline, linetype="dashed",
                 color = "blue", size=1) +
      geom_hline(yintercept=threshold, linetype="dotted",
                 color = "red", size=1) +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
      scale_x_datetime(breaks = scales::pretty_breaks(n = 6))

  } else NULL

}
