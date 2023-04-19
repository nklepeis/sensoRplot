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



plot_streams_peaks <- function(streams, peaks, mergedpeaks=NULL, baseline=NULL,
                               threshold=NULL, merge.threshold=NULL) {

  if (NROW(streams) & NROW(peaks)) {

    peakplot <- ggplot(streams, aes(x=Time, y=ValueSmoothed)) +
      geom_line() +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
      scale_x_datetime(breaks = scales::pretty_breaks(n = 6)) +
      geom_point(data = streamsPeaks, aes(MaxTimestamp, PeakHeight),
                 color = "red", size=2) +
      geom_hline(yintercept=baseline, linetype="dashed",
                 color = "blue", size=1) +
      geom_hline(yintercept=threshold, linetype="dotted",
                 color = "red", size=1) +
      geom_point(data = streamsPeaks, aes(StartTimestamp, StartHeight),
                 color = "darkgreen", size=1.5, shape=24, fill = "darkgreen") +
      geom_point(data = streamsPeaks, aes(EndTimestamp, EndHeight),
                 color = "darkgreen", size=1.5, shape=25, fill = "darkgreen")


    #  Shade in the merged peaks
    if (NROW(mergedpeaks)) {
      peakplot <- peakplot +
        geom_hline(yintercept=merge.threshold, linetype="solid",
                   color = "red", size=1)
      streams$mPeaks <- NA
      for (i in 1:NROW(mergedPeaks)) {
        thePeak <- mergedPeaks[i,]
        streams$mPeaks[streams$Time >= thePeak$StartTimestamp &
                         streams$Time <= thePeak$EndTimestamp] <-
          i
      }
      streams$mPeaks <- as.factor(streams$mPeaks)
      peakplot <- peakplot +
        geom_area(
          data = drop_na(streams, mPeaks),
          aes(x = Time, y = ValueSmoothed, fill=mPeaks),
          alpha=0.15
        )
    }

    peakplot

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
