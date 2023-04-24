#' @name plot_streams_peaks
#'
#' @title Plot identified peaks in a time series
#'
#' @description This function plots peaks that have bee
#' identified in a time series
#'
#' @author David Torres-Mendoza and Neil Klepeis
#'
#' @param x list containing the output of the find_streams_episodes
#'   function with values for streams, peaks, and parameters
#'
#' @return a ggplot object
#'
#' @details Create a ggplot of the streams superimposed with
#' identified peaks, threshold, and baseline
#'
#'----------------------------------------



plot_streams_peaks <- function(x) {

  if (NROW(x$streams) & NROW(x$peaks)) {

    peakplot <- ggplot(x$streams, aes(x=Time, y=ValueSmoothed)) +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
      scale_x_datetime(breaks = scales::pretty_breaks(n = 6)) +
      geom_line(aes(x=Time, y=Value),
                color="orange", linetype="dotted",
                size=0.65) +
      geom_line(size=0.85) +
      geom_point(data =x$peaks, aes(MaxTimestamp, PeakHeight),
                 color = "red", size=2) +
      geom_hline(yintercept=x$baseline, linetype="dashed",
                 color = "blue", size=1) +
      geom_hline(yintercept=x$threshold, linetype="dotted",
                 color = "red", size=1) +
      geom_point(data = x$peaks, aes(StartTimestamp, StartHeight),
                 color = "darkgreen", size=1.5, shape=24, fill = "darkgreen") +
      geom_point(data = x$peaks, aes(EndTimestamp, EndHeight),
                 color = "darkgreen", size=1.5, shape=25, fill = "darkgreen")

      if (NROW(x$streamsRaw))
        peakplot <- peakplot +
        geom_line(data=x$streamsRaw, aes(x=Time, y=Value),
                  color="maroon", linetype="dotted",
                  size=0.65)

    #  Shade in the merged peaks
    if (NROW(x$mergedPeaks)) {
      peakplot <- peakplot +
        geom_hline(yintercept=x$merge.threshold, linetype="solid",
                   color = "red", size=1)
      x$streams$mPeaks <- NA
      for (i in 1:NROW(x$mergedPeaks)) {
        thePeak <- x$mergedPeaks[i,]
        x$streams$mPeaks[x$streams$Time >= thePeak$StartTimestamp &
                         x$streams$Time <= thePeak$EndTimestamp] <-
          i
      }
      x$streams$mPeaks <- as.factor(x$streams$mPeaks)
      peakplot <- peakplot +
        geom_area(
          data = drop_na(x$streams, mPeaks),
          aes(x = Time, y = ValueSmoothed, fill=mPeaks),
          alpha=0.15
        )
    }

    peakplot

  } else if (NROW(x$streams)) {

    ggplot(x$streams, aes(x=Time, y=ValueSmoothed)) +
      geom_line() +
      geom_hline(yintercept=x$baseline, linetype="dashed",
                 color = "blue", size=1) +
      geom_hline(yintercept=x$threshold, linetype="dotted",
                 color = "red", size=1) +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
      scale_x_datetime(breaks = scales::pretty_breaks(n = 6))

  } else NULL

}
