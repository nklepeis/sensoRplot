#' @name fine_stream_episodes
#'
#' @title Find Stream Episodes
#'
#' @description This function identifies episodes in a
#' stream of sensor data using the pracma findpeaks function
#' and loess pre-smoothing of the data
#'
#' @author David Torres-Mendoza and Neil Klepeis
#'
#' @param streams a tibble of sensor data streams in standard long format
#' with Time, Response, and Value variables
#' @param by the time interval to aggregrate the stream data for
#' some pre-smoothing before the loess smoothing, defaults to "30 secs"
#' @param nups findpeaks argument, min number of increasing steps before the peak, defaults to 1
#' @param ndowns findpeaks argument, min number of descreasing steps before the peak, defaults to 1
#' @param zerofindpeaks argument, how to intrepret succeeding steps of the same value, defaults to 0
#' @param minpeakdistance findpeaks argument, min distance in indices peaks have to be to be counted, defaults to 100
#' @param thresh findpeaks argument, min value to be counted as a peak, defaults to 0
#' @param npeaks findpeaks argument, number of peaks to return, defaults to 10
#' @param sortstr findpeaks argument, returned peaks sorted in decreasing order?, defaults to TRUE
#' @param plot logical, whether to make a plot of the found episodes
#'
#' @return a dataframe defining the identified stream episodes
#'
#' @details A stream episode is defined as a segment of time
#' in which data stream values rise from baseline to a peak and
#' fall back to baseline. If multiple responses are present in the passed
#' value only the first one is used to identify episodes.
#' The baseline is taken as the median of smoothed values.
#'
# ---------------------------------------------------------

#  From David's original Script:  4/12/2023

# library(tidyverse)
# library(lubridate)
# library(timetk)
# library(pracma)
# library(dplyr)
# library(zoo)

# #set working directory for David to run script
# dmt_directory <- "C:/Users/david.torres/ETR/CCAP - Documents/Objective 2/Tribal Partners/CTCP funded Tribal Projects_2-11-10/Enterprise/Air Monitoring/"
# working_directory <- dmt_directory
#
# #import sensor data
# sensor_data <- read_rds(file=paste(working_directory, "REPORTS/R SCRIPTS AND R DATA FILES/airMotive_Enterprise.rds", sep="")) %>%
#   filter(Response == "pm2.5_cf_1") %>%
#   filter(Time > as_datetime("2023-02-17 00:00:00", tz="America/Los_Angeles") &
#            Time < as_datetime("2023-03-04 00:00:00", tz="America/Los_Angeles"))

#filter sensor data to only show EP03 for testing purposes
# ep03_sensor_data <- sensor_data %>%
#   filter(Name == "EP03")

find_stream_episodes <- function (streams, nups=1, ndowns=1, zero="0",
                                  minpeakdistance=1,
                                  thresh=0, npeaks=10, sortstr=TRUE,
                                  by="30 seconds", plot=TRUE) {

  # TODO:  combine all columns that are not Time and Value
  #   into a single "Response" variable

  response <- unique(streams$Response)[1]
  streams <- streams %>%
    arrange(Time) %>%
    filter(Response == response)

  #summarize sensor data (minimally since smoothing takes care of a lot of the noise)
  data30 <- summarise_by_time(.data = streams,
                                        .date_var = Time,
                                        .by       = by,
                                        value  = signif(mean(Value),2)
  )

  #smoothing the sensor data
  streams$TimeUnix <- as.numeric(streams$Time)
  myloess_smoother <- loess(Value~TimeUnix,
                            span=.05,
                            data=streams)
  streams$ValueSmoothed <- predict(myloess_smoother,
                                   newdata = streams)


  #compute baseline using median PPM value to use for findpeaks function
  baseline <- median(streams$ValueSmoothed)

  cat("Computed minpeakheight = ", baseline, "\n")

  cat("Data:\n")
  print(streams)

  #find top 10 peaks in dataset and create a new dataframe with those peaks and corresponding info (start, end, max)
  streamsPeaks <- findpeaks(streams$ValueSmoothed,
                          nups = nups,
                          ndowns = ndowns,
                          zero = zero,
                          minpeakheight = baseline,
                          minpeakdistance = minpeakdistance,
                          thresh = thresh,
                          npeaks = npeaks,
                          sortstr = sortstr) %>%
    as.data.frame


  if (NROW(streamsPeaks)) {
    cat("Stream Peaks Found. \n")
    #print(streamsPeaks)

    #clean up column names in peaks df so its more readable
    colnames(streamsPeaks)[1] ="PeakHeight"
    colnames(streamsPeaks)[2] ="PeakPositionMax"
    colnames(streamsPeaks)[3] ="PeakPositionStart"
    colnames(streamsPeaks)[4] ="PeakPositionEnd"

    #this chunk of syntax finds the timestamp for each of the pieces of info in the peaks df since the function only
    #returns row numbers
    streamsPeaks <- streamsPeaks %>%
      mutate(MaxTimestamp = streams$Time[streamsPeaks$PeakPositionMax])
    streamsPeaks <- streamsPeaks %>%
      mutate(StartTimestamp = streams$Time[streamsPeaks$PeakPositionStart])
    streamsPeaks <- streamsPeaks %>%
      mutate(StartHeight = streams$ValueSmoothed[streamsPeaks$PeakPositionStart])
    streamsPeaks <- streamsPeaks %>%
      mutate(EndTimestamp = streams$Time[streamsPeaks$PeakPositionEnd])
    streamsPeaks <- streamsPeaks %>%
      mutate(EndHeight = streams$ValueSmoothed[streamsPeaks$PeakPositionEnd])


    #plot everything!
    if (plot) {
      peakplot <- ggplot(streams, aes(x=Time, y=ValueSmoothed)) +
        geom_line() +
        scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
        scale_x_datetime(breaks = scales::pretty_breaks(n = 6)) +
        geom_point(data = streamsPeaks, aes(MaxTimestamp, PeakHeight),
                   color = "red", size=2) +
        geom_hline(yintercept=baseline, linetype="dashed",
                   color = "blue", size=1) +
        geom_point(data = streamsPeaks, aes(StartTimestamp, StartHeight),
                   color = "darkgreen", size=1.5, shape=24, fill = "darkgreen") +
        geom_point(data = streamsPeaks, aes(EndTimestamp, EndHeight),
                   color = "darkgreen", size=1.5, shape=25, fill = "darkgreen")

      show(peakplot)
    }

    return(streamsPeaks)
  } else {
    show(ggplot(streams, aes(x=Time, y=ValueSmoothed)) +
      geom_line() +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
      scale_x_datetime(breaks = scales::pretty_breaks(n = 6)))
    cat("No peaks found.\n")
    return(NULL)
  }
}
