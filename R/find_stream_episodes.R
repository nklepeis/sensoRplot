#' @name find_stream_episodes
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
#' @param ndowns findpeaks argument, min number of decreasing steps before the peak, defaults to 1
#' @param zerofindpeaks argument, how to interpret succeeding steps of the same value, defaults to 0
#' @param minpeakheight min height to be identified as a peak, if 0, then auto-assigned as median value
#' @param minpeakdistance findpeaks argument, min distance in indices peaks have to be to be counted, defaults to 100
#' @param threshold min value identified peaks must be above their neighboring peaks, defaults to 0
# @param npeaks number of peaks to return, defaults to 0
#' @param merge.threshold threshold value above which all peaks will be
#' merged into a single peak, defaults to Inf
#' @param sortstr findpeaks argument, returned peaks sorted in decreasing order?, defaults to TRUE
#' @param plot logical, whether to make a plot of the found episodes
#' @param returnPlot, logical, whether to return the plot or the data
#' @param fill fill color to use for shading merged peaks
#'
#' @return a dataframe defining the identified stream episodes or, if merge.threshold
#' is not Inf or -Inf, as list with raw peaks data frame component and a mergedPeaks data frame
#' component
#'
#' @details A stream episode is defined as a segment of time
#' in which data stream values rise from baseline to a peak and
#' fall back to baseline. If multiple responses are present in the passed
#' value only the first one is used to identify episodes.
#' The baseline is taken as the median of smoothed values.
#'
# ---------------------------------------------------------

## now uses new find_peaks function taken from pracma with some tweaks
#    to streamline use in our application.  NK 4/13/2023

## TODO:    Auto-assign number of peaks based on continuous regions
#    above a certain threshold.    Merge peaks within these
#    continuous regions - so we have a hybrid peak-based and
#    threshold driven episode detection...

## TODO 2 :  make an interactive shiny front end that
#   allows for adjustments to parameters and seeing the
#   results in terms of identified episodes (color coded
#    segments)

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

find_stream_episodes <- function (streams,
                                  merge = TRUE,
                                  nups=1, ndowns=1,
                                  zero="0",
                                  minpeakheight=0,
                                  minpeakdistance=1,
                                  threshold=0,
                                  merge.threshold=Inf,
                                  sortstr=TRUE,
                                  by="30 seconds",
                                  plot=TRUE,
                                  returnPlot=FALSE,
                                  fill=rgb(0.4,0,0,0.15)) {

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

  if (minpeakheight <= 0)
    baseline <- median(streams$ValueSmoothed)
  else
    baseline = minpeakheight

  cat("Computed minpeakheight = ", baseline, "\n")

  cat("Data:\n")
  print(streams)

  #find top 10 peaks in dataset and create a new dataframe with those peaks and corresponding info (start, end, max)
  streamsPeaks <- find_peaks(streams$ValueSmoothed,
                          nups = nups,
                          ndowns = ndowns,
                          zero = zero,
                          minpeakheight = baseline,
                          minpeakdistance = minpeakdistance,
                          threshold = threshold,
                          #npeaks = npeaks,
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

    # this chunk of syntax finds the timestamp for each of the pieces of
    # info in the peaks df since the function only
    # returns row numbers
    streamsPeaks <- streamsPeaks %>%
      mutate(MaxTimestamp = streams$Time[streamsPeaks$PeakPositionMax])
    streamsPeaks <- streamsPeaks %>%
      mutate(StartTimestamp = streams$Time[streamsPeaks$PeakPositionStart])
    streamsPeaks <- streamsPeaks %>%
      mutate(StartHeight = streams$ValueSmoothed[streamsPeaks$PeakPositionStart])
    streamsPeaks <- streamsPeaks %>%
      mutate(EndTimestamp = streams$Time[streamsPeaks$PeakPositionEnd])
    streamsPeaks <- streamsPeaks %>%
      mutate(EndHeight = streams$ValueSmoothed[streamsPeaks$PeakPositionEnd]) %>%
    arrange(StartTimestamp)


    print(streamsPeaks)

    if (merge.threshold < Inf & merge.threshold > -Inf) {
      # Merge peaks using merge.threshold criterion
      #  "PeakHeight","PeakPositionMax","PeakPositionStart",
      #  "MaxTimestamp","StartTimestamp","StartHeight","EndTimestamp","EndHeight"
      # If
      i <- 1
      mergedPeaks <- data.frame()

      while (i > 0 & i <= NROW(streamsPeaks)) {

        cat("\nMAIN peak i = ", i, " (of ", NROW(streamsPeaks),")\n")
        thePeak <- streamsPeaks[i,]
        print(thePeak)

        if (thePeak$EndHeight > merge.threshold) {

          merge <- TRUE
          j <- i + 1

          while (merge & j <= NROW(streamsPeaks)) {

            cat("\n\n **  Sub peak j = ", j, "\n")
            nextPeak <- streamsPeaks[j,]
            print(nextPeak)
            if (nextPeak$StartHeight > merge.threshold) {
              thePeak$EndTimestamp <- nextPeak$EndTimestamp
              if (j >= NROW(streamsPeaks)) {
                merge <- FALSE
                i <- j + 1
              } else if (nextPeak$EndHeight > merge.threshold) {
                cat("Next Peak End is Above Thresh [j=j+1 Cont]\n")
                j <- j + 1
              } else {
                cat("Next Peak End is Below Thresh [i=j+i Main]\n")
                merge <- FALSE
                i <- j + 1
              }
            } else {
              cat("Next Peak Start is Below Thresh [i = j Main]\n")
              merge <- FALSE
              i <- j
            }

          }

          # We've gone through all the remaining subsequent peaks
          if (j > NROW(streamsPeaks))
            i <- i + 1

        } else {
          i <- i + 1
        }

        cat("\n\nMerged Peak: \n")
        print(thePeak)

        mergedPeaks <- mergedPeaks %>% bind_rows(thePeak)

      }

    }

    # ------------------------------------------------

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
        geom_hline(yintercept=threshold, linetype="dotted",
                   color = "red", size=1) +
        geom_point(data = streamsPeaks, aes(StartTimestamp, StartHeight),
                   color = "darkgreen", size=1.5, shape=24, fill = "darkgreen") +
        geom_point(data = streamsPeaks, aes(EndTimestamp, EndHeight),
                   color = "darkgreen", size=1.5, shape=25, fill = "darkgreen")


      #  Shade in the merged peaks
      if (merge.threshold < Inf & merge.threshold > -Inf) {
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


      if (returnPlot)
        return(peakplot)
      else show(peakplot)
    }

    if (merge.threshold < Inf)
      return(list(peaks=streamsPeaks, mergedPeaks=mergedPeaks))
    else
      return(streamsPeaks)

  } else {
    peakplot <- ggplot(streams, aes(x=Time, y=ValueSmoothed)) +
      geom_line() +
      geom_hline(yintercept=baseline, linetype="dashed",
                 color = "blue", size=1) +
      geom_hline(yintercept=threshold, linetype="dotted",
                 color = "red", size=1) +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
      scale_x_datetime(breaks = scales::pretty_breaks(n = 6))
    cat("No peaks found.\n")
    if (returnPlot)
      return(peakplot)
    else
    return(NULL)
  }
}
