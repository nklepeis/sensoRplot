#' @title Find Stream Peaks
#'
#' @description This function identifies peaks in a
#' stream of sensor data
#'
#' @author Neil Klepeis
#'
#' @param streams a tibble of sensor data streams in standard
#' Time, Response, Value long format
#'
#' @return a tibble with time and value of peaks
#'
#' @details
#'
# peak finding packages and functions:
#
# pracma:  find_peaks
# scorepeak  detect_peaks
#
# ## use home grown routine to get "episodes" around peaks:
# 1.  look for maxima (peaks) above a certain threshold
# 2.  search before and after the peak for continuous segments
#       with values above a certain threshold
#
# ---------------------------------------------------------
#
# i_df_peaks <- findpeaks(i_df_na$Value,
#                         nups = 1,
#                         ndowns = 1,
#                         zero = "0",
#                         minpeakheight = i_df_mean,
#                         minpeakdistance = 5,
#                         thresh = 0,
#                         npeaks = 10,
#                         sortstr = TRUE)

