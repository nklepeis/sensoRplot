#' @name contexts_from_stream_episodes
#'
#' @title Create Contexts from Stream Episodes
#'
#' @description This function creates contexts by
#' analyzing streams for episodes
#'
#' @author Neil Klepeis
#'
#' @param streams a tibble of sensor data streams in standard
#' Time, Response, Value long format
#' @param threshold the response value to use as a threshold when
#' identifying peaks and the extent of each episode
#' @param states names of states to assign to the
#'
#' @return a tibble defining contexts
#'
#' @details A stream episode is defined as a segment of time
#' in which data stream values continuously occur above a specified
#' threshold. If multiple responses are present in the passed
#' value only the first one is used to identify episodes
#'
# ---------------------------------------------------------


contexts_from_stream_episodes <- function(streams, threshold, states) {

  episodes <- find_stream_episodes(streams, threshold)



}
