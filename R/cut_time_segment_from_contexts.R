#' @alias cut_time_segment_from_contexts
#'
#' @title Cut a time segment from contexts
#'
#' @description Removes all contexts in a context sequence in between
#' the specified times
#'
#' @author Neil Klepeis
#'
#' @param ... one more contexts sequences
#' @param start beginning time to start cutting
#' @param end ending time to stop cutting
#'
#' @return Returns a list of context sequences with a common
#' context segment removed
#'
#' @details
#'
#' This function removes contexts in the sequences between the
#'  'start' and 'end' times exclusive of the contexts at the start/end
#'  boundaries, i.e., it leaves any contexts that may occur at the
#'  'start' and 'end' times and removes any contexts
#'  in between those times.
#'
#'
#------------------------------------------------------

cut_time_segment_from_contexts <-
  function (..., start, end) {

    contexts <- list(...)

    contexts %>%
      map(~filter(Time > start & Time < end))

}
