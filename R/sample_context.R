#' @alias sample_context
#'
#' @title Sample Context Sequence
#'
#' @description Return context instances at arbitrary
#' specified times from a context sequence
#'
#' @author Neil Klepeis
#'
#' @param context a context sequences with a Time variable
#' @param times times used to sample contexts
#'
#' @return Returns a context sequence ampled at the specified
#' times
#'
#' @details
#'
#' A context sequence consists of state transitions occurring
#' at specific increasing times; each context's state configuration
#' persists until a new context is specified. A context persists until
#' a new context occurs with possible changes in state.
#'
#' This function takes a set of arbitrary times and returns
#' the corresponding contexts that occur at precise times.
#'
#' Sampled contexts in the sequence will duplicate the states in
#' the immediately preceding context.
#'
#' This function can operate on any context sequence format with a Time
#' variable, i.e., Binary Long, Binary Wide, Active State,
#' or Grouped state.
#'
#' If any of the specified times are less than the minimum time
#' in the sequence, they are silently ignored. If times are greater than
#' the maximum time in the sequence, then the last context
#' in the sequence is repeatedly assigned for each time.
#'
#' Note:  Times do not have to be Time.POSIX objects but can
#' represent any numeric time quantity (e.g., number of seconds,
#' minutes, etc.).
#'
#' @example
#'
#' df <- data.frame(Time=c(1,2,3,4,5,6),GrpA=LETTERS[1:6],GrpB=LETTERS[6:11])
#' sample_contexts(df,times=c(0.5,1,1.5,1.7,4.2,5.2,5.9,6,10))
#'
#--------------------------------------------------------------


sample_context <- function(context, times) {

  # If list is passed, then we use that
  #  otherwise compile arguments into a list of
  #   expected contexts
  #contexts <- list(...)
  #if (is.list(contexts[[1]]))
  #  contexts <- contexts[[1]]

  # Have a wasy to check for a list of contexts..

  times <- sort(times)

  #context %>%
  #  map(function(x) {
      new <- tibble()
      for (i in 1:length(times)) {
        j <- NROW(context) -
          length(context$Time[context$Time > times[i]])
        if (
          times[i] >= min(context$Time)) {
          new <- bind_rows(new,
                           context[j, ] %>%
                             mutate(Time = times[i]))
        }
      }

      new

   # })

}