#' @alias sample_contexts
#'
#' @title Sample Context Sequence
#'
#' @description Return context instances at arbitrary
#' specified times from a context sequence
#'
#' @author Neil Klepeis
#'
#' @param contexts a context sequence with a Time variable
#' @param times times used to sample contexts
#'
#' @return Returns a new context sequence having the specified
#' times
#'
#' @details
#'
#' A context sequence consists of states occurring
#' at specific increasing times, and persisting until a new
#' context is specified. A context persists until
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
#' sample_contexts(df,c(0.5,1,1.5,1.7,4.2,5.2,5.9,6,10))
#'
#--------------------------------------------------------------


sample_contexts <- function(contexts, times) {

  times <- sort(times)
  new <- tibble()
  for (i in 1:length(times)) {
    j <- NROW(contexts) -
      length(contexts$Time[contexts$Time > times[i]])
    if (
        times[i] >= min(contexts$Time)) {
      new <- bind_rows(new,
                       contexts[j, ] %>%
                         mutate(Time = times[i]))
    }
  }

  new
}
