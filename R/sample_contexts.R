#' @alias sample_contexts
#'
#' @title Sample Context Sequence
#'
#' @description Return contexts for arbitrary times from a
#' context sequence
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
#' A context sequence consists of contexts junction occurring
#' at specific increasing times. A context persists until
#' a new context occurs with changes in states.
#' This function takes a set of arbitrary times and returns
#' the corresponding contexts that occur at those times.
#'
#' This function can operate on any context sequence format with a Time
#' variable, i.e., Binary Long, Binary Wide, Active State,
#' or Grouped state.
#'
#' If any of the specific times are less than the minimum time
#' in the sequence, it is silently ignored.  If times are greater than
#' the maximum sequence time, then the last specific context
#' is assigned.
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
