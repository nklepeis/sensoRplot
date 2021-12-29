#' @aliases context_aggregate_time_spent
#'
#' @title Convert contexts in binary format to time spent in
#' regular intervals
#'
#' @author Neil Klepeis
#'
#' @param data a date frame containing contexts in binary long format

#' @details
#'
#' Strategy
#'
#' 1.  Insert context time points at regular intervals
#' 2.  Iterate over each interval computing total time in each group/state
#'
# ---------------------------------------

context_aggregate_time_spent <- function (data, by = "1 hour") {



}
