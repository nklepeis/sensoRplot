#' @name merge_by_time
#'
#' @title Merge Data by Time Interval
#'
#' @description This function provides a general way to merge data frames
#' with time-varying variables by matching individual times in \code{keys} with
#' time intervals in \code{data}, i.e., there is a match when an individual time
#' falls within a given time interval.
#'
#' @author Neil Klepeis
#'
#' @param data time-varying data, with Time variable and any number of other key or response variables
#' @param key time-varying data with key variables to merge into \code{data}
# @param multiple
#'
#' @seealso  As with the 'merge_streams_and_contexts' function, the 'left_join_by_time'
#'  function seems better suited and more efficient for joining data frames by time and
#'  is based on an existing tidyverse function 'left_join'
#'
#' @return a tibble containing data from \code{keys} merged into \code{data}
#'
#' @details This function expects both \code{data} and \code{key} to have a POSIXt column
#' named \code{Time}.  All other columns are retained in the merged result.
#'
#' The time in each row in the \code{keys} data frame is matched to
#' one or more *time intervals* in the \code{data} data frame.  Matching is defined by
#' the individual times in \code{keys} falling within time intervals defined by
#' successful times in \code{data}.
#'
#' Multiple rows in \code{keys} may match one or more rows in \code{data} so the
#' results may have duplicates of rows in either input data frames, i.e., a full_join
#' type of merging in the tidyverse.
#'
#' IMPORTANT:  The underlying assumption is that the times in \code{data} correspond
#' to instantaneous time with associated values and the time in \code{keys} correspond
#' to flags that indicate the activity of other variables values at a moment in
#' time, with activity changing at each subsequent time.   If \code{data} times are
#' too far apart (i.e., wider than intervals between successive times in \code{keys}), then
#' no data in \code{data} will be flagged with key (activity) data.
#'
#' TODO:  Do a left or right join so that duplicates of keys or data are omitted.
#'
#' @examples
#'
#' library(tidyverse)
#' library(future)
#' d <- tibble(Time=c(1,1,1,2,2,2,5,5,5,6,6),A=c(5,3,6,4,3,3,7,8,5,8,3))
#' k <- tibble(Time=c(1.5,2.2,3,4.1,4.1,5,5,5.3),B=c(10,20,40,50,60,70,80,90))
#' merge_by_time(d,k)
# ---------------------------------------------------

## ??  Can we use map_dpr to be faster????

#  TODO: Or use map and the (x >= s1 and x < s2)  approach instead of using
#      the rank-based solution, which may be slow, I don't know.....

# NOTE:  This function was intended to be used in merging stream data in time
#   with arbitrary context data in any format (active states, binary long/wide,
#    or "grouped state" format in which a number of activity variables with
#    mutually-exclusive states.)

# TODO:  Use the furrr package to speed up the function
#      with parallelization:
#   http://zevross.com/blog/2019/02/12/dramatically-speed-up-your-r-purrr-functions-with-the-furrr-package/

merge_by_time <- function(data, keys) {

  # sort keys by time
  keys <- keys %>% arrange(Time)

  dummy.keys <- keys %>% slice(1) %>% select(-Time) %>%
    mutate(across(.cols=everything(), .fns = ~ NA))

  #future_pmap_dfr(
  pmap_dfr(
    data,
    function(TimeD,...) {
      r <- rank(c(TimeD, keys$Time), ties.method="max")  # rank
      #cat("Rank = ", r, "\n")
      #if (r[1] > 1) {
      if (any((r[1] - 1) == r)) {
        idx <- r[which((r[1] - 1) == r)]
        idx <- seq(max(idx) - length(idx) + 1, max(idx), by=1)
      } else if (any(r[1] == r[-1])) {
        idx <- r[-1][which(r[1] == r[-1])]
        idx <- seq(max(idx) - length(idx), max(idx) - 1, by=1)
      } else idx <- 0
        ## TODO:   Merge two DIFFERENT rows. currently merged the same one TWICE...
        #idx <- rank(c(TimeD, keys$Time, ties.method="last"))[1] - 1
        #print(idx)
      #cat("idx = ", idx, "\n")
      if (all(idx > 0))
        tibble(Time=TimeD, ...) %>% bind_cols(keys %>% slice(idx) %>% select(-Time))
      else
        tibble(Time=TimeD, ...) %>% bind_cols(dummy.keys)
    }, .progress=TRUE
  )


}
