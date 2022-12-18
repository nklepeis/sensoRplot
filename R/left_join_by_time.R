#' @name left_join_by_time
#'
#' @alias left_join_by_time
#'
#' @title Join a data frame to another according to row matches by time
#'
#' @description Take two data frames with time varying variables and
#' merge the second into the first based on the first's times falling
#' into the row-defined time intervals of the second
#'
#' @param x a data frame with a Time variable
#' @param y a data frame with a Time variable with columns to "join" or
#' merge with columns of 'y' matching by time interval
#' @param by extra variables to merge by (in addition to "Time"). See \code{\link{left_join}}
#'
#' @return a data frame with all columns of 'x' and columns from 'y' that
#' match time intervals in 'x'
#'
#' @author Neil Klepeis
#'
#' @details
#' First find the times of rows in 'x' that are encompassed by rows
#' rows in 'y' (reassigning 'y' times to those of the 'x' time match), then
#' get distinct rows and do a normal tidyverse 'left_join'
#' to merge the 'y' rows into the matching 'x' rows
#'
#' @examples
#'
#' library(tidyverse)
#' d <- tibble(Time=c(1,1,1,2,2,2,5,5,5,6,6),A=c(5,3,6,4,3,3,7,8,5,8,3))
#' k <- tibble(Time=c(1.5,2.2,3,4.1,4.1,5,5,5.3),B=c(10,20,40,50,60,70,80,90))
#' left_join_by_time(d,k)
# ----------------

#  TODO:  Allow merging by more key variables than just "Time".  done.

# WAS   match_time_segments2.... now renamed.   started join_by_time renamed
#     to left_join_by_time

#  This is the revised "tidyverse" version...  NK 5/15/2022


## Original function from heR.Misc  9Feb2022.   alternative to merge_by_time, which
##   seems kind of slow... Rewrite this to be more "tidyversey"
#    Have it merge a whole data frame with another one based on Time variables
#  like an alternative marge_by_time...   or   join_by_time...

##  data1 %>% match_time_segments(segments)

##  change to merge_time_segments ??

#  This loop solution may be faster than the purrr/furrr solution using
#    pmap_dpr.  We may be able to use map_dpr


left_join_by_time <-
  function(x, y, by=NULL)
  {

    #x$Time <- as.POSIXct(x$Time)
    #y$Time <- as.POSIXct(y$Time)

    y <- y %>% arrange(Time)  # sort increasing by Time

    result <- tibble()

    for (i in 2:NROW(y)) {
      idx <- x$Time >= y$Time[i-1] & x$Time < y$Time[i]
      result <- result %>%
        bind_rows(
          tibble(
            x[idx,] %>%
              select(Time) %>%
              bind_cols(
                y[i-1,] %>% select(-Time)
              )
          ) %>% distinct()
        )
    }


    #print(result)

    left_join(x, result, by=c("Time", by))

  }


## OLD   DOCS
#
# @name join_by_time
#
# @alias join_by_time
#
# @title Join a tibble to another tibble by time
#
# @description Merge tibble rows when times of a one tibble fall between times in another
# tibble
#
# @param data time-varying data, with Time variable and any number of other key or response variables
# @param key time-varying data with key variables to merge into \code{data}
# # @param multiple
# @return a tibble containing data from \code{keys} merged into \code{data}
#
# @details  This function uses a different algorithm than \code{merge_by_time}.
# Hopefully faster.
#
# @author Neil Klepeis
#
# @keywords join, merge

