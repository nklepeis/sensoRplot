#' @name tact_merge
#'
#' @aliases tact_merge
#'
#' @title merge two or more tact objects
#'
#' @description
#'
#' @author Neil Klepeis
#'
#' @details Function to 'merge' a number of time-activity objects, or a
#' multiple time-activity object, into a single time-activity object
#' that contains the separate events sets in the passed
#' object(s).  No information is lost; new time breaks
#' are introduced where appropriate.  The returned `tact' object
#' has the time range of the largest range that all of the passed
#' objects have in common. All `tact' objects, taken together,
#' must have some time in common.
#'



tact_merge <-
  function (..., intersection.only=TRUE)
  {

    # Updated: ongoing.   Option to include union of all outer times, *not* just intersection
    #	of times -- incorporates merging of objects that have no
    #	time in common (joining) - fill in NA for intermediate time
    #	between outer events.    NK 28-Feb-2013

    # Function to 'merge' a number of time-activity objects, or a
    # multiple time-activity object, into a single time-activity object
    # that contains the separate events sets in the passed
    # object(s).  No information is lost; new time breaks
    # are introduced where appropriate.  The returned `tact' object
    # has the time range of the largest range that all of the passed
    # objects have in common. All `tact' objects, taken together,
    # must have some time in common.

    #  If it is desired to pass a list that contains tact objects, this
    #  list must first be coerced to a mtact object.   NK  27-Nov-2002

    data <- list(...)
    if (is_mtact(data[[1]]))  # an mtact is passed
      data <- data[[1]]
    else
      data <- mtact(data)
    n <- length(data)
    if (n == 1) return(data[[1]])
    new <- data[[1]]
    for (i in 2:n) {
      temp <- data[[i]]
      if (!identical(as.character(min(temp$breaks)), as.character(min(new$breaks))) |
          !identical(as.character(max(temp$breaks)), as.character(max(new$breaks)))) {
        if (intersection.only) {
          min <- max(min(temp$breaks), min(new$breaks))
          max <- min(max(temp$breaks), max(new$breaks))
          if (min >= max)
            stop(paste("Time-activity objects have no time in common at object",i))
          temp <- ta_subset(c(min, max), temp)
          new <- ta_subset(c(min, max), new)
        }
      }
      #  ta.addtimes now will add NA events for times out of bounds. NK 1Mar2013
      temp <- ta_addtimes(new$breaks, temp, add.na.events=TRUE)
      new <- ta_addtimes(temp$breaks, new, add.na.events=TRUE)
      new$events <- c(new$events, temp$events)
    }
    tact(new)
  }
