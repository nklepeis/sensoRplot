#' @name ta_samp
#'
#' @aliases ta_samp
#'
#' @title Samps a portion of a tact object
#'
#' @description
#'
#' @author Neil Klepeis
#'
#' @details Function to return event flags for arbitrary values of
#' continuous time input (i.e., sample an event series),
#' given the boundaries of a time-activity event time series and
#' the corresponding event flags (i.e., the event codes or labels) --
#' where event boundaries and flags take the form of a histogram
#' with "breaks" = event boundaries, and "counts" = event flags.
#' 'breaks' can be a tact object (or coercable) with components
#' 'breaks' and 'events'.  `events' can be missing and will
#' be filled in by `tact' command.
#'
#'



ta_samp <-
  function (t, breaks, events, set=1) {

    # Update, Now if we have time equal to top break, we return an NA event to be
    #    consistent with lower bound indicated time in the future. This
    #   makes more logical sense.  NK  1March2013

    #  Update:  Fixed bug where it does not return NA's when samples
    #    are greater than the original break range.
    #    Now any t that is greater than the top
    #    original break is assigned an NA.  NK 10-Feb-04

    # Function to return event flags for arbitrary values of
    # continuous time input (i.e., sample an event series),
    # given the boundaries of a time-activity event time series and
    # the corresponding event flags (i.e., the event codes or labels) --
    # where event boundaries and flags take the form of a histogram
    # with "breaks" = event boundaries, and "counts" = event flags.
    # `breaks' can be a tact object (or coercable) with components
    # `breaks' and 'events'.  `events' can be missing and will
    # be filled in by `tact' command.

    # Returns NA if an input time is beyond the given boundaries
    # Sets sampled upper bound to event flag of last break, other
    #  sampled upper limits are assigned flags for the next break,
    #  i.e., the one for which they are a lower limit. So the
    #  limits are left inclusive, right exclusive, except when
    #  sampling the very highest break point (the highest
    #  upper bound).

    # `set' gives the event set to use, defaults to the firt (and
    # typically the *only* set).

    # Currently sets points sampled at top break to the last known
    #  event flag.  For other intervals, a sampled top break takes on the
    #  event of the next interval (i.e., left inclusive, right
    #  exclusive).

    #  Updated.  Did the same conversion of `breaks' and `t' to character and
    #       back to numeric to avoid weird floating point errors.
    #                --31-Jan-2003

    if (missing(events) && is.list(breaks))
      data <- as_tact(breaks)
    else
      if (missing(events) && !is.list(breaks))
        data <- tact(breaks=breaks)
      else
        data <- tact(breaks=breaks, events=events)
      breaks <- as.numeric(as.character(data$breaks))
      if (is.null(data$events[set]))
        stop("Selected event set does not exist.")
      else
        events <- data$events[[set]]
      t <- as.vector(as.character(t), mode="numeric")
      flag <- rep(NA, length(t))  # flag contains new event values
      if (is.null(names(events))) enames <- FALSE else enames <- TRUE
      if (enames) names(flag) <- rep(NA, length(t))
      for (i in 1:length(t)) {
        # get index of lower relevant boundary
        j <- length(breaks) - length(breaks[breaks > t[i]])
        if (t[i] >= max(breaks) | t[i] < min(breaks)) {
          flag[i] <- NA
          #} else if (j == length(breaks)) { # top break is included with last event
          #  flag[i] <- events[j-1]
          #  if (enames) names(flag)[i] <- names(events)[j-1]
        } else {
          if (enames) names(flag)[i] <- names(events)[j]
          flag[i] <- events[j]
        }
      }
      flag
  }
