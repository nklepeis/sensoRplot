#' @name ta_addtimes
#'
#' @aliases ta_addtimes
#'
#' @title Insert events at time points in a tact object
#'
#' @description
#'
#' @author Neil Klepeis
#'
#' @details  Function to add (insert) times to a time-activity object, adjusting the
#' event and breaks specifications accordingly.
#' The new times must be within the original time boundaries.  All
#' event sets are adjusted.
#'




ta_addtimes <-
  function (newtimes, breaks, events, add.na.events=FALSE)
  {

    # Update.  ongoing...done   Option to allow extending the time range of the
    #	time-activity object by assigning NA's for events at times beyond the
    #	original time domain.   NK 28-Feb-2013

    # Function to add (insert) times to a time-activity object, adjusting the
    #   event and breaks specifications accordingly.
    # The new times must be within the original time boundaries.  All
    #  event sets are adjusted.

    #  This function inserts repeated events for specified times (i.e., 'expands'
    #  the time-activity object);  it is the
    #  opposite of the 'ta.coll' function, which 'collapses' repeated events.

    #  Returns a tact object.

    # Passed event boundaries and flags take the form of a histogram
    # with "breaks" = event boundaries, and "counts" = event flags.
    # `breaks' can also be a tact object (or coercable) with components
    # `breaks' and `events'.

    # NOTE:  problem when a newtimes exactly equals an existing break.
    #             and it is the only newtime.  Apparently fixed...

    if (missing(events) && is.list(breaks))
      data <- as_tact(breaks)
    else
      if (missing(events) && !is.list(breaks))
        data <- tact(breaks=breaks)
      else
        data <- tact(breaks=breaks, events=events)
      breaks <- data$breaks
      events <- data$events
      if (!is.numeric(newtimes))
        stop("`newtimes' must be numeric.")
      newtimes <- unique(sort(newtimes))

      # If we leave in the times that are out of bounds NA's will be introduced
      #   by the ta.samp function used below....  NK 1March2013
      if (!add.na.events) {
        if (min(newtimes) < min(breaks)) {
          warning("Lower bound in `newtimes' too low. Resetting to min(`breaks').")
          newtimes <- newtimes[newtimes > min(breaks)]
          newtimes <- c(min(breaks), newtimes)
        }
        if (max(newtimes) > max(breaks)) {
          warning("Upper bound in `newtimes' too high. Resetting to max(`breaks')")
          newtimes <- newtimes[newtimes < max(breaks)]
          newtimes <- c(newtimes, max(breaks))
        }
      }

      # Remove newtimes that duplicate current breaks; need character
      #     funny business to account for small floating point mismatches
      alltimes <- sort(as.numeric(unique(as.character(c(newtimes,breaks)))))

      # Return original tact object if newtimes are same as old breaks
      if (all(as.character(alltimes) %in% as.character(breaks)))
        return(tact(breaks=breaks, events=events))


      # Now insert the times and event flags.
      #   When sampling on top of a break with `ta.samp',
      #   the flag returned is between that break and the
      #   next highest one, except for the top break of course
      #   (left inclusive, right exclusive).  So the following works.

      breaks <- alltimes
      n <- length(breaks)
      for (i in 1:length(events)) {   # do each event set
        #print(ta.samp(breaks,data))
        events[[i]] <- ta.samp(breaks, data, set=i)[-n]  # returns NA if time is out of bounds
      }

      tact(breaks=breaks, events=events)

  }
