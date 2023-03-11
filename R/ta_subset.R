#' @name ta_subset
#'
#' @aliases ta_subset
#'
#' @title Extracts subset of a tact object
#'
#' @description
#'
#' @author Neil Klepeis
#'
#' @details Function to return a time subset of a time-activity object.
#' Returns a tact object that is a portion of the passed object given
#' two-values for the subset time limits (`range')

#' Passed event boundaries and flags take the form of a histogram
#' with "breaks" = event boundaries, and "counts" = event flags.
#' 'breaks' can also be a tact object (or coercable) with components
#' 'breaks' and 'events'.
#'
#'


ta_subset <-
  function (range, breaks, events, verbose=TRUE)
  {

    # Function to return a time subset of a time-activity object.
    # Returns a tact object that is a portion of the passed object given
    # two-values for the subset time limits (`range')

    # Passed event boundaries and flags take the form of a histogram
    # with "breaks" = event boundaries, and "counts" = event flags.
    # `breaks' can also be a tact object (or coercable) with components
    # `breaks' and `events'.

    if (missing(events) && is.list(breaks))
      data <- as_tact(breaks)
    else
      if (missing(events) && !is.list(breaks))
        data <- tact(breaks=breaks)
      else
        data <- tact(breaks=breaks, events=events)
      breaks <- data$breaks
      events <- data$events
      if (!is.numeric(range))
        stop("`range' must be numeric.")

      # To avoid comparisons between decimal numbers that aren't what they seem:
      breaks <- as.numeric(as.character(breaks))
      range <- as.numeric(as.character(range))


      if (diff(range) <= 0 | length(range) != 2)
        stop("`range' must contain a lower bound as its first element and an upper bound as its second element.")
      if (range[1] < min(breaks)) {
        if (verbose) warning("Lower bound in `range' too low. Resetting to min(`breaks').")
        range[1] <- min(breaks)
      }
      if (range[2] > max(breaks)) {
        if (verbose) warning("Upper bound in `range' too high. Resetting to max(`breaks')")
        range[2] <- max(breaks)
      }

      # Is range completely outside of existing breaks?  Then return NULL.
      if (diff(range) <= 0) {
        if (verbose) warning("Provided `range' is not inside existing breaks; Returning NULL.")
        return(NULL)
      }

      j1 <- length(breaks[breaks <= range[1]])
      j2 <- length(breaks[breaks < range[2]])

      t <- as.numeric(unique(as.character(c(breaks[j1:j2], range[2]))))
      t[1] <- as.numeric(range[1])


      for (j in 1:length(data$events))
        events[[j]] <- events[[j]][j1:j2]


      tact(breaks=t, events=events)

  }
