#' @name tact
#'
#' @aliases tact
#'
#' @title Create a time-activity object (tact)
#'
#' @description Create a tact object
#'
#' @author Neil Klepeis
#'
#' @details Creates a tact (time-activity) object from a series
#' of numeric time breaks and one or more equal-length sets of event flags,
#' there should be one more break than events, and the times must be
#' strictly increasing.  The event flags can either be numeric
#' codes (possibly with names) or character labels.  If `hhmm=TRUE', then
#' the time breaks are interpreted as 24-h clocktime in 'HHMM' character format
#' where HH=hour and MM=minute (eg,14:12); otherwise the breaks are in
#' units of continuous time (e.g., either seconds, minutes, or hours).
#' The HHMM times are assumed to be increasing, and this cannot be
#' checked because, for example, "0012" could occur *after* "1203"
#' on a following day.
#'
#'
#'
# ---------------------------------------------------------------



tact <-

  function (breaks, events, hhmm=FALSE )

  {

    # UPDATE:   check processing of breaks to include decimals.  NK 3-March-2010

    # Creates a tact (time-activity) object from a series
    # of numeric time breaks and one or more equal-length sets of event flags,
    # there should be one more break than events, and the times must be
    # strictly increasing.  The event flags can either be numeric
    # codes (possibly with names) or character labels.  If `hhmm=TRUE', then
    # the time breaks are interpreted as 24-h clocktime in 'HHMM' character format
    # where HH=hour and MM=minute (eg,14:12); otherwise the breaks are in
    # units of continuous time (e.g., either seconds, minutes, or hours).
    # The HHMM times are assumed to be increasing, and this cannot be
    # checked because, for example, "0012" could occur *after* "1203"
    # on a following day.

    # If only `breaks' is given, then it is checked for list-hood with
    # two components: 'breaks' and 'events'.
    # If it is not a list, then it is interpreted as a simple vector, and 'events'
    # is assigned unique, increasing integers for each time interval.

    # The `events' component of the tact object is a list of equal-length
    # vectors (many times just one) containing the sets of event flags.

    # TODO:  Let's allow for compound `events', i.e., have `events' be a list
    # containing superimposed events, corresponding to the same time breaks. The
    # breaks would be specified to accurately describe the event limits across
    # all of the events in the compound event list, splitting a given event into
    # one or more breaks if it goes across breaks in another set of events. First
    # let's just allow for the storage of more than one event, and write code
    # for the combining of single-event tact objects (or coercables) later.

    # Update:  We can have problems with decimal numbers that are not what
    #          they seem.  So we do an as.numeric(as.character()) thingy
    #          on the breaks.   Do with hhmm=TRUE stuff also.  31-Jan-2003

    if (missing(events))
      if (is.list(breaks) && length(breaks) == 2) {
        if (any(is.na(match(c("events","breaks"),names(breaks)))))
          stop("List must contain `events' and `breaks' named components.")
        events <- breaks$events
        breaks <- breaks$breaks
      } else
        if (is.vector(breaks,mode="numeric") || is.vector(breaks,mode="character"))
          events <- list(1:(length(breaks)-1))
        else
          stop("`breaks' must be a vector or a list containing components for `breaks' and `events'.")

        # if necessary, convert HHMM format to elapsed minutes
        if (hhmm) {
          breaks <- as.numeric(as.character(t2m(breaks, increasing=T)))
        } else {
          breaks <- as.numeric(as.character(breaks))
        }

        # Breaks numeric, at least two numbers, and must be strictly increasing
        if (length(breaks) < 2 | any(diff(breaks) <= 0) | !all(is.numeric(breaks)))
          stop("`breaks' must contain at least two numeric values and be strictly increasing in time.")

        # Make sure events is a list (of one or more sets of event flags)
        if (!is.list(events)) events <- list(events)

        # Check length and convert event sets to character type
        for (i in 1:length(events)) {
          if (length(breaks) != length(events[[i]])+1)
            stop("`breaks' should be one element longer than `events'")
          events[[i]] <- as.character(events[[i]])
        }


        data <- list(breaks=breaks, events=events)
        attr(data,"class") <- "tact"
        data
  }
