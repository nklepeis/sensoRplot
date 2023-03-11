#' @name ta_coll
#'
#' @aliases ta_coll
#'
#' @title Collapse time-activity objet
#'
#' @description
#'
#' @author Neil Klepeis
#'
#' @details Collapse a time activity object so that there are no
#' repeated event flags in a row.  There can be single
#' flags repeated at different places in the series as
#' long as another, different flag separates them.
#'
#' In other words, we remove unnecessary time breaks in the
#'  time activity object.  Think of it as removing the
#'  unneeded "clutter".
#'


ta_coll <-
  function(breaks, events) {

    # Updated to collapse all events sets in the passed
    #   tact object, instead of a single selected one,
    #   collapsing each individual event set by itself
    #   and then merging all the event sets to produce the
    #   final tact object. Being careful not to drop
    #   any of the event set names.  27-Nov-2002

    # Updated to deal with NA's for events.  We convert them
    #  to "NA" type character so that they are treated as any
    #  other event code.  They are then converted back to
    #  regular NA's at the end.   24-Nov-2002.   Update to this:
    #  Just use the `exclude=NULL' option in factor to encode
    #  the NA's as another types of factor.....

    # Collapse a time activity object so that there are no
    # repeated event flags in a row.  There can be single
    # flags repeated at different places in the series as
    # long as another, different flag separates them.

    # In other words, we remove unnecessary time breaks in the
    #  time activity object.  Think of it as removing the
    #  unneeded "clutter".

    # Only the given event set is considered, the other sets
    # are ignored and dropped in the output.  Not any more, see above.

    if (missing(events) && is.list(breaks))
      data<-as_tact(breaks)
    else
      if (missing(events) && !is.list(breaks))
        data <- tact(breaks=breaks)
      else
        data <- tact(breaks=breaks, events=events)

      new <- list()
      for (i in 1:length(data$events)) {
        breaks <- data$breaks
        events <- as.character(data$events[[i]])
        ue <- unique(events)

        # events mapped to integers
        ievents <- as.integer(as.character(factor(events, levels=ue,
                                                  labels=1:length(ue),exclude=NULL)))

        if (any(diff(ievents)==0)) {
          # which ones are repeated
          omit <- which(diff(ievents)==0)
          events <- events[-omit]
          breaks <- breaks[-(omit+1)]
          new[[i]] <- tact(breaks=breaks, events=events)
          names(new[[i]]$events) <- names(data$events)[i]
        } else {
          new[[i]] <- ta_select(data, set=i)
        }
      }
      if (length(new) == 1)
        return(new[[1]])
      else
        return(tact_merge(as_mtact(new)))
  }
