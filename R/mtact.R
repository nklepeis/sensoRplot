#' @name mtact
#'
#' @aliases mtact
#'
#' @title Create a multi time-activity object
#'
#' @description Create an mtact object
#'
#' @author Neil Klepeis
#'
#' @details Creates a multiple time-activity object (mtact) from a number
#' of single time-activity (tact) objects (or coercable lists)
#' or a list of tact objects (or coercable lists).
#' Individual tact objects can be named either as <name>=list(...), or
#' by using the `names' argument, which takes precedent.
#'
#'
# ---------------------------------------------------------------

mtact <-
  function (..., names=NULL)
  {

    # Updated.  We no longer sort the returned unique events in
    #    in each event set (as an attribute).  27-Nov-2002

    # Updated to return the number of events sets per tact as
    #    an attribute.  22-Nov-2002

    # Creates a multiple time-activity object (mtact) from a number
    # of single time-activity (tact) objects (or coercable lists)
    # or a list of tact objects (or coercable lists).

    # individual tact objects can be named either as <name>=list(...), or
    # by using the `names' argument, which takes precedent.

    data <- list(...)
    # If we appear to have a list of lists (i.e., list of tact objects)
    # *** There is a problem with this if we try to pass a non-tact list
    # with events coming first as a list of event sets.  We need to
    # check for this case, presence of break/event components.

    if (length(data) == 1 && is.list(data[[1]][[1]])) data <- data[[1]]

    # check that each component is a tact object (or coercable)
    # and tally unique events (ue)  and break limits across
    # all components
    limits <- c(NA,NA)
    ue <- list()   # list for unique events in each event set
    nev <- vector(mode="numeric")   # number of event sets per tact
    for (i in 1:length(data)) {
      data[[i]] <- as_tact(data[[i]])
      # go over each event set, keeping names (which `unique' dumps)
      ev <- data[[i]]$events   # `events' is a list of all event sets
      for (j in 1:length(ev)) {
        if (is.null(ue[j][[1]]))
          ue[[j]] <- as.character(ev[[j]])
        else
          ue[[j]] <- c(ue[[j]], as.character(ev[[j]]))
        ue[[j]] <- ue[[j]][!duplicated(ue[[j]])]
      }
      limits <- c(min(limits[1],min(data[[i]]$breaks),na.rm=T),
                  max(limits[2],max(data[[i]]$breaks),na.rm=T))
      nev[i] <- length(ev)
    }
    if (!is.null(names)) {
      if (length(names) != length(data))
        stop("`names', if specified, must contain names corresponding to each component time-activity object.")
      names(data) <- names
    }
    attr(data,"unique.events") <- ue
    attr(data,"break.limits") <- limits
    attr(data,"event.sets") <- nev
    attr(data,"class") <- "mtact"
    data
  }
