#' @name tact_collapse
#'
#' @aliases tact_collapse
#'
#' @title Collapse time-activity objet
#'
#' @description
#'
#' @author Neil Klepeis
#'
#' @details Collapse events in one or more time-activity objects or
#' a multiple time-activity object.  Adjacent identical
#' events are converted into a single event, with new time breaks
#' corresponding to the previous lowest and highest values.
#'
#'
#'


tact_collapse <-
  function(...) {

    # Collapse events in one or more time-activity objects or
    # a multiple time-activity object.  Adjacent identical
    # events are converted into a single event, with new time breaks
    # corresponding to the previous lowest and highest values.

    # Only considers a single event `set' and drops the others.
    #  Updated.  We now do them all....see ta.coll()

    data <- list(...)
    if (length(data) == 1 && is.list(data[[1]][[1]])) data <- data[[1]]
    data <- as_mtact(data)

    for (i in 1:length(data))
      data[[i]] <- ta_coll(data[[i]])
    data
  }
