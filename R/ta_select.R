#' @name ta_select
#'
#' @aliases ta_select
#'
#' @title Select sets of a tact object
#'
#' @description
#'
#' @author Neil Klepeis
#'
#' @details
#'
#'

ta_select <-
  function(x,  sets=1) {

    # Select one or more event sets from the provided
    # time-activity objects.

    # `sets' specifies the set(s) to select, defaulting to just the
    # first set

    # Returns a `tact' object

    x <- as_tact(x)

    names <- names(x$events[sets])
    if (!is.null(names))
      if (any(is.na(names)))
        stop("One or more NULL (nonexistent) sets specified for selection.")
    x$events <- x$events[sets]
    tact(x)

  }
