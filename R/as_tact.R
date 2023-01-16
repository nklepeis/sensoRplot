#' @name as_tact
#'
#' @aliases as_tact
#'
#' @title Coerce a list to an 'tact' object
#'
#' @description Convert list to a time-activity object (tact)
#'
#' @author Neil Klepeis
#'
#'
# ---------------------------------------------------------------

as_tact <-
  function (x)
  {
    if (is_tact(x))
      x
    else
      tact(x)
  }
