#' @name is_tact
#'
#' @aliases is_tact
#'
#' @title Tests whether an object is a 'tact' object
#'
#' @description Tests if an object is a time-activity object (tact)
#'
#' @author Neil Klepeis
#'
#'
# ---------------------------------------------------------------

is_tact<-
  function(x)
    inherits(x, "tact")
