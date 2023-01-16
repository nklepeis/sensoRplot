#' @name is_mtact
#'
#' @aliases is_mtact
#'
#' @title Tests whether an object is an 'mtact' object
#'
#' @description Tests if an object is a multi-time-activity object (mtact)
#'
#' @author Neil Klepeis
#'
#'
# ---------------------------------------------------------------

is_mtact<-
  function(x)
    inherits(x, "mtact")
