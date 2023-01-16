#' @name as_mtact
#'
#' @aliases as_mtact
#'
#' @title Coerce a list to an 'mtact' object
#'
#' @description Convert list to a multi-time-activity object (mtact)
#'
#' @author Neil Klepeis
#'
#'
# ---------------------------------------------------------------


as_mtact <-
  function (x)
  {
    if (is_mtact(x))
      x
    else
      mtact(x)
  }

# as.tact <-
#   function (x)
#   {
#     if (is.tact(x))
#       x
#     else
#       tact(x)
#   }
