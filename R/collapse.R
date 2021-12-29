#' @aliases collapse
#'
#' @title Get index that collapses a vector to nonrepeated elements
#'
#' @author Neil Klepeis
#'
#' @param x a vector
#'
#' @details Returns an index that can be used to collapse the
#  vector to nonrepeated elements for use in computing
#  sequences of constant values.
#'
# -----------------------------------------------

# Note:  This is same version as in ContextuallizeR (and probably contextModels)
#     24Feb2021

# Function, taken from heR.Misc, to collapse a vector.
#  gives an index that can be used to collapse the
#  vector to nonrepeated elements, for use in computing
#  sequences of constant values...
collapse <- function (x)
{
  x <- as.integer(as.character(factor(x, levels = unique(x),
                                      labels = 1:length(unique(x)), exclude = NULL)))
  x <- rev(x)
  alli <- 1:length(x)
  omit <- which(diff(x) == 0)
  keepi <- length(x) + 1 - which(!alli %in% omit)
  rev(keepi)
}
