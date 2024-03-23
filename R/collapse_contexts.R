#' @alias collapse_contexts
#'
#' @title Collapse contexts with duplicate states
#'
#' @description Collapse context sequence so there are no
#' duplicated contexts in time
#'
#' @author Neil Klepeis
#'
#' @param ... one or more sequences of contexts to collapse
#'
#' @return Returns a list of all passed context sequences with
#' contexts with duplicated states removed
#'
#' @details Context sequence specifications contain transition
#' points where new contexts come into existence. Sometimes these
#' transitions duplicate the states in an immediately preceding
#' transition in which case they do not add any information to the
#' specification.
#'
#' This function removes these duplicated (redundant)
#' context transitions points, i.e., "collapses" the context
#' sequence to only the necessary poitns. No information is lost.
#'
#' This functions uses the dplyr 'distinct' function to remove
#' duplicated context transition points.
#'
#' @seealso \link{\code{distinct}}
#'
#' @example
#' df <- data.frame(Time=c(1,2,3,4,5,6),
#'       GrpA=c("A","A","A","B","B","B"))
#' collapse_contexts(df)
#'
# ---------------------------------------------------------

collapse_contexts <- function(...) {

  contexts <- list(...)

  contexts %>%
    map(~distinct(.x, across(-Time), .keep_all=TRUE))

}
