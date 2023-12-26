#' @alias collapse_contexts
#'
#' @title Collapse contexts with duplicate states
#'
#' @description Collapse context sequence so there are no
#' duplicated contexts in time
#'
#' @author Neil Klepeis
#'
#' @param ... one or more sequences of contexts to quantize
#'
#' @return Returns a list of all passed context sequences with
#' contexts with duplicated states removed
#'
#' @details
#'
#' @seealso distinct
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
