#' @aliases as_context
#'
#' @title Coerce data frame or tibble to a long-format binary
#' context data object
#'
#' @author Neil Klepeis
#'
#' @param x a dataframe or tibble containing Time, State, Value, and, optionally
#' other index variables to convert to a 'binary' 'long' 'context'
#' class of tibble.
#'
#' @details Returns a 'context' data object, a tibble with specific cols types, or
#' an error if the passed object cannot be coerced to a
#' tibble of class 'context' 'binary' 'long' data type
#'
#' TBD:  Try to convert 'x' in binary long, binary wide, or
#' active state or grouped state context to the standard (tidy!)
#' binary long context format.
#'
#' Do we have timeline context to binary converter?
# -------------------------------------------------------

as_context <- function(x) {

  x <- as_tibble(x)

  if (!all(c("Time","State","Value") %in% names(x)))
    stop("'x' must have 'Time', 'State', and 'Value' columns")

  x <- x %>%
    unite("Group", -c(Time, State, Value)) %>%
    select(Time, Group, State, Value)

  new_tibble(x, class=c("context","binary","long"))

  x

}
