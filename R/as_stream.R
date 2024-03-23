#' @aliases as_stream
#'
#' @title Coerce data frame or tibble to a 'stream' data object
#'
#' @author Neil Klepeis
#'
#' @param x a dataframe or tibble containing Time, Value, and,
#' optionally other index variables to convert to a
#' 'stream' 'long' data type
#' @param index.vars TBD if TRUE and there is a "Response" variable
#' in addition to the Time and Value variables then keep any
#' extra variables, if any, as index variables.
#' If TRUE and no variables besides Time and Value
#' are named Response, bomdine all extra variables into a Response
#' variable.
#' If FALSE,t hen combine all extra variables to comprise
#' the "Response" variable, including any existing "Response"
#' variable.
#'
#'
#' variable keep the index variables, if there is no Response
#'
#' @details Returns a 'stream' data object, a tibble with specific
#' cols types, or an error if the passed object cannot be coerced
#' to 'stream' (tidy!) data type.  There must be a Time and Value
#' variables in 'x' and at least one other variable that can
#' serve as a Response, or multiple other variables that can
#' server as a Response or index variable.
# -------------------------------------------------------

as_stream <- function(x, index.vars=FALSE) {

  x <- as_tibble(x)

  if (!all(c("Time","Value") %in% names(x)))
    stop("'x' must have 'Time' and 'Value' columns")

  x <- x %>%
    unite("Response", -c(Time, Value)) %>%
    select(Time, Response, Value)

  new_tibble(x, class=c("stream", "long"))
  ## TBD add a list of index variables to the attributes

}
