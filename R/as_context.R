#' @aliases as_context
#'
#' @title Coerce data frame or tibble to a long-format 'context' data object
#'
#' @author Neil Klepeis
#'
#' @param data a dataframe or tibble containing Time, State, Value, and, optionally
#' other index variables to convert to a 'context' data type
#'
#' @details Returns a 'context' data object, a tibble with specific cols types, or
#' an error if the passed object cannot be coerced to a 'context' data type
# -------------------------------------------------------

as_context <- function(x) {

  x <- as_tibble(x)

  if (!all(c("Time","State","Value") %in% names(x)))
    stop("'x' must have 'Time', 'State', and 'Value' columns")

  x <- x %>%
    unite("Group", -c(Time, State, Value)) %>%
    select(Time, Group, State, Value)

  new_tibble(x, class="context")

  x

}
