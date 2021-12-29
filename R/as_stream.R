#' @aliases as_stream
#'
#' @title Coerce data frame or tibble to a 'stream' data object
#'
#' @author Neil Klepeis
#'
#' @param data a dataframe or tibble containing Time, Value, and, optionally
#' other index variables to convert to a 'stream' data type
#'
#' @details Returns a 'stream' data object, a tibble with specific cols types, or
#' an error if the passed object cannot be coerced to 'stream' data type
# -------------------------------------------------------

as_stream <- function(x) {

  x <- as_tibble(x)

  if (!all(c("Time","Value") %in% names(x)))
    stop("'x' must have 'Time' and 'Value' columns")

  x <- x %>%
    unite("Response", -c(Time, Value)) %>%
    select(Time, Response, Value)

  new_tibble(x, class="stream")

}
