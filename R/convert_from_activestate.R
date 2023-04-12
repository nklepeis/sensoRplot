#' @title Convert Contexts from Active State Format
#'
#' @description This function converts a data frame of contexts in active-state
#' format to binary long format
#'
#' @author Neil Klepeis
#'
#' @param contexts tibble containing contexts in active-state format
#' @param allStates list containing all grouped states in the context data
#' Any states in the data not matched in \code{allStates} will be omitted from the output.
#' Use \code{auto.states=TRUE} to automatically parse states from the \code{States} column.
#' @param sep the separator between contexts, defaults to "|"
#' @param auto.states logical, whether to automatically parse the \code{States}
#' variable to obtain the \code{allStates} grouped state list.
#' @param collapse, logical, whether to collapse the activities to
#' non-repeated elements.   See \code{\link{collapse}}.
#' @param add.empty.context logical, whether to add a context to the end of the
#' contexts in which no states are active
#'
#' @return a tibble containing contexts in binary long format
#'
#' @details The input active-state contexts should have a Time
#' column, a States column, and any additional index columns.
#'
#' As illustrated below, the \code{States} column has a number
#' of Group:State pairs separated by the \code{sep} character,
#' defaulting to "|".
#'
#' Time            |  States
#' ----------------|--------------------------------
#' ________________|________________________________
#' 4/2/02 10:12 am |  Cooking:Frying\|Social:Talking
#' 4/2/02 10:14 am |  Cooking:Frying
#' 4/2/02 10:24 am |
#'
#' When \code{collapse=TRUE}, the function reduces the context
#' data, removing all rows with repeated (redundant) context states.
#'
#' @examples
#'
#' x <- tibble(Time = 1:4, States=c("A:One", "A:Two", "B:Three", ""))
#' convert_from_activestate(x)
#'
#----------------------

#  Partly stolen from reformat.context.R  function...

## TODO:  Clean group and state names of "|" and ":" characters
###  Or allow them to be escaped..


convert_from_activestate <- function(contexts, allStates,
                                     sep="|", auto.states=TRUE,
                                     collapse=TRUE,
                                     add.empty.context=TRUE) {

  #  Add an empty final contexts BEFORE we collapse?
  if (add.empty.context)
    contexts <- contexts %>%
      bind_rows(
        contexts %>% slice_tail() %>%
          mutate(Time = Time + 1) %>%
          mutate(across(.cols=!Time, .fns = ~ ""))
      )

  # Remove repeated context states to create a minimized
  #   context data set
  if (collapse) {
    idx <- collapse(contexts$States)
    contexts <- contexts[idx, ]
  }

  if (missing(allStates))
    if (auto.states) {
      x <- stri_split_fixed(unlist(stri_split_fixed(contexts$States, sep)),
                            ":", simplify=TRUE)
      x <- matrix(x[!apply(x == "", 1, all), ], ncol=2)   # remove rows with empty cells
      y <- as.list(x[,2])  # TODO Remove "" values
      names(y) <- x[,1]
      allStates <- tapply(unlist(y, use.names = FALSE),
                          rep(names(y), lengths(y)), FUN = unique)

      #allStates <- lapply(allStates, function(x) x[!x %in% omit])
     }
  else stop("Specify 'allStates' as a list of groups with component states or specify 'auto.states' to parse States column for grouped states.")

  cat("\nAll States:\n")
  print(allStates)

  # De-list a list with named elements for groups containing
  #    member states into a char vector of group:state elements.
  allVars <- unlist(lapply(1:length(allStates),
                           function(i)
                             paste(names(allStates)[i],
                                   allStates[[i]], sep=":")))

  Group <- stri_split(allVars,regex=":",simplify=TRUE)[,1]
  State <- stri_split(allVars,regex=":",simplify=TRUE)[,2]

  # Iterate over each context (row) and split into binary variables

    purrr::pmap_dfr(
      contexts,
      function(Time, States, ...) {

        #  Keep spaces... tibble can handle it..
        # allVars <- stri_replace_all_fixed(allVars, " ", "")
        #  fix, the values need to be integers to match database data type.  NK 8/2/2021
        values <- rep(0L, length.out = length(allVars))
        names(values) <- allVars

        #  if zero-length, then keep all 0's
        if (length(States) > 0) {
            activeVars <- stri_trim_both(stri_split_fixed(States, sep)[[1]])
            values[match(activeVars, allVars)] <- 1L
        }

        tibble(Time, ..., Group, State, Value=values)

      }
    )



}
