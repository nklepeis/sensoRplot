#' @title Convert Contexts from Active State Format
#'
#' @description This function converts a data frame of contexts in active-state
#' format to binary long format
#'
#' @author Neil Klepeis
#'
#' @param contexts tibble containing contexts in active-state format
#' @param allStates list containing all grouped states in the context data
#' Any states in the data not matched in \code{allStates} will be omitted from the output
#' @param sep the separator between contexts, defaults to "|"
#' @param auto.states logical, whether to automatically parse the \code{States}
#' variable to obtain the \code{allStates} grouped state list.
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
#'
#----------------------

#  Partly stolen from reformat.context.R  function...

## TODO:  Clean group and state names of "|" and ":" characters
###  Or allow them to be escaped..


convert_from_activestate <- function(contexts, allStates,
                                     sep="|", auto.states=TRUE) {

  if (missing(allStates))
    if (auto.states) {
      x <- stri_split_fixed(unlist(stri_split_fixed(contexts$States, "|")),
                            ":", simplify=TRUE)
      y <- as.list(x[,2])
      names(y) <- x[,1]
      allStates <- tapply(unlist(y, use.names = FALSE),
                          rep(names(y), lengths(y)), FUN = unique)
     }
  else stop("Specify 'allStates' as a list of groups with component states or specify 'auto.states' to parse States column for grouped states.")

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
            activeVars <- stri_trim_both(stri_split_fixed(States, "|")[[1]])
            values[match(activeVars, allVars)] <- 1L
        }

        tibble(Time, ..., Group, State, Value=values)

      }
    )



}
