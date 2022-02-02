#' @title Convert Contexts from Active State Format
#'
#' @description This function converts a data frame of contexts in active-state
#' format to binary long format
#'
#' @author Neil Klepeis
#'
#' @param contexts tibble containing contexts in active-state format
#' @param allStates list containing group elements with all states to match
#' @param sep the separator between contexts, defaults to "|"
#'
#' @return a tibble containing contexts in binary long format
#'
#' @details The input active-state contexts should have a Time
#' column, a States column, and any additional index columns.
#'
#----------------------

#  Partly stolen from reformat.context.R  function...


convert_from_activestate <- function(contexts, allStates,
                                     sep="|") {

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
