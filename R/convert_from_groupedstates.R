#' @title Convert Contexts from Groups
#'
#' @description This function converts a data frame of contexts in "grouped state"
#' format") with columns containing state groups to binary long format
#'
#' @author Neil Klepeis
#'
#' @param contexts tibble containing contexts in active-state format
#' @param allStates list containing grouped states.
#' If missing and \code{auto.states = TRUE},
#' then the function parses columns to assign \code{allStates}
#' Any states in the data not matched in \code{allStates} will be omitted from the output
#' @param auto.states logical, automatically parse columns for grouped state list, defaults to TRUE
#'
#' @return a tibble containing contexts in binary long format
#'
#' @details The "grouped state" format is a special kind of "active state" format
#' in which columns contain occurrence of mutually-exclusive states within
#' each group. This was essentially the old "tact" format that was designed
#' with Location and NHAPS activity in mind, which were constrained to be
#' groups of mutually exclusive states (only a single activity or location
#' could occurr at a given time). This specification has columns indicating
#' which states are active for each time (row.)
#'
#' Time            |  Household  | Entertainment | Social
#' ----------------|:-----------:|:-------------:|:-------:
#' ________________|_____________|_______________|_________
#' 4/2/02 10:12 am |  Cooking    | TV            | Talking
#' 4/2/02 10:14 am |  Cooking    |               | Talking
#'
#' An alternative to this function would be to "unite" all the columns into
#' a single "active state" column and then use the \code{\link{convert_from_activestate}}
#' function
#'
#' @examples
#'
#' x <- tibble(Time = 1:10,
#'             Act=c("A","A","A","B","C","C","D","E","E","F"),
#'             Loc=c("","G","G","","H","H","H","I","I","I"))
#' convert_from_groupedstates(x)
#'
#'
#----------------------

#  Also see convert_from_activatestate.   This function is basically the
#    same idea, except we convert the columns to the States vars

# TODO:   Option to parse groups and states from existing
#      columns.  Done.

convert_from_groupedstates <- function(contexts, allStates,
                                       auto.states = TRUE) {

  if (missing(allStates))
    if (auto.states)
      allStates <- as.list(contexts %>% select(-Time)) %>%
        lapply(function (x) unique(x[x != ""]))
    else stop("Specify 'allStates' as a list of groups with component states or specify 'auto.states' to parse columns for grouped states.")

  # De-list a list with named elements for groups containing
  #    member states into a char vector of group:state elements.
  allVars <- unlist(lapply(1:length(allStates),
                           function(i)
                             paste(names(allStates)[i],
                                   allStates[[i]], sep=":")))

  cat("Unique Grouped States:\n")
  print(allVars)

  Group <- stri_split(allVars,regex=":",simplify=TRUE)[,1]
  State <- stri_split(allVars,regex=":",simplify=TRUE)[,2]

  #print(Group)
  #print(State)

  # Iterate over each context (row) and split into binary variables

  purrr::pmap_dfr(
    contexts,
    function(Time, ...) {

      p <- list(...)

      #  Keep spaces... tibble can handle it..
      # allVars <- stri_replace_all_fixed(allVars, " ", "")
      #  fix, the values need to be integers to match database data type.  NK 8/2/2021
      values <- rep(0L, length.out = length(allVars))
      names(values) <- allVars

      States <- paste(paste(names(p), as_tibble(p), sep=":"), collapse="|")

      #  if zero-length, then keep all 0's
      if (length(States) > 0) {
        activeVars <- stri_trim_both(stri_split_fixed(States, "|")[[1]])
        values[match(activeVars, allVars)] <- 1L
      }

      tibble(Time, Group, State, Value=values)

    }
  )



}
