#' @title Reformat Context
#'
#' @description This function converts a single context, consisting of a time and
#' list of grouped active states, to one of three different data frame formats:
#' "ActiveStates", "BinaryWide", or "BinaryLong"
#'
#' @author Neil Klepeis
#'
#' @param time starting time for the new context (required)
#' @param states a list with group elements containing
#' vectors of active states for the new context occurring at 'time'
#' @param allStates optional list with groups elements containing
#' vectors of ALL states for use with Binary formats
#' @param format a string giving the format for the conversion.  See Details.
#'
#' @return a list or tibble containing the reformatted context data
#'
#' @details The 'format' can be one of the following
#'
#'  * ActiveStates - a data frame with a Time column and a column with concatenated strings showing active states:  group1:state1a,state1b group2:state2a,state2b, ...
#'  * BinaryWide - a wide data frame with a Time column and columns for each grouped state with 0 or 1 cell values (inactive or active)
#'  * BinaryLong - a long data frame with Time, Group, State, and Value columns where value is 0 or 1 (inactive or active)
#'
# ------------------------------------------------

#  The ActiveStates format is really just for representing the context for
#    human consumption... the passed list is the easiest way to perform
#    computations...

#   Text time format="%Y-%m-%d %I:%M:%OS3 %p %Z")) %>%

reformat.context <- function(time, states, allStates=NULL, format="BinaryWide") {

  #require(tidyr)
  #require(stringi)

  if (format == "ActiveStates") {

    if (is.null(states)) catStates<- ""
    else if (length(states) > 0) {
      groups <- names(states)
      catStates <- paste(groups,
                         unlist(lapply(states, paste, collapse=",")),
                         sep=":", collapse=" | ")
    } else catStates <- ""

    tibble(
      Time=time,
      States=catStates
    )

  } else if (format == "BinaryWide" | format == "BinaryLong") {

    if (is.null(allStates))  stop("Must specify 'allStates' list.")

    allVars <- unlist(lapply(1:length(allStates),
                             function(i)
                               paste(names(allStates)[i],
                                     allStates[[i]], sep=":")))
    #  Keep spaces... tibble can handle it..
    #allVars <- stri_replace_all_fixed(allVars, " ", "")
    #  fix, the values need to be intergers to match database data type.  NK 8/2/2021
    values <- rep(0L, length.out = length(allVars))
    names(values) <- allVars

    #  if zero-length or NULL states, then keep all 0's
    if (!is.null(states))
      if (length(states) > 0) {
        groups <- names(states)
        activeVars <- paste(groups,
                            unlist(states),
                            sep=":")
        values[match(activeVars, allVars)] <- 1L
      }

    if (format == "BinaryWide") {
        as_tibble(as.list(as.integer(values))) %>%
        mutate(Time = time) %>%
        select(Time, everything())
    } else if (format == "BinaryLong") {
        tibble(Time = time,
               Group = stri_split(allVars,regex=":",simplify=TRUE)[,1],
               State = stri_split(allVars,regex=":",simplify=TRUE)[,2],
               Value=as.integer(values))
    }

  } else stop("'format' not recognized.")

}
