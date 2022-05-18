#' @title Extract list of grouped states from a long context object
#'
#' @description This function extracts a list with named elements
#' for each group containing component states for each group
#'
#' @author Neil Klepeis
#'
#' @param contexts tibble containing contexts in long format
#' @param groupstates logical, whether to return states with embedded groups names or not

#'
#' @return a named list containing states belonging to each group
#'
#' @details
#'
# ----------------------------------------------------------------

#  Function to assign colors to each group:state

extract_groupstates <- function(contexts,
                                groupstates=TRUE,
                                sep=":") {

  contexts <- contexts %>%
    select(-c(Time,Value)) %>%
    distinct()

  if (groupstates)
    contexts <- contexts %>%
      mutate(GroupState = paste(Group,State,sep=sep)) %>%
      select(-State)

  #print(x)

  lapply(split(contexts, contexts[[1]]),
         function(y) y[,-1][[1]])


}
