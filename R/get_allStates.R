#' @title Get all existing and selected context states
#'
#' @description Merge states collected from existing long-format contexts
#' and a list of selected states
#'
#' @author Neil Klepeis
#'
#' @param contextsLong a data frame containing long-format contexts
#' @param contextSets a list containing states from selected context sets
#'
#' @details
#'
# -------------------------------------------------

# Was unique to contextualizeR I think.  Now in sensoRplot!  NK
#    6/30/2023

# fixed.  incase contextsLong is empty, we jsut return the
#   contextSets.  NK 3/6/2023

get_allStates <- function(contextsLong, contextSets) {


  cat("GETTING ALL STATES...\n")
  cat("LONG CONTEXTS:\n")
  print(contextsLong)
  cat("SETS:\n")
  print(contextSets)

  autoAllStates <- list()
  #  States from binary long format
  for (g in unique(contextsLong$Group))
    autoAllStates[[g]] <-
    unique(contextsLong %>% filter(Group==g) %>% pull(State))

  cat("The current context states:\n")
  print(autoAllStates)

  #  Merge with selected states if specified
  allStates <- list()
  if (!missing(contextSets)) {
    if (length(autoAllStates)) {
      for (x in unique(c(names(contextSets), names(autoAllStates)))) {
        allStates[[x]] <-
          unique(c(contextSets[[x]],autoAllStates[[x]]))
      }
    } else allStates <- contextSets
  }

  allStates

}
