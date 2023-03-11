#' @title Find Context Episodes
#'
#' @description This function identifies episodes from an
#' active-state context specification
#'
#' @author Neil Klepeis
#'
#' @param contexts contexts in active-state format
#'
#' @return a vector of breaks identifying context episodes
#'
#' @details A context episode is defined as a segment of time
#' in which some combination of states are active in between periods
#' of no state activity
#'
# ---------------------------------------------------------

### Taken from contextualizeR    3/10/2023 NK

find_context_episodes <- function(contexts) {


  cat("Computing context episodes...\n")

  startidx <- unique(c(1,
                       which(contexts$States=="")+1))

  startidx <- startidx[startidx <= NROW(contexts)]

  endidx <- unique(c(which(contexts$States==""),
                     NROW(contexts)))

  tibble(
    start = contexts$Time[startidx],
    end = contexts$Time[endidx]
  )

}

