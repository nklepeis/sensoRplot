#' @name hist2raw
#'
#' @title Simulate raw values from a counts and limits defining a histogram
#'
#' @author Neil Klepeis
#'
#' @param counts number of observation in each bin as defined by breaks
#' @param breaks histogram breaks (class interval limits)
#' @param simulate logical, whether to simulate new points inside each bin
# @param zero.values optional values to fix the distribution at zero probability
#' (zero counts)  no makes no sense
# @param model what model to use when simulating, e.g., "unif", "norm",
# "lnorm", "poisson", etc.   TBD.
#'
#' @return a tibble containing raw data simulated from a histogram
#'
#' @details Raw data are simulated in each histogram bin assuming
#' a uniform distribution withing the range of values defined by the
#' limits of each bin.
#'
#' The output of this function can be used to compute a kernel density estimation
#' to plot a smoothed histogram
#'
# --------------------------------------------------------

hist2density <- function(counts, breaks, simulate=TRUE) {

  data <-
    tibble(counts = counts / (sum(counts)), left=breaks[-length(breaks)],
           right=breaks[-1]) %>%
    mutate(counts = counts / min(counts)) %>%
    group_by(counts, left, right)

  if (simulate)
    data <- data %>% group_modify(
        ~ tibble(x=runif(.y$counts, .y$left, .y$right)),
        #~ tibble(x=rlnorm(.y$counts, gm=(.y$left + .y$right)/2),
        .keep=TRUE
    )
  else
    data <- data %>% group_modify(
        ~ tibble(x=rep((.y$right+.y$left)/2, length.out=.y$counts)),
      .keep=TRUE
    ) %>%
      bind_rows(tibble(counts=0, left=min(data$left), right=min(data$right), x=min(data$left))) %>%
      bind_rows(tibble(counts=0, left=max(data$left), right=max(data$right), x=max(data$right)))

  data

}
