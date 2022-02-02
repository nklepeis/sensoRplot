#' @name geom_log_probabilility_bins
#'
#' @title Add binned data to log probability plot
#'
#' @description Add binned data to an exisiting log-probability plot
#'
#' @author Neil Klepeis
#'
#' @param ... arguments to geom_qq function
#'
#' @details
#'
# -------------------------------------------------

# TODO: Make this a real geom_* function.

geom_log_probability_bins <- function (h, l, size=2.3, color="red",
        ...) {

  # R function (geom) to add binned data (i.e., a histogram)
  # to a log-probability plot (ggplot)
  # Like bin2lnorm, omits cdf=1 point

  if (!is.vector(h) | !is.vector(l))
    stop("`l' and `'h' must be numeric vectors.")
  l <- as.numeric(l)
  h <- as.numeric(h)
  if (length(h) != length(l)-1)
    stop("Number of bins not equal to number of limits minus 1.")
  if (any(diff(l) <= 0))
    stop("Limits must be strictly increasing.")
  if (any(l < 0))
    stop("Each limit must be zero or greater.")

  cdf <- cumsum(h)/sum(h)
  q<-qnorm(cdf)
  q<-q[1:(length(q)-1)]
  lr <- l[2:(length(l)-1)]

  #print(lr)
  #print(q)

  geom_point(aes(x=q, y=lr), size=size, color=color,...)


}
