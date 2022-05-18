#' @name geom_log_probabilility_bins
#'
#' @title Add binned data to log probability plot
#'
#' @description Add binned data to an exisiting log-probability plot
#'
#' @author Neil Klepeis
#'
#' @param breaks
#' @param counts
# @param ... arguments to geom_qq function
#'
#' @details
#'
# -------------------------------------------------

# TODO: Make this a real geom_* function.

# geom_log_probability_bins <- function (counts, breaks, size=2.3, color="red",
#         ...) {
#
#   # R function (geom) to add binned data (i.e., a histogram)
#   # to a log-probability plot (ggplot)
#   # Like bin2lnorm, omits cdf=1 point
#
#   if (!is.vector(counts) | !is.vector(breaks))
#     stop("`breaks' and `'counts' must be numeric vectors.")
#   breaks <- as.numeric(breaks)
#   counts <- as.numeric(counts)
#   if (length(counts) != length(breaks)-1)
#     stop("Number of bins not equal to number of limits minus 1.")
#   if (any(diff(breaks) <= 0))
#     stop("Limits must be strictly increasing.")
#   if (any(breaks < 0))
#     stop("Each limit must be zero or greater.")
#
#   cdf <- cumsum(counts)/sum(counts)
#   q<-qnorm(cdf)
#   q<-q[1:(length(q)-1)]
#   lr <- breaks[2:(length(breaks)-1)]
#
#   #print(lr)
#   #print(q)
#
#   geom_point(aes(x=q, y=lr), size=size, color=color,...)
#
#
# }


#GeomLPBinsPoint <- ggproto("GeomLPBinsPoint", GeomPoint,
#                        default_aes = aes(color = "black", size = 0.5, shape=16)
#)

geom_log_probability_bins <- function(mapping = NULL, data = NULL, stat="identify",
                                      position="identity", ..., breaks, counts,
                                      na.rm=FALSE,
                                      show.legend = NA, inherit.aes=TRUE) {

  if (!is.vector(counts) | !is.vector(breaks))
    stop("`breaks' and `'counts' must be numeric vectors.")
  breaks <- as.numeric(breaks)
  counts <- as.numeric(counts)
  if (length(counts) != length(breaks)-1)
    stop("Number of bins not equal to number of limits minus 1.")
  if (any(diff(breaks) <= 0))
    stop("Limits must be strictly increasing.")
  if (any(breaks < 0))
    stop("Each limit must be zero or greater.")

  cdf <- cumsum(counts)/sum(counts)
  q<-qnorm(cdf)
  q<-q[1:(length(q)-1)]
  lr <- breaks[2:(length(breaks)-1)]

  data <- data.frame(x = q, y = lr)

  geom_point(mapping=mapping, data=data, stat=stat,
              position=position, ...,
              na.rm=na.rm, show.legend=show.legend,
              inherit.aes=inherit.aes)

}

