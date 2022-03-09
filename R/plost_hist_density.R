#' @name plot_hist_density
#'
#' @title Plot a density over an empirical histogram
#'
#' @author Neil Klepeis
#'
#' @param counts number of observation in each bin as defined by breaks
#' @param breaks histogram breaks (class interval limits)
#' @param show.hist logical, whether to also plot the histogram
#'
#' @details This function plots a smoothed histogram using
#' kernel density estimation.  Optionally the original histogram
#' is also drawn.
#'
#' Alternatives
#'
#' * Just use a Frequencey Plot (geom_poly) to connect all the histogram points
#' * Just overlap the histograms using an alpha channel
#' * Use the ggridges (density ridgeline plot) to show how distribution changes over time/size/etc.
#' * Use splines... or bezier
#'
#' ggridges is AWESOME!!  Use for showing probability distributions changing
#'   over time or as a function of source, distance, etc.
#'
#'  We can do several things to characterize a distribution:
#'
#'  *  histogram
#'  *  frequency plot
#'  *  density esimate
#'  *  mode, median, mean, sd, gsd
#'  *  quartiles...
#'
# --------------------------------------------------------

plot_hist_smoothed <- function (counts, breaks, show.hist=TRUE, show.density=TRUE,
                               show.freqpoly=TRUE, fill.freq=TRUE, trans="log10",
                               simulate=TRUE) {

    x <- hist2density(counts, breaks, simulate=simulate)

  #  counts, xleft, right, x
  #diffs <- diff(breaks)
  #prob <- (1/sum(counts)) * counts / (counts*diffs)

  g <- ggplot(data=x, aes(x=x, ..density..))
  #if (show.hist) g <- g + geom_bar(data=tibble(counts=counts, breaks=breaks[-length(breaks)]),
  #                                             aes(x=breaks, y=prob),
  #                                 stat="identity")
  if (show.hist) g <- g + geom_histogram(alpha=1, breaks=breaks)
  if (show.freqpoly) g <- g + geom_freqpoly(size=1, color="blue",
                                            breaks=breaks)

  if (fill.freq) g <- g + geom_area(fill = "blue", stat = "bin",
                                        breaks=breaks, alpha=0.3)

  g + geom_density(size=1, fill="red", color="red", alpha=0.3) +
    scale_x_continuous(trans=trans)

}
