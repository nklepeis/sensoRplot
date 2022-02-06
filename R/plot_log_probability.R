#' @name plot_log_probability
#'
#' @title Log probability plot
#'
#' @description Create an empty log-probability plot using ggplot
#'
#' @author Neil Klepeis
#'
#' @param xtics vector of x-axis (probability) tic values
#' @param ytics vector of y-axis (sample quantile) tic values
#' @param xlab x axis label, defaults to "Normal Cumulative Probability [%]"
#' @param ylab y axis label, default to "Sample Quantiles"
# @param data.raw tibble containing raw data to plot
# @param data.dist tibble containing GM and GSD's of distributions to plot
# @param data.bin tibble containing binned data to plot
#'
#' @details This function creates a log-probability plot without any
#' data. To add series, using the geom_log_probability_data,
#' geom_log_probability_lnorm, and geom_log_probability_bins geom functions to
#' add raw data, a lognormal distribution (with gm and gsd specifications), or
#' binned (histogram) data (counts and class interval limits).
#'
#' @examples
#'
#' s <- rlnorm(1000)
#' lfit <- fit_log_normal(s)
#' r <- hist(s, breaks=100)
#'
#' plot_log_probability(
#'     xtics=c(0.001,0.05,0.25, 0.5,0.75, 0.95, 0.999),
#'     ytics=c(0.05,100)) +
#' geom_qq(aes(sample=s)) +
#' geom_log_probability_lnorm(gm=lfit$geometric.mean,
#'                            gsd=lfit$geometric.standard.deviation) +
#' geom_log_probability_bins(h=r$counts, l=r$breaks)
# -------------------------------------------------

plot_log_probability <- function (xtics, ytics,
                                  xlab="Normal Cumulative Probability [%]",
                                  ylab="Sample Quantiles") {

  if (missing(xtics))
    xtics <- c(0.0000001, 0.000001, 0.00001, 0.0001, 0.001, 0.01,
      0.05 ,0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99, 0.999,
      0.9999, 0.99999, 0.999999, 0.9999999)

  if (missing(ytics))
    ytics <- c(0.1,1,10,100)

  #print(xtics)

  #p <- ggplot()

  #if (!is.null(data.raw)) {

  #p <- p + geom_qq(aes(sample=data.raw$Response))

  #}

  ggplot() +
    scale_y_log10(
      limits=c(min(ytics), max(ytics))
    ) +
    scale_x_continuous(
      breaks = qnorm(xtics),
      labels = signif(100*xtics, 5),
      limits=c(min(qnorm(xtics)), max(qnorm(xtics)))
    ) +
    xlab(xlab) +
    ylab(ylab)
}
