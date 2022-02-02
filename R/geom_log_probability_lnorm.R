#' @name geom_log_probabilility_lnorm
#'
#' @title Add log-normal distribution to log probability plot
#'
#' @description Add log-normal distribution to an exisiting log-probability plot
#'
#' @author Neil Klepeis
#'
#' @param ... arguments to geom_line function
#'
#' @details
#'
# -------------------------------------------------

geom_log_probability_lnorm <- function (gm=1, gsd=exp(1), ...) {

  # adds a plot of a lognormal distribution (i.e., a line)
  # to a log-probability plot, given a geometric
  # mean and geometric standard deviation
  # X axis : normal probabilities
  # Y axis : data (model) quantiles

  #slope <- qnorm(0.8413)/log10(gsd)
  #intercept <- -qnorm(0.8413)*log10(gm)/log10(gsd)

  #slope <- qnorm(0.841344)/gsd
  #intercept <- qnorm(0.841344)/(2 - log(gsd)/log(gm))

  # *** Need to switch axes.. recompute slope/int since
  #   x,y are switched for this plot from the fit_log_normal fits..

  slope <- log10(gsd)/qnorm(0.841344)
  intercept <- qnorm(0.841344)*log10(gm)*slope/log10(gsd)

  #slope = - intercept / log(gm)



  # From fitting functionm -- slope and intercept in log space
  #gm <- exp((qnorm(0.5)-intercept)/slope)
  #gsd <- exp((qnorm(0.8413)-intercept)/slope)/gm

  #x1<-par("usr")[3]
  #x2<-par("usr")[4]
  #y1<-x1*slope + intercept
  #y2<-x2*slope + intercept

  geom_abline(slope=slope, intercept=intercept, size=1, color="red")

}
