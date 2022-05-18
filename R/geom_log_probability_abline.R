#' @name geom_log_probability_abline
#'
#' @title Add log-normal distribution to log probability plot
#'
#' @description Add log-normal distribution to an exisiting log-probability plot
#'
#' @author Neil Klepeis
#'
#' @param mapping
#' @param data
#' @param ...
#' @param gm geometric mean for the line
#' @param gsd geometric standard deviation for the line
#' @param na.rm
#' @param show.legend
#'
#' @details
#'
#' This geom is an extension of the \code{geom_abline} that takes gm and gsd
#' parameters and computes appropriate slope and intercept to plot the
#' line on a log-probability axis, representing a log-normal probability
#' distribution
#'
# -------------------------------------------------

#########geom_log_probability_lnorm <- function (gm=1, gsd=exp(1), size=1,
###############                                        color="red", ...) {

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

  #######slope <- log10(gsd)/qnorm(0.841344)
  ########intercept <- qnorm(0.841344)*log10(gm)*slope/log10(gsd)

  #slope = - intercept / log(gm)

  # From fitting functionm -- slope and intercept in log space
  #gm <- exp((qnorm(0.5)-intercept)/slope)
  #gsd <- exp((qnorm(0.8413)-intercept)/slope)/gm

  #x1<-par("usr")[3]
  #x2<-par("usr")[4]
  #y1<-x1*slope + intercept
  #y2<-x2*slope + intercept

  #####geom_abline(slope=slope, intercept=intercept, size=size,
       ####       color=color, ...)

#}


# To make a new Geom:

## See https://cran.r-project.org/web/packages/ggplot2/vignettes/extending-ggplot2.html

##  and https://bookdown.org/rdpeng/RProgDA/building-new-graphical-elements.html

#GeomLPAbline <- ggproto("GeomLPAbline", GeomAbline,
#                        default_aes = aes(color = "black", size = 0.5, linetype = 1,
#                        alpha = NA)
#)

geom_log_probability_abline <- function(mapping = NULL, data = NULL, ..., gm, gsd,
                       na.rm = FALSE, show.legend = NA) {

  slope <- log10(gsd)/qnorm(0.841344)
  intercept <- qnorm(0.841344)*log10(gm)*slope/log10(gsd)

  geom_abline(mapping=mapping, data=data, ..., slope=slope, intercept=intercept,
              na.rm=na.rm, show.legend=show.legend)

}

