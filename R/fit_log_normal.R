#' @name fit_log_normal
#'
#' @title Fit a log normal distribution to data
#'
#' @description Fit a log normal probability distribution to data
#'
#' @author Neil Klepeis
#'
#' @param y numeric data vector
#' @param forceNA whether to force values <= 0 to NA, defaults to \code{FALSE}
#' @param plot whether to plot the result, defaults to \code{TRUE}
#'
#' @returns A list with elements containing the fitted \code{geometric.mean},
#' the fitted \code{geometric.standard.deviation},
#' the computed \code{probabilities}, and the \code{log.y.values}.
#'
#' @details This function takes a data vector y and
#' fits a log-normal probability model to it, optionally plotting
#' the fit and the data together as a CDF. i.e.,
#' plots a CDF (log-probability) with the fitted line through the
#' data points.   Based on \code{qqplot}/\code{qnorm} R functions.
#'
# -------------------------------------------------

# This was originally the data2lnorm function in heR.Misc.


fit_log_normal <-  function (y, forceNA = TRUE, plot=FALSE) {

    if (forceNA) NA->y[y<=0]
    y <- y[!is.na(y)]
    if (0 == (n <- length(y)))
      stop("Sample vector is empty.")
    ylim<-range(y)
    if (ylim[1] < 0 )
      stop("Samples values must be greater than zero.")

    q <- qnorm(ppoints(n))[order(order(y))]
    ly<-log(y)

    ls.out<-lsfit(ly, q)
    intercept<-ls.out$coefficients[1]
    slope<-ls.out$coefficients[2]
    names(intercept)<-NULL
    names(slope)<-NULL

    gm <- exp((qnorm(0.5)-intercept)/slope)
    gsd <- exp((qnorm(0.841344)-intercept)/slope)/gm

    main <- paste("Lognormal Fit to Data: ( GM=",
                  format(gm,digits=3), ", GSD=",
                  format(gsd,digits=3),")")

    if (plot) {
      xlab <- "Log(Data Values)"
      ylab <- "Standard Normal Quantiles"
      plot(q, ly, type = "p", main=main)
      abline(intercept, slope)
    }

    list(geometric.mean=gm, geometric.standard.deviation=gsd,
         probabilities=q, log.y.values=ly)

}
