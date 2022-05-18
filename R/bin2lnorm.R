#' @name bin2lnorm
#'
#' @title Fit and optinally plot a log-normal probability model to binned data (histogram)
#'
#' @description This function uses weighted least squares to fit and return a log-normal
#' probability model to binned data, i.e., counts of data points within
#' specific class intervals (a histogram).  Plots the results by default.
#'
#' @author Neil Klepeis
#'
#' @param breaks class interval limits
#' @param counts "height" values of the histogram (counts)
#' @param wt weights for each bin
#' @param plot logical, whether to plot the fit
#' @param density logical, whether to plot the density function or the linear regression plot
#' @param mcol model color (line)
#' @param dcol data color (points)
#' @param lty line type for model
#' @param lwd line width for model
#' @param shade.density density of shading for histogram (density)
#' @param shade.angle angle of shading for histogram (density)
#' @param rect.border.col border color for histogram (density)
#' @param rect.lwd line width for histogram (density)
#' @param rect.lty line type for histogram (density)
#' @param cex size of symbols for data
#' @param main title of the plot
#' @param xlab x label for the plot
#' @param ylab y label for the plot
#' @param ... additional arguments to the plot function
#'
#@param log logical, whether x axis will be log (log=TRUE) or linear\
#'
#' @returns A list containing
#' \code{counts}, \code{breaks}, \code{log(breaks)},
#' the probability density (PDF), the cumulative probabilities (CDF),
#' the normal quartiles, the geometric mean (GM), and the
#' geometric standard deviation (GSD)
#'
#' @details  As a side effect the function will plot the
#' result of the fit either as a density plot (probability versus value)
#' or as a cumulative distribution function (CDF).
#'
#' This function takes binned data (i.e., a histogram) and
#' fits a lognormal model to it, using weighted least-squares,
#' and optionally plotting the fit and the data together as a CDF.
#' If density = TRUE then it will plot a histogram with
#' the fitted lognormal function superimposed, otherwise it plots
#' a cdf (log-probability plot) with the fitted line through the
#' data points.
#'
#' This is similar to the 'method of quantiles' for
#' estimating the parameters of a log-normal distribution,
#' except that we use all of the data excluding the top point. See
#' Ott, 1995, page 268, "Environmental Statistics and Data
#' Analysis".
#'
#' @examples
#'
#' x <- hist(rlnorm(1000), breaks=c(0,2,5,10,25,100), plot=FALSE)
#' bin2lnorm(x$breaks, x$counts)
#'
# -------------------------------------------------

bin2lnorm<-
  function (breaks, counts, wt, plot=FALSE, density=FALSE,
            mcol="red", dcol="black", lty="solid", lwd=3,
            shade.density=15, shade.angle=35,
            rect.border.col=NULL, rect.lwd=1, rect.lty=1, cex=1.4, main,
            xlab, ylab, ...) {

    # This function takes binned data (i.e., a histogram) and
    # fits a lognormal model to it, using weighted least-squares,
    # and optionally plotting the fit and the data together as a CDF.
    # If density = TRUE then it will plot a histogram with
    # the fitted lognormal function superimposed, otherwise it plots
    # a cdf (log-probability plot) with the fitted line through the
    # data points.

    # ToDO:  Add possibility for normalization by log of limits
    # See the dX.dlogDp functions.....

    # Now allows for weighted least-squares, and outputs the results
    # of `lsfit()', i.e., the residuals and such

    # Now also outputs the normalization constant so that the
    # un-normalized pdf can be reconstructed

    # l is a vector containing the bin limits, h is the data
    # in each bin, e.g., the counts

    # Note:  We omit the probability=1 point(s) on the CDF.

    # This is kinda like the 'method of quantiles' for
    # estimating the parameters of a lognormal distributtion,
    # except we use all except the top point. See
    # Ott, 1995, page 268, "Environmental Statistics and Data
    # Analysis".

    # -> Modified to coerce input into numeric format

    counts<-as.numeric(counts)
    breaks<-as.numeric(breaks)

    if (!is.vector(breaks) || !is.vector(counts))
      stop("Count and bin specs must be numeric vectors (or coercable).")
    if (length(counts) != length(breaks)-1) {
      cat("Counts:\n")
      print(counts)
      cat("Breaks:\n")
      print(breaks)
      stop("Number of bins not equal to number of limits minus 1.")
    }
    if (any(diff(breaks) <= 0))
      stop("Limits must be strictly increasing.")
    if (any(breaks < 0))
      stop("Each limit must be zero or greater.")

    # Set weights=1 if missing
    if (missing(wt)) wt <- rep(1,length(counts))
    if (!missing(wt) & length(wt) != length(counts))
      stop("`wt', if specified, must contain weights corresponding to each bin with length the same as that of `h'")

    n<-length(breaks)

    pdf <- counts/(diff(breaks)*sum(counts))
    norm <- sum(counts)
    cdf <- cumsum(counts)/sum(counts)

    # omit where cdf=1 (could be multiple instances due to 0 counts)
    # also omit where cdf=0 (zero counts in beginning)
    lr <- breaks[-1]   #cdf is taken at right bin limits
    #print(lr)
    #print(cdf)
    # New by Neil to include all points.  3/14/2022
    #lr[lr==0] <- 1e-12
    #cdf[cdf==1] <- 0.999999999999
    #cdf[cdf==0] <- 1e-12

    #lr <- lr[cdf<1]
    #wt <- wt[cdf<1]
    #cdf <- cdf[cdf<1]  # do cdf last

    #  Old code to remove the points ad cdf=0 or 1
    lr <- lr[cdf<1 & cdf > 0]
    wt <- wt[cdf<1 & cdf > 0]
    cdf <- cdf[cdf<1 & cdf > 0]  # do cdf last

    q<-qnorm(cdf)
    ll <- log(lr)

    #print(q)
    #print(ll)

    ls.out<-lsfit(ll, q, wt=wt)
    intercept<-ls.out$coefficients[1]
    slope<-ls.out$coefficients[2]
    names(intercept)<-NULL
    names(slope)<-NULL

    gm <- exp((qnorm(0.5)-intercept)/slope)
    gsd <- exp((qnorm(0.8413)-intercept)/slope)/gm

    if (plot) {
      if (missing(main))
        main <- paste0("Bin2LN: GM = ", signif(gm,3), "; GSD = ", signif(gsd,3),
                      "; Norm = ",signif(norm,4))
      #   Some weirdness with resetting par on exit made the axes strange:
      #        1:10 for both
      #   old.par<-par(no.readonly=TRUE)
      #   on.exit(par(old.par))
      if (!density) {
        par(cex=cex, las=1)
        if (missing(xlab)) xlab <- "ln(Bin Limits)"
        if (missing(ylab)) ylab <- "Standard Normal Quantiles"
        plot(ll, q, type = "p", lty=lty, main=main, ylab=ylab, xlab=xlab,
             col=dcol, pch=16, axes=FALSE, ...)
        abline(intercept, slope, col=mcol, lwd=2)
      } else {
        if (missing(xlab)) xlab <- "Bin Limits"
        if (missing(ylab)) ylab <- "Probability Density"
        x<-seq(breaks[1],breaks[length(breaks)],
               by=(breaks[length(breaks)]-breaks[1])/100)
        y <- dlnorm(c(0,x), log(gm), log(gsd))
        par(cex=cex, las=1)
        plot.new()
        plot.window(xlim=c(0,max(l)), ylim=c(0,max(pdf,y)), ...)
        title(main=main, xlab=xlab, ylab=ylab)
        rect(l[1:(length(l)-1)], 0, l[-1], pdf, col=dcol, density=shade.density,
             angle=shade.angle, border=rect.border.col, lwd=rect.lwd,
             lty=rect.lty)
        points(c(0,x), y, type="l", lty=lty, lwd=lwd, col=mcol)
      }
      axis(1); axis(2); box()
    }

    list(lsfit=ls.out, counts=counts, breaks=breaks, logbreaks=ll, probabilities=pdf,
              cumulative.probabilities=cdf, normal.quantiles=q, geometric.mean=gm,
              geometric.standard.deviation=gsd, normalization=norm)
}
