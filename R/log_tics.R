#' @title Compute tics for a log10 axis
#'
#' @description Find nice log base-10 axis tics.  Values in x less than 0 are removed.
#' Can set axis tics to exact multiples of ten, e.g., 0.1, 1, 10, if \code{exact10},
#' or nearest 10^i increment, e.g., 0.2 or 0.3 (for i = -1) otherwise
#'
#' @author Neil Klepeis
#'
#' @param x a vector of data or data range for which tics will be created
#' @param exact10, logical, whether to compute exact base-10 tics
#'
#' @return a list containing the input data range (from x), the calculated tic range,
#' and the calculated major and minor tic values.
#'
# -------------------------------------------------

# Taken from heR.Misc  log.tics function.   31-Jan-2022

log_tics <-  function (x, exact10=TRUE) {

  x<-as.numeric(x)
  xlim <- range(x, na.rm=TRUE)

  if (any(x <= 0)) {
    warning("Some x data <= zero. Setting to NA.")
    x[x <=0] <- NA
    ok <- complete.cases(x)
    x<-x[ok]
    xlim <- range(x, na.rm=T)
  }

  x2 <- ifelse(xlim[2]<=0.1,trunc(log10(xlim[2])-0.0001),trunc(log10(xlim[2])+0.9999))
  x1 <- ifelse(xlim[1]>=10,trunc(log10(xlim[1])),trunc(log10(xlim[1])-0.9999))
  if (!exact10) {
    xlim[1] <- 10^x1*trunc(xlim[1]/10^x1)
    xlim[2] <- 10^(x2-1)*ceiling(xlim[2]/10^(x2-1))
  } else {
    xlim <- c(10^x1,10^x2)
  }
  tics.min <- c()
  tics.maj <- c()
  if (10^x1 >= xlim[1])
    tics.maj <- as.numeric(formatC(10^x1,format="fg"))
  for (i in x1:(x2-1)) {
    if (10^(i+1) <= xlim[2])
      tics.maj <- c(tics.maj, as.numeric(formatC(10^(i+1),format="fg")))
    f <- ifelse(i==x1, xlim[1] / 10^x1, 1)
    e <- ifelse(i==(x2-1), xlim[2], 10^(i+1)-10^i)
    tics.min <- c(tics.min, seq(f*10^i, e, by=10^i))
  }

  list(true.range=range(x), tic.range=range(tics.min),
       minor.tics=tics.min[-which(tics.min %in% tics.maj)],
       major.tics=tics.maj)

}
