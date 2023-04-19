#' @name find_peaks
#'
#' @title Find Peaks Based on pracma 'findpeaks' function
#'
#' @description This function finds peaks in a time series
#'
#' @author Hans W. Borchers and Neil Klepeis
#'
#' @param x a vector of numeric values with no NA values
#' @param nups min number of increasing steps before the peak, defaults to 1
#' @param ndowns min number of decreasing steps before the peak, defaults to 1
#' @param zero how to interpret succeeding steps of the same value, defaults to "0"
#' @param peakpat regular pattern to identify peaks, replaces nups/ndowns if specified
#' @param minpeakheight min height of identified peaks, defaults to -Inf
#' @param minpeakdistance min distance in indices peaks have to be to be counted, defaults to 1
#' @param threshold min value identified peaks must be above their neighboring peaks, defaults to 0
# @param npeaks number of peaks to return, defaults to 0
#' @param sortstr returned peaks sorted in decreasing order?, defaults to TRUE
#'
#' @return a dataframe defining the identified stream episodes
#'
#' @details This is based on the findpeaks function in the pracma package
#' by Hans W. Borchers with a few tweaks by Neil Klepeis to make it easier
#' to identify intuitive sensor stream episodes with minimal user specification
#' of parameters
#'
#' * Return all peaks
#' * Return NULL if no peaks are found
#'
#'
#------------------------------------------------------------

## see matlab version:  https://www.mathworks.com/help/signal/ref/findpeaks.html

##  See:  https://stackoverflow.com/questions/67698847/finding-peaks-with-minimum-peak-width-in-r-similar-to-matlab-function

##  https://cran.r-project.org/web/packages/cardidates/cardidates.pdf

## Removed npeaks argument, return NULL if no peaks.. NK 4/13/2023

find_peaks <-
function (x, nups = 1, ndowns = nups, zero = "0", peakpat = NULL,
          minpeakheight = -Inf, minpeakdistance = 1, threshold = 0,
          sortstr = TRUE)
{
  stopifnot(is.vector(x, mode = "numeric") || length(is.na(x)) ==
              0)
  if (!zero %in% c("0", "+", "-"))
    stop("Argument 'zero' can only be '0', '+', or '-'.")
  xc <- paste(as.character(sign(diff(x))), collapse = "")
  xc <- gsub("1", "+", gsub("-1", "-", xc))
  if (zero != "0")
    xc <- gsub("0", zero, xc)
  if (is.null(peakpat)) {
    peakpat <- sprintf("[+]{%d,}[-]{%d,}", nups, ndowns)
  }
  rc <- gregexpr(peakpat, xc)[[1]]
  if (rc[1] < 0)
    return(NULL)
  x1 <- rc
  x2 <- rc + attr(rc, "match.length")
  attributes(x1) <- NULL
  attributes(x2) <- NULL
  n <- length(x1)
  xv <- xp <- numeric(n)
  for (i in 1:n) {
    xp[i] <- which.max(x[x1[i]:x2[i]]) + x1[i] - 1
    xv[i] <- x[xp[i]]
  }
  inds <- which(xv >= minpeakheight & xv - pmax(x[x1], x[x2]) >=
                  threshold)
  X <- cbind(xv[inds], xp[inds], x1[inds], x2[inds])
  if (minpeakdistance < 1)
    warning("Handling 'minpeakdistance < 1' is logically not possible.")
  if (sortstr || minpeakdistance > 1) {
    sl <- sort.list(X[, 1], na.last = NA, decreasing = TRUE)
    X <- X[sl, , drop = FALSE]
  }
  if (length(X) == 0) {
    #return(c())
    cat("find_peaks: No peaks found with current arguments.\n")
    return(NULL)
  }
  if (minpeakdistance > 1) {
    no_peaks <- nrow(X)
    badpeaks <- rep(FALSE, no_peaks)
    for (i in 1:no_peaks) {
      ipos <- X[i, 2]
      if (!badpeaks[i]) {
        dpos <- abs(ipos - X[, 2])
        badpeaks <- badpeaks | (dpos > 0 & dpos < minpeakdistance)
      }
    }
    X <- X[!badpeaks, , drop = FALSE]
  }
  #if (npeaks > 0 && npeaks < nrow(X)) {
  #  X <- X[1:npeaks, , drop = FALSE]
  #}
  return(X)
}
