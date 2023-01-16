#' @name t2m
#'
#' @aliases t2m
#'
#' @title convert hhmm format to elapsed minutes
#'
#' @description Convert time in hhmm format to minutes after midnight
#'
#' @author Neil Klepeis
#'
#' @details  Simple R function to convert time
#' in `hhmm' 24-h format to minutes after midnight;
#' if `increasing=TRUE', then the
#' times are interpreted as constantly
#' increasing (perhaps over more than
#' a single day) and the elapsed minutes
#' are calculated accordingly (i.e., by adding
#' i*1440 to times that occur on the i=0th,1st,2nd...
#' day).
#'
#'
# ---------------------------------------------------------------


t2m<-
  function (x, increasing=TRUE, split=TRUE, zero=FALSE) {

    # Update:  split hours:mins by ":" if split=TRUE (the default),
    #          make the elapsed minutes start at zero, if
    #          zero=TRUE, can only be TRUE if increasing is
    #          also TRUE.  NK-9/16/03

    # Simple R function to convert time
    # in `hhmm' 24-h format to minutes after midnight;
    # if `increasing=TRUE', then the
    # times are intrepreted as constantly
    # increasing (perhaps over more than
    # a single day) and the elapsed minutes
    # are calculated accordingly (i.e., by adding
    # i*1440 to times that occur on the i=0th,1st,2nd...
    # day).

    if (zero & !increasing) stop("Minutes can only be zeroed if `increasing'=TRUE.")

    n <- length(x)
    if (split) {
      #  list -> data.frame -> transposed matrix
      x <- t(data.frame(strsplit(x, split=":")))
      hours <- as.numeric(x[,1])
      mins <- as.numeric(x[,2])
    } else {
      x <- as.numeric(x)
      hours <- trunc(x / 100)
      mins <- 100 * (x/100 - hours)
    }

    if (any(hours < 0 | hours > 24))
      stop("Hours must be between 0 and 24, inclusive.")
    if (any(mins < 0 | mins > 59))
      stop("Minutes must be between 0 and 59, inclusive.")
    newmins <- hours*60 + mins

    # Note:  hour 24 is allowed even though
    # it is redundant with 0000

    if (increasing) {
      newday <- which(diff(newmins) < 0) + 1
      if (length(newday) >= 1)
        for (i in 1:length(newday))
          newmins[newday[i]:n] <- newmins[newday[i]:n] + 1440
    }

    if (zero) newmins <- newmins - newmins[1]
    newmins
  }
