#' @aliases binary2timeline
#'
#' @title Convert an environmental pathway in binary row (wide) format to timeline
#' format
#'
#' @author Neil Klepeis
#'
#' @param data a date frame containing an environmental pathway
#' @param state.names the state names for an environment we expect to be
#' present in 'data'
#' @param time.var the expected name for the time column in
#' 'data' (defaults to "time")
#' @param units time units to use when encoding elapsed time.
#' @param ystart the y coordinate for the origin
#' @param yspacing the spacing between state lanes
#' @param yheight the height of timeline lanes (defaults to 1)
#'
#' @details This function converts a data frame containing
#' an enviroment flow in terms of rows of binary states ("Binary Wide" format) and
#' a time variable to a timeline-formatted dataframe with xstart,
#' xend, ystart, and yend for easy plotting.  Heights for each
#' state are set to unit = 1
#'
#'------------------------------------------------------------

# NOTE: This was taken from the contextModels package and renamed
#    from pathway2timeline to binary2timeline.   24Feb2021.  NK


binary2timeline <- function(data, time.var="Time", units="mins",
                             ystart=0, yspacing=0.25, yheight=1,
                            sep=":") {

  #require(tidyr)

  if (!is.data.frame(data))
    stop("'data' must be a data frame")
  else if (NROW(data) == 0) {
    warning("'data' is an empty data frame")
    return(NA)
  }

  if (!time.var %in% names(data))
    stop("`time.var' not found in col names.")

  #data <- data[order(data[,time.var]), ]
  data <- data[order(data[[time.var]]), ]
  eend <- data[[time.var]][NROW(data)]
  #  Get all names that don't have time.var, latency, elapsed
  #    in them

  # NOTE: We assume all variables not time, latency, elapsed, are state activity
  #   values and should be integers.....
  state.names <-
    names(data)[-grep(paste(time.var,"latency","elapsed",sep="|"),
                      names(data))]
  if (length(state.names) < 1) {
    warning("No detected states in 'data'")
    return(NA)
  } #else
    #print(state.names)

  newdata <- tibble()
  ybottoms <- ystart
  ytops <- ystart + yheight

  for (i in 1:length(state.names)) {
    b <- state.names[i]
    idx <- collapse(data[[b]])
    vals <- data[[b]][idx]
    xstarts <- data[[time.var]][idx]
    xends <- c(xstarts[-1], eend)

    # print(b)
    # print(vals)
    # print(xstarts)
    # print(xends)
    # print(ybottoms)
    # print(ytops)

    # TODO: 1.  Split stategroup name into state and group variables. done.
    #    2.  remove content == 0 states. only show "active" states == 1. done.

    newdata <- bind_rows(
      newdata,
      tibble(
        #state=b,
        group = strsplit(b,sep)[[1]][1],
        state = strsplit(b,sep)[[1]][2],
        content=vals,
        xleft=xstarts, xright=xends,
        ybottom=ybottoms, ytop=ytops,
        duration = as.numeric(difftime(xends, xstarts, units=units))
      ) %>%
        filter(content == 1)
    )

    ybottoms <- ytops + yspacing
    ytops <- ybottoms + yheight

  }

  names(newdata)[names(newdata) == "duration"] <-
    paste("duration",units,sep=".")

  newdata

}
