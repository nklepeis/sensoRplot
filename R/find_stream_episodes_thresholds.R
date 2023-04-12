#' @name fine_stream_episodes_thresholds
#'
#' @title Find Stream Episodes Using Simple Thresholds
#'
#' @description This function identifies episodes in a
#' stream of sensor data
#'
#' @author Neil Klepeis
#'
#' @param streams a tibble of sensor data streams in standard
#' Time, Response, Value long format
#' @param lower the response value to use as a lower bound when
#' identifying peaks and the extent of each episode
#' @param upper the response value to use as an upper bound when
#' identifying peaks and the extent of each episode
#' @param context if TRUE return in active-state context format
#' other return a tibble with start/end times for each episode
#' @param states a list of with names consisting of groups and
#' elements consisting of state names within each group
#'
#' @return a vector of breaks identifying stream episodes
#'
#' @details A stream episode is defined as a segment of time
#' in which data stream values contiuously occur above a specified
#' threshold. If multiple responses are present in the passed
#' value only the first one is used to identify episodes
#'
# ---------------------------------------------------------

#   rewritten to have lower/upper bounds 3/13/2023

##  Add stats ...
##  Add grouped state format for contexts...
###   No just use this to identify the episodes, then another function
#       to compute stats...

find_stream_episodes_thresholds <- function(streams, lower, upper, context=TRUE,
                                 states=list(Episodes="Episode")) {

  if (missing(lower))
    stop("Please provide a lower bound to use in defining peaks
         and the extent of episodes.")

  if (missing(upper)) {
    upper <- max(streams$Value)
    warning("'upper' not given - Using max response value as in defining peaks
         and the extent of episodes.")
  }

  # from reformat.context....
  if (is.null(states)) catStates<- ""
  else if (length(states) > 0) {
    groups <- names(states)
    catStates <- paste(groups,
                       unlist(lapply(states, paste, collapse=",")),
                       sep=":", collapse=" | ")
  } else catStates <- ""

  cat("Computing breaks of stream episodes...\n")

  response <- unique(streams$Response)[1]
  streams <- streams %>%
    arrange(Time) %>%
    filter(Response == response) %>%
    mutate(Episode = (Value - lower >= 0) & (-Value + upper) >= 0)
  #  Get starting points of episodes,
  #    * Remove leading FALSE
  #    * Add trailing FALSE
  idx <- collapse(streams$Episode)
  lastTime <- tail(streams$Time, 1)
  streams <- streams[idx,]
  if (!streams$Episode[1]) streams <- streams[-1,]  # remove FALSE leader
  if (tail(streams$Episode, 1))  # TRUE is last starting point? add ending FALSE
    streams <- streams %>%
    bind_rows(tail(streams, 1) %>%
                mutate(Time = lastTime,
                       Episode = FALSE))
  cat("Breaks (Starting points of episodes):\n")
  print(streams)

  if (context)
    tibble(Time=streams$Time,
           States = if_else(streams$Episode,
                            catStates,
                            ""
           )
    )
  else
    tibble(
      start = streams$Time[seq(1, NROW(streams)-1, by=2)],
      end = streams$Time[seq(2, NROW(streams), by=2)]
    )

  #States=rep(c(paste(group,state,sep=":"), ""),
  #length.out=length(streams$Time[idx])))
  # else
  #   as_tibble(t(matrix(streams$Time[idx], nrow=2))) %>%
  # rename(start=1, end=2)


}

