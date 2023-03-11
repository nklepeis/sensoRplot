#' @title Find Stream Episodes
#'
#' @description This function identifies episodes in a
#' stream of sensor data
#'
#' @author Neil Klepeis
#'
#' @param streams a tibble of sensor data streams in standard
#' Time, Response, Value long format
#' @param threshold the response value to use as a threshold when
#' identifying peaks and the extent of each episode
#' @param context if TRUE return in active-state context format
#' other return a tibble with start/end times for each episode
#'
#' @return a vector of breaks identifying stream episodes
#'
#' @details A stream episode is defined as a segment of time
#' in which data stream values contiuously occur above a specified
#' threshold. If multiple responses are present in the passed
#' value only the first one is used to identify episodes
#'
# ---------------------------------------------------------


find_stream_episodes <- function(streams, threshold, context=TRUE,
                                 group="Episodes", state="Episode") {

  if (missing(threshold))
    stop("Please provide a value threshold to use in defining peaks
         and the extent of episodes.")

  cat("Computing breaks of stream episodes...\n")

  response <- unique(streams$Response)[1]
  streams <- streams %>%
    filter(Response == response) %>%
    mutate(Episode = (Value - threshold) > 0)
  idx <- collapse(streams$Episodes)
  if (context)
    tibble(Time=streams$Time[idx],
           Group="Episodes",
           States=rep(c(state,""),
               length.out=length(streams$Time[idx])))
    else
      as_tibble(t(matrix(streams$Time[idx], nrow=2))) %>%
    rename(start=1, end=2)


}

