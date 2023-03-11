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
# @param context if TRUE return in active-state context format
# other return a tibble with start/end times for each episode
#' @param group name of group to use when assigning context
#' @param state name of state to use when assigning context
#'
#' @return a vector of breaks identifying stream episodes
#'
#' @details A stream episode is defined as a segment of time
#' in which data stream values contiuously occur above a specified
#' threshold. If multiple responses are present in the passed
#' value only the first one is used to identify episodes
#'
# ---------------------------------------------------------


find_stream_episodes <- function(streams, threshold,
                                 group="Episodes", state="Episode") {

  if (missing(threshold))
    stop("Please provide a value threshold to use in defining peaks
         and the extent of episodes.")

  cat("Computing breaks of stream episodes...\n")

  response <- unique(streams$Response)[1]
  streams <- streams %>%
    arrange(Time) %>%
    filter(Response == response) %>%
    mutate(Episode = (Value - threshold) > 0)
    #mutate(
    #  Episode = recode(TRUE = "Episode", FALSE = "")
    #)
  idx <- collapse(streams$Episode)
  cat("Breaks:\n")
  print(streams$Time[idx])
  #if (context)
    tibble(Time=streams$Time[idx],
           States = if_else(streams$Episode[idx],
                            paste(group,state,sep=":"),
                            ""
                            )
           )
           #States=rep(c(paste(group,state,sep=":"), ""),
               #length.out=length(streams$Time[idx])))
    # else
    #   as_tibble(t(matrix(streams$Time[idx], nrow=2))) %>%
    # rename(start=1, end=2)


}

# Error in factor(x, levels = unique(x), labels = 1:length(unique(x)), exclude = NULL) :
#   invalid 'labels'; length 2 should be 1 or 0
# In addition: Warning message:
#   Unknown or uninitialised column: `Episodes`.

