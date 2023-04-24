#' @aliases episodes2contexts
#'
#' @title Convert episodes in non-overlapping
#' timeline format to active-state context format
#'
#' @author Neil Klepeis
#'
#' @param data a date frame containing timelines of individual events/episodes
#' @param time1.var the expected name for the starting time column in 'data' (defaults to "TimeStart")
#' @param time2.var the expected name for the ending time column in 'data' (defaults to "TimeEnd")
#' @param group.var the expected name for the group column in 'data' (defaults to "Group")
#' @param state.var the expected name for the state column in 'data' (defaults to "State")
#'
#' @details This function converts a timeline-formatted dataframe of non-
#' overlapping episodes with Start and End time values, and Group and State columns,
#' to an active-state format context data frame with a Time column and columns containing
#' a Group:State value (only 1 since episodes are non-overlapping).
#'
#' IMPORTANT:  This function cannot handle overlapping episodes.
#' All episodes must be non-overlapping in time (they *can* have overlapping endpoints).
#' This restriction is not currently checked.
#'
#' @example
#' as <- tibble(StartTime=c(ti+0:5,ti+c(11,15), ti+c(18,25)),
#'             EndTime=c(ti+1:6, ti+c(13,17), ti+c(20,30)),
#'             Group = c(1,1,1,1,1,1,1,1,1,2),
#'             State = c(1:9, 1))
#'
#'
#'------------------------------

episodes2contexts <- function (data, time1.var="StartTime",
                             time2.var="EndTime", group.var="Group",
                             state.var="State") {

  data <- as_tibble(data)

  if (!group.var %in% names(data))
    data[[group.var]] <- "my"
  if (!state.var %in% names(data))
    data[[state.var]] <- "episode"

  data$groupstate1 <- paste(data[[group.var]],
                            data[[state.var]], sep=":")
  data$groupstate2 <- NA
  #print(data)

  data1 <- data %>%
    select(all_of(c(time1.var, "groupstate1")))
  names(data1) <- c("Time","States")
  data2 <- data %>%
    select(all_of(c(time2.var, "groupstate2")))
  names(data2) <- c("Time","States")

  #print(data1)
  #print(data2)

  ### Check to make sure episodes are non-overlapping
  ## pivot_longer  Start/end time .... based on  row number
  #  check for non-increasing values....





  # ----------------------------------------


  bind_rows(data1, data2) %>%
    arrange(Time) %>%
    group_by(Time) %>%
    summarize(   # paste really not needed since there is no overlap...
      States = paste(unique(States[!is.na(States)]),
                     collapse=" | ")
      )
  #%>%
   # mutate(
  #   States = recode(States, " | " = "")
   # )

}
