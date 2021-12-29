#' @aliases plot_streams_calendar

#' @title Plot sensor data streams faceted as a calendar

#' @author Neil Klepeis

#' @param data Long-format sensor data frame for multiple streams
#' containing a 'Time' column, index columns, and Response and Value columns
#' @param by time unit used to create an index variable to use in faceting
#' data by time

#' @return a ggplot object
#'
#' @details Plot streams faceted in the shape of a calendar
#' Aggregates to hours.
#----------------------------------------------------

plot_streams_calendar <- function(data,
                                area=TRUE, line=FALSE,
                                x.lab = "Time", y.lab = "Value",
                                title="Sensor Stream Data",
                                scales="free_x") {

  data <- data %>%
    mutate(Date = as.Date(Time)) %>%
    group_by(Date, Response) %>%
    summarize_by_time(.date_var=Time, .by="1 hour",
                      Value = mean(Value, na.rm=TRUE)) %>%
    mutate(Hour = hour(Time))

  p <- ggplot(data=data,
              aes(x=Hour, y=Value)) +
    labs(
      x = x.lab,
      y = y.lab,
      title = title
    )

  if (area) p <- p + geom_area()
  if (line) p <- p + geom_line()

  p + facet_calendar(~Date, scales=scales)

}
