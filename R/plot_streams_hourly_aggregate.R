#' @name plot_streams_hourly_aggregate
#'
#' @title Plot column chart showing hourly averages across all days
#'
#' @author Neil Klepeis
#'
#' @param data Long format data frame containing a 'Time', a 'Response' variable
#' indicating the sensor stream and a 'Value' variable containing the stream
#' data values.
#'
#' @return a ggplot object
#'
#' @details This function produces a column chart showing the mean
#' level of the data stream by hour of the day, aggregated across
#' all available days.
#'
# ----------------------------------------------------

plot_streams_hourly_aggregate <- function(data, x.lab="Hour",
                                          y.lab="Value",
                                          title="Sensor Stream Hourly Means",
                                          breaks=c(0,6,12,18),
                                          labels=c("Midnight","6a","Noon","6pm")) {

  data <- data %>%
    mutate(Hour = hour(Time)) %>%
    group_by(Hour, Response) %>%
    summarize(Value = mean(Value, na.rm=TRUE))

  p <- ggplot(data=data,
         aes(x=Hour, y=Value)) +
    scale_x_continuous(expand=c(0,0),
        labels=labels,
        breaks = breaks
      ) +
  labs(
    x = x.lab,
    y = y.lab,
    title = title
  ) +
    geom_col()

  p

}
