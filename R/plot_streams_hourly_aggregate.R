#' @name plot_streams_hourly_aggregate
#'
#' @title Plot column chart showing hourly averages across all days
#'
#' @author Neil Klepeis
#'
#' @param data Long format data frame containing a 'Time', a 'Response' variable
#' indicating the sensor stream and a 'Value' variable containing the stream
#' data values.
#' @param groupvars option character vector of grouping variables for faceting
#' @param x.lab x axis label
#' @param y.lab y axis label
#' @param fill.color fill color
#' @param title title
#' @param break time breaks in hours
#' @param labels time break labels
#' @param ... arguments to facet_wrap if groupvar is non=NULL
#'
#' @return a ggplot object
#'
#' @details This function produces a column chart showing the mean
#' level of the data stream by hour of the day, aggregated across
#' all available days.
#'
# ----------------------------------------------------

## Update:  Use any variable athat is not Time, Response, or Value as
#   a grouping variable

plot_streams_hourly_aggregate <- function(data, groupvars=NULL,
                                          x.lab="Hour",
                                          y.lab="Value", fill.color="blue",
                                          title="Sensor Stream Hourly Means",
                                          breaks=c(0,6,12,18),
                                          labels=c("Midnight","6a","Noon","6pm"),
                                          ...) {

  data <- data %>%
    mutate(Hour = hour(Time)) %>%
    #group_by(Hour, Response) %>%
    group_by(Hour, Response, across(groupvars)) %>%
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
    geom_col(fill=fill.color)

  if (!is.null(groupvars))
    p <- p + facet_wrap(groupvars, ...)

  p

}
