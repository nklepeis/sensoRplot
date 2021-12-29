#' @name plot_contexts_hourly_aggregate
#'
#' @title Plot column chart showing time spent in contexts by hour
#' aggregated across all days
#'
#' @author Neil Klepeis
#'
#' @param data Data frame in long binary context format containing
#' variables for 'Time','Group', and 'State'
#'
#' @return a ggplot object
#'
#' @details This function produces a column chart showing the mean
#' level of the data stream by hour of the day, aggregated across
#' all available days.
#'
# ----------------------------------------------------

plot_contexts_hourly_aggregate <- function(data, x.lab="Hour",
                                          y.lab="Value",
                                          title="Sensor Contexts Hourly Means",
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
