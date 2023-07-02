#' @name plot_boxplot_plotly
#'
#' @title Plot boxplots summarizing sensor data streams
#'
#' @description Plot boxplots for  data streams
#'
#' @author Neil Klepeis
#'
#' @param data Long-format sensor data frame for multiple streams and monitoring sites
#' containing a 'Time' column, index (grouping) columns, and Response and Value columns
#' @param averaging averaging time used to aggregate the data streams
# @param FUN function used to compute a statistic for each stream
#' @param title title of the plot
#' @param height height of the plot in pixels
#' @param fg foreground color for text
#' @param bg background color
#' @param plot.bg plot background color
#' @param legend.bg legend background color
#' @param legend.fg legend foreground color
#' @param line.width linewidth for plotted series
#'
#'
#' @return a plotly object
#'
#' @details This function plots a boxplot comparing distributions
#' statistic computed from the sensor data streams
#'
# ----------------------------------------------------

#  from airmotive ST  now in sensoRPlot!  7/1/2023

plot_boxplot_plotly <- function(data,
                                averaging=NULL,
                                title=NULL,
                                height=500, legend.inside=FALSE,
                                fg="white", bg='rgba(23,103,124,1)',
                                plot.bg = "gray90",
                                legend.bg='rgba(255,255,255,0.75)',
                                legend.fg="black",
                                line.width=1) {

  # library(tidyverse)
  # library(plotly)
  # library(timetk)

  # Now aggregate
  if (!is.null(averaging)) {
    cat("Aggregrating by: ", averaging, "\n")

    data <- data %>%
      select(Name, Response, Time, Value) %>%
      group_by(Name, Response) %>%
      summarize_by_time(Value = mean(Value, na.rm=TRUE),
                        .by=averaging)
  }

  data <- data %>%
    unite(col="Name", !any_of(c("Time","Value","Response")), sep=":")

  myNames <- unique(data$Name)
  cat("BoxPlot Names:\n")
  print(myNames)
  #if (reverse)  index <- rev(index)

  fig <- plot_ly(type="box", height=height, alpha=0.6)  # Empty plot


  #for (name in myNames) {
  #df <- data %>% filter(Name==i)
  responses <- unique(data$Response)
  for (response in responses) {
    df <- data %>% filter(Response == response)
    fig <- fig %>%
      add_trace(y = df$Value, x = df$Name,
                color = response,
                name = response,
                legendgroup = response
      )
  }
  #}


  #### PLOT LAYOUT

  mrg <- list(l = 50, r = 50,
              b = 50, t = 60,
              pad = 0)

  fig <- fig %>% layout(
    boxmode="group",
    #barmode = "overlay",
    plot_bgcolor=plot.bg,
    paper_bgcolor= bg,
    title = if (is.null(title))
      NULL
    else
      list(text = title,
           #subtitle = paste("Using Statistic:", statistic),
           xref="paper",
           font = list(
             family = "sans-serif",
             size = 20,
             weight="bold",
             color = "white"),
           x = 0.5, y = 120,
           pad = list(b = 150, l = 0, r = 20 )),
    legend = if (legend.inside) {
      list(x = 0.99, y = 0.99, orientation="h",
           xanchor="right",
           font = list(
             family = "sans-serif",
             size = 11,
             color = legend.fg),
           bgcolor = legend.bg,
           borderwidth = 0)
    } else {
      list(
        x = 100, y = 0.5, orientation="v",
        xanchor="right",
        font = list(
          family = "sans-serif",
          size = 11,
          color = legend.fg
        ),
        bgcolor=legend.bg,
        borderwidth = 0
      )

    },

    xaxis = list(
      #type = if (logaxis) "log" else NULL,
      #title=FUN,
      color=fg,
      rangeselector = NULL,
      rangeslider = NA,
      tickfont = list(
        size=10
      )
    ),

    yaxis=list(
      color=fg,
      #fixedrange=TRUE,
      #rangemode = "tozero",
      tickfont = list(
        size=10
      )
    ),

    margin=mrg

  ) %>%
    plotly::config(displayModeBar = TRUE,
                   toImageButtonOptions = list(
                     format = "svg",  #png svg
                     filename = "datastreams_boxplots",
                     width = 1200,
                     height = 800
                   )
    )

  fig

}
