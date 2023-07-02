#' @name plot_histograms_plotly2
#'
#' @title Plot histograms for sensor data streams
#'
#' @description Plot histograms for all sites and their selected responses
#'
#' @author Neil Klepeis
#'
#' @param data Long-format sensor data frame for multiple streams and monitoring sites
#' containing a 'Time' column, index (grouping) columns, and Response and Value columns
#' @param logaxis whether to plot histogram(s) with a log X axis
#' @param averaging averaging time used to aggregate the data streams
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
#' @details This function plots overlapping histograms for all responses
#' and all sites in the passed data frame
#'
# ----------------------------------------------------

#3 from airMotiveST  6/30/2023 now in sensorPlot!! NK

#  Version to use LONG FORMAT DATA.    July 29, 2021. NK


plot_histograms_plotly2 <- function(data,
                                    logaxis=TRUE,
                                    averaging=NULL,
                                    title=NULL,
                                    height=500, legend.inside=FALSE,
                                    fg="white", bg='rgba(23,103,124,1)',
                                    plot.bg = "gray90",
                                    legend.bg='rgba(255,255,255,0.75)',
                                    legend.fg="black",
                                    line.width=1, marker.size=8) {

  library(tidyverse)
  library(tidymodels) # for the fit() function
  library(plotly)
  library(timetk)

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
    unite(col="Name", !any_of(c("Time","Value")), sep=":")

  index <- unique(data$Name)
  cat("Histogram Names:\n")
  print(index)
  #if (reverse)  index <- rev(index)

  fig <- plot_ly(height=height, alpha=0.6)  # Empty plot


  for (i in index) {
    fig <- fig %>%
      add_histogram(x = ~Value,
                    data = data %>% filter(Name == i),
                    color = ~Name,
                    legendgroup = ~Name,
                    name=~Name)
  }




  #### PLOT LAYOUT

  mrg <- list(l = 50, r = 50,
              b = 50, t = 60,
              pad = 0)

  fig <- fig %>% layout(
    barmode = "overlay",
    plot_bgcolor=plot.bg,
    paper_bgcolor= bg,
    title = if (is.null(title))
      NULL
    else
      list(text = title, xref="paper",
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
                     format = "svg",
                     filename = "datastreams_histograms_plot",
                     width = 1200,
                     height = 800
                   )
    )

  # add_annotations(
  #     text = paste(paste(equations,"<br>"),collapse=" "),
  #     x = 0.15,
  #     y = 0.95,
  #     align = "left",   # doesn't work
  #     yref = "paper",
  #     xref = "paper",
  #     xanchor = "left",
  #     yanchor = "top",
  #     showarrow = FALSE,
  #     bgcolor = 'rgba(1,1,1,0.3)',
  #     bordercolor = 'rgba(1,1,1,0.3)',
  #     borderwidth = 1,
  #     font = list(
  #       size = 12,
  #       color="lightgray"
  #     )
  # )

  fig

}



## EXAMPLE histogram
#fig <- plot_ly(alpha = 0.6)
#fig <- fig %>% add_histogram(x = ~rnorm(500))
#fig <- fig %>% add_histogram(x = ~rnorm(500) + 1)
#fig <- fig %>% layout(barmode = "overlay")
