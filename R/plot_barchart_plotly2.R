#' @name plot_barchart_plotly2
#'
#' @title Plot barcharts summarizing sensor data streams
#'
#' @description Plot barcharts for different statistics of data streams
#'
#' @author Neil Klepeis
#'
#' @param data Long-format sensor data frame for multiple streams and monitoring sites
#' containing a 'Time' column, index (grouping) columns, and Response and Value columns
#' @param averaging averaging time used to aggregate the data streams
#' @param FUN function used to compute a statistic for each stream
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
#' @details This function plots a barchart comparing a selected
#' statistic computed from the sensor data streams
#'
# ----------------------------------------------------

# originally in Airmotive, move to senoRplot.  7/1/2023

plot_barchart_plotly2 <- function(data,
                                  averaging=NULL,
                                  FUN="median",
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

  if (!is.character(FUN)) {
    warning("FUN should be the name of a function as a character string")
    FUN = "median"
  }

  MYFUN = match.fun(FUN)

  # Now aggregate
  if (!is.null(averaging)) {
    cat("Aggregrating by: ", averaging, "\n")
    data <- data %>%
      select(Name, Response, Time, Value) %>%
      group_by(Name, Response) %>%
      summarize_by_time(Value = mean(Value, na.rm=TRUE),
                        .by=averaging)
  }

  print(data)

  data <- data %>%
    unite(col="Name", !any_of(c("Time","Value","Response")), sep=":")

  dataSummary <- data %>%
    group_by(Name, Response) %>%
    summarize(Value=MYFUN(Value, na.rm=TRUE)) %>%
    mutate(Name = fct_reorder(Name, Value))

  cat("Barchart data summary:\n")
  print(dataSummary)

  cat("\nStatistic: ", FUN, "\n")

  index <- unique(dataSummary$Response)
  cat("Stream Responses:\n")
  print(index)
  #if (reverse)  index <- rev(index)

  #fig <- plot_ly(height=height, alpha=0.6)  # Empty plot

  fig <- plot_ly(#data=dataSummary,
    #x = ~Value, y = ~Name,
    height=height, type = 'bar',
    orientation = 'v' #name = 'All Data',
    #marker = list(color = 'black',
    #       line = list(color = 'darkcyan',
    #                  width = line.width))
  )

  for (i in index) {
    data <- dataSummary %>% filter(Response == i)
    fig <- fig %>%
      add_trace(y = ~Value, x = ~Name,
                data = data,
                color = ~Response,
                legendgroup = ~Response,
                name=~Response,
                text = signif(data$Value, 3),
                textposition = 'auto',
                textfont = list(color = fg))
  }


  #### PLOT LAYOUT

  mrg <- list(l = 50, r = 50,
              b = 50, t = 60,
              pad = 0)

  fig <- fig %>% layout(
    #barmode = "overlay",
    plot_bgcolor=plot.bg,
    paper_bgcolor= bg,
    title = if (is.null(title))
      NULL
    else
      list(text = paste(title, " - Stat = ",FUN),
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

    yaxis = list(
      #type = if (logaxis) "log" else NULL,
      title=FUN,
      color=fg,
      rangeselector = NULL,
      rangeslider = NA,
      tickfont = list(
        size=10
      )
    ),

    xaxis=list(
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
                     filename = "datastreams_regresssion_plot",
                     width = 1200,
                     height = 800
                   )
    )

  fig

}



## EXAMPLE BAR CHART

# first aggregrate the data
# data <- data %>%
#   group_by(Label, Response) %>%
#   summarize(Median=median(Value)) %>%
#   mutate(Label = fct_reorder(Label, Median),
#          Response = fct_reorder(Response, Median))
#
#
#
# fig <- plot_ly(data, x = ~Median, y = ~Label,
#                type = 'bar',
#                orientation = 'h', name = 'All Data',
#                marker = list(color = 'black',
#                              line = list(color = 'darkcyan',
#                                          width = 2)))
