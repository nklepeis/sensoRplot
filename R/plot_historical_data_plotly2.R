#' @title Plot LONG historical sensor data using the plotly package
#'
#' @author Neil Klepeis
#'
#' @param data Long-format sensor data frame for multiple streams frame
#' containing a 'Time' column, index columns, and Response and Value columns
#'
#' @return a plotly object
#'
#' @details concatenates all columns not Time or Value to
#' a single index variable that is used to plot data series.
#'
# See 'plot_historical_subplot_plotly.R' which calls this function
#' See 'plot_multiple_historical_dataL.R' which calls this function
#' to produce stacked subplots for each non-Response index variable, i.e.,
#' all index variables besides Response are used to facet the data into
#' separated plots
#'
# ----------------------------------------------------

#  Version to use LONG FORMAT DATA.    July 29, 2021. NK

#  TODO:  Add option for adding lines, rectangles, and text (with hover tooltips)
#   to the plot to visualize contexts.  (alternative to the subplot plotly approach)
#    which I couldn't get working for two separate stream/context plots... NK  3/4/2021
#  Done.  see plot combos..

##  TODO:  Add options for subplots for Response and each index variable
#   OR keep default of concatening all index variables + response to a single
#    index variable == "Name"

plot_historical_data_plotly2 <- function(data,
                                         area=TRUE,
                                        title=NULL,
                                        showlegend=TRUE,
                                        legend.inside=TRUE,
                                        shape="hv",
                                        source="source",
                                        fg="white", bg='rgba(23,103,124,1)',
                                        register=TRUE,
                                        include.rangeSelector=FALSE,
                                        include.rangeSlider=FALSE) {

  #require(plotly)
  #require(dplyr)
  #require(tidyr)

  #  Data consists of LONG data frame with
  #    Time column, index columns, and Response/Value columns for all
  #     the sensor readings from a given device or group of devices

  #print(head(data))

  fig <- plot_ly(source=source, showlegend=showlegend)

  rangeSelector <- list(
    bordercolor="white",
    buttons = list(
      list(
        count = 1,
        label = "1 h",
        step = "hour",
        stepmode = "backward"),
      list(
        count = 6,
        label = "6 h",
        step = "hour",
        stepmode = "backward"),
      list(
        count = 1,
        label = "1 d",
        step = "day",
        stepmode = "backward"),
      list(
        count = 1,
        label = "1 wk",
        step = "week",
        stepmode = "backward"),
      list(step = "all")
    )
  )

  #rangeSlider <-  list(type = "date")
  rangeSlider <- if (include.rangeSlider) list(type="date") else NULL

  #cat("Show rangeSlider: ", include.rangeSlider,"\n")
  #print(rangeSlider)


  ##   MODIFY DATA

  #  create concatenated index variable
  #     Response + all others not Time or Value

  data <- data %>%
    unite(col="Name", !any_of(c("Time","Value")), sep=":")

  index <- unique(data$Name)

  #### ADD SERIES

  #  loop through series
  #  See for line "shapes" for interpolation:
  #      https://plotly.com/javascript/line-charts/#line-shape-options-for-interpolation

  for (i in index)
    fig <- fig %>% add_lines(x = ~Time, y = ~Value,
                             data = data %>% filter(Name == i),
                             #name=i,

                             color = ~Name,
                             #colors = trace.colors,
                             legendgroup = ~Name,

                             name=~Name,
                             mode="lines", type="scatter",
                             connectgaps=TRUE,
                             fill= if (area) "tozeroy" else NULL,
                             line = list(shape = shape,   #hv, linear, spline, etc.
                                         width=1))


  #### PLOT LAYOUT

  mrg <- list(l = 50, r = 50,
              b = 50, t = 60,
              pad = 0)

  fig <- fig %>% layout(
    plot_bgcolor='gray90',
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
    #showlegend=showlegend,
    legend = if (legend.inside) {
      list(x = 0.99, y = 0.99, orientation="h",
                  xanchor="right",
                  font = list(
                    family = "sans-serif",
                    size = 11,
                    color = "black"),
                  bgcolor = 'rgba(255,255,255,0.5)',
                  #bordercolor = 'rgba(0,0,0,0)',
                  borderwidth = 0)
      } else {
        list(
          x = 100, y = 0.5, orientation="v",
          xanchor="right",
          font = list(
            family = "sans-serif",
            size = 11,
            color = "white"),
          bgcolor = 'rgba(0,0,0,0.6)',
          borderwidth = 0
        )

      },

    xaxis = list(
      color=fg,
      rangeselector = if (include.rangeSelector) rangeSelector else NULL,
      rangeslider = if (include.rangeSlider) rangeSlider else NA,
      tickfont = list(
        size=10
      )
      #rangeslider = NULL #rangeSlider

    ),

    yaxis=list(
      color=fg,
      fixedrange=TRUE,
      rangemode = "tozero",
      tickfont = list(
        size=10
      )
    ),

    margin=mrg

    #yaxis = list(title = "Time"))
  ) %>%
    plotly::config(displayModeBar = TRUE,
                              toImageButtonOptions = list(
                                format = "png",
                                filename = "datastreams_plot",
                                width = 1400,
                                height = 700
                            )
    )


  if (register)
    fig <- fig %>%
    event_register('plotly_legendclick') %>%
    event_register('plotly_relayout')

  fig



}

