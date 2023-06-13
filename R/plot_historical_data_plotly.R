#' @title Plot historical sensor data using the plotly package
#'
#' @author Neil Klepeis
#'
#' @param data Data frame containing a 'Time' column and columns
#' containing the sensor data series
#'
#' @return a plotly object
#'
# ----------------------------------------------------

#  TODO:  Add option for adding lines, rectangles, and text (with hover tooltips)
#   to the plot to visualize contexts.  (alternative to the subplot plotly approach)
#    which I couldn't get working for two separate stream/context plots... NK  3/4/2021

plot_historical_data_plotly <- function(data,
                                        title=NULL,
                                        showlegend=TRUE,
                                        shape="hv",
                                        source="source",
                                        fg="white", bg='rgba(23,103,124,1)',
                                        register=TRUE,
                                        include.rangeSelector=FALSE,
                                        include.rangeSlider=FALSE) {

  #require(plotly)

  #  Data consists of wide data frame with
  #    Time column and columns for all
  #     the sensor readers

  #print(head(data))

  fig <- plot_ly(data, x = ~Time, source=source)

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

  #  loop through series
  #  See for line "shapes" for interpolation:
  #      https://plotly.com/javascript/line-charts/#line-shape-options-for-interpolation
  for (s in names(data[-1]))
    fig <- fig %>% add_lines(y = data[[s]], name=s,
                             mode="lines", type="scatter",
                             connectgaps=TRUE,
                             fill="tozeroy",
                             line = list(shape = shape,   #hv, linear, spline, etc.
                                         width=1))

  mrg <- list(l = 50, r = 50,
              b = 50, t = 60,
              pad = 0)

  fig <- fig %>% layout(
    plot_bgcolor='gray90',
    paper_bgcolor= bg,
    title = list(text = title, xref="paper",
                 x = 0, y = 120,
                 pad = list(b = 150, l = 0, r = 20 )),
    showlegend=showlegend,
    legend = list(x = 0.99, y = 0.99, orientation="h",
                  xanchor="right",
                  font = list(
                    family = "sans-serif",
                    size = 11,
                    color = "slategray"),
                  bgcolor = 'rgba(255,255,255,0.5)',
                  #bordercolor = 'rgba(0,0,0,0)',
                  borderwidth = 0),

    xaxis = list(
      color=fg,
      rangeselector = if (include.rangeSelector) rangeSelector else NULL,
      rangeslider = if (include.rangeSlider) rangeSlider else NA
      #rangeslider = NULL #rangeSlider

    ),

    yaxis=list(
      color=fg,
      fixedrange=TRUE,
      rangemode = "tozero"
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

