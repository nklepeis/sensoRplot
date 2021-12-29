#' @title Plot historical sensor data using the plotly package, L version
#'
#' @author Neil Klepeis
#'
#' @param data Long format data frame containing a 'Time' column variables
#' containing the sensor data series
#'
#' @return a plotly object
#'
# ----------------------------------------------------

## TAKEN From annotatoR,    12/17/2021

#  TAKEN from airMotive but modified to take LONG data as input
#          6/2/1021  NK

#  TODO:  Add option for adding lines, rectangles, and text (with hover tooltips)
#   to the plot to visualize contexts.  (alternative to the subplot plotly approach)
#    which I couldn't get working for two separate stream/context plots... NK  3/4/2021

plot_historical_dataL <- function(data, device.name="DeviceName", time.name = "Time",
                                  value.name="Value", response.name = "Response",
                                  device.group="DeviceGroup",
                                  title=NULL, showlegend=TRUE,
                                  displayModeBar = TRUE, shape="hv",
                                  area=TRUE, line.width=1,
                                  source="source",
                                  fg=NULL, bg.plot=NULL,bg.paper=NULL,
                                  legend.x=0.5, legend.y=100,
                                  legend.orientation = "h",
                                  register=TRUE,
                                  include.rangeSelector=FALSE,
                                  include.rangeSlider=FALSE,
                                  visible.x=TRUE,
                                  visible.y=TRUE,
                                  trace.colors=NULL,
                                  traceorder="normal") {


  #require(plotly)
  #require(dplyr)

  #  Data consists of wide data frame with
  #    Time column and columns for all
  #     the sensor readers

  #print(head(data))

  #  split data by the "index" variables

  data <- data %>%
    select(any_of(c(device.name, time.name, response.name, value.name,  device.group))) %>%
    mutate(deviceIndex = paste(get(device.group),":",get(device.name),":",get(response.name),sep="")) %>%
    group_split(deviceIndex)
    #group_split(across(any_of(device.name, device.group, response.name)))

  #print(data)

  #fig <- plot_ly(data, x = ~Time, source=source)
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

  #  loop through series
  #  See for line "shapes" for interpolation:
  #      https://plotly.com/javascript/line-charts/#line-shape-options-for-interpolation
  #for (s in names(data[-1]))

  #print(data)

  for (s in 1:length(data))
    fig <- fig %>% add_lines(x = ~get(time.name), y = ~get(value.name),
                             data = data[[s]],
                             #color = ~get(deviceIndex),
                             color = ~deviceIndex,
                             colors = trace.colors,
                             #legendgroup = ~get(deviceIndex),
                             legendgroup = ~deviceIndex,
                             #name=unique(data[[s]][deviceIndex]),
                             name=~deviceIndex,
                             mode="lines",
                             type="scatter",
                             connectgaps=TRUE,
                             fill=ifelse(area, "tozeroy", "none"),
                             line = list(shape = shape,   #hv, linear, spline, etc.
                                         width=line.width)
                             )


  #cat("added traces\n")

  if (!is.null(title))
    mrg <- list(l = 50, r = 50,
                b = 50, t = 60,
                pad = 0)
  else
    mrg <- list(l=0, r=0, b=0, t=0, pad=0)

  fig <- fig %>% layout(
    #colorway = if (!is.null(colors)) colors else NULL,
    plot_bgcolor=bg.plot,
    paper_bgcolor= bg.paper,
    title = if (is.null(title)) NULL else
      list(text = title,
           font = list(
             family = "sans-serif",
             size = 16,
             color = fg)
           # xref="paper",
           #       x = 0, y = 120,
           #       pad = list(b = 150, l = 0, r = 20
           #                  )
           ),
    #showlegend=showlegend,
    legend = list(x = legend.x, y = legend.y,
                  orientation=legend.orientation,
                  xanchor="center",
                  font = list(
                    family = "sans-serif",
                    size = 12,
                    color = fg),
                  bgcolor = 'rgba(0,0,0,0)',
                  #bordercolor = 'rgba(0,0,0,0)',
                  borderwidth = 0,
                  traceorder=traceorder),

    xaxis = list(
      visible = visible.x,
      title = time.name,
      color=fg,
      gridcolor = "gray",
      gridwidth = 1,
      rangeselector = if (include.rangeSelector) rangeSelector else NULL,
      rangeslider = if (include.rangeSlider) rangeSlider else NA
      #rangeslider = NULL #rangeSlider

    ),

    yaxis=list(
      visible = visible.y,
      title = value.name,
      color=fg,
      gridcolor = "gray",
      gridwidth = 1,
      fixedrange=TRUE,
      rangemode = "tozero"
    ),

    margin=mrg

    #yaxis = list(title = "Time"))
  )

  fig <- fig %>%
    config(
      displayModeBar=displayModeBar
    )

  if (register)
    fig <- fig %>%
    event_register('plotly_legendclick') %>%
    event_register('plotly_relayout')

  fig



}

