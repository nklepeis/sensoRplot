#' @title Plot LONG historical sensor data using the plotly package
#'
#' @author Neil Klepeis
#'
#' @param data Long-format sensor data frame for multiple streams frame containing a 'Time' column, index columns,
#' and Response and Value columns
#' @param area
#' @param showlegend
#' @param legend.inside
#' @param height
#' @param shape
#' @param mode
#' @param source
#' @param fg
#' @param bg
#' @param plot.bg
#' @param showgrid
#' @param gridcolor
#' @param griddash
#' @param legend.bg
#' @param legend.fg
#' @param legend.font.size
#' @param axis.font.size
#' @param line.width
#' @param marker.size
#' @param reverse
#' @param register
#' @param include.rangeSelector
#' @param include.rangeSlider
#' @param verbose logical, print debugging output, defaults to FALSE
#'
#'
#' @return a plotly object
#'
#' @details concatenates all columns not Time or Value to
#' a single index variable that is used to plot data series.
#'
#' See 'plot_historical_subplot_plotly.R' which calls this function
#' to produce stacked subplots for each non-Response index variable, i.e.,
#' all index variables besides Response are used to facet the data into
#' separated plots
#'
# ----------------------------------------------------

# from latest airMotiveST.  (needs to updated/revised). ?NK 12/26/2022

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
                                         height=NULL,
                                         xrange=NULL, yrange=NULL,
                                         shape="hv", mode="lines",
                                         source="source",
                                         fg="white", bg='rgba(23,103,124,1)',
                                         plot.bg = "gray90",
                                         showgrid=TRUE,
                                         gridcolor= "gray",
                                         griddash = "dot",
                                         legend.bg='rgba(255,255,255,0.75)',
                                         legend.fg="black",
                                         legend.font.size=11,
                                         axis.font.size=10,
                                         line.width=1, marker.size=6,
                                         #fill.opacity=0.5,
                                         #marker.opacity=1,
                                         reverse=FALSE,
                                         register=TRUE,
                                         include.rangeSelector=FALSE,
                                         include.rangeSlider=FALSE,
                                         verbose=FALSE) {

  #require(plotly)
  #require(dplyr)
  #require(tidyr)
  #require(stringr)

  ## plotly line shapes:  linear, spline, vhv, hvh, vh hv


  #  Data consists of LONG data frame with
  #    Time column, index columns, and Response/Value columns for all
  #     the sensor readings from a given device or group of devices

  #print(head(data))

  if (verbose) {
    cat("Mode:", mode, "\n")
    cat("Symbol Size:", marker.size, "\n")
  }

  fig <- plot_ly(source=source, showlegend=showlegend, height=height)

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
  if (reverse)  index <- rev(index)

  #### ADD SERIES

  #  loop through series
  #  See for line "shapes" for interpolation:
  #      https://plotly.com/javascript/line-charts/#line-shape-options-for-interpolation

  for (i in index) {
    fig <- fig %>% add_trace(x = ~Time, y = ~Value,
                             data = data %>% filter(Name == i),
                             #name=i,
                             color = ~Name,
                             #colors = trace.colors,
                             legendgroup = ~Name,
                             name=~Name,
                             mode=mode, type="scatter",
                             connectgaps=TRUE,
                             #opacity=1,
                             #alpha=fill.opacity,
                             #alpha_stroke=1,
                             line = if (str_detect(mode, "line"))
                               list(shape = shape,   #hv, linear, spline, etc.
                                    width=line.width)
                             else NULL,
                             marker = if (str_detect(mode, "marker"))
                               list(size=marker.size)
                             else NULL,
                             fill= if (area) "tozeroy" else NULL)
  }


  #### PLOT LAYOUT

  mrg <- list(l = 50, r = 50,
              b = 50, t = 60,
              pad = 0)

  fig <- fig %>% layout(
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
    #showlegend=showlegend,
    legend = if (legend.inside) {
      list(x = 0.99, y = 0.99, orientation="h",
           xanchor="right",
           font = list(
             family = "sans-serif",
             size = legend.font.size,
             color = legend.fg),
           bgcolor = legend.bg,
           #bordercolor = 'rgba(0,0,0,0)',
           borderwidth = 0)
    } else {
      list(
        x = 100, y = 0.5, orientation="v",
        xanchor="right",
        font = list(
          family = "sans-serif",
          size = legend.font.size,
          color = legend.fg
        ),
        #bgcolor = 'rgba(0,0,0,0.6)',
        bgcolor=legend.bg,
        borderwidth = 0
      )

    },

    xaxis = list(
      range=xrange,
      color=fg,
      rangeselector = if (include.rangeSelector) rangeSelector else NULL,
      rangeslider = if (include.rangeSlider) rangeSlider else NA,
      tickfont = list(
        size=axis.font.size
      ),
      showgrid=showgrid,
      gridcolor=gridcolor,
      griddash=griddash
      #rangeslider = NULL #rangeSlider

    ),

    yaxis=list(
      range = yrange,
      color=fg,
      fixedrange=TRUE,
      rangemode = "tozero",
      tickfont = list(
        size=axis.font.size
      ),
      showgrid=showgrid,
      gridcolor=gridcolor,
      griddash=griddash
    ),

    margin=mrg

    #yaxis = list(title = "Time"))
  ) %>%   # Export as SVG file
    plotly::config(displayModeBar = TRUE,
                   toImageButtonOptions = list(
                     format = "svg",
                     filename = "datastreams_plot",
                     width = 1400,
                     height = 700
                   )
    )


  if (register) {
    if (verbose)
      cat("\n\nRegistering plotly source: ", source,"\n\n")
    fig <- fig %>%
      event_register('plotly_legendclick') %>%
      event_register('plotly_relayout')
  }

  fig



}

