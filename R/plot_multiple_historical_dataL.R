#' @title Plot faceted historical sensor data using the plotly package, L version
#'
#' @author Neil Klepeis
#'
#' @param data Long format data frame containing a 'Time' column variables
#' containing the sensor data series
#' @param by variable name to use in splitting data for individual facets,
#' must be a numeric, e.g., date-time variable that is used to sort the
#' plots in increasing order
#' @param by.format the datetime format to use in printing by value on each plot
#' @param nrows how many rows to use in creating subplots
#' @param shareX logical, whether to share the x axis between subplots, defaults to TRUE
#' @param shareY logical, whether to share the y axis between subplots, defaults to TRUE
#' @param margin value of plot margin(s) between 0 and 1 (left, right, top, bottom)
#' @param ... arguments to pass on to the 'plot_historical_data' function
#'
#' @return a plotly object with subplots
#'
#' @details This function creates plotly subplots (facets) for each unique 'by'
#' value in the data
#'
# ----------------------------------------------------

## TAKEN From annotatoR::plot_multiple_historical_dataL    12/17/2021

#  TODO:  Add contextual data to each of the facetes (subplots)

#  Adapted from regular plot historical function.

#  TAKEN from airMotive but modified to take LONG data as input
#          6/2/1021  NK

#  TODO:  Add option for adding lines, rectangles, and text (with hover tooltips)
#   to the plot to visualize contexts.  (alternative to the subplot plotly approach)
#    which I couldn't get working for two separate stream/context plots... NK  3/4/2021
# NOTE UPDATE:  I did get it working but had to uses the lines/rect approach instead
#   of using the ggplotly approach....

#  group_by(Species) %>%
#    group_map(~ plot_ly(data=., x = ~Sepal.Length,
#        y = ~Sepal.Width, color = ~Species, type = "scatter",
#        mode="markers"), keep=TRUE) %>%
#    subplot(nrows = 1, shareX = TRUE, shareY=TRUE)

#  Creat all individual plots first and then subplot them together.
# subplot(fig1, fig2,
#         nrows = 2,  shareX=TRUE, heights=c(0.75,0.25)
# ) %>%
#   plotly::layout(showlegend=TRUE, showlegend2=TRUE,
#                  plot_bgcolor="gray80",
#                  paper_bgcolor= bg
#   )

#  Fix for colors changing between plots
#  https://community.plotly.com/t/how-do-i-force-identical-colors-for-all-plots-in-a-subplot/16323

#  This works for colors and legend....title??
## https://stackoverflow.com/questions/63555242/%D0%A1ustomize-r-plotly-plots-colors-in-subplots
##   https://community.plotly.com/t/subplot-no-longer-uses-different-colors/4343/7

plot_multiple_historical_dataL <- function(data, by, nrows,
                                           by.format=NULL,
                                           shareX=FALSE,
                                           shareY=TRUE, margin=0.0025,
                                           device.name="DeviceName", time.name="Time",
                                           response.name="Response", value.name="Value",
                                           device.group="DeviceGroup",
                                           fg=NULL, bg.plot=NULL, bg.paper=NULL,
                                           visible.x=FALSE,
                                           visible.y=FALSE,
                                           trace.colors=NULL,
                                           area=TRUE, line.width=1,
                                           traceorder="normal") {


  #require(plotly)
  #require(dplyr)

  # #  Data in LONG format
  # figs <- list()
  #
  # #  first split by "by" variable, one figure per split
  # data <- data %>%
  #   select(any_of(c(by, index, time, value))) %>%
  #   group_split(across(any_of(by)))
  #
  #
  # for (i in 1:length(data)) {
  #   print(data[[i]])
  #   figs[[i]] <- plot_historical_dataL(data=data[[i]],
  #                                      index=index, time=time, value=value,
  #                                      ...)
  # }

  nindex <- length(unique(paste(data[[device.name]], data[[response.name]],
                                data[[device.group]])))

  if (!is.null(trace.colors))
      colors <- rep(trace.colors, length.out=nindex+1)

  #print(trace.colors)

  #  the "by" datetime variable is used to insure proper
  #   datetime sorting.
  #  the by.format is used in printing the 'by' value on each plot


  #  Only show legend for first plot to avoid duplicates with subplot...

  first.by <- unique(data[[by]])[1]

  #cat(first.by, "\n")

  data <- data %>%
    #select(any_of(c(by, index, time, value))) %>%
    select(any_of(c(by,
      device.name, time.name,
      response.name, device.group, value.name
    ))) %>%
    arrange(across(any_of(by))) %>%
    mutate(legend = if_else(get(by)==first.by, TRUE, FALSE)) %>%
    mutate(by.index = factor(get(by), labels="")) %>%
    group_by(across(any_of(by)), by.index)

  #print(data)
  #prin(by.format)

  data <- data %>%
    group_map(~ plot_historical_dataL(data=.,
                                      #index=index, time=time, value=value,
                                      device.name=device.name, time.name=time.name,
                                      response.name=response.name, value.name=value.name,
                                      device.group=device.group,
                                      #title=paste(by,":",.y[[by]]),
                                      visible.x=visible.x,
                                      visible.y=visible.y,
                                      trace.colors=trace.colors,
                                      area=area, line.width=line.width,
                                      traceorder=traceorder,
                                      showlegend=.x$legend[1]) %>%
                add_annotations(
                  text = paste(by,"#",.y$by.index,":\t",
                               format(.y[[by]], format=by.format),sep=""),
                  x = 0.5,
                  y = 0,
                  yref = "paper",
                  xref = "paper",
                  xanchor = "center",
                  yanchor = "bottom",
                  showarrow = FALSE,
                  bgcolor = 'rgba(1,1,1,0.3)',
                  bordercolor = 'rgba(1,1,1,1)',
                  borderwidth = 1,
                  font = list(
                    size = 12,
                    color="white"
                    )
                ),
            .keep=TRUE)


  #return(data)

  #  TODO:  Fix color coding to be consistent
  #   TODO:   add title to each plot with "by"=value


  #cat("created plots\n")

  if (missing(nrows)) nrows <- ceiling(length(data) / 3)

  #  Was getting list' object cannot be coerced to type 'double'
  #   When nrows was greater than number of plots.
  if (nrows > length(data)) nrows <- length(data)

  plotly::subplot(data, nrows=nrows, shareX=shareX, shareY=shareY,
            margin=margin) %>%
  layout(
    plot_bgcolor=bg.plot,
    paper_bgcolor= bg.paper,
    showlegend=TRUE,
    xaxis = list(
      enabled=FALSE
    ),

    legend = list(
                  orientation="h",
                  xanchor="center",
                  font = list(
                    family = "sans-serif",
                    size = 14,
                    color = fg),
                  bgcolor = 'rgba(0,0,0,0)',
                  #bordercolor = 'rgba(0,0,0,0)',
                  borderwidth = 0),

    yaxis=list(
      enabled=FALSE
    )
  ) %>%
    config(
      displayModeBar=FALSE
    )




  # %>% plotly::layout(showlegend=TRUE, showlegend2=TRUE,
  #                plot_bgcolor="gray80",
  #                paper_bgcolor= bg
  # )

}

