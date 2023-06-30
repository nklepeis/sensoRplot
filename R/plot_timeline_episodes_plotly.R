#' @name plot_timeline_episodes_plotly
#'
#' @title Create stacked plots of grouped data streams
#'
#' @description This function creates a stacked plotly combination plot
#' with individual subplots for each unique set of index values
#'
#' @author Neil Klepeis
#'
#' @param timeline a dataframe containing group, state, xleft, xright
#' @param breaks a dataframe with start/end columns defining episodes
#' @param fg foreground color
#' @param bg background color
#' @param displayModeBar logical, whether to show the mode bar for the plot
#'
#' @details The subplots are stacked on top of one another
#'
# ------------------------------------------------
# all argumetns from ...plotly2 are in ... except data, showlegend, displayModebar

## Seems unique to contextualizeR originally. now in sensorPlot!!
#    6/30/2023 NK


plot_timeline_episodes_plotly <- function(timeline, breaks,
                                          ncols=3,
                                          height=NULL,
                                          bg="white", fg="black",
                                          ...,
                                          displayModeBar=TRUE) {

  require(plotly)
  require(tidyr)
  require(dplyr)

  if (!NROW(timeline %>% select(!any_of(c("xleft","xright","group","state")))))
    stop("'timeline' data must have xleft, xright, group, state columns")

  #if (!NROW(episodes %>% select(!any_of(c("start","stope")))))
  #  stop("'episodes' data must have start, stop columns")

  #breaks <- sort(breaks)
  plots <- list()

  for (i in 1:NROW(breaks)) {
    cat("\n\nPlotting episode:\n")
    print(c(breaks$start[i], breaks$end[i]))
    data <- timeline %>% filter(xleft >= breaks$start[i] &
                                  xright <= breaks$end[i])
    print(data)
    if (NROW(data)) {
      cat("\nPlotting subtimeline:\n")
      plots[[i]] <-
        plot_state_timeline_plotly(
          data,
          fg=fg, bg=bg, showticklabelsY=FALSE,
          date_labels = "%H:%M \n %m/%d/%y",
          source="source",
          ...)
    }
  }

  if (length(plots))
    subplot(plots,
            nrows = ceiling(length(plots) / ncols),
            #nrows = nrows,
            shareX=FALSE, shareY=FALSE,
            margin= c(0.025,0.025,0.05,0.05)
    ) %>%   # units 0:1 margin left right top bottom
    plotly::layout(showlegend=FALSE,
                   plot_bgcolor= bg,
                   paper_bgcolor= bg,
                   height=height
    ) %>%
    plotly::config(
      displayModeBar=displayModeBar
      #yaxis=list(
      #  tickangle=90,
      #  showticklabels=FALSE
      #)
    )

}
