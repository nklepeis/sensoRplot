#' @name plot_combo_episodes_plotly
#'
#' @title Create stacked plots of grouped combo timeline/streams data
#'
#' @description This function creates a grid of plotly combination
#' with individual subplots containing subplots of timelines and streams
#' for each define episode
#'
#' @author Neil Klepeis
#'
#' @param timeline a dataframe containing group, state, xleft, xright
#' @param streams Long-format data streams (Time, Response, Value)
#' @param breaks a dataframe with start/end columns defining episodes
#' @param ncols number of columns for plotting panels of episodes
#' @param fg foreground color
#' @param bg background color
#' @param displayModeBar logical, whether to show the mode bar for the plot
#'
#' @details The subplots are stacked on top of one another
#'
# ------------------------------------------------
# all argumetns from ...plotly2 are in ... except data, showlegend, displayModebar

## I think this is originally unique to contextualizER or annotatoR
#   now in sensoRplot!!!  NK  6/30/2023

plot_combo_episodes_plotly <- function(timeline, streams, breaks,
                                       ncols=3,
                                       height=NULL,
                                       heights=c(0.7, 0.3),
                                       bg="white", fg="black",
                                       collapse=FALSE,
                                       ...,
                                       displayModeBar=TRUE) {

  #require(plotly)
  #require(tidyr)
  #require(dplyr)

  if (!NROW(timeline %>% select(!any_of(c("xleft","xright","group","state")))))
    stop("'timeline' data must have xleft, xright, group, state columns")

  if (!NROW(streams))
    stop("'streams' must contain at least one data stream in long format.")

  #breaks <- sort(breaks)
  plots <- list()

  for (i in 1:NROW(breaks)) {
    cat("\n\nPlotting episode:\n")
    print(c(breaks$start[i], breaks$end[i]))
    mytimeline <- timeline %>% filter(xleft >= breaks$start[i] &
                                        xright <= breaks$end[i])
    mystreams <- streams %>% filter(Time >= breaks$start[i] &
                                      Time <= breaks$end[i])
    cat("\nPlotting subplots of timelines/streams combos:\n")
    #print(data)
    if (NROW(mytimeline) & NROW(mystreams))
      plots[[i]] <-
      plot_combo_context_streams2(mytimeline, mystreams,
                                  fg=fg, bg=bg,
                                  legend.bg=bg, legend.fg=fg,
                                  timeline.font.size=12,
                                  heights=heights,
                                  collapse=collapse,
                                  include.rangeSelector=FALSE,
                                  include.rangeSlider=FALSE, ...)

  }

  cat("\nPlotting ", length(plots), " plotly subplots....\n\n")

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
  else NULL

}
