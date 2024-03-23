#' @name plot_combo_context_streams2
#'
#' @title Combo Plot with Contexts and Streams v2
#'
#' @description This function create a plotly combination plot (subplot)
#' containing both context and stream data
#'
#' @author Neil Klepeis
#'

#' @param timeline timeline format contexts (group, state, xleft, xright)
#' @param streams Long-format data streams (Time, Response, Value)
#' @param xrange range for the xaxis, used to specify zoom level
#' @param fg foreground color
#' @param bg background color
#' @param collapse logical, whether to collapse grouped states into a single lane
#' @param displayModeBar logical, whether to show the mode bar for the plot
#' @param verbose logical, display debugging output, defaults to FALSE
#'
#' @details
#'
#' This function is a rewrite of v. 1 to be more convenient, taking
#' more standard long-format contexts and long-format sensors stream data
#' and creating a nice combo plot
# ------------------------------------------------

## OK This was folded back into SensoRPlot on 6/30/2023
#  It is very similar to the 'plot_combo_context_streams_plotly' already here
#   with maybe just a change in format of contexts argument. Keeping both
#    for now.  NK

# NK 3/22/2024.  this one seems to work better than "plotly" one, revised
#    and keeping for now...

# Taken from airmotive (not used there) 12/26/2022 NK
#  Back to taking timeline format as input....12/26/2022
#   [so we don't have to do extra computation of coverting since we
#     have already done that in the server code...]


#  Note this is the new version of the context/stream combo plot
#   to make a more standard function with standard inputs.  Not
#    currently used in AirMotive... NK 11/22/2021


plot_combo_context_streams2 <- function(timeline, streams,
                                        xrange=NULL, height=NULL,
                                        showlegend=TRUE,
                                        showTimelineLabels=FALSE,
                                        fg="white", bg="#2a8094",
                                        legend.bg=bg, legend.fg=fg,
                                        timeline.font.size=12,
                                        heights=c(0.7,0.3),
                                        collapse=FALSE,
                                        include.rangeSelector=FALSE,
                                        include.rangeSlider=FALSE,
                                        verbose=FALSE,
                                        ...) {

  require(plotly)

  if (!NROW(timeline))
    stop("'contexts' must contain at least one context in timeline format.")

  if (!NROW(streams))
    stop("'streams' must contain at least one data stream in long format.")

  # Convert long to wide format contexts
  # contexts <- contexts %>%
  #   pivot_wider(id_cols=c("Time"),
  #               names_from=c("Group","State"),
  #               values_from="Value",
  #               values_fn=min,   # if duplicates, use minimum value
  #               names_sep=":"
  #   ) %>%
  #   arrange(Time)
  #
  # print(contexts)

  #  convert to timeline format from binary Wide Format
  #timeline <- binary2timeline(contexts, time.var="Time")

  if (verbose) print(timeline)

  #  Timeline plot  (newest one)
  fig2 <- plot_state_timeline_plotly(timeline,
                                     auto.y=TRUE, bg=bg,
                                     fg=fg, collapse=collapse,
                                     font.size=timeline.font.size,
                                     showticklabelsY=showTimelineLabels,
                                     source="source")

  #fig2 <- plot_state_timeline2(timeline, collapse=collapse,
  #                             bg=bg, fg=fg, font.size=font.size)
  fig2$x$config <- NULL # config is NULL for timeline subplot

  #  Data stream plot:  v2 uses long format as input
  #    this one from latest AirMotiveST
  fig1 <- plot_historical_data_plotly2(streams,
                                       #height=height,
                                       title=NULL,
                                       showlegend=showlegend,
                                       #legend.inside=FALSE,
                                       source="source",
                                       fg=fg, bg=bg,
                                       legend.bg=legend.bg,
                                       legend.fg=legend.fg,
                                       register=TRUE,
                                       include.rangeSelector=include.rangeSelector,
                                       include.rangeSlider=include.rangeSlider,
                                       ...)

  # Combo plot
  subplot(fig1, fig2,
          nrows = 2,  shareX=TRUE, heights=heights
  ) %>%
    plotly::layout(showlegend=TRUE, showlegend2=TRUE,
                   #plot_bgcolor="gray80",
                   height=height,
                   xaxis=list(range=xrange),
                   paper_bgcolor= bg
    )

}
