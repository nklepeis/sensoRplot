#' @name plot_combo_contexts_streams_plotly
#'
#' @title Combo Plot with Contexts and Streams v2
#'
#' @description This function create a plotly combination plot (subplot)
#' containing both context and stream data
#'
#' @author Neil Klepeis
#'

#' @param contexts Long-format binary contexts (Time, Group, State, Value)
#' @param streams Long-format data streams (Time, Response, Value)
#' @param fg foreground color
#' @param bg background color
#' @param collapse logical, whether to collapse grouped states into a single lane
#' @param displayModeBar logical, whether to show the mode bar for the plot
#'
#' @details
#'
#' This function is a rewrite of v. 1 to be more convenient, taking
#' more standard long-format contexts and long-format sensors stream data
#' and creating a nice combo plot
# ------------------------------------------------


#  Note this is the new version of the context/stream combo plot
#   to make a more standard function with standard inputs.  Not
#    currently used in AirMotive... NK 11/22/2021

##  Used to be called plot_combo_context_streams2

plot_combo_contexts_streams_plotly <- function(contexts, streams,
                                       fg="white", bg="#2a8094", font.size=12,
                                       heights=c(0.7,0.3),
                                       collapse=FALSE, include.rangeSelector=FALSE,
                                       include.rangeSlider=FALSE) {

  #require(plotly)

  if (!NROW(contexts))
    stop("'contexts' must contain at least one context in long format.")

  if (!NROW(streams))
    stop("'streams' must contain at least one data stream in long format.")

  # Convert long to wide format contexts
  contexts <- contexts %>%
    pivot_wider(id_cols=c("Time"),
                names_from=c("Group","State"),
                values_from="Value",
                values_fn=min,   # if duplicates, use minimum value
                names_sep=":"
    ) %>%
    arrange(Time)

  print(contexts)

  #  convert to timeline format from binary Wide Format
  timeline <- binary2timeline(contexts, time.var="Time")

  print(timeline)

  #  Timeline plot
  #fig2 <- plot_state_timeline2(timeline, collapse=collapse,
  #                             bg=bg, fg=fg, font.size=font.size)
  fig2 <- plot_state_plotly(timeline, collapse=collapse,
                               bg=bg, fg=fg, font.size=font.size)
  fig2$x$config <- NULL # config is NULL for timeline subplot

  #  Data stream plot:  v2 uses long format as input
  fig1 <- plot_historical_data_plotly2(streams ,area=TRUE,
                                       title=NULL,
                                       showlegend=TRUE,
                                       legend.inside=TRUE,
                                       shape="hv",source="source",
                                       fg=fg, bg=bg,
                                       register=TRUE,
                                       include.rangeSelector=include.rangeSelector,
                                       include.rangeSlider=include.rangeSlider)

  # Plotly Combo plot
  subplot(fig1, fig2,
          nrows = 2,  shareX=TRUE, heights=heights
          ) %>%
    plotly::layout(showlegend=TRUE, showlegend2=TRUE,
                   plot_bgcolor="gray80",
                   paper_bgcolor= bg
                   )

}
