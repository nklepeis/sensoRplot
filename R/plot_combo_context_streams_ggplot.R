#' @title Combo Plot with Contexts and Streams Using \code{ggplot}
#'
#' @description This function create a ggplot combination (multiple) plot
#' containing both context and stream data
#'
#' @author Neil Klepeis
#'

#' @param contexts Long-format binary contexts (Time, Group, State, Value)
#' @param streams Long-format data streams (Time, Response, Value)
#' @param contexts.args list of named arguments to pass to the \code{plot_state_timeline} function
#' @param streams.args list of named argments to pass to the \code{plot_streams_ggplot} function
#' @param heights 2-element vector with elements in [0,1] giving the relative heights of the plots
#'
#' @details
#'
#' This function is similar to the plotly version that plots context data
#' in a separate but datetime-axis-linked below the sensor stream data.
#'
# ------------------------------------------------

## NOTE:  OK for this to work we have to have the exact same x-axis scale
#   on both plots... so we have to compute the maximum range of the x scale
#  to accomodate both data sets and then set the x limits to have this
#   exact range.....  TODO

# NOTE:  TODO  I don't think the combo plot works with faceting.
#   So we will need to use a ggplot object that doesn not have
#     facets.  This is a non-faceted plot....
#  Maybe make a plot_streams_ggplot that does not facet at all.....
##  Hmmm yes we have that but the datetime axis still doesn't show up...


#  Note this is the new version of the context/stream combo plot
#   to make a more standard function with standard inputs.  Not
#    currently used in AirMotive... NK 11/22/2021


plot_combo_context_streams_ggplot <- function(contexts, streams,
                                              contexts.args=list(),
                                              streams.args=list(),
                                              heights=c(0.5,0.5)) {
  if (!NROW(contexts))
    stop("'contexts' must contain at least one context in long format.")

  if (!NROW(streams))
    stop("'streams' must contain at least one data stream in long format.")

  # Compute common limits for date-time axis
  xlimits <- range(contexts$Time, streams$Time)

  if (!"xlimits" %in% contexts.args)
    contexts.args <- c(contexts.args, list(xlimits=xlimits))
  if (!"xlimits" %in% streams.args)
    streams.args <- c(streams.args, list(xlimits=xlimits))

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

  #  Timeline plot in ggplot format
  cc0<- colorRampPalette(c("#C2FCF6","#033FFF"))
  cc1<- colorRampPalette(c("#69DADB","#193498"))
  cc2<- colorRampPalette(c("#D3E0EA","#0A043C"))
  fig2 <- exec("plot_state_timeline", timeline, !!!contexts.args) +
    theme_streams_clean() +
    theme(
      axis.text.x=element_text(size=8, hjust=0.5),
      legend.justification = c(0,0),
      legend.text = element_text(size=8),
      axis.title = element_text(size=14),
      legend.title = element_blank(),
      axis.text.y = element_blank(),
      axis.line.x = element_blank(),
      axis.ticks.y = element_blank(),
      axis.title.x = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_line(size=0.3, color="black", linetype="dotted"),
      panel.ontop = FALSE
    ) +
    scale_fill_manual(values=c(sample(cc0(4),4),sample(cc1(5),5),sample(cc2(5),5))) +
    guides(fill=guide_legend(ncol=6, byrow=TRUE, title="Activities"))

  #  Data stream plot:  v2 uses long format as input
  fig1 <- exec("plot_streams_ggplot", streams, !!!streams.args) +
    theme(
      #text=element_text(family="kumbhsans"),
      legend.position="none",
      #axis.title.x = element_text(size=18),
      axis.title.x = element_blank(),
      axis.title.y = element_text(size=14),
      panel.grid.major.x = element_line(size=0.3, color="black", linetype="dotted")

    ) +
    scale_colour_manual(values=c("darkblue")) +
    scale_fill_manual(values="#B1D0E0")

  # Combo plot

  #grid.newpage()
  #grid.draw(rbind(ggplotGrob(fig1), ggplotGrob(fig), size = "last"))

  cowplot::plot_grid(fig1, fig2, align = "v", ncol = 1, rel_heights = heights)
  #egg::ggarrange(fig1, fig2, heights = heights)
  #fig1
}
