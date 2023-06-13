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
#' @param heights vector with elements in [0,1] giving the relative heights of the plots
#'
#' @details
#'
#' This function is similar to the plotly version that plots context data
#' in a separate but datetime-axis-linked below the sensor stream data.
#'
# ------------------------------------------------

## NOTE:  OK for this to work we have to have the exact same x-axis scale
#   on both plots... so we have to compute the maximum range of the x scale
#  to accommodate both data sets and then set the x limits to have this
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
                                              groupscales,
                                              contexts.args=list(),
                                              streams.args=list(),
                                              title=NULL,
                                              theme = "light",
                                              heights=0.5) {
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

  groupedstates <- extract_groupstates(contexts, sep=":")

  #  By default all groups are categorical variables
  #     Other scale types are
  if (missing(groupscales))
    groupscales <- list(
      Activities = list(
          groups = names(groupedstates),
          pre.fun = as.numeric,
          fillstate = "state",
          scale.fun = scale_fill_brewer,
          args = list(type="qual", palette=1)
        )
    )



  # Convert long to wide format contexts
  contexts <- contexts %>%
    pivot_wider(id_cols=c("Time"),
                names_from=c("Group","State"),
                values_from="Value",
                values_fn=min,   # if duplicates, use minimum value
                names_sep=":"
    ) %>%
    arrange(Time)

  #print(contexts)

  #  convert to timeline format from binary Wide Format
  timeline <- binary2timeline(contexts, time.var="Time")

  #print(timeline)


  # ------------------------------------------------------

  # #  default sequential color palettes from RColorBrewer
  # #  Used in order and recycled for different state groups.
  # CBpalSeq <- c("Blues","Purples","Greys","Reds","Oranges","Greens",
  #             "BuGn","RdPu","YlOrRd","PuRd","OrBu","GnBu")
  # CBpalCat <- c("Set3","Set2","Set1","Pastel2","Pastel1","Paired","Dark2","Accent")
  # CBpalDiv <- c("Spectral","BrBg","PRGn")
  #
  # CBpal <- c("Set3","Spectral","BrBg","PRGn")
  #
  # # TODO:  Groups are specified as seq, cat, div.. assign CB accordingly...
  #
  # CBpalSeq <- rep(CBpalSeq, length.out = length(names(groupedstates)))
  # CBpalCat <- rep(CBpalCat, length.out = length(names(groupedstates)))
  # CBpal <- rep(CBpal, length.out = length(names(groupedstates)))
  #
  # numstates <- unlist(lapply(groupedstates, length))
  #
  # groupcolors <- list()
  #
  # for (i in 1:length(groupedstates)) {
  #   groupcolors[[names(groupedstates)[i]]] <-
  #   colorRampPalette(brewer.pal(9, CBpal[i]))(numstates[i])
  #   names(groupcolors[[names(groupedstates)[i]]]) <-
  #     groupedstates[[i]]
  # }
  #
  # cat("The Colors:\n")
  # names(groupcolors) <- NULL
  # groupcolors <- unlist(groupcolors)
  # print(groupcolors)

  # ------------------------------------------------



  #  Timeline plot in ggplot format
  #cc0<- colorRampPalette(c("#C2FCF6","#033FFF"))
  #cc1<- colorRampPalette(c("#69DADB","#193498"))
  #cc2<- colorRampPalette(c("#D3E0EA","#0A043C"))

  #  R colorbrewer:  reds, oranges, purples, greens, greys, blues

  plots <-list()

  #  Data stream plot:  v2 uses long format as input
  n.streams <- unique(streams$Response)
  lcols <- colorRampPalette(c("darkblue","darkred"))(10)
  fcols <- colorRampPalette(c("blue","red"))(10)
  plots[[1]] <- exec("plot_streams_ggplot", streams, !!!streams.args)

  if (theme == "dark")
    plots[[1]] <- plots[[1]] + theme_streams_dark() +
    theme(
      panel.grid.major.x = element_line(size=0.3, color="gray", linetype="dotted"))
  else
    plots[[1]] <- plots[[1]] + theme_streams_light() +
    theme(
      panel.grid.major.x = element_line(size=0.3, color="gray", linetype="dotted"))

  plots[[1]] <- plots[[1]] +
    theme(
      #text=element_text(family="kumbhsans"),
      legend.position="none",
      #axis.title.x = element_text(size=18),
      axis.title.x = element_blank(),
      axis.title.y = element_text(size=14),

    ) +
    scale_colour_manual(values=lcols) +
    #scale_fill_manual(values="#B1D0E0")
    scale_fill_manual(values=fcols)




  ## TODO:  assign color palette for each group....
  j <- 1
  for (i in 1:length(groupscales)) {

    cat("Making context plot for groups: ", groupscales[[i]]$groups,"\n")

    timeline.groups <- timeline %>%
      filter(group %in% groupscales[[i]]$groups)

    if (is.function(groupscales[[i]]$pre.fun))
      timeline.groups <- timeline.groups %>%
      mutate(state = exec(groupscales[[i]]$pre.fun, state))

    cat("\nTimeline Groups:\n")
    print(timeline.groups)
    cat("\nGroup Scales:\n")
    print(groupscales[[i]])

    if (NROW(timeline.groups)) {

      j <- j + 1

      plots[[j]] <-
        exec("plot_state_timeline_ggplot", timeline.groups,
             fillstate = groupscales[[i]]$fillstate,
             !!!contexts.args)

      # Omit x axis labels in dark theme, make a cleaner plot...
      if (theme == "dark") plots[[j]] <- plots[[j]] + theme_contexts_dark() +
              theme(axis.text.x = element_blank(),
                    axis.ticks.x = element_blank(),
                    panel.grid.major.x = element_line(size=0.3, color="gray", linetype="dotted"),)
        else plots[[j]] <- plots[[j]] + theme_contexts_clean() +
          theme(axis.text.x = element_text(size=7, hjust=0.5),
                panel.grid.major.x = element_line(size=0.3, color="black", linetype="dotted"),)

      plots[[j]] <- plots[[j]] +
        theme(
          legend.justification = "right", #c(0,0),
          legend.position = "bottom",
          legend.text = element_text(size=8),
          axis.title = element_text(size=12),
          #legend.title = element_blank(),
          legend.title = element_text(size=11, face="bold"),
          legend.title.align = 0.5,
          legend.key.size = unit(0.5,"line"),

          axis.text.y = element_blank(),
          axis.line.x = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.x = element_blank(),
          panel.grid.major.y = element_blank(),

          panel.ontop = FALSE
        ) +
      #scale_fill_manual(values=c(sample(cc0(4),4),sample(cc1(5),5),sample(cc2(5),5))) +
      #scale_fill_manual(values=unlist(groupcolors)) +
      exec(groupscales[[i]]$scale.fun, !!!groupscales[[i]]$args ) +
      guides(fill=guide_legend(ncol=6, byrow=TRUE,
                               title=names(groupscales)[i]))

    }


  }





  # Combo plot

  #grid.newpage()
  #grid.draw(rbind(ggplotGrob(fig1), ggplotGrob(fig), size = "last"))

  #theplots <- cowplot::plot_grid(plotlist=plots, align = "v", ncol = 1,

  theplots <- plot_grid(plotlist=plots, align = "v", ncol = 1,
                                 rel_heights = heights)

  if (!is.null(title)) {
    # make a title
    title <- ggdraw() +
      draw_label(
        title,
        size = 18,
        fontface = 'bold',
        color = if (theme=="dark") "white" else "black",
        x = 0.5,
        hjust = 0.5
      )
    if (theme == "dark")
      title <- title +
          theme(
            plot.background = element_rect(fill = "black", color="transparent"),
            plot.title=element_text(color="white")
          )
    theplots <-
      plot_grid(
        title, theplots,
        ncol = 1,
        # rel_heights values control vertical title margins
        rel_heights = c(0.1, 1)
      )
  }

  theplots


  #egg::ggarrange(fig1, fig2, heights = heights)
  #fig1
}
