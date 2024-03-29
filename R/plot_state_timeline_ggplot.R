#' @aliases plot_state_timeline_ggplot
#'
#' @title Plot a static timeline of state activity using ggplot
#'
#' @description This function creates a static timeline plot of context states
#' using the ggplot2 library with the option of converting to an interactive
#' timeline using the \code{ggplotly} function in the plotly library
#'
#' @author Neil Klepeis
#'
#' @param data a timeline format dataframe containing named columns for
#' group, state, xleft, xright, ybottom, ytop. x coordinates
#' are datetime values and y coordinates are numeric indices indicating
#' vertical height of state lanes
#' @param ggplot logical, whether to return a ggplot object (default) or convert
#' from ggplot to plotly object
#' @param fillstate the variable to use in defining the state fill color,
#' either "group" or "groupstate"
#' @param auto.y (logical) whether to automatically compute lane
#' y coordinates or use ybottom/ytop arguments.
# @param xlimits optional, a 2-element vector of date/times used to specify the
# x limits of the plot. If NULL, the xlimits defaults to the range inherent in
# the min and max time values in the data: c(min(xleft), max(xright)) OR a function taking the
#  default limits and returning new limits.
# option is used to visualize the contexts within a broader time period.
#' @param collapse (logical) whether to draw grouped states in a single lane or
#' draw individual state activity in separate lanes.  Collapsing will result
#' in overlap of state rectangles unless states in the group are
#' mutually exclusive
#' @param lanes logical, added shaded rectangles to highlight lanes for each group/state
#' @param yspace vertical space with value [0,1] to add between lanes
# @param xspace horizontal space with value in seconds to add between states
#' @param border thickness of border around rectangles
#' @param color.border color of border lines for rectangles
#' @param legend whether to draw a legend for the 0/1 states
#' @param lab.x x axis label
#' @param lab.y y axis label
#' @param date_breaks a string giving the datetime breaks for the plot
#' (e.g., "10 mins", "3 hours")
#' @param date_labels the format for the datetime axis labels
#' @param date_angle angle to position date axis labels
#' @param popup_date format for dates in the popups (tooltips)
# @param fill.colors a two-element vector containing
# the colors to use to fill in the 0 and 1 state values, respectively.
#' @param bg the background color for the plot
#' @param fg the foreground color for the plot (text and axis lines)
#' @param displayModeBar If using plotly, whether to display a toolbar
#' over the plot or not
#' @param height a number giving the height of the plotly plot in pixels
#' @param source string identification for plot events
#' @param config logical, whether to configure the plot toolbar (may want
#' to skip this when configuring subplots)
#' @param verbose logical, whether to print out debugging output, defaults to FALSE
#'
#' @details  This function creates a static state timeline plot using the ggplot2 library.
#' It has an option to convert from a ggplot object to an interactive plotly object using the
#' \code{ggplotly} function.
#'
# -----------------------------------------------

## TODO:  Add faceting to this so we can plot contexts in panels
#  TODO:    ALSO, create a version that combines stream and context
#        data in the same facet panels!!!  with some nice transparency settings...
#    like we did with lattice plots a long time ago...
## TODO:  We may have to have a 'breaks' argument to insure when
#     ticks are drawn...like with streams version.

# Copied from AirMotive,  12/18/2021..  modified to return ggplot
#   by default, and use ggplotly to output plotly if specified.

#   Stolen from ContextualizeR  24Feb2021.   This is the one to USE!!

#   Stolen from contextModels code 10/20/2020

#  TODO:  Come up with a scheme for computing/specifying the y coords
#  for each state or each group (i.e., if collapse=TRUE)


plot_state_timeline_ggplot <-
  function (data, ggplot=TRUE, fillstate="groupstate",
            auto.y=TRUE, collapse=FALSE,
            lanes=TRUE, yspace=0.05, border=0.3, color.border="lightgray",
            legend=FALSE, lab.x="Time", lab.y="States",
            title=waiver(),
            date_breaks=waiver(),
            date_labels="%I:%M:%S %p \n %a %m/%d",
            xlimits=NULL,
            date_angle=0,
            popup_date="%a %m/%d/%y %I:%M:%S %p",
            #fill.colors=c(rgb(240/255,255/255,240/255,0.5),
            #              "tomato"),
            bg="white", fg="black",
            displayModeBar=TRUE,
            height=450,
            source="source", config=TRUE,
            verbose=FALSE) {

    #require(ggplot2)
    #require(dplyr)
    #require(plotly)

    #  If no state data are available, then
    #     return an empty ggplot
    # if (!is.data.frame(data)) {
    #
    #   height <- 80
    #   displayModeBar <- FALSE
    #
    #   p <- ggplot() +
    #     theme(
    #       plot.background=element_rect(fill = bg),
    #       plot.title=element_text(color=fg),
    #       panel.background = element_rect(fill = bg)
    #     ) +
    #     ggtitle("EMPTY State Timeline")
    #
    # } else {

      #  Compute state duration in minutes
      data <- data %>%
        mutate(
          groupstate = paste(group, state, sep=":"),
          'duration.mins' = round(difftime(xright, xleft,
                                           units="mins"),
                                  digits=2)) %>%
        arrange(groupstate, xleft)

      # compute y coordinates?
      if (auto.y) {
        if (!collapse)
          idx <- unique(data$groupstate)
        else
          idx <- unique(data$group)
        ybottom <- 0:(length(idx)-1)
        names(ybottom) <- idx
        ytop <- (1:length(idx)) - yspace
        names(ytop) <- idx
        #print(ybottom)
        #print(ytop)
        data <- data %>%
          mutate(ybottom = if (collapse)
            recode(group, !!!ybottom)
            else recode(groupstate, !!!ybottom),
            ytop = if (collapse)
              recode(group, !!!ytop)
            else recode(groupstate, !!!ytop)
          )
      }

      # TODO:  Compute y coordinates for collapsed groups and name
      #     the lane as the group.   Have a legend for all the
      #     state colors??

      # Recode "content"
      # TODO:  Don't need this, only plotting the active = 1 states...
      # data$content[data$content == 1] <- "Active"
      # data$content[data$content == 0] <- "Inactive"
      # names(data)[names(data) == "content"] <- "Activity"

      #names(fill.colors) <- c("Inactive","Active")

      if (verbose) {
        cat("Plotting timeline \n")
        print(head(data))
      }

      ## TODO: Lanes data
      lanesData <- tibble(
        xleft=min(data$xleft),
        xright=max(data$xright),
        ybottom=unique(data$ybottom),
        ytop=unique(data$ytop)
      )

      p <- ggplot(data=data, aes(xmin=xleft, xmax=xright, ymin=ybottom, ymax=ytop,
                            fill = .data[[fillstate]],
                            text = paste( "<b style='font-size:18px'>",
                                          paste(group,":",state,sep=""),
                                          "</b>\n",
                                          "Start: ", format(xleft, popup_date), "\n",
                                          "End: ", format(xright, popup_date), "\n",
                                          "Duration (mins): ", duration.mins, "\n",
                                          sep="")
            )) + geom_rect(color=color.border,
                           size=border) +
        #geom_hline(yintercept=data$ytop, color="gray",
        #           size=0.2) +
        #geom_hline(yintercept=data$ybottom, color="gray",
        #           size=0.2) +
        #geom_vline(xintercept=data$xleft, color="gray",
        #           size=0.2) +
        #geom_vline(xintercept=data$xright, color="white",
        #           size=1) +
        scale_x_datetime(expand = c(0, 0), date_breaks=date_breaks,
                         date_labels=date_labels,
                         limits=if (is.null(xlimits)) NULL else xlimits) +
        scale_y_continuous(expand = c(0, 0),
                           breaks=(data$ybottom+data$ytop)/2,
                           labels=if (collapse)
                             as.character(data$group)
                           else as.character(data$groupstate)
        ) +
        #scale_fill_manual(values = fill.colors) +
        xlab(lab.x) +
        ylab(lab.y) +
        labs(title = title) +
        #ggtitle(paste("State Timeline for '", enviro, "' Environment",sep="")) +
        theme(
          axis.text.x = element_text(angle = date_angle, hjust = 1,
                                     size=9),
          panel.grid.major.x = element_line(color=fg, size=0.3,
                                            linetype="dotted"),
          panel.grid.minor.x = element_line(color=NA),
          panel.grid.major.y = element_line(color=NA),
          panel.grid.minor.y = element_line(color=NA),
          panel.background = element_rect(fill = NA),
          #axis.line=element_line(color="white", linetype="solid", size=1),    # border line
          panel.ontop=TRUE
        )

      #  Add shaded rectangles as lanes?  TODO:  Not working yet...
      #if (lanes) p <- p + geom_rect(data=lanesData,
      #                              aes(xmin=xleft, xmax=xright,
      #                              ymin=ybottom, ymax=ytop),
      #                              fill="lightgray")

      if (!legend) p <- p + theme(legend.position='none')

      p <- p +
        theme(
          plot.background=element_rect(fill = bg),
          plot.title=element_text(color=fg),
          axis.title=element_text(color=fg),
          axis.text=element_text(color=fg),
          axis.ticks=element_line(color=fg)
        )

    #}

    # Convert to plotly object or keep as ggplot object?
    if (!ggplot) {

      p <- ggplotly(p, height=height, source=source,
               tooltip=c("text")) %>%
        layout(xaxis=list(fixedrange=FALSE, linecolor=fg, linewidth=2, mirror=TRUE)) %>%
                        #  range=if (is.null(xrange)) NULL else range(xrange))) %>%
        layout(yaxis=list(fixedrange=FALSE, linecolor=fg, linewidth=2, mirror=TRUE)) %>%
        layout(hoverlabel=list(font=list(size=14)))

      if (config)
        p <- p %>% plotly::config(displayModeBar = displayModeBar,
               toImageButtonOptions = list(
                 format = "svg",
                 filename = "timeline_plot",
                 width = 1200,
                 height = 700
               )
        )

    }


    p

  }
