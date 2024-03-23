#' @aliases plot_state_timeline_plotly
#'
#' @title Plot an interactive timeline of state activity using plotly
#'
#' @author Neil Klepeis
#'
#' @param data a timeline format dataframe containing named columns for
#' group, state, xleft, xright, ybottom, ytop. x coordinates
#' are datetime values and y coordinates are numeric indices indicating
#' vertical height of state lanes
#' @param auto.y (logical) whether to automatically compute lane
#' y coordinates or use ybottom/ytop arguments.
#' @param xrange optional, vector of date/times used to specify the
#' xrange of the plot. If NULL, the xrange defaults to the range inherent in
#' the min and max time values in the data: c(min(xleft), max(xright)). This
#' option is used to visualize the contexts within a broader time period.
#' @param collapse (logical) whether to draw grouped states in a single lane or
#' draw individual state activity in separate lanes.  Collapsing will result
#' in overlap of state rectangles unless states in the group are
#' mutually exclusive
#' @param yspace vertical space with value [0,1] to add between lanes
# @param xspace horizontal space with value in seconds to add between states
# @param border thickness of border around rectangles
#' @param legend whether to draw a legend for the 0/1 states
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
#' @param shape.linecolor color of line around rectangles
#' @param line.width width of line around rectangles
#' @param source string identification for plot events
#' @param config logical, whether to configure the plot plot toolbar (may want
#' to skip this when configuring subplots)
#'
#' @details  This function creates a state timeline plot using the plotly library
#' the plotly \code{add_trace} function.   See \code{plot_state_timeline_ggplot} for
#' a version that uses the ggplot2 library, but also has an optio for converting
#' from a ggplot object to a plotly object using \code{ggplotly}.
#'
#'
# -----------------------------------------------

## OK Back to its home in SensoRplot  6/30/2023.....


## OK Nice update:  Turns out we can use add_polygons with a fill to draw
#    nice rectangles and have a hover inside the whole rectangle.  12/23/2022

# Some clean ups for background and fixing errors.  12/22/2022
#    TODO:  Look at the combo one in Airmotive that seemed to work OK...

# OK this seems to have come from contextualizer originally, then copied to Airmotive, then to
#  sensorPLot now back again!!!!!!      12/22/2022


##  Renamed from "plot_state_timeline2" to "plot_state_timeline_plotly"
#    18Feb2022

# TODO:    Have options to sort the order of the groups/states...

#  UPDATE:  This version uses only plotly and does not start with ggplot --> ggplotly
#    We use  plotly line traces to define the box regions and then use plotly::layout rectangle
#    shapes to shade the regions.

#   Stolen from ContextualizeR  24Feb2021.   This is the one to USE!!

#   Stolen from contextModels code 10/20/2020

#  TODO:  Come up with a scheme for computing/specifying the y coords
#  for each state or each group (i.e., if collapse=TRUE)

#   Use vistime R package for plotly !!!!

##  https://cran.r-project.org/web/packages/vistime/vignettes/vistime-vignette.html

#  Doesn't use vistime anymore

# OK We rewrote this from scratch.. uses line traces with popups
#    and filled rectangles to shade the states by fill color.    This
#    works with subplot, whereas version #1 based on ggplotly did not...


plot_state_timeline_plotly <-
  function (data, auto.y=TRUE, labels=TRUE,
            xrange=NULL, collapse=FALSE,
            yspace=0.05, #border=0.3,
            legend=FALSE, date_breaks=waiver(),
            date_labels="%I:%M:%S %p \n %a %m/%d",
            date_angle=0,
            popup_date="%a %m/%d/%y %I:%M:%OS3 %p",
            bg="white", fg="black",
            displayModeBar=TRUE,
            height=450,
            font.size=12,  shape.opacity = 0.4,
            shape.linecolor="lightgray", line.width=1,
            source="source", register=TRUE,
            showticklabelsY=TRUE, config=TRUE,
            verbose=FALSE) {

    #require(vistime)   #  rewrote from scratch...

    #  Set state colors.  like default palette for ggplot
    color_hue <- function(n) {
      hues = seq(15, 375, length = n + 1)
      hcl(h = hues, l = 65, c = 100)[1:n]
    }

    if (!NROW(data))
      stop("Timeline 'data' must contain at least one context.")

    #  Make cols names lower case
    data <- data %>% rename_with(tolower, any_of(c("State","Group")))
    #print(data)

    state.colors <- color_hue(length(unique(data$state)))
    names(state.colors) <- unique(data$state)

    #print(state.colors)

    #  Compute state duration in minutes
    data <- data %>%
      mutate(
        groupstate = paste(group, state, sep=":"),
        'duration.mins' = round(difftime(xright, xleft,
                                         units="mins"),
                                digits=2)) %>%
      mutate(statecolor = recode(state, !!!state.colors)) %>%
      arrange(groupstate, xleft)

    # We can sort the y labels if we want....TODO
    groupStates <- unique(as.character(data$groupstate))
    groups <- unique(as.character(data$group))

    if (auto.y) {
      if (!collapse)
        idx <- groupStates
      else
        idx <- groups
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


    #  Sort of works, but shading is off ....  do our own plotly routine.
    #p <- vistime(data, col.event="state", col.start="xleft", col.end="xright",
    #        col.group="group", optimize_y=TRUE, show_labels=TRUE,
    #        linewidth=25)
    #p$x$source <- "source"
    #p

    #  Base plotly plot
    p <- plot_ly(type = "scatter", mode = "lines", source=source,
                 height=height)

    #  Store values to draw filled rectangles for each state
    shapes <- list()

    #  Add range traces
    if (NROW(data) > 0) {

      for (i in seq_len(nrow(data))) {

        currentRow <- data[i, ]

        # p <- add_markers(p,
        #   x= c(currentRow$xleft, currentRow$xright, currentRow$xright,
        #        currentRow$xleft, currentRow$xleft),
        #   y = c(currentRow$ybottom, currentRow$ybottom, currentRow$ytop,
        #         currentRow$ytop, currentRow$ybottom),
        # )

        #  "Bar" is visualized by the width of the line
        p <- add_polygons(p,
                          #x = c(),
                          #y = currentRow$ybottom + (currentRow$ytop - currentRow$ybottom) /2,

                          ## !!!! These seem to be off by a few seconds on either side
                          #  a bit too late in the beginning and end..
                          #  No, I think we just need to add the milliseonds on there..

                          x= c(currentRow$xleft, currentRow$xright, currentRow$xright,
                               currentRow$xleft, currentRow$xleft),
                          y = c(currentRow$ybottom, currentRow$ybottom, currentRow$ytop,
                                currentRow$ytop, currentRow$ybottom),

                          ##y = currentRow$ybottom + (currentRow$ytop - currentRow$ybottom)*2/3,
                          #y = currentRow$ytop,
                          ##line = list(width = currentRow$ytop - currentRow$ybottom),
                          opacity=shape.opacity,
                          #line = list(width = 2, color=currentRow$statecolor), #currentRow$statecolor),   # width in pixel, need to use rectangle in layout ..
                          line = list(width = line.width, color=shape.linecolor),
                          showlegend = FALSE,
                          hoverinfo = "text",
                          textposition="left top",
                          #text = toAdd$tooltip
                          name = paste( "<b style='font-size:18px'>",
                                        paste(currentRow$group,":",currentRow$state,sep=""),
                                        "</b>\n",
                                        "Start: ", format(currentRow$xleft, popup_date), "\n",
                                        "End: ", format(currentRow$xright, popup_date), "\n",
                                        "Duration (mins): ", currentRow$duration.mins, "\n",
                                        sep=""
                          ),
                          #type = "contour",
                          fillcolor=currentRow$statecolor
                          #inherit = FALSE
        )

        # Add state labels on markers?
        if (labels) {
          p <- add_text(p,
                        #x = currentRow$xleft + (currentRow$xright - currentRow$xleft) /2,
                        x = currentRow$xleft,
                        #y = currentRow$ybottom + (currentRow$ytop - currentRow$ybottom)*4/5,
                        #y = currentRow$ybottom,
                        y = currentRow$ytop,
                        textfont = list(family = "Arial", size = font.size,
                                        color=fg),
                        textposition = "right bottom",
                        showlegend = FALSE,
                        text = currentRow$state,
                        hoverinfo = "none")

        }

        # don't seem to need these, since polygon hover is working.  NK 12/23/2022
        # shapes[[i]] <-
        #       list(type = "rect",
        #            fillcolor = currentRow$statecolor, opacity = shape.opacity,
        #            line = list(color = shape.linecolor),
        #            x0 = currentRow$xleft, x1 = currentRow$xright, xref = "x",
        #            y0 = currentRow$ybottom, y1 = currentRow$ytop, yref = "y")

      }

      #print(shapes)

      #print(data$ytop)
      #print(data$ybottom)
      if (verbose) {
        cat("y values:\n")
        print(data$ybottom+(data$ytop - data$ybottom)/2)
      }

      p <- p %>% layout(
        #shapes = shapes,
        plot_bgcolor=bg,
        #fig_bgcolor=bg
        #plot_bgcolor='gray90',
        paper_bgcolor= bg,
        yaxis = list(
          #tickangle=90,
          fixedrange=TRUE,
          color=fg,
          linecolor=fg, linewidth = 1,
          mirror = FALSE,
          showgrid = TRUE,  #gridcolor=fg,
          textfont=list(family="Arial", size=font.size,
                        color=fg),
          range = c(min(data$ybottom), max(data$ytop)),
          title = "",
          tickmode = "array",
          ticks="inside",
          #tickvals = seq(0.5, max(data$ytop)),
          tickvals = unique(data$ybottom + (data$ytop - data$ybottom)/2),
          tickfont = list(
            size=font.size
          ),
          #ticktext = as.character(unique(data$group)),
          ticktext = if (collapse) groups
          else groupStates,
          showticklabels = if (showticklabelsY) TRUE
          else FALSE

        ),
        xaxis = list(
          xrange=xrange,
          fixedrange=FALSE,
          color=fg
        )
      )

    }

    if (register) {
      if (verbose)
        cat("\n\nRegistering plotly source: ", source,"\n\n")
      p <- p %>%
        event_register('plotly_legendclick') %>%
        event_register('plotly_relayout')

    }

    p

  }



#  vistime plotly plot routine.

# plot_plotly <- function(data, linewidth, title, show_labels, background_lines) {
#
#   # 1. Prepare basic plot
#   p <- plot_ly(type = "scatter", mode = "lines")
#
#   y_ticks <- tapply(data$y, data$subplot, mean)
#
#   # 2. Divide subplots with horizontal lines
#   hline <- function(y = 0) list(type = "line", x0 = 0, x1 = 1, xref = "paper", y0 = y, y1 = y, line = list(color = "grey65", width = 0.5))
#   vline <- function(x = 0) list(type = "line", y0 = 0, y1 = 1, yref = "paper", x0 = x, x1 = x, line = list(color = "grey90", width = 0.1))
#   horizontal_lines <- lapply(setdiff(seq_len(max(data$y)), data$y), hline)
#
#   # 3. Add vertical lines
#   if(!is.null(background_lines)){
#     day_breaks <- as.POSIXct(seq(min(c(data$start, data$end)), max(c(data$start, data$end)),
#                                  length.out = round(background_lines) + 2), origin = "1970-01-01")
#     vertical_lines <- lapply(day_breaks, vline)
#   }else{
#     vertical_lines <- list()
#   }
#
#   p <- layout(p,
#               hovermode = "closest",
#               plot_bgcolor = "#FCFCFC",
#               title = title,
#               shapes = append(vertical_lines, horizontal_lines),
#               # Axis options:
#               xaxis = list(linewidth = 1,  mirror = TRUE,
#                            showgrid = is.null(background_lines),
#                            gridcolor = "grey90", title = ""),
#               yaxis = list(
#                 linewidth = 1, mirror = TRUE,
#                 range = c(0, max(data$y) + 1),
#                 showgrid = F, title = "",
#                 tickmode = "array",
#                 tickvals = y_ticks,
#                 ticktext = as.character(unique(data$group))
#               )
#   )
#
#   # 4. plot ranges
#   range_dat <- data[data$start != data$end, ]
#
#   lw <- ifelse(is.null(linewidth), min(100, 300/max(data$y)), linewidth) # 1-> 100, 2->100, 3->100, 4->70
#
#   if(nrow(range_dat) > 0){
#     # draw ranges piecewise
#     for (i in seq_len(nrow(range_dat))) {
#       toAdd <- range_dat[i, ]
#
#       p <- add_trace(p,
#                      x = c(toAdd$start, toAdd$end), # von, bis
#                      y = toAdd$y,
#                      line = list(color = toAdd$col, width = lw),
#                      showlegend = F,
#                      hoverinfo = "text",
#                      text = toAdd$tooltip
#       )
#       # add annotations or not
#       if (show_labels) {
#         p <- add_text(p,
#                       x = toAdd$start + (toAdd$end - toAdd$start) / 2, # in der Mitte
#                       y = toAdd$y,
#                       textfont = list(family = "Arial", size = 14, color = toRGB(toAdd$fontcol)),
#                       textposition = "center",
#                       showlegend = F,
#                       text = toAdd$label,
#                       hoverinfo = "none"
#         )
#       }
#     }
#   }
#
#   # 5. plot events
#   event_dat <- data[data$start == data$end, ]
#   if(nrow(event_dat) > 0){
#     # alternate y positions for event labels
#     event_dat$labelY <- event_dat$y + 0.5 * rep_len(c(1, -1), nrow(event_dat))
#
#     # add all the markers for this Category
#     p <- add_markers(p,
#                      x = event_dat$start, y = event_dat$y,
#                      marker = list(
#                        color = event_dat$col, size = 0.7 * lw, symbol = "circle",
#                        line = list(color = "black", width = 1)
#                      ),
#                      showlegend = F, hoverinfo = "text", text = event_dat$tooltip
#     )
#
#     # add annotations or not
#     if (show_labels) {
#       p <- add_text(p,
#                     x = event_dat$start, y = event_dat$labelY, textfont = list(family = "Arial", size = 14,
#                                                                                color = toRGB(event_dat$fontcol)),
#                     textposition ="center", showlegend = F, text = event_dat$label, hoverinfo = "none"
#       )
#     }
#
#   }
#
#
#   return(p)
# }
