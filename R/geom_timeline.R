#' @name geom_timeline
#'
#' @title Add timeline data to a ggplot
#'
#' @description Add timeline data to an existing ggplot
#'
#' @author Neil Klepeis
#'
#' @param data a tibble in grouped state format with Time and one or more cols containing
#' state groups in which states are mtually exclusive states (only one state can occur
#' at a time)
#  [time, group, state, xleft, xright, ybottom, ytop]
#' @param auto.y compute automatic y spacing
#' @param group logical, whether to collapse states in a given group to a single lane
#' @param position where to group the timeline, "top", "bottom", "middle"
#' @param size.timeline a number between 0 and 1 giving vertical extent of the
#' timeline relative to the vertical plot area, e.g., 0.5 for 50% of the y extent
#' @param yspace y spacing between lanes in points
#' @param col.border color of borders of rectangles
#' @param size.border size of the rectangle border
#' @param alpha alpha value for the rectangle fill
#' @param ... arguments to pass to the \code{geom_rect} function
#'
#' @details
#'
#' TODO:  Have options to group the timeline at top, bottom, middle,
#'   or fill the entire plot area,  maybe given actual numeric range
#'   or "top", "bottom", "middle" and another parameter for how much of
#'   the y scale
#'
#'   grobTree
#'   textGrob
#'
#'   http://zevross.com/blog/2014/08/04/beautiful-plotting-in-r-a-ggplot2-cheatsheet-3/#working-with-annotation
#'
#'  "..using the library grid that allows you to specify the location based on scaled coordinates where 0 is low and 1 is high."

##  https://stackoverflow.com/questions/31722610/annotation-custom-with-npc-coordinates-in-ggplot2

# g <- rectGrob(y=0,height = unit(0.05, "npc"), vjust=0,
#              gp=gpar(fill="black"))
# p + annotation_custom(g, xmin=3, xmax=7, ymin=-Inf, ymax=Inf)


## ** Use Package "patchwork"  to merge two plots onto the same plot..
#    https://www.christophenicault.com/post/npc_ggplot2/

## ggpattern is cool making different fill patterns
#   https://coolbutuseless.github.io/package/ggpattern/


## This may work...
## https://stackoverflow.com/questions/51088379/ggplot2-add-annotation-spanning-facet-edges

## use current.viewport?   From panel.mtact

#  ylim <- current.viewport()$yscale
#  xlim <- current.viewport()$xscale
#  AR <- diff(ylim)/diff(xlim)
#  fullheight <- diff(range(ylim))
#  ytop <- ylim[2]

##  https://pparacch.github.io/2017/09/25/plotting_in_R_ggplot2_part_5.html

# -------------------------------------------------

# TODO: Make this a real geom_* function.

geom_timeline <- function (data, cols, auto.y=TRUE, group=FALSE,
                           position="middle", size.timeline=1,
                           yspace=0.05, col.border="lightgray",
                           size.border=0.3, alpha=0.5, ...) {


  data <- binary2timeline(
      convert_from_groupedstates(
      data %>% select(Time, all_of(cols))
    )
  )

  data <- data %>%
    mutate(
      groupstate = paste(group, state, sep="\n"),
      'duration.mins' = round(difftime(xright, xleft,
                                       units="mins"),
                              digits=2)) %>%
    arrange(groupstate, xleft)

  # compute y coordinatesS
  if (auto.y) {
    if (!collapse)
      idx <- unique(data$groupstate)
    else
      idx <- unique(data$group)
    ybottom <- 0:(length(idx)-1)
    names(ybottom) <- idx
    ytop <- (1:length(idx)) - yspace
    names(ytop) <- idx
    data <- data %>%
      mutate(ybottom = if (group)
        recode(group, !!!ybottom)
        else recode(groupstate, !!!ybottom),
        ytop = if (collapse)
          recode(group, !!!ytop)
        else recode(groupstate, !!!ytop)
      )
  }


  # g <- rectGrob(y=0,height = unit(0.05, "npc"), vjust=0,
  #              gp=gpar(fill="black"))
  # p + annotation_custom(g, xmin=3, xmax=7, ymin=-Inf, ymax=Inf)

  geom_rect(data=data,
            aes(xmin=xleft, xmax=xright, ymin=ybottom, ymax=ytop,
                fill = groupstate),
            color=col.border, size=size.border,
            alpha=alpha, ...)



}
