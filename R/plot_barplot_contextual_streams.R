#' @title Barplot of Contextual Data Streams for Current WorkSpace
#'
#' @description Create a barplot of medians for data streams broken out by contextual states for
#' the current data-stream workspace
#'
#' @author Neil Klepeis
#'
#' @param streams data frame in long format containing sensor data streams
#' @param contexts data frame containing contextual data in "active-state" format for each
#' defined workspace
#'
#' @return Returns a list of lists containing plots for current
#' workspace and combination of contextual states
#'
#' @details
#'
#'  IN FUTURE:  (TODO)
#'
#' Plot by each combination of states
#'
#' Aggregate:
#'
#' 1.  Within workspace, across state groups (exposure from mobile site/recpeotr)
#'
#' 2.  Within state grouping, across workspace (exposure from fixed sites)
#'
#'
# -------

#   data frames:

#   streams = airData$df
#   contexts = context$active

# --------------------------------------------------------

#  TODO:  Do a facet_wrap by Contexts and have bars for each data stream
#     inside each panel....

## IDEA:  Save contexts in timeline mode?  And convert to long format "in app"?

## IDEA!!:  more compact longFormat :  missing states are assumed to be "0"
##   in active....  We can combine with a context state spec
#   BUT, if no states are active how do we know which ones have ended?

# NO just juse the timeline format.

# TODO:  this is for the CURRENT WORKSPACE only... we can do multiple workspaces
#    later with a different function...

plot_barplot_contextual_streams <- function(streams, contexts,
                                    bg="white", fg="black",
                                    date_angle=30,
                                    height=600) {

  #require(dplyr)
  #require(purrr)
  #require(ggplot2)
  #require(readr)
  #require(naniar)
  #require(forcats)
  #require(plotly)
  #require(stringi)

  # #  1.  Get state combinations within each stream workspace
  #
  # timeline %>%
  #   mutate(groupState = paste(group, state, sep=":")) %>%
  #   select(-c(group, state)) %>%
  #   group_by(userName, Workspace, Time) %>%
  #   group_map()

  # Time, Label, Response, Value

  # sort contexts
  contexts <- contexts %>% arrange(Time)

  # Match States by Time
  streams <- streams %>%
    mutate(
      States = purrr::pmap_chr(list(Time),
                               function(Time) {
                                 idx <- rank(c(Time, contexts$Time),ties.method="last")[1] - 1
                                 if (idx > 0) contexts$States[idx] else NA_character_
                               }
      )
    ) %>%
    replace_with_na(replace=list(States = "")) %>%
    drop_na() %>%
    mutate(States2 = stri_replace_all_fixed(States, " | ", "\n"))


  x <- streams %>%
    group_by(States, States2, Label, Response) %>%
    summarize(Median = median(Value, na.rm=TRUE)) %>%
    #mutate(States = fct_reorder(States, Median)) %>%
    arrange(Median)

  print(x)

  panel.data <- x %>% ungroup() %>%
    group_by(States, States2, Response) %>%
    summarize(xmid = max(Median, na.rm=TRUE)/2,
              ymid = (n()-1)/2)

  print(panel.data)

  maxMedian <- max(x$Median)
  maxNum <- length(unique(x$Label))

  p <- ggplot(x, aes(fill=Response, y=reorder(Label,Median), x=Median)) +
                     #text = paste( "<b style='font-size:18px'>",
                    #               States, ":<br>",Median,
                    #               "</b>", sep=""))) +
    geom_bar(position="dodge", stat="identity") +
    facet_wrap(~States2, labeller = label_wrap_gen(width=80)) +
    geom_text(size=3, color=fg, aes(label=Median)) +
    geom_text(data=panel.data, size=2.6, color=fg,  x=maxMedian/1.6, y=maxNum/2,
              aes(label=States2)) +
    labs(
      x = "Median",
      y = "Label",
      title = "Data Stream Median Values by Context"
      #subtitle = ""
    ) +
    theme(
      #axis.text.y = element_text(angle = date_angle, hjust = 1,
      #                           size=9),
      panel.grid.major.x = element_line(color=fg, size=0.3,
                                        linetype="dotted"),
      panel.grid.minor.x = element_line(color=NA),
      panel.grid.major.y = element_line(color=NA),
      panel.grid.minor.y = element_line(color=NA),
      panel.background = element_rect(fill = NA),
      #axis.line=element_line(color="white", linetype="solid", size=1),    # border line
      panel.ontop=TRUE,
      plot.background=element_rect(fill = bg),
      plot.title=element_text(color=fg),
      axis.title=element_text(color=fg),
      axis.text.x=element_text(color=fg, size=12),
      axis.text.y=element_text(color=fg, size=12),
      axis.ticks.x=element_line(color=fg),
      axis.ticks.y=element_line(color=fg),
      legend.background=element_rect(fill=bg),
      legend.text = element_text(color=fg),
      strip.background = element_blank(), #rect(color=rgb(0,0,0,0.3)),
      #strip.text.x = element_text(size=11, margin = margin(24,0,0,0, "pt"))
      strip.text.x = element_blank()
    )

  g <- ggplotly(p, height=height) %>%
    layout(xaxis=list(fixedrange=FALSE, linecolor=fg, linewidth=2, mirror=TRUE)) %>%
    layout(yaxis=list(fixedrange=FALSE, linecolor=fg, linewidth=2, mirror=TRUE)) %>%
    layout(hoverlabel=list(font=list(color="white",size=14)))

  g <- g %>% plotly::config(displayModeBar = TRUE,
                            toImageButtonOptions = list(
                              format = "png",
                              filename = "bycontext_barplot",
                              width = 1200,
                              height = 600
                            ))


  g

  #print(streams)
  #write_csv(streams, file="streamswithcontexts.csv")

  # streams <-
  #   streams %>%
  #   group_by(SensorID) %>%
  #   mutate(UniqueID = NA, Role = NA, VisitABC=NA, VenueID =NA, Date=NA) %>%
  #   group_modify(function(x, y) { # x = group data tibble; y = group var tibble
  #     z <- qaSRC %>% filter(SRC_SidePakID2 == y$SensorID)
  #     for (i in 1:NROW(z)) {
  #       idx <- x$DateTime >= z$SRC_StartTime[i] & x$DateTime <= z$SRC_EndTime[i]
  #       x$UniqueID[idx] <- z$UniqueID[i]
  #       x$Role[idx] <- "SRC"
  #       x$VisitABC[idx] <- z$VisitABC[i]
  #       x$VenueID[idx] <- z$SRC_VenueID[i]
  #       x$Date[idx] <- z$SRC_Date1[i]
  #     }
  #     x
  #   }) %>%
  #   ungroup() %>%
  #   drop_na()



}
