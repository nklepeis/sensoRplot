#' @aliases plot_streams
#'
#' @title Plot sensor data streams faceted by time using ggplot
#'
#' @author Neil Klepeis
#'
#' @param data Long-format sensor data frame for multiple streams
#' containing a 'Time' column, index columns, and Response and Value columns
#' @param by time unit used to create an index variable to use in faceting
#' data by time
#' @param by.format time format for strip lables
#' @param calendar logical, whether to create a calendar-style facet
#' by adding padding on either side to create dummy (blank) panels
#' @param area logical, whether to fill in area under the curve when plotting data
#' @param step logical, whether to draw steps to represent data points
#' @param line logical, whether to draw lines when plotting data
#' @param point logical, whethe to draw points when plotting data
#' @param facet logical, whether to create wrapped facets (panels) conditioned on the
#' 'by' parameter
#' @param pad logical, whether to pad time series to nearest start or end of "by"
#' time period
#' @param numcols the number of columns to use when faceting
#' @param alpha the alpha value to draw data series (fills)
#' @param date_labels the tick labels to use for time on the x-axis
#' @param date_breaks the tick breaks to use for time on the x-axis
#' @param xlimits a 2-element vector of limits or a function taking defaults limits and outputting new limits
#' for the datetime x axis.
#' @param show.legend logical, whether to show a legend for area/line data series (Response)
#' @param scales the 'scales' argument to ggplot facets determining whether x and y axis sacles
#' are allowed to be "free" or remain "fixed" over all panels
#' @param x.lab label to use for the x-axis (Time)
#' @param y.lab label to use for the y-axis (Value)
#' @param title the title to use for the plot
# @param panel.spacing spacing between panels in units of "lines", default 0
# @param tick.length length of ticks in units of "lines", default 1
# @param fg foreground color for all plot elements
# @param bg.strip background color for facet strips
# @param bg.panel background color for facet panels
# @param bg.plot background color for the plot
# @param grid.color color of the grid lines
# @param border.panel.color color of the panel border
# @param axis.color.ticks color of the axis ticks
# @param axis.color.line color of the axis lines
# @param strip.line.size size of the strip border lines
# @param panel.line.size size of the panel border lines
#'
#' @return a ggplot object
#'
#' @details
#'
#' This function is intended to be used in create attractive and easy to understand
#' time series plots faceted by a specified time grouping, defaulting to a single day.
#'
#' If variables not Time, Response, or Value are in the passed data, the function
#' concatenates all columns not Time, Response or Value to a single index
#' variable that is used to facet data series in addition
#' to faceting by the specified time grouping.
#----------------------------------------------------

## TODO !!  Create a custom theme for this plot that is applied automatically
##        but users can customize or specify another theme.  Done.

## Standard ggplot theme:  theme_gray()   Build from this for our custom theme

## TODO: Make this like facet_calendar, where we add dummy panels and dummy padding
#    data to make days fall in a calendar format with panels all the same time
#    duration...
#  Add a dummy data series that is plotted transparent color, with points at
#   midnight and y = 0.   Add for missing week back to nearest Monday (use lubridate)
#  TODO:  Shade weekends darker than weekdays (panel background)

## UPDATE:  Added dummy (invisible) series to make sure that the panels are visible and
#    have full x-axis range even if there are missing values in 'data'

plot_streams_ggplot <- function(data, by="1 day", by.format="%a, %m/%d",
                                calendar=TRUE, facet.response=FALSE,
                                area=TRUE, line=FALSE, step=FALSE, point=FALSE,
                                facet=TRUE, pad=TRUE, numcols=7, alpha=0.8,
                                xlimits=NULL,
                                upper.limit=NA,
                                labels = NULL,
                                date_labels="%I%p",
                                breaks = NULL,
                                date_breaks="12 hour",
                                show.legend=TRUE,
                                scales="free_x",
                                x.lab = "Time", y.lab = "Value",
                                title=paste("Sensor Streams Grouped by", by),
                                expand.x = c(0,0), expand.y=c(0,0),
                                mytheme=theme_streams_light, ...
) {


  #  If we have extra faceting variables, concatenate to a new
  #   Response variable, including old Response variable
  #   If we only have repsonse, then new response is same as old response

  #print(data)

  data <- data %>%
    unite("Response", -c(Time, Value))

  #print(data)

  # If breaks is null and date breaks is not, we
  #   reassign breaks to intervals in date_breaks
  #   starting at beginning of time interval.
  if (is.null(breaks) & !is.null(date_breaks)) {
    breaks <- seq.POSIXt(floor_date(min(data$Time), unit=by),
                         ceiling_date(max(data$Time), unit=by),
                         by=date_breaks)
    date_breaks <- waiver()
  }

  #print(breaks)


  # fill in NA=0 for missing times points starting/ending to nearest start/end of week/day
  if (pad) {

    if (calendar)
      minT <- floor_date(min(data$Time), unit="week")
    else
      minT <- floor_date(min(data$Time), unit="day")

    maxT <- ceiling_date(max(data$Time),  unit="23 hour")

    data <- data %>%
      group_by(Response) %>%
      pad_by_time(
        .start_date = minT, .end_date = maxT,
        .pad_value = 0  # set NA values to 0 (baseline values)
      )
  }

  #  Create factor for each time groups, equal to the starting time
  #    of the grouping
  data <- data %>%
    mutate(Starting = format(
        lubridate::floor_date(
          Time, unit = by
        ), format=by.format)
      ) %>%
   mutate(Starting=fct_reorder(Starting, Time)
    )

  #print(data)

  if (pad)
    ddummy <- data %>%
      mutate(Response="dummy")

  p <- ggplot(data=data,
              aes(x=Time, y=Value)) +
    scale_x_datetime(breaks=breaks, labels=labels,
                     date_labels = date_labels,
                     date_breaks = date_breaks,
                     expand=expand.x,
                     limits=if (is.null(xlimits)) NULL else xlimits) +
    scale_y_continuous(expand=expand.y, limits=c(0, upper.limit)) +
    labs(
      x = x.lab,
      y = y.lab,
      title = title
    )

  if (pad) p <- p + geom_blank(data=ddummy)

  if (area & !step) p <- p + geom_area(aes(fill=Response), show.legend=show.legend,
                               position="identity", alpha=alpha)

  if (line & !step) p <- p + geom_line(aes(color=Response), show.legend=show.legend)

  if (point) p <- p + geom_point(aes(color=Response), show.legend=show.legend)

  if (step) {
    p <- p + geom_step(aes(color=Response), show.legend=show.legend,
                             stat="identity", direction="hv")
    if (area) p <- p + geom_rect(aes(fill=Response, xmin = Time, xmax = lead(Time),
                             ymin = 0, ymax = Value), alpha = 0.3)
  }

  if (facet)
    if (!facet.response) {
      p <- p + facet_wrap(~Starting, nc=numcols, scales=scales,
                                labeller=label_value,
                                drop = FALSE)
    } else {
      p <- p + facet_grid(Response ~ Starting, scales=scales,
                          labeller=label_value,
                          drop = FALSE,
                          margins=FALSE)
    }

  #print(mytheme)

  p <- p + mytheme(...)

  # theme(
  #   axis.text = element_text(color = fg),
  #   axis.title = element_text(color = fg, size=),
  #   axis.ticks = element_line(color=axis.color.ticks, linetype="solid"),
  #   axis.line = element_line(color=axis.color.line, linetype="solid"),
  #   axis.ticks.length = unit(tick.length, "lines"),
  #   title = element_text(color = fg),
  #   panel.spacing = unit(panel.spacing, "lines"),
  #   strip.text.x = element_text(color=fg, hjust = 0.5),
  #   strip.background = element_rect(color = bg.strip, size=strip.line.size),
  #   panel.background = element_rect(color = border.panel.color, fill = bg.panel, size=panel.line.size),
  #   panel.grid.major = element_line(color = grid.color, linetype = "solid", size = 1),
  #   panel.grid.minor = element_line(color = grid.color, linetype = "solid", size = 1),
  #   plot.background = element_rect(fill = bg.plot)
  #
  # )

  p

}
