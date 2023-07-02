#' @name plot_log_probability_plotly2
#'
#' @title Plot log-probability for sensor data streams
#'
#' @description Plot log-probability for selected responses
#'
#' @author Neil Klepeis
#'
#' @param data Long-format sensor data frame for multiple streams and monitoring sites
#' containing a 'Time' column, index (grouping) columns, and Response and Value columns
#' @param xtics the tics to use on the x (probability) axis
#' @param averaging averaging time used to aggregate the data streams
#' @param title title of the plot
#' @param height height of the plot in pixels
#' @param fg foreground color for text
#' @param bg background color
#' @param plot.bg plot background color
#' @param legend.bg legend background color
#' @param legend.fg legend foreground color
#' @param marker.size symbol size for plotted series
#' @param line.width linewidth for plotted series
#'
#' @return a plotly object
#'
#' @details This function plots data stream values on log-probability axes
#' for all responses and all sites in the passed data frame
#'
# ----------------------------------------------------

## Uses log.tics and plot.log.probability
#     Stolen from the senoRplot package.    12/19/2022

## I think the plotly version was new to airMotive ST.
#   this was taken from AirMotiveST 6/30/2023.  Now in sensorPlot!

plot_log_probability_plotly2 <-
  function(data,
           xtics=c(0.0001,0.001, 0.05, 0.10, 0.25, 0.5, 0.75, 0.9, 0.95, 0.999, 0.9999),
           averaging=NULL,
           title=NULL,
           height=500, legend.inside=FALSE,
           fg="white", bg='rgba(23,103,124,1)',
           plot.bg = "gray90",
           legend.bg='rgba(255,255,255,0.75)',
           legend.fg="black",
           line.width=1, marker.size=8) {

    # require(tidyverse)
    # require(plotly)
    # require(timetk)

    # Now aggregate
    if (!is.null(averaging)) {
      cat("Aggregrating by: ", averaging, "\n")

      data <- data %>%
        select(Name, Response, Time, Value) %>%
        group_by(Name, Response) %>%
        summarize_by_time(Value = mean(Value, na.rm=TRUE),
                          .by=averaging)
    }

    data <- data %>%
      unite(col="Name", !any_of(c("Time","Value")), sep=":")

    index <- unique(data$Name)
    cat("log-probability Names:\n")
    print(index)
    #if (reverse)  index <- rev(index)

    # Need to do a plotly version...
    # plot_log_probability(
    #   xtics=c(0.10, 0.25, 0.5,0.75, 0.9),
    #   ytics=c(0.01,0.3), ylab="Particle Diameter, Dp [microns]") +
    #   labs(title="Particle Size Distribution by Source, Time, and Sensor",
    #        subtitle="Log-Probability Plot") +
    #   geom_abline(data=streamsLN.ALL, aes(slope=slope, intercept=intercept, color=Source),
    #               size=0.2, linetype="dashed") +
    #   facet_grid(SN ~ Activity) +
    #   guides(color = guide_legend(override.aes = list(size = 1)))

    #fig <- plot_ly(height=height, alpha=0.6)  # Empty plot

    fig <- plot_log_probability(
      xtics=xtics,
      ytics=log_tics(data$Value)$major.tics) +
      labs(title="Log-Probability Plot") +
      ylab("Sample Quantiles") +
      xlab("Normal Cumulative Probability (%)")

    for (i in index) {
      fig <- fig +
        geom_qq(data=data %>% filter(Name==i),
                aes(sample=Value, color=Name))
    }

    fig <- fig +
      theme(
        axis.text = element_text(color=fg, size=11),
        axis.title =  element_text(color=fg, size=14),
        axis.ticks = element_line(color="gray", linetype="dotted")

      )


    fig <- ggplotly(fig)


    #### PLOT LAYOUT

    mrg <- list(l = 50, r = 50,
                b = 50, t = 60,
                pad = 0)

    fig <- fig %>% layout(
      barmode = "overlay",
      plot_bgcolor=plot.bg,
      paper_bgcolor= bg,
      title = if (is.null(title))
        NULL
      else
        list(text = title, xref="paper",
             font = list(
               family = "sans-serif",
               size = 20,
               weight="bold",
               color = "white"),
             x = 0.5, y = 120,
             pad = list(b = 150, l = 0, r = 20 )),
      legend = if (legend.inside) {
        list(x = 0.99, y = 0.99, orientation="h",
             xanchor="right",
             font = list(
               family = "sans-serif",
               size = 11,
               color = legend.fg),
             bgcolor = legend.bg,
             borderwidth = 0)
      } else {
        list(
          x = 100, y = 0.5, orientation="v",
          xanchor="right",
          font = list(
            family = "sans-serif",
            size = 11,
            color = legend.fg
          ),
          bgcolor=legend.bg,
          borderwidth = 0
        )

      },

      xaxis = list(
        #type = if (logaxis) "log" else NULL,
        color=fg,
        gridcolor="gray",
        rangeselector = NULL,
        rangeslider = NA,
        tickfont = list(
          size=10
        )
      ),

      yaxis=list(
        color=fg,
        gridcolor="gray",
        #fixedrange=TRUE,
        #rangemode = "tozero",
        tickfont = list(
          size=10
        )
      ),

      margin=mrg

    ) %>%
      plotly::config(displayModeBar = TRUE,
                     toImageButtonOptions = list(
                       format = "svg",
                       filename = "datastreams_log_probability_plot",
                       width = 1200,
                       height = 800
                     )
      )

    fig

  }

