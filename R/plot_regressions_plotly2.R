#' @name plot_regressions_plotly2
#'
#' @title Plot linear regressions for sensor data streams
#'
#' @description Plot linear regressions for all combinations of multiple dependent and
#' independent variables between and within multiple monitoring sites
#'
#' @author Neil Klepeis
#'
#' @param data Long-format sensor data frame for multiple streams and monitoring sites
#' containing a 'Time' column, index (grouping) columns, and Response and Value columns
#' @param xvars Response variables that are to be used as the independent (x-axis) variables
#' @param yvars Response variables that are to be used as the dependent (y-axis) variables
#' @param averaging Averaging time to use when aggregating the Values, defaults to 10 min
#' @param height height of the plot in pixels
#' @param fg foreground color for text
#' @param bg background color
#' @param plot.bg plot background color
#' @param legend.bg legend background color
#' @param legend.fg legend foreground color
#' @param line.width linewidth for plotted series
#' @param marker.size marker (symbol) size in px for plotted series
#'
#'
#' @return a plotly object
#'
#' @details This function tags all the specified x and y data stream responses
#' in the passed 'data' tibble and plots all x-y combinations within and between
#' monitoring sites
#'
#' See 'plot_regressions_subplot_plotly.R' which calls this function
#' to produce stacked subplots for each non-Response index variable, i.e.,
#' all index variables besides Response are used to facet the data into
#' separated plots
#'
# ----------------------------------------------------

#  from airmotiveST 6/30/2023 now in SensoRplot!!  NK

#  Version to use LONG FORMAT DATA.    July 29, 2021. NK

#  TODO:  Add option for adding lines, rectangles, and text (with hover tooltips)
#   to the plot to visualize contexts.  (alternative to the subplot plotly approach)
#    which I couldn't get working for two separate stream/context plots... NK  3/4/2021
#  Done.  see plot combos..

##  TODO:  Add options for subplots for Response and each index variable
#   OR keep default of concatening all index variables + response to a single
#    index variable == "Name".   Done..


plot_regressions_plotly2 <- function(data, xsites, ysites, xvars, yvars,
                                     averaging = "10 min",
                                     title=NULL,
                                     height=500, legend.inside=FALSE,
                                     fg="white", bg='rgba(23,103,124,1)',
                                     plot.bg = "gray90",
                                     legend.bg='rgba(255,255,255,0.75)',
                                     legend.fg="black",
                                     line.width=1, marker.size=8) {

  library(tidyverse)
  library(tidymodels) # for the fit() function
  library(plotly)
  library(timetk)

  # x <- rep(TRUE, length(xvars))
  # y <- rep(TRUE, length(yvars))
  # names(x) <- xvars
  # names(y) <- yvars

  cat("\nUsing X Variables for Regressions:", xvars)
  cat("\nUsing X Sites for Regressions:", xsites)
  cat("\nUsing Y Variables for Regressions:", yvars)
  cat("\nUsing Y Sites for Regressions:", ysites)

  indexX <- paste(xsites, xvars, sep="__")
  indexY <- paste(ysites, yvars, sep="__")

  # Don't plot regressions for variables within a site that is both X and Y
  #   Only plot regressions for same variables BETWEEN sites

  ##   MODIFY DATA - filter, recode, group, and aggregate
  data <- data %>%
    #filter(Response %in% c(xvars, yvars), Name %in% c(xsites, ysites)) %>%
    #mutate(X=FALSE,
    #       Y=FALSE,
    #       X = recode(Response, !!!x),
    #       Y = recode(Response, !!!y)) %>%
    #unite(col="Name", !any_of(c("Time","Value","X","Y")), sep="__")
    unite(col="Name", !any_of(c("Time","Value")), sep="__") %>%
    filter(Name %in% indexX | Name %in% indexY)

  #%>%
  #group_by(Name, X, Y)
  #%>%
  #summarize_by_time(Value = mean(Value, na.rm=TRUE), .date_var="Time",
  #                  .by=averaging)

  #print(data)

  #indexX <- data %>% filter(X==TRUE) %>% distinct(Name) %>% pull(Name)
  #indexY <- data %>% filter(Y==TRUE) %>% distinct(Name) %>% pull(Name)



  # Now aggregate
  cat("Aggregrating by: ", averaging, "\n")

  data <- data %>%
    select(Name, Time, Value) %>%
    group_by(Name) %>%
    summarize_by_time(Value = mean(Value, na.rm=TRUE),
                      .by=averaging)


  # Wide versionof data
  data <- data %>%
    pivot_wider(id_cols = Time, names_from=Name, values_from=Value)

  cat("\nData for computing response regressions:\n")
  print(data)
  cat("\nX - Independent Variables:\n")
  print(indexX)
  cat("\nY - Dependent Variables:\n")
  print(indexY)

  #return(NULL)

  fig <- plot_ly(height=height)  # Empty plot

  ### ADD SERIES



  #if (!NROW(data)) {
  #  cat("No Values non-NA values to do regression")
  #  return(fig)
  #}

  equations <- c()

  #  loop through series (by Name)

  for (dep in indexY)

    for (ind in indexX) {

      # If X and Y are same variable (compound Site:Response), then omit
      if (dep != ind) {

        cat("Regressing `", ind, "` on `", dep, "`...\n",sep="")

        data2 <- data %>% drop_na(any_of(c(ind,dep)))

        if (NROW(data2)) {

          cat("Valid data points: ", NROW(data2), "\n")

          f <- formula(paste("`",dep,"`"," ~ ","`",ind,"`",sep=""))
          cat("Regression Formula: \n ")
          print(f)

          lm_model <- linear_reg() %>%
            set_engine('lm') %>%
            set_mode('regression') %>%
            fit(f, data = data2)

          cat("Model:\n")
          print(tidy(lm_model))
          #print(summary(lm_model$fit))

          intercept <- tidy(lm_model)$estimate[1]
          slope <- tidy(lm_model)$estimate[2]
          rsquared <- summary(lm_model$fit)$r.squared
          equation <-
            paste("y = ", signif(slope,3),
                  "*x + (",signif(intercept,3),");  R2 = ",
                  signif(rsquared,3),sep="")
          equations <- c(
            equations,
            paste(paste(dep, "~", ind)," ::: ",
                  equation,sep="")
          )
          #cat("\n",equations,"\n")

          x_range <- seq(min(data2[[ind]]), max(data2[[ind]]),
                         length.out = 100)
          #x_range <- matrix(x_range, nrow=100, ncol=1)

          xdf <- tibble(x_range)
          names(xdf) <- ind
          ydf <- lm_model %>% predict(xdf)
          colnames(ydf) <- dep

          xy <- tibble(xdf, ydf)
          cat("Model data:\n")
          print(head(xy))

          ndata <- NROW(data2)

          fig <- fig %>% add_trace(x=data2[[ind]], y = data2[[dep]],
                                   alpha = 0.9, type="scatter", mode="markers",
                                   marker=list(size=marker.size),
                                   name = paste(dep, " ~ ", ind, "  (n = ", ndata,")", sep=""),
                                   legendgroup = paste(dep, "~", ind))

          fig <- fig %>% add_trace(x = xy[[ind]], y = xy[[dep]],
                                   alpha = 1, type="scatter", mode="lines",
                                   line = list(width=line.width),
                                   name = equation,
                                   legendgroup = paste(dep, "~", ind))

        }
      }
    }


  #### PLOT LAYOUT

  mrg <- list(l = 50, r = 50,
              b = 50, t = 60,
              pad = 0)

  fig <- fig %>% layout(
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
      color=fg,
      rangeselector = NULL,
      rangeslider = NA,
      tickfont = list(
        size=10
      )
    ),

    yaxis=list(
      color=fg,
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
                     filename = "datastreams_regresssion_plot",
                     width = 1200,
                     height = 800
                   )
    )

  # add_annotations(
  #     text = paste(paste(equations,"<br>"),collapse=" "),
  #     x = 0.15,
  #     y = 0.95,
  #     align = "left",   # doesn't work
  #     yref = "paper",
  #     xref = "paper",
  #     xanchor = "left",
  #     yanchor = "top",
  #     showarrow = FALSE,
  #     bgcolor = 'rgba(1,1,1,0.3)',
  #     bordercolor = 'rgba(1,1,1,0.3)',
  #     borderwidth = 1,
  #     font = list(
  #       size = 12,
  #       color="lightgray"
  #     )
  # )

  fig

}

# ----------------------------------------------------------

## Example regression model using plotly



# data(tips)
# y <- tips$tip
# X <- tips$total_bill
#
#
# lm_model <- linear_reg() %>%
#   set_engine('lm') %>%
#   set_mode('regression') %>%
#   fit(tip ~ total_bill, data = tips)
#
# x_range <- seq(min(X), max(X), length.out = 100)
# x_range <- matrix(x_range, nrow=100, ncol=1)
#
# xdf <- data.frame(x_range)
# colnames(xdf) <- c('total_bill')
# ydf <- lm_model %>% predict(xdf)
# colnames(ydf) <- c('tip')
#
# xy <- data.frame(xdf, ydf)
#
# fig <- plot_ly(tips, x = ~total_bill, y = ~tip, type = 'scatter',
#                alpha = 0.65, mode = 'markers', name = 'Tips')
# fig <- fig %>% add_trace(data = xy, x = ~total_bill, y = ~tip,
#                          name = 'Regression Fit', mode = 'lines', alpha = 1)
# fig
