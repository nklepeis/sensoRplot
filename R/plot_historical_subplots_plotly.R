#' @name plot_historical_subplots_plotly
#'
#' @title Create stacked plots of grouped data streams
#'
#' @description This function creates a stacked plotly combination plot
#' with individual subplots for each unique set of index values
#'
#' @author Neil Klepeis
#'
#' @param data a dataframe containing 'Time', 'Response', and 'Value' columns
#' and at least one additional index column to be used in creating stacked subplots
#' @param fg foreground color
#' @param bg background color
#' @param displayModeBar logical, whether to show the mode bar for the plot
#'
#' @details The subplots are stacked on top of one another
#'
# ------------------------------------------------
# all argumetns from ...plotly2 are in ... except data, showlegend, displayModebar

## from airmotiveST now in sensoRplot!!  7/1/2023...Nk

plot_historical_subplots_plotly <- function(data,
                                            shareX=TRUE,
                                            shareY=FALSE,
                                            ncols=2,
                                            paper.bg ="white",
                                            ...,
                                            displayModeBar=TRUE) {

  require(plotly)
  require(tidyr)
  require(dplyr)

  if (!NROW(data %>% select(!any_of(c("Time","Value","Response")))))
    stop("'data' must have at least one index variable besides 'Response'.")

  data <- data %>%
    unite(col="Name", !any_of(c("Time","Value","Response")), sep=":")

  print(data)
  plots <- list()

  thenames <- unique(data$Name)

  for (i in thenames)
    plots[[i]] <- plot_historical_data_plotly2(data = data %>% filter(Name  ==  i) %>%
                                                 select(-Name),
                                               #title=i,
                                               ...,
                                               #showlegend=FALSE,
                                               showlegend = if (i == thenames[1]) TRUE else FALSE) %>%
    add_annotations(
      text = i,
      x = 0.95,
      y = 0.95,
      yref = "paper",
      xref = "paper",
      xanchor = "right",
      yanchor = "top",
      showarrow = FALSE,
      bgcolor = 'rgba(1,1,1,0.3)',
      bordercolor = 'rgba(1,1,1,1)',
      borderwidth = 1,
      font = list(
        size = 12,
        color="white"
      )
    )

  subplot(plots,
          nrows = ceiling(length(plots) / ncols),
          #nrows = nrows,
          shareX=shareX, shareY=shareY) %>%
    plotly::layout(showlegend=TRUE,
                   plot_bgcolor= paper.bg,
                   paper_bgcolor= paper.bg
    ) %>%
    plotly::config(
      displayModeBar=displayModeBar
    )

}
