#' @name explorePeaks
#'
#' @title Shiny app to interactively identify peaks in a time series
#'
#' @description A shiny app to visualize identified peaks in a time series
#'
#' @author Neil Klepeis
#'
#' @param stream a tibble containing data streams with
#' Time, Response, Value columns

# -------------------------------------------------------

explorePeaks <- function(streams, maxHeight=1000,
                         maxDist = 500,
                         maxThreshold = 100) {
  require(shiny)
  shinyApp(
    ui = fluidPage(
      sidebarLayout(
        sidebarPanel(
          sliderInput("minpeakheight", "Min Height",
                      min=0, max=maxHeight, value=0),
          sliderInput("minpeakdistance", "Min Distance",
                      min=1, max=maxDist, value=1),
          sliderInput("threshold", "Threshold",
                      min=0, max=maxThreshold, value=0),
          sliderInput("smoothingInterval", "Smoothing Interval, s",
                      min=1, max=120, value=60),
          sliderInput("nups", "No. Ups",
                      min=1, max=120, value=1),
          sliderInput("ndowns", "No. Downs",
                      min=1, max=120, value=1)

        ),
        mainPanel(plotOutput("peaks"))
      )
    ),
    server = function(input, output) {

      ## TODO: Separate out the data return and the plotting..

      output$peaks <- renderPlot(
        find_stream_episodes(streams, nups=input$nups,
                             ndowns=input$ndowns,
                             zero="0",
                             minpeakheight=input$minpeakheight,
                             minpeakdistance=input$minpeakdistance,
                             threshold=input$threshold,
                             sortstr=TRUE,
                             by=paste(input$smoothingInterval,"seconds"),
                             plot=TRUE, returnPlot=TRUE)
      )

    }
  )
}
