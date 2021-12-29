#' @aliases theme_streams_light
#'
#' @title A light ggplot theme for plotting sensor streams
#'
#' @author Neil Klepeis
#'
#' @details  Based on the default 'theme_gray' ggplot theme.
#'
#----------------------------------------------

theme_streams_light <- function(
  panel.spacing=0,
  tick.length = 0.2,
  bg.panel="gray80",
  bg.strip="white",
  grid.color=NA,
  axis.color.ticks = "gray60",
  axis.color.line = NA,
  strip.line.size = 1.3,
  panel.line.size = 1.3,
  border.panel.color="white"
) {

  theme_gray() +

    theme(

      axis.ticks = element_line(color=axis.color.ticks, linetype="solid"),
      axis.line = element_line(color=axis.color.line, linetype="solid"),
      axis.ticks.length = unit(tick.length, "lines"),
      panel.spacing = unit(panel.spacing, "lines"),
      strip.text.x = element_text(hjust = 0.5),
      strip.background = element_rect(color = bg.strip, size=strip.line.size),
      panel.background = element_rect(color = border.panel.color, fill = bg.panel,
                                      size=panel.line.size),
      panel.grid.major = element_line(color = grid.color, linetype = "solid", size = 1),
      panel.grid.minor = element_line(color = grid.color, linetype = "solid", size = 1)

    )

}
