#' @aliases theme_contexts_dark
#'
#' @title A minimal dark ggplot theme for plotting contexts
#'
#' @author Neil Klepeis
#'
#' @details  Based on the default 'theme_dark' in the ggthemes package
#'
#' @seealso
#'
#' scales_fill_contexts
#' scales_color_contexts
#'
#----------------------------------------------

#  Same as theme_streams_dark to start. NK  4/27/2022

theme_contexts_dark <- function(
  panel.spacing=0,
  tick.length = 0.2,
  grid.color=NA,
  bg.plot = "black",
  axis.color.ticks = "white",
  axis.color = "white",
  axis.color.line = NA,
  strip.line.size = 1.3,
  bg.strip = "gray20",
  panel.line.size = 1.3,
  bg.panel="gray10",
  border.panel.color="black",
  border.strip.color="black"
) {

  theme_dark() +

    theme(

      axis.ticks = element_line(color=axis.color.ticks, linetype="solid"),
      axis.line = element_line(color=axis.color.line, linetype="solid"),
      axis.text = element_text(color=axis.color),
      axis.ticks.length = unit(tick.length, "lines"),
      panel.spacing = unit(panel.spacing, "lines"),
      strip.text.x = element_text(hjust = 0.5),
      strip.background = element_rect(size=strip.line.size, color=border.strip.color,
                                      fill=bg.strip),
      panel.background = element_rect(color = border.panel.color,
                                      fill = bg.panel, size=panel.line.size),
      panel.grid.major = element_line(color = grid.color, linetype = "solid", size = 1),
      panel.grid.minor = element_line(color = grid.color, linetype = "solid", size = 1),
      plot.background = element_rect(fill = bg.plot, color="black"),
      #legend.background = element_rect(fill = bg.plot),
      legend.background=element_blank(),
      title = element_text(color=axis.color),
      legend.text = element_text(color=axis.color),
      legend.title = element_text(color=axis.color),
      legend.key = element_rect(fill = "transparent", colour = "transparent")


    )

}
