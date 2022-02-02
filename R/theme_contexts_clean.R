#' @aliases theme_contexts_clean
#'
#' @title A minimal blue ggplot theme for plotting contexts
#'
#' @author Neil Klepeis
#'
#' @details  Based on the default 'theme_hc' in the ggthemes package
#' with the 'theme_economist' scale colors.
#'
#' @seealso
#'
#' scales_fill_contexts
#' scales_color_contexts
#'
#----------------------------------------------

theme_contexts_clean <- function(
  panel.spacing=0,
  tick.length = 0.2,
  axis.text.size = 11,
  axis.title.size = 32,
  axis.font = "sans"
) {

  theme_hc() +

    theme(
      axis.text = element_text(size=axis.text.size,
                               family=axis.font),
      axis.title = element_text(size=axis.title.size,
                                family=axis.font, face="bold"),
      axis.ticks.length = unit(tick.length, "lines"),
      panel.spacing = unit(panel.spacing, "lines"),
      strip.text = element_blank()
    )


}
