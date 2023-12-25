#' @alias quantize_contexts
#'
#' @title Quantize the times of a context sequence
#'
#' @description Round times of a context sequence to tne nearest
#' grid value
#'
#' @author Neil Klepeis
#'
#' @param ... one or more sequences of contexts to quantize
#' @param unit time interval to define the quantization
#' @param method one of "floor", "ceiling", or "round"
#'
#' @return Returns a list of all passed context sequences with
#' quantized times
#'
#' @details
#'
#' "round" quantizes the context times to nearest time unit value
#' "floor" quantizes the context times DOWN to the nearest unit boundary
#' "ceiling" quantizes the context times UP to the nearest unit boundary
#'
#' @seealso lubridate::round_date
#'
#------------------------------------------------------

quantize_contexts <- function (..., unit="10 seconds",
                               method="round") {

  contexts <- list(...)

  contexts %>%
    map(function (x) {
      switch(method,
        round = x %>% mutate(Time = round_date(unit=unit)),
        floor = x %>% mutate(Time = round_date(unit=unit)),
        ceiling = x %>% mutate(Time = round_date(unit=unit))
      )
    })

}
