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
#' * "round" quantizes the context times to nearest time unit value
#' * "floor" quantizes the context times DOWN to the nearest unit boundary
#' * "ceiling" quantizes the context times UP to the nearest unit boundary
#'
#' Note:  'Times' in the contexts objects need to be Time.POSIX objects
#' or coerceable to meaningful times using 'lubridate::as_datetime'.
#'
#' If quantization of times results in repeated times for sequential
#' contexts, the first context is included with others being discarded.
#'
#' @seealso lubridate::round_date
#'
#' @example
#' df <- data.frame(Time=c(1.1,2.04,2.9,4.3,5.5,6.4),
#'       GrpA=LETTERS[1:6],GrpB=LETTERS[6:11])
#' quantize_contexts(df,unit="1 second")
#'
#------------------------------------------------------

quantize_contexts <- function (..., unit="10 seconds",
                               method="round") {

  contexts <- list(...)
  if (inherits(contexts[[1]], "list"))
    contexts <- contexts[[1]]

  contexts %>%
    map(function (x) {
      switch(method,
        round = x %>% mutate(Time = round_date(as_datetime(Time), unit=unit)) %>%
          distinct(Time, .keep_all = TRUE),
        floor = x %>% mutate(Time = round_date(as_datetime(Time), unit=unit)) %>%
          distinct(Time, .keep_all = TRUE),
        ceiling = x %>% mutate(Time = round_date(as_datetime(Time), unit=unit)) %>%
          distinct(Time, .keep_all = TRUE)
      )
    })

}
