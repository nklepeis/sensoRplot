#' @name segmented_elapsed_time
#'
#' @alias segmented_elapsed_time
#'
#' @title Computed a segmented elapsed time variable
#'
#' @description Compute elapsed times and time factors with the "0" restarting at the
#' beginning of time segments defined by grouping variables
#'
#' @param x a data frame with a Time variable
#' @param interval the time interval in 'units' to use in computing time factors
#' @param units a string giving the units to use, e.g., "hour"s, mins", "secs",
#' in computing elapsed time and time factors
#' @param group.var a character vector of variables names to use in grouping
#'
#'
#' @return a data frame with a new elapsed time variable and a time factor variable
#'
#' @author Neil Klepeis
#'
#' @details
#' This function creates a elapsed time variable that resets to "0"
#' at the beginning of time segments defined by a set of specified
#' grouping variables.   The elapsed time has units defined by 'units'.
#' The time factor variable (computed using the 'time_factor' function) is assigned
#' over successive intervals defined by 'interval' and 'units'
#'
#'
# -----------------------------------------------

# Parts stolen from   her.Misc  see time.factor.2 and get_time.POSIX

#  https://stackoverflow.com/questions/49563848/how-to-pass-multiple-group-by-arguments-and-a-dynamic-variable-argument-to-a-dpl


segmented_elapsed_time <- function (x, interval=1, units="min", group.var) {

  if (!is.POSIXt(x$Time)) stop("'x$Time' must be a date time variable.")

  #group.var <- enquo(group.var)

  x %>%
    #group_by(!!group.var) %>%
    group_by(across(group.var)) %>%
    group_modify(
      ~ {
        .x <- .x %>% arrange(Time) %>%
          mutate(Time = floor_date(Time))
        origin <- floor_date(min(.x$Time))
        cat("Grouping: \n")
        print(.y)
        varname <- paste0("elapsed.",units)
        .x %>% mutate(
          !!varname := as.numeric(difftime(.x$Time,
                                origin, units=units)),
          timef = time_factor(.x$Time, interval=interval, units=units,
                              origin=origin),
          units = paste(interval, units)

        )
      }
    )

}
