#' @name deviation_from_reference
#'
#' @alias deviation_from_reference
#'
#' @title Compute deviation of test sensors from a reference sensor
#'
#' @description Compute deviation from a reference for concurrent
#' sensor data streams with one column per sensor
#'
#' @param data  a data frame in WIDE format with rows containing sensor data
#' at specific times with each column containing data for an individual sensor
#' @param ref name or number of the column containing the reference data,
#' defaults to the first column in the data frame
#' @param use.percent whether to compute deviations as "relative" (value) or
#'  "percent" values (does percents by default)

#' @return a data frame containing raw deviations from the reference and computed
#' statistics for deviations from the mean (see Details below)
#'
#' @author Neil Klepeis
#'
#' @details This function is intended for use in evaluating the agreement between
#' a number of 'test' sensors of the same type and a 'reference' sensor by computing
#' deviations of 'test' sensor readings from the 'reference' reading at different
#' times
#'
#' If a "Time" column is not present in the data frame, then one is added.
#'
#' All variables not named "Time" are assumed to be streams of sensor data
#' with 'ref' containing the name or number of the column containing the reference
#' data stream
#'
#' This functions works the same as tte 'deviation_from_mean' function except the
#' 'ref' column takes the place of the mean.
#'
#' Rows with any missing values are dropped.
#'
#' At each time (row), this function computes:
#'
#' * The overall row mean of 'test' sensors
#' * The relative deviation from the reference for each sensor (using same names as input columns)
#' * The minimum absolute deviation from the reference across all sensors
#' * The maximum absolute deviation from the reference across all sensors
#' * The minimum relative deviation from the reference across all sensors
#' * The maximum relative deviation from the reference across all sensors
#' * The 25th percentile of relative deviations from the reference
#' * The 75th percentile of relative deviations from the reference
#'
#' @example
#' x <- data.frame(A=c(1,6,4,3,7,8),B=c(8,9,3,2,8,9),C=c(9,8,7,8,9,6))
#' deviation_from_reference(x, ref="B")
#'
#'
# -----------------------

deviation_from_reference <- function(x, ref=names(x)[1],
                                     use.percent=FALSE) {

  if (!"Time" %in% names(x)) data$Time <- 1:NROW(x)

  if (!ref %in% names(x))
    stop("'ref' must contain a column name indicatings the reference data stream")

  cols <- names(x %>% select(-Time) %>% select(-c(all_of(ref))))

  ddata <- x %>%
  drop_na() %>%
    rowwise() %>%
    mutate(MinAbs = min(c_across(all_of(cols)), na.rm=TRUE)) %>%
    mutate(MaxAbs = max(c_across(all_of(cols)), na.rm=TRUE)) %>%
    #mutate(Mean = mean(c_across(all_of(cols)))) %>%   # leave out to avoid confusion
    ungroup() %>%
    rename_with(.cols=ref, .fn = function(x) "REFERENCE") %>%
    select(REFERENCE, everything())

  #print(ddata)

  if (use.percent)
    ddata <- ddata %>% mutate(across(all_of(cols), ~ 100*(. - REFERENCE) / REFERENCE))
  else
    ddata <- ddata %>% mutate(across(all_of(cols), ~ (. - REFERENCE)))

      #{if (!percent) mutate(across(all_of(cols), (~ . - Mean) / Mean))} %>%
  ddata %>%
    rowwise() %>%
    mutate(MinDev = min(c_across(all_of(cols)), na.rm=TRUE)) %>%
    mutate(MaxDev = max(c_across(all_of(cols)), na.rm=TRUE)) %>%
    mutate(P0.25 = quantile(c_across(all_of(cols)),0.25),na.rm=TRUE) %>%
    mutate(P0.75 = quantile(c_across(all_of(cols)),0.75),na.rm=TRUE) %>%
    ungroup()

}


