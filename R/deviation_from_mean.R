#' @name deviation_from_mean
#'
#' @alias deviation_from_mean
#'
#' @title Compute deviation from mean for a set of sensors
#'
#' @description Compute deviation from the mean for concurrent
#' sensor data streams with one column per sensor
#'
#' @param data  a data frame in WIDE format with rows containing sensor data
#' at specific times with each column containing data for an individual sensor
#' @param use.percent whether to compute deviations as "relative" (value) or
#'  "percent" values (does percents by default)
# @param statistic Mean by default but can be set to Median or other
#' central value statistic
#'
#' @return a data frame containing raw deviations from the mean and computed
#' statistics for deviations from the mean (see Details below)
#'
#' @author Neil Klepeis
#'
#' @details This function is intended for use in evaluating the agreement between
#' sensors of the same type by computing deviations of sensor readings from the
#' mean reading
#'
#' If a "Time" column is not present in the data frame, then one is added.
#'
#' All variables not named "Time" are assumed to be streams of sensor data
#' for the same type of sensor.
#'
#' Rows with any missing values are dropped.
#'
#' At each time (row), this function computes:
#'
#' * The overall row mean
#' * The relative deviation from the mean for each sensor (using same names as input columns)
#' * The minimum absolute deviation from the mean across all sensors
#' * The maximum absolute deviation from the mean across all sensors
#' * The minimum relative deviation from the mean across all sensors
#' * The maximum relative deviation from the mean across all sensors
#' * The 25th percentile of relative deviations from the mean
#' * The 75th percentile of relative deviations from the mean
#'
#' @example
#' x <- data.frame(A=c(1,6,4,3,7,8),B=c(8,9,3,2,8,9),C=c(9,8,7,8,9,6))
#' deviation_from_mean(x)
#'
#'
# -----------------------

deviation_from_mean <- function(data, use.percent=TRUE) {

  if (!"Time" %in% names(data)) data$Time <- 1:NROW(data)

  cols <- names(data %>% select(-Time))

  ddata <- data %>%
  drop_na() %>%
    rowwise() %>%
    mutate(MinAbs = min(c_across(all_of(cols)))) %>%
    mutate(MaxAbs = max(c_across(all_of(cols)))) %>%
    mutate(Mean = mean(c_across(all_of(cols)))) %>%
    ungroup()

  if (use.percent)
    ddata <- ddata %>% mutate(across(all_of(cols), ~ 100*(. - Mean) / Mean))
  else
    ddata <- ddata %>% mutate(across(all_of(cols), ~ (. - Mean)))

      #{if (!percent) mutate(across(all_of(cols), (~ . - Mean) / Mean))} %>%
  ddata %>%
    rowwise() %>%
    mutate(MinDev = min(c_across(all_of(cols)))) %>%
    mutate(MaxDev = max(c_across(all_of(cols)))) %>%
    mutate(P0.25 = quantile(c_across(all_of(cols)),0.25)) %>%
    mutate(P0.75 = quantile(c_across(all_of(cols)),0.75)) %>%
    ungroup()

  # if (summarize) {  # compute
  #   data %>% pivot_longer(
  #       cols=all_of(cols), names_to = "sensorID",
  #       values_to = "Dev"
  #     ) %>%
  #     group_by(Time, Mean) %>%
  #     summarize(
  #       Min=min(Dev) + Mean,
  #       Max=max(Dev) + Mean,
  #       P0.25=quantile(Dev, 0.25) + Mean,
  #       P0.75=quantile(Dev, 0.75) + Mean) %>%
  #     ungroup()
  # } else data  # Return raw time series with deveiation from mean computed for each time/row

  #%>%  # This does mutate across multiple cols
    #pivot_longer(
    #  cols=all_of(cols), names_to = "sensorID",
    #  values_to = "Dev"
    #) #%>%
    #group_by(Time, Mean) %>%
    #reframe(
    #  Min=min(Dev) + Mean,
    #  Max=max(Dev) + Mean,
    #  P0.25=quantile(Dev, 0.25) + Mean,
    #  P0.75=quantile(Dev, 0.75) + Mean) %>%
    #ungroup()
}

