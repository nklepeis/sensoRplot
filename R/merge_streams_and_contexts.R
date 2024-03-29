#' @aliases merge_streams_and_contexts
#'
#' @title Merge long-format sensor data streams and contexts
#'
#' @author Neil Klepeis
#'
#' @param streams Long-format sensor data frame for multiple streams
#' containing columsn for Time, Response and Value, and optionally other index columns.
#' @param contexts Long-format data frame containing binary contextual data consisting of
#' Time, Group, State, and Value columns.
#' @param include.binary logical, whether to include columns for each context state
#' containing binary values of each state (active = 1; inactive = 0).

#' @seealso  The 'left_join_by_time' function seems to be more efficient (and reliable)
#' and based on the existing tidyverse left_join function.
#'
#' @return A data frame consisting of the steams data frame with added
#' columns containing active-state format context data time-matched to each
#' each data streams row.  Unmatched times in the streams are dropped.
#'
#' @details  This function merges discrete context data into data streams
#' (continuous time series) data by time-matching contexts to data streams.
#'
#' Two index columns are added, one with context states separated by "|" and
#' one with a new line character "\n" used to delineate context states
#'
#' NOTE: Times not having any active states in the 'streams' data are
#' assigned an empty context:  "" for active-state format and all 0's for binary format.
#' These are converted to NA's, which are dropped from the output.
#'
#' IMPORTANT:  If the streams time resolution is equal or smaller
#' than, and an even divisor of the context time resolution, then
#' multiple contexts cannot occur between stream intervals
#' (i.e., causing context information to be lost).
#'
#----------------------------------------------

# TODO: Make option to merge binary-wide format data to make it easier to
#   analyze by specific context states and not just the active-states in a given
#   context.  Done.

#  TODO:  Create a 'streams' and 'contexts' object types and check to make
#   sure that inputs conform...


merge_streams_and_contexts <-
  function (streams, contexts, include.binary=FALSE) {

  streams <- as_stream(streams)
  contexts <- as_context(contexts)

  #  Add column showing active context states  Group:State1 | Group:State2[...]
  #  Convert binary contexts to "active state" format
  contextsActive <- convert_binary_contexts(contexts, toBinary=FALSE)

  streams <- streams %>%
    mutate(
      States = purrr::pmap_chr(
        list(Time),
        function(Time) {
          idx <- rank(c(Time, contextsActive$Time),ties.method="last")[1] - 1
          if (idx > 0) contextsActive$States[idx] else NA_character_
        }
      )
    ) %>%
    replace_with_na(replace=list(States = "")) %>%
    drop_na() %>%
    mutate(States2 = stri_replace_all_fixed(States, " | ", "\n"))


  # Optionally add columns for binary states (wide format)
  if (include.binary) {

    #   contexts converted to widebinary format
    contextsWide <- convert_binary_contexts(contexts)
    # a dummy row with all 0 (inactive) values
    dummy.row <- contextsWide %>% slice(1) %>% select(-Time) %>%
      mutate(across(.cols=everything(), .fns = ~ 0))

    streams <- streams %>%
      bind_cols(
        purrr::pmap_dfr(
          list(streams$Time),
          function(Time) {
            idx <- rank(c(Time, contextsWide$Time),ties.method="last")[1] - 1
            if (idx > 0) contextsWide %>% slice(idx) %>% select(-Time)
            else dummy.row
          }
        )
      )
  }

  #TODO : Why is streams truncated ???

  streams

}
