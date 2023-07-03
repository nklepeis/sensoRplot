#' @alias convert_from_binary_to_groupedstate_contexts
#'
#' @title Convert Contexts from Binary to Grouped State Format
#'
#' @description This function converts a context sequence in WIDE binary format
#' to grouped-state context format
#'
#' @author Neil Klepeis
#'
#' @param contexts a data frame containing a sequence of contexts in WIDE binary format
#' @param index.vars Index variables to use for grouping (in addition to "Time")
#' @param sep delimiter for Groups and States in the WIDE column names
#' @param state_sep delimiter for multiple states concurrent for a given group
#'
#' @details  If states within groups are mutually exclusive, concurrent states
#' are separated by the 'state_dep' delimiter in the returned tibble.  Having
#' mutually-exclusive states in each group produces a cleaner context space.
#'
#' The 'contexts' tibble must have a Time column and multiple columns
#' named using Group:State pairs, containing 0 or 1 values indicating the occurrence of
#' a given state at each time.
#'
#' @return A tibble containing a sequence of states in grouped-state format
#'
#' @example
#'
#' library(tidyverse)
#' tibble(Time = 1:3, `House:LivRm` = c(0,1,0),
#'   `House:Patio` = c(0,0,1),
#'   `Smoking:Cigar`= c(0,0,1),
#'   `Smoking:Cannabis` = c(1,0,1)) %>%
#' convert_from_binary_to_groupedstate_contexts()
#'
# -------------------------------------------------

# TODO: Input either wide or long binary format.

convert_from_binary_to_groupedstate_contexts <-
  function(contexts, index.vars=NULL, sep=":", state_sep=";") {

    #  1. get to long format
    #  2. replace binary values with state names
    #  3  get back to wide format

      contexts %>%
      pivot_longer(cols=!(c("Time", all_of(index.vars))),
                   names_to=c("Group","State"),
                   values_to="Value",
                   names_sep=sep
      ) %>%
      #mutate(GroupState = paste(Group, State, sep=":")) %>%
      #group_by(Time, Value) %>%
      #group_modify(~ tibble(States=paste(.x$GroupState,
      #                                   collapse=" | "))) %>%
      #group_by(Time) %>%
      #slice_max(Value) %>% # take Value=1 if there is one or 0 if not
      #ungroup() %>%
      mutate(State = case_when(Value == 0 ~ "", Value >= 1 ~ State)) %>%
      arrange(Time) %>%
      select(!Value) %>%
      mutate(State = na_if(State, "")) %>%
      pivot_wider(
         id_cols = "Time",
         names_from="Group",
         values_from="State",
         values_fn = function(x) {x <- x[!is.na(x)]; if (length(x))
             paste(x, collapse=state_sep) else NA}
      ) #%>%
      #mutate(across(!Time, ~ na_if(.x, "")))

  }
