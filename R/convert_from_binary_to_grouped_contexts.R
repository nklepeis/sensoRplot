#' @title Convert Contexts from Binary to Grouped State Format
#'
#' @description This function converts a context in WIDE binary format
#' to grouped-state context format
#'
#' @author Neil Klepeis
#'
#' @param contexts a data frame containing WIDE binary contexts
#' @param index.vars Index variables to use for grouping (in addition to "Time")
#' @param sep delimiter for Groups and States in the WIDE column names
#'
#' @details
#'
# -------------------------------------------------

# TODO: Input either wide or long binary format.

convert_from_binary_to_grouped_contexts <-
  function(contexts, index.vars, sep=":") {

    #  1. get to long format
    #  2. replace binary values with state names
    #  3  get back to wide format

      contexts %>%
      pivot_longer(cols=!(c("Time", index.vars)),
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
      mutate(States = case_when(Value == 0 ~ "", Value >= 1 ~ States)) %>%
      arrange(Time) %>%
      select(!Value) %>%
      pivot_wider(

      )


  }
