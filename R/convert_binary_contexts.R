#' @title Convert Binary Context
#'
#' @description This function converts a context in binary format
#' to an alternative binary format (long or wide) or to active-state
#' format (timestamped list of active states)
#'
#' @author Neil Klepeis
#'
#' @param bc a data frame containing binary contexts (the default storage format is LONG)
#' @param fromLong logical to convert binary long to wide (if TRUE) format or from wide format to long (if FALSE)
#' @param toBinary whether format of returned value is binary or active-state format
#' @param index.vars Index variables to use for grouping (in addition to "Time")
#' @param sep the character to use/expect as a separate between group and state values
#' @param values_fn a function to use if there are duplicates when converting from long to wide binary format.
#' "max" returns the maximum value ("1") so that merging multiple contexts in a long
#' format will the expected occurrence of the context
#'
#' @details When converting to active-state format, state values >=1 are
#' recognized as part of "active" contexts
#'
# -------------------------------------------------

# updated in contextualizeR. pivot_longer/all_of NK 1/21/2022

#  Note:  Stolen from airMotive on 6/4/2021
#    removed reference to userEmail, userName, workSpace....
#  stolen from sensoRplot  12/25./2022.   We need to jsut start using sensoRplot
#   for all this to avoid confusion....!!!

convert_binary_contexts <-
  function(bc, fromLong=TRUE, toBinary=TRUE,
           index.vars=NULL,
           sep=":", values_fn = max) {

    #require(tidyr)
    #require(dplyr)

    #print(head(bc))

    if (fromLong) {  # from Long

      if (toBinary) {   # long to wide format

        bc %>%
          #pivot_wider(id_cols=c("Time","userEmail","userName","workSpace"),
          pivot_wider(#id_cols=!c("Group","State"),
            id_cols="Time",
            names_from=c("Group","State"),
            values_from="Value",
            values_fn=values_fn,   # if duplicates use MAX value (changed from Min)
            names_sep=sep
          ) %>%
          arrange(Time)

      }

    } else {  # from Wide

      if (toBinary) {   # wide to long format

        bc %>%
          #pivot_longer(cols=!matches(c("Time","userEmail","userName","workSpace")),
          pivot_longer(cols=!(all_of(c("Time", index.vars))),
                       names_to=c("Group","State"),
                       values_to="Value",
                       names_sep=sep
          ) %>%
          arrange(Time)

      } else {  #  wide to active state (could use wide to long, long to active)
        #  OR...
        # 1. replace 1's with col names 0's with ""
        # 2. use 'unite' to paste together the columns
        # do first way..
        bc %>%
          #pivot_longer(cols=!matches(c("Time","userEmail","userName","workSpace")),
          pivot_longer(cols=!(c("Time", index.vars)),
                       names_to=c("Group","State"),
                       values_to="Value",
                       names_sep=sep
          ) %>%
          mutate(GroupState = paste(Group, State, sep=":")) %>%
          #group_by(Time, Value, userEmail, userName, workSpace) %>%
          group_by(Time, Value) %>%
          group_modify(~ tibble(States=paste(.x$GroupState,
                                             collapse=" | "))) %>%
          #group_by(Time, userEmail, userName, workSpace) %>%
          group_by(Time) %>%
          slice_max(Value) %>% # take Value=1 if there is one or 0 if not
          ungroup() %>%
          mutate(States = case_when(Value == 0 ~ "", Value >= 1 ~ States)) %>%
          arrange(Time) %>%
          select(!Value)

      }

    }

  }
