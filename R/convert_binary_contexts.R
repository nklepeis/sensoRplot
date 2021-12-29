#' @title Convert Binary Context
#'
#' @description This function converts a context in binary format
#' to an alternative binary format (long or wide) or to active-state
#' format (timestamped list of active states)
#'
#' @author Neil Klepeis
#'
#' @param bc a data frame containing binary contexts (the default storage format is LONG)
#' @param from format of 'bc' argument, "long" or "wide" binary format
#' @param toBinary whether format of returned value is binary or active-state format
#'
#' @details When converting to active-state format, state values >=1 are
#' recognized as part of "active" contexts
#'
# -------------------------------------------------


#  Note:  Stolen from airMotive on 6/4/2021
#    removed reference to userEmail, userName, workSpace....

convert_binary_contexts <-
  function(bc, fromLong=TRUE, toBinary=TRUE,
           sep=":") {

    #require(tidyr)
    #require(dplyr)

    #print(head(bc))

    if (fromLong) {  # from Long

      if (toBinary) {   # long to wide format

        bc %>%
          #pivot_wider(id_cols=c("Time","userEmail","userName","workSpace"),
          pivot_wider(id_cols="Time",
                      names_from=c("Group","State"),
                      values_from="Value",
                      values_fn=min,   # if duplicates use minimum value
                      names_sep=sep
          ) %>%
          arrange(Time)

      } else {   # long to active-state format
        bc %>%
          #filter(Value == 1) %>%
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

    } else {  # from Wide

      if (toBinary) {   # wide to long format

        bc %>%
          #pivot_longer(cols=!matches(c("Time","userEmail","userName","workSpace")),
          pivot_longer(cols=!matches("Time"),
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
          pivot_longer(cols=!matches("Time"),
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
