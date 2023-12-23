#' @name combine_contexts
#'
#' @alias combine_contexts
#'
#' @title Combine two or more context sequences
#'
#' @description Combine two or more context sequences into a
#' single context sequences with following contexts either merging
#' with prior sequences or replacing sequences
#'
#' @param ... context sequence data frames in BINARY WIDE format
#' @param replace whether to merge with prior sequenes or replace sequences
#'
#' @return a data frame containing a sequence consisting of merged or
#' replaced conetxt sequences
#'
#' @author Neil Klepeis
#'
#' @details
#'
#' Iterating from i = 2 to n sequences we modify the i=1 sequence to
#' include all times and sequences from the following sequences
#'
#' This consists of standardizing context groups/states, standardizing
#' the times, and then adding context values for aligned rows of
#' different context sequences
#'
#' After we have the sames groups/states in all sequences (inserting 0's for missing ones)
#' Iterate from i = 2 to n:
#' (1) Add times of ith sequence to i = 1 sequence
#' (2) Add times of i = 1 sequence to ith sequence
#' (3) Sum the context values for aligned rows
#'
#'  Could we just use a left_join or full_join to
#'  combine all of these together, if there
#'  are duplicated, then we sum them togehter. maybe..no
#'
#'  For replacements, perhaps we remove all the contexts first
#'  before doing the join... between min/max time of following
#'  context sequences
#'
#' @examples
#'
#' library(tidyverse)
# -------------------------------------------

# 1.  Standardize gruops.states
# 2.  Join together into a single context sequence adding together duplicates

# Wuill this work

combine_contexts <- function(..., replace=FALSE) {

  contexts <- list(...)

  #  TBD:  Create common binary context groups/states
  cols = !names(contexts[[1]]) %in% c("Time")

  if (length(contexts) > 1) {
    first <- contexts[[1]]
    new <- first
    for (i in 2:length(contexts)) {
      ith <- contexts[[i]]
      # to replace we remove all the sequences between <> min/max of ith

      # Add any missing groups/states
      for (x in names(ith))
        if (!x %in% names(new))
          new[[x]] <- 0L   # make sure it is an integer!!!!

      ## Could we juse left_join and right_join to add times????????
      ## maybe we can???
      new <- add_times_to_contexts(new, ith$Time)
      ith <- add_times_to_contexts(ith, new$Time)

      new <- bind_rows(new, ith)

      ## NEW: Use full_join command instead???  can we merge/replce here? no.
    }

    ## now add the binary values for aligned rows
    # Then sum the duplicates... here is where we replace or merge.? no.
    #library(plyr)
    # return the final merged context sequence
    ddply(new, "Time", numcolwise(sum))

          #new <-
      #  bind_rows(new, ith) %>%
      #  group_by(across(all_of(c("Time")))) %>%
      #    group_modify(~ .x + segment[1,cols]) %>%
      #    ungroup()

    #}
  } else stop("Specify at least 2 context sequences.")

}
