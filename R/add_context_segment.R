#' @alias add_context_segment
#'
#' @title Add Context Segment
#'
#' @description Based on a two-context sequence, this function computes the
#' endpoints and middle points of a corresponding context sequence
#' to be added to an existing sequence of contexts with the option of
#' replacing or merging with the existing context sequence
#'
#' @author Neil Klepeis
#'
#' @param segment sequence of two contexts defining a segment to add to 'contexts',
#' argument in binary long format
#' @param contexts a sequence of any number of contexts to which a 'segment'
#' of contexts will be added via merging with existing contexts or replacing existing
#' contexts.
#' @param merge logical, whether to merge the segment with the
#' existing sequence or replace existing overlapping contexts, default=FALSE
#' @param verbose logical, whether to print out progress messages
#'
#' @return Returns a drop-in context sequence for the passed segment with
#' appropriate endpoint and middle contexts.
#'
#' @details
#'
#' Contexts are specified in "binary long" format. 'segments' and 'contexts'
#' must have the same basis of groups and states
#'
#' Typically the 2nd context in the segment is empty (no active
#' states) to define a period in which a set of states are active and then
#' become inactive.  However, this function will still process
#' segments that have active states at the second context
#
# --------------------------------------------------------

## Note:  this was the old code in AirMotive.  Now deprecated in
# favor of newer 'combine_contexts' that can treat multple
#  contexts, merges new states and overwrite existing states in a
#  more natural way. NK 12/24/2023

## Now included in SensoRplot.  NK 6/30/2023

## Updated in the contextualizeR app to have verbose arg.  3/13/2023. NK.

## Updated in the contextualizeR app.  1/21/2023. NK.
#    give error if segment and contexts do not have overlapping
#    column names...

## Taken from airmotive  12/24/2022 NK
##   Edited for no more username useremail, etc.

#  IDEA:  Do all analysis in LONG mode.. may actually be easier..

# UPDATE:   merge and replace only differ in code in whether
#   the LEFT abd MIDDLE values are merged or replaced.

add_context_segment <- function(segment, contexts, merge=TRUE,
                                verbose=FALSE) {


  if (missing(segment) | missing(contexts))
    stop("Must provide 'segment' and 'contexts' long-format binary context data frames.")

  if (!NROW(segment)) stop("Specify a non-empty context segment.")
  if (!NROW(contexts)) stop("Specify a non-empty context segment.")

  # TODO:  Make these objects and check for object class..
  if (!all(c("Time","State","Group","Value") %in% names(segment)) &
      !all(all(c("Time","State","Group","Value") %in% names(contexts))))
    stop("'Time', 'State', 'Group', 'Value' variables must be in both segment and contexts")

  # coerce Values to integer
  segment$Value <- as.integer(segment$Value)
  contexts$Value <- as.integer(contexts$Value)

  if (verbose) {
    cat("Add Context :: Merging segment into contexts:\n")
    cat("Attempting to Merge This Segment (Binary Long format):\n")
    print(segment)
    cat("Into These Contexts (Binary Long format):\n")
    print(contexts)

    cat("Add Context :: Merging segment into contexts:\n")
    cat("Attempting to Merge This Segment (Active State format):\n")
    print(convert_binary_contexts(segment, toBinary=FALSE))
    cat("Into These Contexts (Active State format):\n")
    print(convert_binary_contexts(contexts, toBinary=FALSE))
  }

  # convert to wide binary format
  segment <- segment %>% convert_binary_contexts() %>% arrange(Time)
  if (NROW(segment) != 2)
    stop("Segment must have two and only two contexts.")
  contexts <- contexts %>% convert_binary_contexts() %>% arrange(Time)


  ## TODO: If segment columns do not exist in contexts,
  #   can we add them????  done.

  # check here now for consistent names
  if (NROW(contexts)) {
    if (!all(names(segment) %in% names(contexts))) {
      warning("'segment' and 'contexts' must have consistent ordered group/state specifications.  Adding names with value=0 to contexts.")
      for (x in names(segment))
        if (!x %in% names(contexts))
          contexts[[x]] <- 0L   # make sure it is an integer!!!!
    }
  } else return (segment)


  if (verbose) {
    cat("Segment Wide Form:\n")
    print(segment)
    cat("Contexts Wide Form:\n")
    print(contexts)
  }

  #1. add new trailing edge of segment
  #2. remove/merge contexts in the segment range
  #3. add leading edge of segment

  if (verbose) cat("*** CONTEXT MERGE *** = ", merge, "\n")

  # TODO: Merge==TRUE doesn't seem to work.  Check it!!! NK 1/22/2023

  #if (merge) {   # add to existing contexts
  #cols = !names(segment) %in% c("Time","userEmail","userName","workSpace")
  cols = !names(segment) %in% c("Time")
  #if (!all(cols %in% names(contexts)))
  #  stop("Mismatch between segments and context columns.  Segment cols: ", cols, "\n")
  left <- contexts %>%
    filter(Time <= segment[1,]$Time)
  if (merge)
    if (NROW(left)) {  # if merge, combine states
      left <- left %>%
        slice_max(Time) %>%
        mutate(Time = segment[1,]$Time)
      left[1,cols] <- segment[1,cols] + left[1,cols]
    } else left <- segment[1,]
  else left <- segment[1,]
  middle <- contexts %>%
    filter(Time > segment[1,]$Time & Time < segment[2,]$Time)
  if (merge) {
    if (NROW(middle)) {  # if !merge, then we don't add middle values together.
      #    middle[,cols] <- middle[,cols] + segment[1,cols]
      if (verbose) cat("computing a MIDDLE!!!!\n")
      middle <- middle %>%
        #group_by(across(all_of(c("Time","userEmail","userName","workSpace")))) %>%
        group_by(across(all_of(c("Time")))) %>%
        group_modify(~ .x + segment[1,cols]) %>%
        ungroup()
    }
  } else middle <- middle %>% filter(FALSE)

  right <- contexts %>%
    filter(Time <= segment[2,]$Time)
  if (NROW(right)) {
    right <- right %>%
      slice_max(Time) %>%
      mutate(Time = segment[2,]$Time)
    right[1,cols] <- segment[2,cols] + right[1,cols]
  } else right <- segment[2,]

  #cat("Is this right?\n")
  #print(bind_rows(left, middle, right))
  if (verbose) {
    cat("LEFT\n")
    if (NROW(left)) {
      print(left)
      #print(convert_binary_contexts(left, fromLong=FALSE, toBinary=FALSE))
    }
    cat("MIDDLE\n")
    if (NROW(middle))
      print(middle)
    #print(convert_binary_contexts(middle, fromLong=FALSE, toBinary=FALSE))
    cat("RIGHT\n")
    if (NROW(right))
      print(right)
    #print(convert_binary_contexts(right, fromLong=FALSE, toBinary=FALSE))
  }

  # Bind and recode >1 state values to 1
  ##  https://stackoverflow.com/questions/64189561/using-case-when-with-dplyr-across
  bind_rows(left, middle, right) %>%
    # mutate(
    #   across(!contains(c("Time","userEmail","userName","workSpace")),
    #        ~ case_when(. > 1 ~ 1,
    #                    TRUE ~ .))
    # ) %>%
    #  Need to make sure new values are integers not doubles
    #  e.g., https://community.rstudio.com/t/case-when-why-does-it-work/59977
    mutate(
      #across(-all_of(c("Time","userEmail","userName","workSpace")),
      across(-all_of(c("Time")),
             ~ case_when(. > 1 ~ 1L,
                         TRUE ~ .))
    ) %>%
    convert_binary_contexts(fromLong=FALSE) %>%
    arrange(Time)

  #mutate( var =  case_when(
  #  var >= 1 ~ 1
  #))
  #  if >=, set to 1
  #  add other pieces....

  #} else {   # replace existing contexts
  #new <- contexts %>%
  #  filter(Time < segment[1,]$Time | Time > segment[2,]$Time) %>%
  #   #  bind_rows(segment[1,])
  #   left <- segment[1,]
  #   # Get most recent context before the segment leading edge
  #   right <- contexts %>% filter(Time <= segment[2,]$Time)
  #   if (NROW(right))
  #     # new <- new %>%
  #     #       bind_rows(
  #     #          right %>%
  #     #            slice_min(Time) %>%
  #     #            mutate(Time = segment[2,]$Time)
  #     #        )
  #     right <- right %>%
  #       slice_min(Time) %>%
  #       mutate(Time = segment[2,]$Time)
  #   #else new <- new %>% bind_rows(segment[2,])
  #   else right <- segment[2,]
  #   #  TODO:  keep segment 'right' if there is activity
  #   #      only resume previous context if segment has empty right side
  #   #   Not needed for current airMotive version
  #   bind_rows(left, right) %>%
  #     convert_binary_contexts(fromLong=FALSE) %>%
  #     arrange(Time)
  # }

}
