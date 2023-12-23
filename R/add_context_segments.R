#' @alias add_context_segments
#'
#' @title Add Context Segments
#'
#' @description Add one sequence of y to another with the option of
#' replacing or merging with the existing context sequence
#' (TBD:  Have option of full left or right join of y)
#'
#' @author Neil Klepeis
#'
#' @param x a sequence of y sequences in binary wide format
#' @param y a sequence of y to add to the 'x' sequence
#' y.
#' @param replace logical, whether to merge the x with the
#' existing sequence or replace existing overlapping y, default=FALSE
#'
#' @return Returns a new sequence that results from adding y to x.
#'
#' @details
#'
#' y are specified in "binary wide" format.  The groups and states
#' in the returned sequenc are superset of all the groups and states in
#' x and y.
#'
# --------------------------------------------------------

### See combine_contexts  (merge or replace)

add_context_segments <- function(x, y, replace=TRUE) {


  if (missing(x) | missing(y))
    stop("Must provide 'x' and 'x' wide-format binary context data frames.")

  if (!NROW(x)) stop("Specify a non-empty context x.")
  if (!NROW(y)) stop("Specify a non-empty context x.")

  # check here now for consistent names
  if (NROW(x)) {
    if (!all(names(y) %in% names(x))) {
      warning("'x' and 'y' must have consistent ordered group/state specifications.  Adding names with value=0 to y.")
      for (z in names(y))
        if (!z %in% names(x))
          y[[z]] <- 0L   # make sure it is an integer!!!!
    }
  } else return (y)

  #1. add new trailing edge of x
  #2. remove/merge y in the x range
  #3. add leading edge of x

  cols = !names(x) %in% c("Time")
  left <- y %>%
    filter(Time <= x[1,]$Time)
  if (!replace)
    if (NROW(left)) {  # if merge, combine states
      left <- left %>%
        slice_max(Time) %>%
        mutate(Time = x[1,]$Time)
      left[1,cols] <- x[1,cols] + left[1,cols]
    } else left <- x[1,]
  else left <- x[1,]
  middle <- y %>%
    filter(Time > x[1,]$Time & Time < x[2,]$Time)
  if (!replace) {
    if (NROW(middle)) {  # if !merge, then we don't add middle values together.
      middle <- middle %>%
        group_by(across(all_of(c("Time")))) %>%
        group_modify(~ .x + x[1,cols]) %>%
        ungroup()
    }
  } else middle <- middle %>% filter(FALSE)

  right <- y %>%
    filter(Time <= x[2,]$Time)
  if (NROW(right)) {
    right <- right %>%
      slice_max(Time) %>%
      mutate(Time = x[2,]$Time)
    right[1,cols] <- x[2,cols] + right[1,cols]
  } else right <- x[2,]

  bind_rows(left, middle, right) %>%
    mutate(
      across(-all_of(c("Time")),
             ~ case_when(. > 1 ~ 1L,
                         TRUE ~ .))
    ) %>%
    arrange(Time)

}
