#' @inheritParams get_bouts
#' @keywords internal
#' @rdname bouts_internal
group_runs_sequential <- function(
  x, longest_allowable_interruption
) {

  x %<>%
    index_runs(.) %>%
    within({group = 1})

  current_state <- x$values[1]
  new_state <- FALSE
  cumulative_interruption <- 0

  for (i in 2:nrow(x)) {

    ## Start by assuming the active group will be assigned
    x$group[i] <- x$group[i - 1]

    ## The state needs to change if the cumulative interruption
    ## exceeds threshold; however, if the current row value matches
    ## the original, it should not be added to the cumulative
    ## interruption total
    cumulative_interruption <-
      {x$values[i] == current_state} %>%
      ifelse(0, x$lengths[i]) %>%
      sum(cumulative_interruption)

    if (cumulative_interruption > longest_allowable_interruption) {
      new_state <- TRUE
    }

    ## If we are not in a new state, we were correct to assume assignment would
    ## be to the active group. As such, no further action is needed.
    if (!new_state) {
      next
    }

    ## Otherwise, we need to update the state, start a new group,
    ## and reset the state tracking variables (new_state and
    ## cumulative_interruption)
    current_state <- x$values[i]
    x$group[i] %<>% {. + 1}
    new_state <- FALSE
    cumulative_interruption <- 0

  }

  df_reorder(x, "group", "end_index") %>%
  collapse_runs(.)

}
