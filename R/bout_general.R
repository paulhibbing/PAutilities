#' Assign run-length-encoded data to groups
#'
#' This allows for primary runs to be interrupted by secondary runs
#' having length up to \code{threshold}
#'
#' @param x run-length-encoded data frame (output
#' from \code{PAutilities::index_runs})
#' @param threshold the allowable length of interruptions
#'
#' @return the input data frame with an additional column indicating
#'  which group each run has been assigned to
#'
#' @keywords internal
#' @name general_bouts
group_runs <- function(x, threshold = 10) {

  x$group <- 1
  active_run <- FALSE

  for (i in 2:nrow(x)) {

    ## Begin by assuming the next event
    ## is a continuation of the bout
    x$group[i] <- x$group[i - 1]

    ## If the run length does not exceed the minimum,
    ## no further work is needed
    if (x$lengths[i] < threshold) {

      active_run <- TRUE
      next

    }

    ## Otherwise, we have to check if a new
    ## behavior is starting -- first identify
    ## the expected behavior for this run
    run_class <-
      seq(i) %>%
      x$group[.] %>%
      {. == x$group[i]} %>%
      which(.) %>%
      .[1] %>%
      x$values[.]

    ## If the current behavior is different,
    ## then label it as a new run
    if (x$values[i] != run_class) {

      x$group[i] %<>% {. + 1}
      active_run <- FALSE

    }

  }

  PAutilities::df_reorder(x, "group", "end_index")

}

#' @rdname general_bouts
#' @keywords internal
collapse_runs <- function(x, threshold = 10) {

  group_runs(x, threshold) %>%
  split(., .$group) %>%
  lapply(function(df) {
    data.frame(
      device = df$device[1],
      start_index = df$start_index[1],
      end_index = df$end_index[nrow(df)],
      group = unique(df$group),
      values = df$values[1],
      lengths = sum(df$lengths),
      interruption_length = sum(
        ifelse(df$values == df$values[1], 0, df$lengths)
      ),
      start = df$start[1],
      end = df$end[nrow(df)]
    ) %>%
    within({interval = lubridate::interval(start, end)})
  }) %>%
  do.call(rbind, .)

}
