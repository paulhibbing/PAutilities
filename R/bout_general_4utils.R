#' Bout analysis internal functions
#'
#' @inheritParams get_bouts
#'
#' @keywords internal
#'
#' @name bouts_internal
collapse_runs <- function(x, target) {

  split(x, x$group) %>%
  lapply(function(df, target) {
    cases <- df$values == target
    data.frame(
      n_total_events = nrow(df),
      n_value_events = sum(cases),
      n_interruption_events = sum(!cases),

      length_total = sum(df$lengths),
      length_value = sum(
        ifelse(cases, df$lengths, 0)
      ),
      length_interruption = sum(
        ifelse(cases, 0, df$lengths)
      ),
      longest_interruption_event = max(
        ifelse(cases, 0, df$lengths)
      )
    ) %>%
    within({percent_time_engaged = length_value / length_total * 100})
  }, target) %>%
  do.call(rbind, .)

}

#' @param run a candidate run for which to retrieve basic information
#' @keywords internal
#' @rdname bouts_internal
run_info <- function(run) {
  data.frame(
    start_index = run$start_index[1],
    end_index = run$end_index[nrow(run)],
    group = unique(run$group),
    values = unique(run$values),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}

# Format Checks -----------------------------------------------------------

#' @inheritParams get_bouts
#' @keywords internal
#' @rdname bouts_internal
check_clusterBased <- function(
  x, target = NULL, required_percent, target_buffer
) {

  if (is.null(target)) stop(
    "Value must be supplied for `target` argument", call. = FALSE
  )

  if (is.null(target_buffer)) warning(
    "No value specified for `target_buffer`.\nDefaulting to Inf, ",
    "which may dramatically increase runtime", call. = FALSE
  )

  stopifnot(length(target) == 1)
  stopifnot(required_percent <= 100)

  if (required_percent < 1) {
    warning(
      "required_percent should be a percentage,",
      " not a proportion.\nDid you mean to pass ",
      round(required_percent * 100, 1), " rather than ",
      required_percent, "?", call. = FALSE
    )
  }

  if (!target %in% x) {
    stop(
      "Can't find any occurrences of `",
      target, "`", call. = FALSE
    )
  }

}


#' @param results output from \code{group_runs_clusterBased}
#' @keywords internal
#' @rdname bouts_internal
check_no_bouts <- function(results, x, target) {

  if (nrow(results) > 0) {

    structure(results, anyBouts = TRUE)

  } else {

    warning(
      "Found a case with no valid bouts -- the current solution",
      " (returning zeroes) may not meet your needs.\nFeel free to post",
      " an issue/pull request on GitHub.", call. = FALSE
    )

    data.frame(
      start_index = 1,
      end_index = length(x),
      values = target,
      n_total_events = 0,
      n_value_events = 0,
      n_interruption_events = 0,
      length_total = length(x),
      length_value = 0,
      length_interruption = 0,
      longest_interruption_event = 0,
      percent_time_engaged = 0
    ) %>%
    structure(anyBouts = FALSE)

  }

}
