#' @inheritParams get_bouts
#' @keywords internal
#' @rdname bouts_internal
group_runs_clusterBased = function(
  x, target, target_buffer, required_percent,
  longest_allowable_interruption, max_n_interruptions
) {

  ifelse(x == target, target, "other") %>%
  index_runs(.) %>%
  within({group = cumsum(
    values == "other" & lengths >= target_buffer
  )}) %>%
  split(., .$group) %>%
  lapply(function(df, target) df[df$values == target, ], target) %>%
  .[sapply(., function(df) nrow(df) > 0)] %>%
  lapply(
    process_clusterBased_set, x, required_percent,
    longest_allowable_interruption, max_n_interruptions
  ) %>%
  do.call(rbind, .) %>%
  structure(., h = .$h) %>%
  within({h = NULL})

}

#' @inheritParams get_bouts
#' @param runs pre-processed input (mostly run length encoded)
#' @keywords internal
#' @rdname bouts_internal
process_clusterBased_set <- function(
  runs, x, required_percent, longest_allowable_interruption,
  max_n_interruptions
) {

  if (nrow(runs) == 1) {
    result <-
      collapse_runs(runs) %>%
      cbind(h = NA, .)
    return(result)
  }

  if (nrow(runs) > 100) message(
    "\nDetected more than 100 potential bouts",
    " (exact n = ", nrow(runs), ").",
    "\nAlgorithm will take a long time to run.",
    "\nTo circumvent this, consider splitting `x`",
    " into subsets,\n  e.g. by analyzing ",
    "each day of data separately, or by",
    "\n  setting `target_buffer` to a higher value."
  )

  tree <-
    runs$start_index %>%
    stats::dist(method = "euclidian") %>%
    stats::hclust("complete")

  results <- mapply(
    test_clusterBased_bouts,
    h = c(0, tree$height),
    MoreArgs = list(
      x = x, runs = runs, tree = tree,
      required_percent = required_percent,
      longest_allowable_interruption = longest_allowable_interruption,
      max_n_interruptions = max_n_interruptions
    ),
    SIMPLIFY = FALSE
  )

  sapply(results, "[[", "meets_requirements") %>%
  which(.) %>%
  max(.) %>%
  results[[.]] %>%
  .$result

}

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

#' @inheritParams get_bouts
#' @keywords internal
#' @rdname bouts_internal
test_clusterBased_bouts <- function(
  x, runs, tree, h, required_percent,
  max_n_interruptions, longest_allowable_interruption
) {

  runs$group <- stats::cutree(tree, h = h)

  result <-
    collapse_runs(runs) %>%
    {.[ ,1:4]} %>%
    split(., seq(nrow(.))) %>%
    lapply(function(run, x) {
      seq(run$start_index, run$end_index) %>%
      x[.] %>%
      index_runs(.) %>%
      within({group = 1}) %>%
      collapse_runs(.) %>%
      .[ ,-c(1:4)] %>%
      cbind(run, .)
    }, x) %>%
    do.call(rbind, .) %>%
    cbind(h = h, .)

  meets_requirements <- all(
    result$percent_time_engaged >= required_percent,
    result$longest_interruption_event <= longest_allowable_interruption,
    result$n_interruption_events <= max_n_interruptions
  )

  list(
    meets_requirements = meets_requirements,
    result = result
  )

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
