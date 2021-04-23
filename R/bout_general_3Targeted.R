#' @inheritParams get_bouts
#' @keywords internal
#' @rdname bouts_internal
group_runs_targeted = function(
  x, target, required_percent,
  longest_allowable_interruption,
  max_n_interruptions
) {

  runs <-
    index_runs(x) %>%
    subset(values == target)

  if (nrow(runs) > 100) message(
    "\nDetected more than 100 potential bouts",
    " (exact n = ", nrow(runs), ")",
    "\nAlgorithm will take a long time to run.",
    "\nTo circumvent this, consider splitting `x`",
    " into subsets,\n  e.g. by analyzing ",
    "each day of data separately."
  )

  tree <-
    runs$start_index %>%
    stats::dist(method = "euclidian") %>%
    stats::hclust("complete")

  results <- mapply(
    test_targeted_bouts,
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
check_targeted <- function(
  x, target = NULL, required_percent
) {

  if (is.null(target)) stop(
    "Value must be supplied for `target` argument\n  when",
    " using method = \"targeted\"", call. = FALSE
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
test_targeted_bouts <- function(
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
    cbind(
      h = h,
      required_percent = required_percent,
      max_n_interruptions = max_n_interruptions,
      longest_allowable_interruption = longest_allowable_interruption,
      .
    )

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
