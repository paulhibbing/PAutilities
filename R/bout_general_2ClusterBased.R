#' @inheritParams get_bouts
#' @keywords internal
#' @rdname bouts_internal
group_runs_clusterBased = function(
  x, target, target_buffer, required_percent,
  longest_allowable_interruption, max_n_interruptions
) {

  x %<>%
    {. == target} %>%
    ifelse(target, "other")

  index_runs(x) %>%
  within({group = cumsum(
    values == "other" & lengths >= target_buffer
  )}) %>%
  split(., .$group) %>%
  lapply(function(df, target) df[df$values == target, ], target) %>%
  .[sapply(., function(df) nrow(df) > 0)] %>%
  lapply(
    process_clusterBased_set, x, target, required_percent,
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
  runs, x, target, required_percent, longest_allowable_interruption,
  max_n_interruptions
) {

  if (nrow(runs) == 1) {
    result <-
      collapse_runs(runs, target) %>%
      cbind(h = NA, run_info(runs), .)
    return(result)
  }

  if (nrow(runs) > 100) message(
    "\nDetected more than 100 potential bouts",
    " (exact n = ", nrow(runs), ").",
    "\nAlgorithm will take a long time to run.",
    "\nTo circumvent this, consider splitting `x`",
    " into subsets,\n  e.g. by analyzing ",
    "each day of data separately, or by",
    "\n  setting `target_buffer` to a smaller value."
  )

  tree <-
    runs[ ,c("start_index", "end_index")] %>%
    stats::dist(method = "euclidian") %>%
    stats::hclust("complete")

  results <- mapply(
    test_clusterBased_bouts,
    h = c(0, tree$height),
    MoreArgs = list(
      x = x, target = target, runs = runs, tree = tree,
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
test_clusterBased_bouts <- function(
  x, target, runs, tree, h, required_percent,
  max_n_interruptions, longest_allowable_interruption
) {

  result <-
    stats::cutree(tree, h = h) %T>%
    {stopifnot(length(.) == nrow(runs))} %>%
    {within(runs, {group = .})} %>%
    split(., .$group) %>%
    lapply(function(run, x, target) {

      info <- run_info(run)

      seq(info$start_index, info$end_index) %>%
      x[.] %>%
      index_runs(.) %>%
      within({group = 1}) %>%
      collapse_runs(target) %>%
      cbind(info, .)

    }, x = x, target = target) %>%
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
