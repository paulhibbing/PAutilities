#' @rdname paired_equivalence_test
#' @export
paired_equivalence_test.default <- function(
  x, y, y_type = c("both", "criterion", "comparison"),
  alpha = 0.05, na.rm = TRUE,
  scale = c("relative", "absolute"),
  absolute_region_width = NULL,
  relative_region_width = NULL,
  ...
) {

  stopifnot(is.numeric(x))

  if ("both" %in% y_type) {
    y_type <- c("criterion", "comparison")
  }

  if ("criterion" %in% y_type && scale == "absolute") {

    criterion_results <- paired_equivalence_wrapper(
      x, y, absolute_region_width,
      absolute_region_width, alpha, scale, na.rm
    )

  }

  if ("criterion" %in% y_type && scale == "relative") {

    criterion_results <- paired_equivalence_wrapper(
      x, y, relative_region_width,
      relative_region_width * mean(y),
      alpha, scale, na.rm
    )

  }

  if ("comparison" %in% y_type && scale == "absolute") {

    comparison_results <- paired_equivalence_wrapper(
      x, y, absolute_region_width,
      absolute_region_width, alpha, scale, na.rm
    )

  }

  if ("comparison" %in% y_type && scale == "relative") {

    comparison_results <- paired_equivalence_wrapper(
      x, y, relative_region_width,
      relative_region_width * mean(y),
      alpha, scale, na.rm, FALSE
    )

    ## Do the lower bound test

    lower <- 1 - relative_region_width

    lower_diffs <- x - (lower * y)
    lower_result <- stats::t.test(
      lower_diffs, alternative = "greater", mu = 0
    )$p.value

    ## Do the upper bound test

    upper <- 1/lower

    upper_diffs <- x - (upper * y)
    upper_result <- stats::t.test(
      upper_diffs, alternative = "less", mu = 0
    )$p.value

    comparison_results$tost_p <- max(
      lower_result, upper_result
    )

    comparison_results$tost_sig <- ifelse(
      comparison_results$tost_p < alpha,
      "*", "NS"
    )

  }

  ## Finish up

  results <- rbind(
    criterion_results, comparison_results
  )

  object <- list(
    x = x, y = y, alpha = alpha,
    absolute_region_width = absolute_region_width,
    relative_region_width = relative_region_width,
    scale = unique(results$scale),
    results = results
  )

  class(object) <- "paired_equivalence"

  object

}
