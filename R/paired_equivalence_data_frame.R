#' @rdname paired_equivalence_test
#' @export
paired_equivalence_test.data.frame <- function(
  x, y, y_type = c("both", "criterion", "comparison"),
  alpha = 0.05, na.rm = TRUE,
  scale = c("relative", "absolute"),
  absolute_region_width = NULL,
  relative_region_width = NULL,
  ...
) {

  column_wise <- sapply(x, paired_equivalence_test,
    y = y, y_type = "both", alpha = alpha, na.rm = na.rm,
    scale = "relative", absolute_region_width = absolute_region_width,
    relative_region_width = relative_region_width,
    simplify = FALSE
  )

  column_wise <- lapply(
    seq(column_wise), function(n) {
      result <- column_wise[[n]]$result
      result$variable <- names(column_wise)[n]
      result[ ,c("variable", setdiff(names(result), "variable"))]
    }
  )

  results <- do.call(rbind, column_wise)

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
