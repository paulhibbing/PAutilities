#' Determine the minimum equivalence zone necessary for establishing equivalence
#' in a paired equivalence test
#'
#' Paired equivalence tests are conducted based on a pre-specified zone of
#' equivalence. Following the test, it is useful to know how minimally small or
#' large the zone would have needed to be in order for the test to indicate
#' equivalence.
#'
#' @param result data frame constructed in
#'   \code{\link{paired_equivalence_wrapper}} that provides information about
#'   the paired equivalence test
#'
#' @keywords internal
equivalent_at <- function(result) {

  if (any(result$region_high != abs(result$region_low))) {
    warning(paste(
      "Asymmetrical equivalence region(s) detected, which violates",
      "code\n  assumptions in `equivalent_at`.",
      "This needs fixing."
    ))
  }

  nrow(result) %>%
  seq(.) %>%
  split(result, .) %>%
  lapply(function(x) {
    x$equivalent_at <- switch(
      x$scale,
      "absolute" = get_absolute_equivalent_at(x),
      "relative" = get_relative_equivalent_at(x)
    )
    x
  }) %>%
  do.call(rbind, .)

}

#' @rdname equivalent_at
get_absolute_equivalent_at <- function(result) {

  lim <-
    result[ ,c("CI_low", "CI_high")] %>%
    unlist(.) %>%
    abs(.) %>%
    max(.)

  ## Determine smallest increment above CI that will permit equivalence

  as.character(lim) %>%
  gsub("^.*\\.", "", .) %>%
  nchar(.) %>%
  {. * -1} %>%
  {10^.} %>%
  {lim + .} %>%
  as.character(.)

}

#' @rdname equivalent_at
get_relative_equivalent_at <- function(result) {

  get_absolute_equivalent_at(result) %>%
  as.numeric(.) %>%
  {. / result$mean_y} %>%
  as.character(.)

}
