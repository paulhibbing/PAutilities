#' Re-vectorize run length encoded data
#'
#' @param bouts output from \code{\link{get_bouts}}
#'
#' @return a vector of individual values comprising the runs in \code{bouts}
#' @export
#'
#' @examples
#' data(ex_data, package = "PAutilities")
#' intensity <- as.character(get_intensity(ex_data$METs))
#' bouts <- get_bouts(intensity, "sequential", 1)
#' tail(bout_expand(bouts), 40)
bout_expand <- function(bouts) {

  nrow(bouts) %>%
  seq(.) %>%
  split(bouts, .) %>%
  lapply(function(x) rep(x$values, x$end_index - x$start_index + 1)) %>%
  c(use.names = FALSE) %>%
  do.call(c, .)

}
