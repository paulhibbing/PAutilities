#' Run length encoding with indices
#'
#' @param x vector of values on which to perform run length encoding
#' @param zero_index logical. Should indices be indexed from zero (useful for
#'   Rcpp)?
#'
#' @return A data frame with information about the runs and start/stop indices
#' @export
#'
#' @examples
#'
#' x <- c(
#'   FALSE, TRUE, FALSE, FALSE, FALSE, TRUE,
#'   FALSE, TRUE, TRUE, FALSE, TRUE, FALSE,
#'   FALSE, FALSE, FALSE, FALSE, TRUE, TRUE,
#'   FALSE, TRUE
#' )
#' head(index_runs(x))
index_runs <- function(x, zero_index = FALSE) {

  result <-
    rle(x) %>%
    do.call(data.frame, .) %>%
    data.frame(., end_index = cumsum(.$lengths)) %>%
    data.frame(., start_index = .$end_index - .$lengths + 1L) %>%
    rev(.)

  if (!zero_index) {

    result

  } else {

    result[ ,1:2] %>%
    sapply(`-`, 1) %>%
    data.frame(result[ ,3:4])

  }

}
