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

  if ("other" %in% bouts$values) warning(
    "`bout_expand` may behave oddly when",
    " the input includes 'other' in its values"
  )

  result <-
    attr(bouts, "input_length") %>%
    rep("other", .)

  for (i in seq(nrow(bouts))) {

    result[bouts$start_index[i]:bouts$end_index[i]] <- bouts$values[i]

  }

  result

}
