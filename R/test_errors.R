#' Compare numeric variables in a data frame based on root-squared differences
#'
#' @param reference a data frame giving reference data
#' @param target a data frame giving target data
#' @param vars character vector of variable names to compare in each data frame
#' @param tolerance allowable difference between numeric values
#'
#' @return Named logical vector with one element per variable compared,
#'   indicating whether the maximum and mean squared differences fall within the
#'   tolerance.
#' @note It is assumed that \code{reference} and \code{target} have equal
#'   numbers of rows
#' @export
#'
#' @examples
#' reference <- data.frame(
#' a = 1:100, b = 75:174
#' )
#'
#' target <- data.frame(
#'   a = 0.001 + (1:100),
#'   b = 76:175
#' )
#'
#' test_errors(reference, target, c("a", "b"))
test_errors <- function(
  reference, target, vars,
  tolerance = 0.001005
) {

  stopifnot(
    inherits(reference, "data.frame"),
    inherits(target, "data.frame")
  )
  stopifnot(
    nrow(reference) == nrow(target),
    all(vars %in% names(reference)),
    all(vars %in% names(target))
  )
  stopifnot(
    all(sapply(reference[ ,vars], is.numeric)),
    all(sapply(target[ ,vars], is.numeric))
  )

  sapply(
    vars,
    function(variable) {

      e <- reference[ ,variable] -
        target[ ,variable]
      rse <- sqrt(e^2)

      all(
        mean(rse) <= tolerance,
        max(rse) <= tolerance
      )

    }
  )

}
