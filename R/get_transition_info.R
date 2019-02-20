#' Convert a set of predicted and actual activity transitions to an object that
#' can be analyzed
#'
#' @param predictions A dummy-coded vector of predicted transitions (1)
#'   interspersed with non-transitions (0)
#' @param references A dummy-coded vector of actual (i.e., reference)
#'   transitions (1) interspersed with non-transitions (0)
#' @param window_size The maximum number of indices that are allowed to separate
#'   a predicted and reference transition, before the two are considered
#'   incompatible
#' @param ... additional arguments passed to or from methods, not currently used
#'
#' @return an object of class \code{transition} that contains necessary
#'   information for evaluating the effectiveness of the predictions.
#' @export
#'
#' @examples
#' predictions <- sample(c(0,1), 100, TRUE, c(3, 1))
#' references  <- sample(c(0,1), 100, TRUE, c(4,1))
#' get_transition_info(predictions, references, 10)
get_transition_info <- function(predictions, references, window_size = 1, ...) {

  stopifnot(
    length(predictions) == length(references),
    all(predictions %in% c(0, 1)),
    all(references %in% c(0,1))
  )

  prefs <- get_preferences(predictions, references, window_size)

  prefs$matchings <- get_matchings(prefs)

  # Clean up the object

  prefs$false_negative_indices <- prefs$student_reference_i[
    !prefs$student_reference_i %in% prefs$matchings$Reference_Index
  ]

  prefs$false_positive_indices <- prefs$college_prediction_i[
    !prefs$college_prediction_i %in% prefs$matchings$Prediction_Index
  ]

  stopifnot(
    nrow(prefs$matchings) + length(prefs$false_negative_indices) ==
      length(prefs$student_reference_i),
    nrow(prefs$matchings) + length(prefs$false_positive_indices) ==
      length(prefs$college_prediction_i)
  )

  class(prefs) <- "transition"
  return(prefs)

}
