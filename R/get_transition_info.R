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
#' set.seed(100)
#' predictions <- (sample(1:100)%%2)
#' references  <- (sample(1:100)%%2)
#' window_size <- 7
#' get_transition_info(predictions, references, window_size)
get_transition_info <- function(
  predictions, references, window_size = 1, ...
) {

  validate_transition_info_input(predictions, references)

  prefs <- get_preferences(
    predictions, references, window_size, missing_info
  )

  prefs$matchings <- get_matchings(prefs)

  # Clean up the object

  rejects <- prefs$matchings$rejected

  prefs$false_negative_indices <- prefs$student_reference_i[
    !prefs$student_reference_i %in%
      prefs$matchings$Reference_Index[!rejects]
  ]

  prefs$false_positive_indices <- prefs$college_prediction_i[
    !prefs$college_prediction_i %in%
      prefs$matchings$Prediction_Index[!rejects]
  ]

  stopifnot(
    nrow(prefs$matchings[!rejects, ]) + length(prefs$false_negative_indices) ==
      length(prefs$student_reference_i),
    nrow(prefs$matchings[!rejects, ]) + length(prefs$false_positive_indices) ==
      length(prefs$college_prediction_i)
  )

  prefs$missing_cases <- missing_cases

  structure(prefs, class = "transition")

}

#' @rdname get_transition_info
#' @keywords internal
validate_transition_info_input <- function(predictions, references) {

  stopifnot(
    length(predictions) == length(references)
  )

  complete <- stats::complete.cases(predictions, references)
  if (any(!complete)) {
    warning(paste(
      "Addressing", sum(!complete), "cases with",
      "missing prediction and/or reference"
    ), call. = FALSE)
  }

  missing_info <- data.frame(
    old_index = which(complete),
    new_index = seq(which(complete))
  )

  predictions <- predictions[complete]
  references <- references[complete]

  stopifnot(
    all(predictions %in% c(0, 1)),
    all(references %in% c(0,1))
  )

  assign("predictions", predictions, parent.frame())
  assign("references", references, parent.frame())
  assign("missing_info", missing_info, parent.frame())
  assign("missing_cases", which(!complete), parent.frame())

  invisible()

}
