#' Evaluate the effectiveness of predicted transitions for an object of class
#' \code{transition}
#'
#' @param object a \code{transition} object to analyze
#' @inheritParams get_transition_info
#' @param ... further arguments passed to or from methods, currently unused
#'
#' @return a data frame containing indicators that reflect, in different ways,
#'   the effectiveness of predicted transitions compared to the set of actual
#'   (reference) transitions
#' @export
#'
#' @examples
#' predictions <- sample(c(0,1), 100, TRUE, c(3, 1))
#' references  <- sample(c(0,1), 100, TRUE, c(4,1))
#' transitions <- get_transition_info(predictions, references, 10)
#' summary(transitions)
summary.transition <- function(object, window_size, ...) {

  total_reference <- sum(object$student_reference)
  total_prediction <- sum(object$college_prediction)

  true_positives <- nrow(object$matchings)

  false_negatives <- length(object$false_negative_indices)
  false_positives <- length(object$false_positive_indices)

  lags <- apply(object$matchings, 1, function(x)
    abs(x["Reference_Index"] - x["Prediction_Index"]))

  lags <- mean_sd(lags, digits = 1, nsmall = 1)

  data.frame(
    total_transitions_reference = total_reference,
    total_transitions_predicted = total_prediction,
    total_true_positive_pairs = nrow(object$matchings),

    detection_rate = nrow(object$matchings) / total_reference,
    false_negative_rate = false_negatives / total_reference,
    false_positive_rate = false_positives / total_prediction,

    mean_lag_indices = lags$mean,
    sd_lag_indices = lags$sd,
    mean_sd_lag_indices = lags$sum_string,
    window_size = object$window_size,

    row.names = NULL,
    stringsAsFactors = FALSE
  )
}
