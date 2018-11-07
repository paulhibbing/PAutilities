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
summary.transition <- function(object, ...) {

  # Marginal totals
  reference_positives <- sum(object$student_reference)
  predicted_positives <- sum(object$college_prediction)
  reference_negatives <- sum(object$student_reference == 0)
  predicted_negatives <- sum(object$college_prediction == 0)

  # Cell totals
  true_positives <- nrow(object$matchings)
  false_positives <- length(object$false_positive_indices)
  true_negatives <- reference_negatives - false_positives
  false_negatives <- length(object$false_negative_indices)

  # Lags
  lags <- apply(object$matchings, 1, function(x)
    abs(x["Reference_Index"] - x["Prediction_Index"]))

  lags <- mean_sd(lags, digits = 1, nsmall = 1)

  # Summarize
  data.frame(

    window_size = object$window_size,

    reference_positives = reference_positives,
    reference_negatives = reference_negatives,
    predicted_positives = predicted_positives,
    predicted_negatives = predicted_negatives,

    true_positives = true_positives,
    false_positives = false_positives,
    true_negatives = true_negatives,
    false_negatives = false_negatives,

    true_positive_rate = true_positives / reference_positives,
    false_positive_rate = false_positives / reference_negatives,
    true_negative_rate = true_negatives / reference_negatives,
    false_negative_rate = false_negatives / reference_positives,
    positive_predictive_value = true_positives / predicted_positives,
    negative_predictive_value = true_negatives / predicted_negatives,
    accuracy_percent = (
      sum(true_positives, true_negatives) /
        sum(true_positives, false_positives, true_negatives, false_negatives)
    ) * 100,

    mean_lag_indices = lags$mean,
    sd_lag_indices = lags$sd,
    mean_sd_lag_indices = lags$sum_string,

    row.names = NULL,
    stringsAsFactors = FALSE
  )
}
