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
#' predictions <- (sample(1:100)%%2)
#' references  <- (sample(1:100)%%2)
#' window_size <- 7
#' transitions <- get_transition_info(predictions, references, window_size)
#' summary(transitions)
summary.transition <- function(object, ...) {

  # Marginal totals
  reference_positives <- sum(object$student_reference)
  predicted_positives <- sum(object$college_prediction)

  # Cell totals
  rejects <- object$matchings$rejected
  true_positives <- nrow(object$matchings[!rejects, ])

  # Lags & RMSE
  abs_lags <- mean_sd(
    object$matchings$abs_lag[!rejects], digits = 1, nsmall = 1
  )
  signed_lags <- mean_sd(
    object$matchings$signed_lag[!rejects], digits = 1, nsmall = 1
  )
  rmse <- round(
    sqrt(mean(object$matchings$abs_lag[!rejects]^2)),
    1
  )
  rmse_prop <- 1 - rmse/object$window_size

  # Summarize
  data.frame(

    window_size = object$window_size,

    reference_positives = reference_positives,
    predicted_positives = predicted_positives,

    true_positives = true_positives,

    recall = true_positives / reference_positives,
    precision = true_positives / predicted_positives,

    n_rejected_pairs = sum(rejects),

    mean_abs_lag_indices = abs_lags$mean,
    sd_abs_lag_indices = abs_lags$sd,
    mean_sd_abs_lag_indices = abs_lags$sum_string,

    mean_signed_lag_indices = signed_lags$mean,
    sd_signed_lag_indices = signed_lags$sd,
    mean_sd_signed_lag_indices = signed_lags$sum_string,

    rmse_lag_indices = rmse,
    rmse_prop = rmse_prop,

    row.names = NULL,
    stringsAsFactors = FALSE

  ) %>% cbind(
    ., aggregated_performance = rowMeans(
      .[ ,c("recall", "precision", "rmse_prop")]
    )
  )

}
