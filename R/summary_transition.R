#' An S4 class containing summary information about a \code{transition} object
#'
#' @slot result a data frame with the summary information
summaryTransition <- setClass(
  "summaryTransition", representation(result = "data.frame")
)

#' Evaluate the effectiveness of predicted transitions for an object of class
#' \code{transition}
#'
#' @param object a \code{transition} object to analyze
#' @inheritParams get_transition_info
#' @param rand_index the (possibly adjusted) Rand indices to return from
#'   \code{\link[clues]{adjustedRand}}. Any or all of \code{c("Rand", "HA",
#'   "MA", "FM", "Jaccard")}
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
summary.transition <- function(
  object, rand_index = c("Rand", "HA", "MA", "FM", "Jaccard"),
  ...
) {

  rand_index <- match.arg(
    rand_index, c("Rand", "HA", "MA", "FM", "Jaccard", "Error"), TRUE
  )

  # Marginal totals
  reference_positives <- sum(object$student_reference)
  predicted_positives <- sum(object$college_prediction)

  # Cell totals
  rejects <- object$matchings$rejected
  true_positives <- sum(!rejects)

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

  rand_index <-
    reconstruct_transitions(object) %>%
    .[c("student_reference", "college_prediction")] %>%
    sapply(cumsum, simplify = FALSE) %>%
    stats::setNames(c("cl1", "cl2")) %>%
    do.call(clues::adjustedRand, .) %>%
    .[rand_index] %>%
    {stats::setNames(., paste0("Rand_Index_", names(.)))} %>%
    as.list(.) %>%
    data.frame(stringsAsFactors = FALSE, row.names = NULL)

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

    rand_index,

    row.names = NULL,
    stringsAsFactors = FALSE

  ) %>% cbind(
    ., aggregated_performance = rowMeans(
      .[ ,c("recall", "precision", "rmse_prop")]
    )
  ) %>%
  new("summaryTransition", result = .)

}

#' Subtraction for objects of class \code{summaryTransition}
#'
#' @param e1 the first object
#' @param e2 the second object
#'
#' @keywords internal
subtract_summaryTransition <- function(e1, e2) {

  diff_vars <- c(
    "window_size","recall", "precision",
    "mean_abs_lag_indices", "mean_signed_lag_indices",
    "rmse_lag_indices", "rmse_prop", "aggregated_performance"
  )

  result <-
    sapply(
      diff_vars,
      function(x) e1@result[ ,x] - e2@result[ ,x],
      simplify = FALSE
    ) %>%
    do.call(data.frame, .) %>%
    {stats::setNames(., paste("diff", names(.), sep = "_"))} %>%
    data.frame(
      window_size = e1@result$window_size, .,
      stringsAsFactors = FALSE, row.names = NULL
    )

  if(result$diff_window_size != 0) {
    warning("Spurious pairing thresholds differ for the objects")
    result$window_size <- paste(
      e1@result$window_size, e2@result$window_size, sep = "--"
    )
  }

  list(differences = result, e1 = e1@result, e2 = e2@result)

}

#' @rdname subtract_summaryTransition
#' @export
setMethod("-", signature(e1 = "summaryTransition", e2 = "summaryTransition"),
  function(e1, e2) subtract_summaryTransition(e1, e2))

#' As("summaryTransition", "data.frame")
#'
#' @name as
#' @family summaryTransition
setAs("summaryTransition", "data.frame", function(from) from@result)

#' As("summaryTransition", "list")
#'
#' @name as
#' @family summaryTransition
setAs("summaryTransition", "list", function(from) as.list(from@result))
