# Core functionality ------------------------------------------------------

#' An S4 class containing summary information about a \code{transition} object
#'
#' @slot result a data frame with the summary information
summaryTransition <- setClass(
  "summaryTransition", representation(
    result = "data.frame", trans_object = "list"
  )
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

    reference_positives <- sum(object$references)
    predicted_positives <- sum(object$predictions)

  # Cell totals

    rejects <- object$matchings$rejected
    true_positives <- sum(!rejects)

  # Lags & RMSE

    abs_lags <-
      object$matchings$abs_lag[!rejects] %>%
      mean_sd(digits = 1, nsmall = 1)

    signed_lags <-
      object$matchings$signed_lag[!rejects] %>%
      mean_sd(digits = 1, nsmall = 1)

    rmse <-
      object$matchings$abs_lag[!rejects]^2 %>%
      mean() %>%
      sqrt() %>%
      round(1)

    rmse_prop <- 1 - rmse/object$window_size

  # Rand

    rand_index <-
      object[c("references", "predictions")] %>%
      sapply(cumsum, simplify = FALSE) %>%
      stats::setNames(c("cl1", "cl2")) %>%
      do.call(clues::adjustedRand, .) %>%
      .[rand_index] %>%
      {stats::setNames(., paste0("Rand_Index_", names(.)))} %>%
      as.list(.) %>%
      data.frame(stringsAsFactors = FALSE, row.names = NULL)

  # Summarize

    class(object) <- "list"
    data.frame(

      window_size = object$window_size,

      reference_positives = reference_positives,
      predicted_positives = predicted_positives,

      true_positives = true_positives,

      n_rejected_pairs = sum(rejects),

      mean_abs_lag_indices = abs_lags$mean,
      sd_abs_lag_indices = abs_lags$sd,
      mean_sd_abs_lag_indices = abs_lags$sum_string,

      mean_signed_lag_indices = signed_lags$mean,
      sd_signed_lag_indices = signed_lags$sd,
      mean_sd_signed_lag_indices = signed_lags$sum_string,

      recall = true_positives / reference_positives,
      precision = true_positives / predicted_positives,

      rmse_lag_indices = rmse,
      rmse_prop = rmse_prop,

      row.names = NULL,
      stringsAsFactors = FALSE

    ) %>% cbind(
      ., aggregated_performance = rowMeans(
        .[ ,c("recall", "precision", "rmse_prop")]
      )
    ) %>% cbind(
      rand_index, stringsAsFactors = FALSE
    ) %>%
    new("summaryTransition", result = ., trans_object = object)

}

# Addition and Subtraction ------------------------------------------------

#' Addition and subtraction for objects of class \code{summaryTransition}
#'
#' @param e1 the first object
#' @param e2 the second object
#'
#' @keywords internal
add_summaryTransition <- function(e1, e2) {

  not_used <- "Not used in combined objects"

  rand_e1 <-
    names(e1@result) %>%
    {.[grepl("Rand_Index_", .)]} %>%
    gsub("Rand_Index_", "", .)
  rand_e2 <-
    names(e2@result) %>%
    {.[grepl("Rand_Index_", .)]} %>%
    gsub("Rand_Index_", "", .)
  rand <-
    c(rand_e1, rand_e2) %>%
    unique()

  e1 <- e1@trans_object
  e2 <- e2@trans_object

  student_reference <- c(
    e1$student_reference, e2$student_reference
  )

  college_prediction <- c(
    e1$college_prediction, e2$college_prediction
  )

  matchings <- rbind(
    e1$matchings, e2$matchings
  )

  window_size <-
    e1$window_size %>%
    c(e2$window_size) %>%
    unique() %T>%
    {if (length(.) > 1) stop(
      "Cannot add if spurious pairing thresholds",
      " differ for the objects",
      call. = FALSE
    )} %>%
    as.numeric()

  list(
    window_size = window_size,
    student_reference = student_reference,
    college_prediction = college_prediction,
    student_reference_i = not_used,
    college_prediction_i = not_used,
    student_reference_colnames = not_used,
    college_prediction_colnames = not_used,
    false_negative_indices = not_used,
    false_positive_indices = not_used,
    missing_info = not_used,
    student_reference_prefs = not_used,
    college_prediction_prefs = not_used,
    matchings = matchings,
    missing_cases = not_used
  ) %>%
  summary.transition(rand)

}

#' @rdname add_summaryTransition
#' @export
setMethod("+", signature(e1 = "summaryTransition", e2 = "summaryTransition"),
  function(e1, e2) add_summaryTransition(e1, e2))

#' @rdname add_summaryTransition
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
    warning(
      "Spurious pairing thresholds differ for the objects",
      call. = FALSE
    )
    result$window_size <- paste(
      e1@result$window_size, e2@result$window_size, sep = "--"
    )
  }

  list(differences = result, e1 = e1@result, e2 = e2@result)

}

#' @rdname add_summaryTransition
#' @export
setMethod("-", signature(e1 = "summaryTransition", e2 = "summaryTransition"),
  function(e1, e2) subtract_summaryTransition(e1, e2))

# Coercion ----------------------------------------------------------------

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
