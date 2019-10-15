#' Account for cases that refuse all matches
#'
#' To run the college admissions algorithm, it is assumed that each
#' student/college has at least one possible match. In the activity transition
#' application, possible matches are restricted to "nearby" cases, i.e. within
#' some specified \code{window_size}. Thus, if there are no matches close by,
#' there are no possible matches, and the case needs to be removed from the
#' analysis in the college admissions algorithm. It needs to be re-inserted
#' afterwards and labeled as a false positive or false negative, depending on
#' whether it was a college (i.e., a predicted transition) or a student (i.e.,
#' an actual transition), respectively.
#'
#' @param prefs an object passed from \code{\link{get_preferences}}
#'
#' @return A pruned list of matrices containing only cases with at least one
#'   possible match
#' @keywords internal
#'
prune_prefs <- function(prefs) {

  # Initialize the preferences
    ref_prefs <- get_proposer_rank(
      prefs$student_reference_i,
      prefs$college_prediction_i,
      prefs$window_size
    )

    pred_prefs <- get_proposer_rank(
      prefs$college_prediction_i,
      prefs$student_reference_i,
      prefs$window_size
    )

  # Test for cases to remove
    ref_test <- apply(
      ref_prefs, 2, function(x) all(is.na(x))
    )
    pred_test <- apply(
      pred_prefs, 2, function(x) all(is.na(x))
    )

  # Perform the pruning
    if (any(ref_test)) {
      prefs$false_negative_indices <-
        prefs$student_reference_i[which(ref_test)]
      prefs$student_reference_colnames <-
        prefs$student_reference_i[-which(ref_test)]
    }

    if (any(pred_test)) {
      prefs$false_positive_indices <-
        prefs$college_prediction_i[which(pred_test)]
      prefs$college_prediction_colnames <-
        prefs$college_prediction_i[-which(pred_test)]
    }

  # Get the final preferences
    prefs$student_reference_prefs <- get_proposer_rank(
      prefs$student_reference_colnames,
      prefs$college_prediction_colnames,
      prefs$window_size
    )

    prefs$college_prediction_prefs <- get_proposer_rank(
      prefs$college_prediction_colnames,
      prefs$student_reference_colnames,
      prefs$window_size
    )

    return(prefs)

}

#' Rank preferences for an arbitrary proposer and rejecter, based on distance
#' (i.e., difference) between them
#'
#' @param proposer A vector containing indices of transitions for the proposing
#'   (optimal) party
#' @param rejecter A vector containing indices of transitions for the rejecting
#'   (pessimal) party
#' @inheritParams summary.transition
#'
#' @return A matrix with \code{length(proposer)} columns and
#'   \code{length(rejecter)} rows, giving ordered preferences for the proposer,
#'   based on the absolute difference between the ith proposer index and the
#'   rejecter indices, where differences larger than \code{window_size} are
#'   treated as omitted preferences, i.e., non-possibilities.
#' @keywords internal
#'
get_proposer_rank <- function(proposer, rejecter, window_size) {

  ranks <-
    sapply(
      proposer, function(y) rejecter[order(abs(y - rejecter))],
      simplify = FALSE
    ) %>%
    do.call(cbind, .)

  distances <-
    sapply(
      proposer, function(y) abs(y - rejecter)[order(abs(y - rejecter))],
      simplify = FALSE
    ) %>%
    do.call(cbind, .)

  ranks[distances > window_size] <- NA

  # Convert to relative ranks
  apply(
    ranks, 2, function(y)
      ifelse(y %in% rejecter, match(y, rejecter), y)
  )

}

#' Obtain preference lists for predicted and actual (reference) activity
#' transitions
#'
#' When predicting activity transitions, the behavior of the predictor is not
#' known a priori. It may predict too many or too few transitions, and its
#' "intent" is also unknown. Therefore, some method is necessary in order to
#' determine which predictions (if any) should be taken to correspond to a
#' reference transition. There should also be a record of false positives and
#' false negatives. The problem is treated as an instance of the college
#' admissions problem, wherein both parties give their preferences for who they
#' would like to be matched with, and a stable arrangement is sought. This
#' function supports the overall goal by assigning the preferences based on the
#' temporal proximity of predicted and actual transitions. Preferences beyond a
#' specified \code{window_size} are not allowed.
#'
#' @inheritParams get_transition_info
#' @inheritParams summary.transition
#'
#' @return A list of matrices giving distance-based preferences for both the
#'   predicted and reference transitions, formatted to pass directly to
#'   \code{\link[matchingMarkets]{hri}}
#' @keywords internal
#'
#' @rdname summary.transition
get_preferences <- function(
  predictions, references, window_size, missing_info
) {

  # Indices of transitions
    ref_i <-
      which(references == 1) %>%
      match(missing_info$new_index) %>%
      {missing_info$old_index[.]}
    pred_i <-
      which(predictions == 1) %>%
      match(missing_info$new_index) %>%
      {missing_info$old_index[.]}

  # Initialize
    prefs <- list(
      window_size = window_size,
      student_reference = references,
      college_prediction = predictions,
      student_reference_i = ref_i,
      college_prediction_i = pred_i,
      student_reference_colnames = ref_i,
      college_prediction_colnames = pred_i,
      false_negative_indices = c(),
      false_positive_indices = c(),
      missing_info = missing_info
    )

  prune_prefs(prefs)

}
