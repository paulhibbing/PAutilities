#' Obtain the matchings for predicted and actual activity transitions using the
#' college admissions algorithm
#'
#' @inheritParams prune_prefs
#'
#' @return A data frame giving the relative and absolute indices of the
#'   matchings, based on optimal outcomes for the students (i.e., the actual
#'   transitions)
#' @keywords internal
#'
get_matchings <- function(prefs) {

  ## Get the matchings

  matchings <- matchingMarkets::hri(
    s.prefs = prefs$student_reference_prefs,
    c.prefs = prefs$college_prediction_prefs
  )

  matchings <- matchings$matchings[
    matchings$matchings$sOptimal == 1,
    c("college", "student")
  ]

  ## Set/check formatting

  names(matchings) <- c("Prediction", "Reference")

  ## Use of characters (in newer matchingMarkets versions) causes problems with
  ## indexing, so need to test for presence of characters

  if (is.character(matchings$Prediction)) {
    matchings$Prediction <- as.numeric(matchings$Prediction)
  }

  if (is.character(matchings$Reference)) {
    matchings$Reference <- as.numeric(matchings$Reference)
  }

  matchings$Prediction_Index <-
    prefs$college_prediction_colnames[matchings$Prediction]
  matchings$Reference_Index <-
    prefs$student_reference_colnames[matchings$Reference]

  ## Newer versions of matchingMarkets use characters instead of integers, which
  ## messes up the ordering of `matchings`. It doesn't affect functionality, but
  ## reordering (i.e., the next step that also returns `matchings`) is good for
  ## assuaging paranoia

  data.frame(
    matchings[order(matchings$Prediction), ],
    row.names = NULL
  )

}
