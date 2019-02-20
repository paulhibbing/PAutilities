rm(list = ls())
devtools::load_all()
source("data-raw/hri_version_compare/hri_0_3_6.R")

set.seed(8)
predictions <- sample(c(0,1), 100, TRUE, c(3, 1))
references  <- sample(c(0,1), 100, TRUE, c(4,1))
window_size <- 10


# From get_transition_info ------------------------------------------------

## (up through get_preferences, no calls to matchingMarkets are made)

stopifnot(
  length(predictions) == length(references),
  all(predictions %in% c(0, 1)),
  all(references %in% c(0,1))
)

prefs <- get_preferences(predictions, references, window_size)

# From get_matchings ------------------------------------------------------
library(rJava)
matchings <- hri( ##NOTE: This is where we're calling LOCAL hri (0.3-6)
  s.prefs = prefs$student_reference_prefs,
  c.prefs = prefs$college_prediction_prefs
)

matchings <- matchings$matchings[
  matchings$matchings$sOptimal == 1,
  c("college", "student")
  ]

## Set/check formatting

names(matchings) <- c("Prediction", "Reference")

is.character(matchings$Prediction) ##FYI
if (is.character(matchings$Prediction)) {
  matchings$Prediction <- as.numeric(matchings$Prediction)
}

is.character(matchings$Reference) ##FYI
if (is.character(matchings$Reference)) {
  matchings$Reference <- as.numeric(matchings$Reference)
}

matchings$Prediction_Index <-
  prefs$college_prediction_colnames[matchings$Prediction]
matchings$Reference_Index <-
  prefs$student_reference_colnames[matchings$Reference]

# Back to get_transition_info ---------------------------------------------

prefs$matchings <- matchings

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

saveRDS(prefs, file = "data-raw/hri_version_compare/prefs_old.rds")
