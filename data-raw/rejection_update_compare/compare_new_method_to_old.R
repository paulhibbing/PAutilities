rm(list = ls())
devtools::load_all()
set.seed(8)
predictions <- sample(c(0,1), 100, TRUE, c(3, 1))
references  <- sample(c(0,1), 100, TRUE, c(4,1))

info <- get_transition_info(predictions, references, 10)
info_old <- readRDS("data-raw/hri_version_compare/prefs_old.rds")

info$false_negative_indices <- info$false_negative_indices[
  !info$false_negative_indices %in% info$matchings$Reference_Index
]
info$false_positive_indices <- info$false_positive_indices[
  !info$false_positive_indices %in% info$matchings$Prediction_Index
]
info$matchings$rejected <- NULL
info$matchings$lag <- NULL
# > all.equal(info, info_old)
# [1] TRUE

info$matchings$lag <- 0
info$matchings$rejected <- FALSE
info_old$matchings$lag <- 0
info_old$matchings$rejected <- FALSE

# > all.equal(summary(info), summary(info_old))
# [1] TRUE
