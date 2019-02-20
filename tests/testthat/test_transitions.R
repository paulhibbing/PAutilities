context("Transition analyses")
library(PAutilities)

testthat::test_that("Transition analyses produce expected output", {
  set.seed(8)
  predictions <- sample(c(0,1), 100, TRUE, c(3, 1))
  references  <- sample(c(0,1), 100, TRUE, c(4,1))

  info <- get_transition_info(predictions, references, 10)

  testthat::expect_equal_to_reference(
    info,
    "transition_info.rds"
  )
  testthat::expect_equal_to_reference(
    summary(info),
    "transition_summary.rds"
  )
})
