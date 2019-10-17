context("Transition analyses")
library(PAutilities)

# Manual legacy test procedure --------------------------------------------

  ## get_transition_info
  # old <- readRDS("tests/testthat/transition_info.rds")
  # -------> Run the requisite code in `testthat::test_that` block
  # all.equal(...) <-- Syntax would need tweaking depending on the change
  # -------> Reset the cache once the right result has been achieved

  ## summary.transition
  # old <- readRDS("tests/testthat/transition_summary.rds")
  # ------> Run the requisite code in `testthat::test_that` block
  # new <- summary(info)
  # all.equal(old@result, new@result[ ,names(old@result)])
  # -------> Reset the cache once the right result has been achieved

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
