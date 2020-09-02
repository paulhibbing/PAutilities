context("CVD risk assessment")
library(PAutilities)

testthat::test_that("cvd_risk produces expected output", {

  single <- cvd_risk(
    sex = "Female", age = 111, total_cholesterol = 111,
    systolic = 111, hdl = 11, bp_treated = FALSE,
    diabetes = TRUE, smoker = TRUE
  )

  testthat::expect_equal_to_reference(
    single, "cvd_single.rds"
  )

  multi <- structure(
    list(
      sex = structure(c(1L, 2L, 2L, 1L, 1L, 2L, 2L,
      2L, 2L, 1L, 1L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 1L, 2L, 1L, 1L, 2L,
      2L, 1L, 2L, 2L, 2L, 1L, 1L, 2L, 2L, 2L, 1L, 2L, 2L, 2L, 2L, 1L,
      2L, 2L, 2L, 2L, 2L, 1L, 2L, 2L, 1L, 2L, 1L),
      .Label = c("Female", "Male"), class = "factor"),
      age = c(66L, 88L, 81L, 90L, 96L, 45L, 50L, 45L, 58L, 36L, 87L, 68L, 67L, 91L, 60L, 52L, 78L, 55L,
      98L, 45L, 52L, 51L, 85L, 43L, 54L, 83L, 81L, 33L, 73L, 92L, 35L,
      40L, 38L, 43L, 39L, 74L, 93L, 86L, 89L, 33L, 33L, 87L, 48L, 63L,
      31L, 100L, 99L, 65L, 77L, 90L), tc = c(237L, 170L, 176L, 203L,
      260L, 294L, 171L, 263L, 279L, 196L, 293L, 226L, 229L, 150L, 161L,
      263L, 162L, 262L, 172L, 225L, 188L, 278L, 197L, 249L, 181L, 231L,
      243L, 151L, 295L, 287L, 252L, 155L, 237L, 178L, 161L, 203L, 240L,
      229L, 259L, 183L, 260L, 254L, 205L, 150L, 191L, 211L, 255L, 178L,
      169L, 162L), hdl = c(68L, 52L, 49L, 52L, 41L, 42L, 63L, 40L,
      64L, 41L, 59L, 41L, 34L, 37L, 41L, 35L, 68L, 37L, 47L, 48L, 42L,
      51L, 51L, 67L, 39L, 54L, 52L, 59L, 40L, 31L, 61L, 40L, 40L, 57L,
      46L, 42L, 37L, 61L, 43L, 64L, 41L, 65L, 41L, 52L, 42L, 37L, 58L,
      38L, 58L, 56L), sbp = c(102L, 151L, 169L, 136L, 164L, 162L, 100L,
      101L, 161L, 162L, 173L, 155L, 103L, 149L, 170L, 155L, 150L, 168L,
      168L, 165L, 135L, 119L, 142L, 170L, 149L, 122L, 158L, 163L, 178L,
      129L, 177L, 124L, 120L, 163L, 101L, 121L, 132L, 129L, 173L, 152L,
      157L, 131L, 166L, 157L, 104L, 152L, 160L, 152L, 145L, 126L),
      bpmed = c(FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, TRUE, TRUE,
      TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, TRUE, FALSE, FALSE,
      FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE,
      TRUE, FALSE, FALSE, TRUE, TRUE, FALSE, TRUE, TRUE, FALSE,
      FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, FALSE, TRUE, FALSE,
      TRUE, TRUE, FALSE, FALSE, FALSE, FALSE), diabetes = c(TRUE,
      FALSE, TRUE, TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, TRUE,
      FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, TRUE,
      FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE,
      TRUE, TRUE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE,
      TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, FALSE,
      FALSE, FALSE, TRUE, FALSE), smoker = c(FALSE, FALSE, FALSE,
      FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE,
      TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, FALSE,
      TRUE, TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE,
      FALSE, TRUE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, FALSE,
      TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, FALSE, FALSE
      )), class = "data.frame", row.names = c(NA, -50L))

  multi_noCombine <- cvd_risk(
    multi, sex = "sex", age = "age",
    total_cholesterol = "tc", hdl = "hdl",
    systolic = "sbp", bp_treated = "bpmed",
    diabetes = "diabetes", smoker = "smoker",
    combine = FALSE
  )

  testthat::expect_equal_to_reference(
    multi_noCombine, "cvd_multi_noCombine.rds"
  )

  multi_default <- cvd_risk(
    multi, sex = "sex", age = "age",
    total_cholesterol = "tc", hdl = "hdl",
    systolic = "sbp", bp_treated = "bpmed",
    diabetes = "diabetes", smoker = "smoker"
  )

  testthat::expect_equal_to_reference(
    multi_default, "cvd_multi_default.rds"
  )


})
