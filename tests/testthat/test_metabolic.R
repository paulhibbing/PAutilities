context("Metabolic equations")
library(PAutilities)

testthat::test_that("BMR equations behave as expected", {

  ## Variables
  Sex <- "M"
  Age <- get_age(
    as.Date("1990-01-01"), as.Date("2020-02-19"), "years"
  )
  Ht <- 2
  Wt <- 73

  ## Calculations
  test1 <- get_bmr(Sex, Ht, Wt, Age)

  test2 <- get_bmr(Sex, Ht, Wt, Age, RER = 0.84)

  test3 <- get_bmr(
    Sex, Ht, Wt, Age, RER = 0.865, MJ_conversion = c("all")
  )

  test4 <- get_bmr(
    Sex, Ht, Wt, Age, MJ_conversion = c("all")
  )

  test5 <- get_bmr(
    Sex, Ht, Wt, Age,
    method = "FAO", kcal_conversion = 4.86
  )

  test6 <- get_bmr(
    Sex, Ht, Wt, Age,
    method = "FAO", kcal_conversion = 5
  )

  test7 <- get_bmr(
    Sex, Ht, Wt, Age,
    method = "Schofield", RER = 0.8,
    kcal_table = "Peronnet"
  )

  test8 <- get_bmr(
    Sex, Ht, Wt, Age,
    equation = "ht_wt",
    method = "Schofield", RER = 0.8,
    kcal_table = "Peronnet"
  )

  ## Tests
  testthat::expect_equal_to_reference(
    list(
      test1, test2, test3, test4,
      test5, test6, test7, test8
    ),
    "BMR_calculations.rds"
  )

  testthat::expect_false(
    test5$basal_VO2_mlkgmin == test6$basal_VO2_mlkgmin
  )

})

testthat::test_that("REE is calculated correctly", {

  testthat::expect_equal_to_reference(
    ree_single(
      "female", 20, "harris_benedict", 2, c("", ""),
      ht_cm = 150, wt_kg = 55
    ), "ree_single.rds"
  )

  sex <- c(
    "female", "male", "male", "female", "female", "female",
    "male", "female", "male", "female", "female", "male",
    "female", "female", "male", "male", "male", "male",
    "female", "female", "female", "female", "female",
    "female", "female", "female", "female", "male",
    "female", "male", "female", "male", "female",
    "female", "male", "male", "female", "female",
    "male", "female", "male", "male", "female",
    "male", "female", "male", "male", "female",
    "male", "female"
  )

  age_yr <- c(
    50L, 33L, 49L, 26L, 41L, 40L, 24L, 46L, 24L, 43L, 37L,
    42L, 49L, 25L, 25L, 38L, 50L, 50L, 43L, 27L, 36L, 39L,
    46L, 42L, 49L, 27L, 40L, 50L, 32L, 49L, 30L, 45L, 31L,
    32L, 35L, 38L, 48L, 39L, 36L, 33L, 30L, 46L, 30L, 28L,
    49L, 29L, 42L, 34L, 26L, 29L
  )

  ht_cm <- c(
    153.899429098862, 156.449531109798, 138.683132331324, 169.978686097473,
    135.499750119466, 147.391361884656, 179.862994949995, 142.358393569362,
    153.897425574191, 144.019054645969, 163.629598400022, 174.945291318321,
    148.133350061667, 157.049564969358, 117.107542809753, 161.555181526137,
    139.31168791624, 146.357610089821, 160.736403423533, 143.999759856612,
    176.156777154755, 156.361829696808, 133.583469271787, 147.89802358324,
    120.710838680354, 165.488303924418, 161.13319839405, 163.188132893329,
    146.313192902691, 133.810713048932, 159.06819824761, 167.795429574508,
    134.741870973543, 152.936027095195, 151.323816682635, 164.664135505807,
    126.883959952987, 167.897507991043, 160.864621056094, 173.132747062575,
    154.360637698933, 151.251042317779, 144.496572884847, 160.382629833889,
    169.313805942982, 162.784649017849, 155.527278677847, 160.304413009994,
    157.060268643773, 179.832004386151
  )

  wt_kg <- c(
    54.0525460176776, 58.7908728543728, 54.4849432836005, 59.169741566184,
    55.8560161220426, 51.8274945010032, 50.9727819487109, 61.5436042236631,
    59.9390647855398, 40.2005118224386, 57.1508420846847, 58.7658525630338,
    63.0587067708559, 49.3383134476241, 55.8484985455866, 55.8046432086778,
    54.5724318460451, 64.6913026078116, 46.9294390800997, 57.260712965969,
    44.9565695333445, 53.4492717361328, 57.2699406740885, 54.8488955665347,
    48.1733931045991, 55.4547327294623, 53.3361998979177, 54.7638711967502,
    55.7141524410229, 46.0219440790679, 60.4110023074338, 57.6072825585234,
    46.1498908821021, 54.777135967868, 52.042763007325, 50.433052237351,
    55.2599591641094, 53.8006933787892, 46.2911153826443, 45.3938686068436,
    59.4755003627954, 64.5675962465389, 53.7637440701423, 48.5844142910398,
    58.0060105850734, 46.0109940252522, 42.8624826314596, 53.4926080236247,
    52.4297344226973, 59.9817823140877
  )

  df <- data.frame(
    sex = sex,
    age_yr = age_yr,
    ht_cm = ht_cm,
    wt_kg = wt_kg,
    other_var = NA_integer_,
    other_other = NA_character_,
    stringsAsFactors = FALSE
  )

  result_default <- get_ree(
    "harris_benedict", sex, age_yr,
    ht_cm = ht_cm, wt_kg = wt_kg
  )

  result_df <- get_ree(
    "harris_benedict", "sex", "age_yr",
    ht_cm = "ht_cm", wt_kg = "wt_kg", df = df
  )

  testthat::expect_equal_to_reference(result_default, "ree_default.rds")
  testthat::expect_equal_to_reference(result_df, "ree_df.rds")
  testthat::expect_identical(result_default, result_df, "ree_identical.rds")

})
