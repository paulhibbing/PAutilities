# global variables --------------------------------------------------------

  .age_breaks <- c(-Inf, seq(35, 75, 5), Inf)
  .tc_breaks  <- c(-Inf, 160, 200, 240, 280, Inf)
  .hdl_breaks <- c(-Inf, 35, 45, 50, 60, Inf)
  .sbp_breaks_male   <- c(-Inf, 120, 130, 140, 160, Inf)
  .sbp_breaks_female <- c(-Inf, 120, 130, 140, 150, 160, Inf)

# Generic and methods -----------------------------------------------------

#' Calculate risk of cardiovascular disease
#'
#' @param x optional data frame. If provided, the other arguments will be taken
#'   as column names under the assumption that each row represents a separate
#'   person, and each column provides one of the requested pieces of information
#' @param method character. Currently only \code{method = "D'Agostino_2008"} is
#'   supported.
#' @param sex character scalar indicating either sex for one person (i.e.,
#'   \code{male} or \code{female}), or a column name in \code{x} containing sex
#'   values for multiple people
#' @param age either a numeric scalar indicating age for one person, or a
#'   character scalar indicating the name of the column in \code{x} that
#'   contains age information. Units are years
#' @param total_cholesterol same as \code{age}, but for total cholesterol, in mg/dL
#' @param hdl same as \code{age}, but for HDL, in mg/DL
#' @param systolic same as \code{age}, but for systolic blood pressure, in mmHg
#' @param bp_treated either a logical scalar indicating whether a person is
#'   taking blood pressure medication, or a character scalar pointing to the
#'   column in \code{x} that contains the same information for multiple people
#' @param diabetes same as \code{bp_treated}, but for the presence of diabetes
#' @param smoker same as\code{bp_treated}, but for smoking status
#' @param ... arguments passed to other methods
#'
#' @return One or more risk profiles
#' @export
#'
#' @references \href{D'Agostino et al. (2008)}{http://doi.org/10.1161/CIRCULATIONAHA.107.699579}
#'
#' @examples
#'
#' cvd_risk(sex = "Female", age = 111, total_cholesterol = 111, systolic = 111,
#' hdl = 11, bp_treated = FALSE, diabetes = TRUE, smoker = TRUE)
#'
#' \donttest{
#' df <- data.frame(
#'   sex = sample(c("Male", "Female"), 5, TRUE),
#'   age = sample(30:100, 5, TRUE),
#'   tc = sample(150:300, 5, TRUE),
#'   hdl = sample(30:70, 5, TRUE),
#'   sbp = sample(100:180, 5, TRUE),
#'   bpmed = sample(c(TRUE, FALSE), 5, TRUE),
#'   diabetes = sample(c(TRUE, FALSE), 5, TRUE),
#'   smoker = sample(c(TRUE, FALSE), 5, TRUE)
#' )
#'
#' cvd_risk(
#'   df, sex = "sex", age = "age",
#'   total_cholesterol = "tc", hdl = "hdl",
#'   systolic = "sbp", bp_treated = "bpmed",
#'   diabetes = "diabetes", smoker = "smoker",
#'   combine = FALSE
#' )
#'
#' }
#'
cvd_risk <- function(
  x = NULL, method = "D'Agostino_2008", sex, age, total_cholesterol,
  hdl, systolic, bp_treated, diabetes, smoker, ...
) {

  if (method != "D'Agostino_2008") {

    warning(
      "`cvd_risk` does not currently support any methods",
      " other than D'Agostino 2008.\n  Results will be given ",
      "using the latter method.", call. = FALSE
    )

    method <- "D'Agostino_2008"

  }

  UseMethod("cvd_risk", x)

}

#' @rdname cvd_risk
#' @export
cvd_risk.default <- function(
  x = NULL, method = "D'Agostino_2008", sex, age,
  total_cholesterol, hdl, systolic, bp_treated,
  diabetes, smoker, ...
) {

  stopifnot(is.logical(c(bp_treated, diabetes, smoker)))

  sex <-
    match.arg(sex, c("Male", "Female", "Error")) %T>%
    {stopifnot(. %in% c("Male", "Female"))}

  DAgostino_wrapper(
    sex, age, total_cholesterol,
    hdl, systolic, bp_treated,
    diabetes, smoker
  )

}

#' @rdname cvd_risk
#' @param combine logical. Give results as a list of \code{risk_profile}
#'   objects, or combine the list into an integer vector (default)?
#' @export
cvd_risk.data.frame <- function(
  x = NULL, method = "D'Agostino_2008", sex, age,
  total_cholesterol, hdl, systolic, bp_treated,
  diabetes, smoker, combine = TRUE, ...
) {

  result <-
    c(
      sex, age, total_cholesterol, hdl, systolic,
      bp_treated, diabetes, smoker
    ) %>%
    x[ ,.] %>%
    as.list(.) %>%
    stats::setNames(c(
      "sex", "age", "total_cholesterol", "hdl", "systolic",
      "bp_treated", "diabetes", "smoker"
    )) %>%
    within({sex = as.character(sex)}) %>%
    {mapply(
      cvd_risk,
      sex = .$sex,
      age = .$age,
      total_cholesterol = .$total_cholesterol,
      hdl = .$hdl,
      systolic = .$systolic,
      bp_treated = .$bp_treated,
      diabetes = .$diabetes,
      smoker = .$smoker,
      MoreArgs = list(x = NULL),
      SIMPLIFY = FALSE,
      USE.NAMES = FALSE
    )}

  if (combine) {
    result %<>% do.call(c, .)
  }

  result

}

# Major functions ---------------------------------------------------------

#' Assess risk using the method of D'Agostino et al. (2008)
#' @rdname DAgostino
#' @inheritParams cvd_risk
#' @note Parameter descriptions have been inherited from \code{\link{cvd_risk}},
#'   but only direct scalar values (i.e., not column names) are allowed for this
#'   function
#' @references \href{D'Agostino et al. (2008)}{http://doi.org/10.1161/CIRCULATIONAHA.107.699579}
#' @keywords internal
DAgostino_wrapper <- function(
  sex, age, total_cholesterol,
  hdl, systolic, bp_treated,
  diabetes, smoker
) {

  sbp_points <-
    paste(sex, bp_treated) %>%
    switch(
      "Male FALSE"   = c(-2, 0:3),
      "Male TRUE"    = c(0, 2:5),
      "Female FALSE" = c(-3, 0:2, 4:5),
      "Female TRUE"  = c(-1, 2:3, 5:7)
    )

  if (sex == "Male") {

    risk_DAgostino(
      age, total_cholesterol, hdl, systolic,
      c(0, 2, 5:6, 8, 10, 11:12, 14:15),
      0:4, .sbp_breaks_male, sbp_points,
      diabetes, smoker
    )

  } else {

    risk_DAgostino(
      age, total_cholesterol, hdl, systolic,
      c(0, 2, 4:5, 7:12),
      c(0:1, 3:5), .sbp_breaks_female, sbp_points,
      diabetes, smoker
    )

  }


}

#' @rdname DAgostino
#' @param age_points numeric vector giving the risk values assigned to each age
#'   group. Used as a lookup reference for \code{x}
#' @param tc_points same as \code{age_points} but for total cholesterol
#' @param sbp_breaks numeric vector specifying which blood pressure levels to cut at
#' @param sbp_points same as \code{age_points} but for systolic blood pressure
risk_DAgostino <- function(
  age, total_cholesterol, hdl, systolic,
  age_points, tc_points, sbp_breaks,
  sbp_points, diabetes, smoker
) {

  age %<>% get_risk(.age_breaks, age_points)

  total_cholesterol %<>% get_risk(.tc_breaks, tc_points)

  hdl %<>% get_risk(.hdl_breaks, 2:-2)

  systolic %<>% get_risk(sbp_breaks, sbp_points)

  diabetes %<>%
    {. * 3} %>%
    as.integer(.)

  smoker %<>%
    {. * 4} %>%
    as.integer(.)

  all <- c(
    age, total_cholesterol, hdl,
    systolic, diabetes, smoker
  )

  lab <- paste(
    c("age", "tc", "hdl", "sbp", "diabetes", "smoker"),
    all, sep = ": ", collapse = " -- "
  )

  sum(all) %>%
  structure(class = "risk_profile", label = lab)

}

#' @rdname DAgostino
#' @param x a value on which to lookup a risk score
#' @param breaks passed to \code{base::cut}
#' @param values numeric vector giving the risk values assigned to each interval
#'   in \code{breaks}
get_risk <- function(x, breaks, values) {

  cut(x, breaks, as.character(values), right = FALSE) %>%
  as.character(.) %>%
  as.numeric(.) %T>%
  {stopifnot(. %% 1 == 0)} %>%
  as.integer(.)

}
