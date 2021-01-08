#' Internal helper functions for \code{\link{get_BMI_percentile}}
#'
#' @inheritParams get_BMI_percentile
#' @param reference reference subset for calculations
#' @param colname character. Column name of \code{reference} on which to operate
#' @param greater_index upper match index of \code{reference}
#' @param lesser_index lower match index of \code{reference}
#' @param increment proportional distance between
#'   \code{reference[lesser_index, ]} and \code{reference[greater_index, ]}
#' @param L lambda
#' @param M mu
#' @param S sigma
#' @param p percentile for which to back-calculate a BMI (must be between 0 and
#'   1)
#'
#' @seealso \href{https://pubmed.ncbi.nlm.nih.gov/24992748/}{Flegal and Cole (2013)}
#'
#' @name bmi_internal
#' @keywords internal
NULL

#' @rdname bmi_internal
#' @keywords internal
percentile_age <- function(age_mos, age_yrs) {

  if (is.null(age_mos)) {

    if (is.null(age_yrs)) stop(
      "`age_mos` is NULL, but `age_yrs` has not been provided"
    )

    age_mos <-
      {age_yrs * 365.2425} %>% # Convert to age in days
      {. / 30.4375} # Then to age in months

  }

  age_mos

}

#' @rdname bmi_internal
#' @keywords internal
percentile_BMI <- function(BMI, weight_kg, height_cm) {

  if (is.null(BMI)) {

    weight_kg / (height_cm ^ 2) * 10000

  } else {

    BMI

  }

}

#' @rdname get_BMI_percentile
#' @keywords internal
percentile_df <- function(
  df, weight_kg = "default", height_cm = "default", age_yrs = "default",
  age_mos = "default", sex = "default", BMI = "default",
  output = c("percentile", "classification", "both", "summary")
) {

  stopifnot(inherits(df, "data.frame"))

  args <-
    list(
      weight_kg = weight_kg, height_cm = height_cm,
      age_yrs = age_yrs, age_mos = age_mos,
      sex = sex, BMI = BMI
    ) %>%
    .[!sapply(., is.null)] %>%
    .[!sapply(., function(x) x == "default")] %T>%
    {stopifnot(all(
      sapply(., is.character)
    ))} %>%
    do.call(c, .) %>%
    unname(.) %T>%
    {stopifnot(all(. %in% names(df)))}

  output <- match.arg(output)

  nrow(df) %>%
  seq(.) %>%
  split(df[ ,args], .) %>%
  {lapply(
    .,
    function(x, output) {
      as.list(x) %>%
      c(output = list(output)) %>%
      do.call(get_BMI_percentile, .) %T>%
      {if (!is.list(.)) stopifnot(length(.) == 1)} %>%
      {if (is.list(.)) . else list(.)} %>%
      {if (is.null(names(.))) stats::setNames(., output) else .} %>%
      c(stringsAsFactors = FALSE) %>%
      do.call(data.frame, .)
    },
    output
  )} %>%
  do.call(rbind, .) %>%
  stats::setNames(., paste0("bmi_", names(.))) %>%
  stats::setNames(., gsub("^bmi_severe", "severe", names(.))) %>%
  stats::setNames(., gsub("^bmi_BMI$", "bmi_auto_kg_m2", names(.)))

}

#' @rdname bmi_internal
#' @keywords internal
percentile_index <- function(reference, age_mos) {
  {reference$Age <= age_mos} %>%
  which(.) %>%
  max(.)
}

#' @rdname bmi_internal
#' @keywords internal
percentile_lms <- function(
  reference, colname, lesser_index, greater_index, increment
) {

  lesser_proportion <-
    reference[lesser_index, colname] * (1 - increment)

  greater_proportion <-
    reference[greater_index, colname] * increment

  lesser_proportion + greater_proportion

}

#' @rdname bmi_internal
#' @keywords internal
percentile_reference <- function(sex) {

  {standards$Sex == sex} %>%
  standards[., ] %T>%
  {stopifnot(
    !any(duplicated(.$Age)),
    all(diff(order(.$Age)) == 1),
    nrow(.) > 0
  )}

}

#' @rdname bmi_internal
#' @keywords internal
percentile_sex <- function(sex = c("error", "male", "female")) {
  # Handling of match.arg is pretty ugly here. Goal is to prevent a default sex
  # from being silently selected, but I am sure that could be accomplished in a
  # much more elegant way. I just don't have the patience to set it up right
  # now.
  sex <- tolower(sex)
  sex <- match.arg(sex)
  if (!sex %in% c("male", "female")) stop(
    "Could not match sex to the available",
    " options (\"Male\" or \"Female\")"
  )
  switch(sex, "male" = "M", "female" = "F") # To match format of standards$Sex
}

#' @rdname bmi_internal
#' @keywords internal
percentile_z <- function(BMI, L, M, S) {
  {BMI/M} %>%
  {.^L} %>%
  {.-1} %>%
  {./(L*S)}
}

#' @rdname bmi_internal
#' @keywords internal
percentile_back_calculate <- function(p = 0.95, L, M, S) {
  p %T>%
  {stopifnot(. >= 0, . <= 1)} %>%
  stats::qnorm(.) %>%
  {. * L * S} %>%
  {. * (M ^ L)} %>%
  {. + (M ^ L)} %>%
  {. ^ (1/L)}
}
