#' Calculate body mass index
#'
#' @param wt body mass
#' @param wt_unit character. Units of body mass (or desired units if
#'   \code{target} is passed. See below.)
#' @param ht height
#' @param ht_unit character. Units of height
#' @param target an optional target BMI (kg/m^2), for which the corresponding
#'   body mass will be returned
#'
#' @return If \code{target} is \code{NULL} (default), the function returns the
#'   body mass index. If \code{target} is provided, the function will use
#'   \code{target} and \code{ht} to back-calculate a target body mass in the
#'   units defined by \code{wt_unit} (kg by default).
#' @export
#'
#' @examples
#' get_bmi(160, "lb", 180, "cm")
#' get_bmi(ht = 5.90, ht_unit = "ft", target = 24, wt_unit = "st")
#' get_bmi(wt = c(60:79, NA), ht = c(NA, 160:179))
get_bmi <- function(
  wt, wt_unit = c("kg", "lb", "oz", "st"),
  ht, ht_unit = c("cm", "m", "in", "ft"),
  target = NULL
) {

  if (!is.null(target)) {

    target_wt(target, ht, ht_unit, wt_unit)

  } else {

    {get_wt_kg(wt, wt_unit) /
    get_ht_m2(ht, ht_unit)} %T>%
    bmi_check(.)

  }

}

#' @rdname get_bmi
#' @keywords internal
get_wt_kg <- function(wt, wt_unit = c("kg", "lb", "oz", "st")) {

  stopifnot(
    inherits(wt, c("integer", "numeric"))
  )

  wt_unit <- match.arg(wt_unit)

  switch(
    wt_unit,
    "kg" = wt,
    "lb" = wt * 0.453592,
    "oz" = wt * 0.0283495,
    "st" = wt * 6.35029,
    NA
  )

}

#' @rdname get_bmi
#' @keywords internal
get_ht_m2 <- function(ht, ht_unit = c("cm", "m", "in", "ft")) {

  stopifnot(
    inherits(ht, c("integer", "numeric"))
  )

  ht_unit <- match.arg(ht_unit)

  switch(
    ht_unit,
    "cm" = (ht/100)^2,
    "m" = ht^2,
    "in" = (ht*0.0254)^2,
    "ft" = (ht*0.3048)^2,
    NA
  )

}

#' @param bmi internal argument
#' @rdname get_bmi
#' @keywords internal
bmi_check <- function(bmi) {

  if (any(!is.na(bmi))) {

    if (any(stats::na.omit(bmi) < 10)) warning(
      "BMI < 10 provided. Recommend checking",
      " calculations/units.",
      call. = FALSE
    )

    if (any(stats::na.omit(bmi) > 80)) warning(
      "BMI > 80 provided. Recommend checking",
      " calculations/units.",
      call. = FALSE
    )

  }

}

#' @rdname get_bmi
#' @keywords internal
target_wt <- function(
  target, ht, ht_unit = c("cm", "m", "in", "ft"),
  wt_unit = c("kg", "lb", "oz", "st")
) {

  wt_unit <- match.arg(wt_unit)

  get_ht_m2(ht, ht_unit) %>%
  {. * target} %>%
  {switch(
    wt_unit,
    "kg" = . * 1,
    "lb" = . * 2.20462,
    "oz" = . * 35.2739199982575,
    "st" = . * 0.15747285713507810923,
    NA
  )} %>%
  stats::setNames(paste0("target_wt_", wt_unit))

}
