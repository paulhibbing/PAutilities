#' Calculate youth BMI percentile from CDC 2000 standards
#'
#' @param weight_kg Weight in kilograms
#' @param height_cm height in centimeters
#' @param age_yrs age in years
#' @param age_mos age in months (optional)
#' @param sex Character scalar indicating participant's sex
#'
#' @return A numeric scalar giving the BMI percentile
#' @export
#'
#' @details If \code{age_mos} is *not* provided, it will be calculated based on
#'   \code{age_yrs}, assuming 365.2425 days per year and 30.4375 days per month.
#'   Depending on how the initial age calculation was made, rounding error will
#'   occur. Thus, use of the \code{\link{get_age}} function is recommended. If
#'   \code{age_mos} *is* provided, \code{age_yrs} can be passed as \code{NULL}.
#'
#' @references
#' https://www.cdc.gov/obesity/childhood/defining.html
#'
#' https://www.cdc.gov/healthyweight/downloads/BMI_group_calculator_English.xls
#'
#' @examples
#' get_BMI_percentile(39.4, 144.5, 12.35, sex = "M")
get_BMI_percentile <- function(
  weight_kg, height_cm, age_yrs,
  age_mos = NULL, sex = c("M", "F")
) {

  sex <- match.arg(sex, c("M", "F", "Error"), several.ok = FALSE)
  stopifnot(sex %in% c("M", "F"))

  reference <- standards[standards$Sex == sex, ]
  stopifnot(
    !any(duplicated(reference$Age)),
    all(diff(order(reference$Age)) == 1)
  )

  if (is.null(age_mos)) {
    daysold <- age_yrs * 365.2425
    age_mos <- daysold / 30.4375
  }

  BMI <- weight_kg / (height_cm ^ 2) * 10000

  increment <- age_mos - floor(age_mos + 0.5) + 0.5

  greater_index <- max(
    which(reference$Age <= (age_mos + 1))
  )

  lesser_index <- max(
    which(reference$Age <= (age_mos))
  )

  L <- (increment * reference$L[greater_index]) +
    ((1-increment)* reference$L[lesser_index])
  M <- (increment * reference$M[greater_index]) +
    ((1-increment)* reference$M[lesser_index])
  S <- (increment * reference$S[greater_index]) +
    ((1-increment)* reference$S[lesser_index])

  Z_score <- (((BMI/M)^L)-1)/(L*S)

  floor(stats::pnorm(Z_score) * 1000) / 10

}
