#' Calculate youth BMI percentile from CDC standards
#'
#' @param weight_kg Weight in kilograms
#' @param height_cm height in centimeters
#' @param age_yrs age in years
#' @param age_mos age in months (optional)
#' @param sex Character scalar indicating participant's sex
#' @param BMI numeric. Body mass index
#' @param output What should be returned: raw percentile, weight status
#'   classification, both percentile and classification, or a full summary (BMI,
#'   percentile, classification, and severe obesity cutoff)?
#'
#' @return One of: A numeric scalar giving the BMI percentile (for \code{output
#'   = "percentile"}); a factor scalar giving the weight status (for
#'   \code{output = "classification"}); a list with the percentile and
#'   classification (for \code{output = "both"}); or a list with the BMI,
#'   percentile, classification, and severe obesity cutoff (for \code{output =
#'   "summary"}).
#' @export
#'
#' @details Only one of \code{age_mos} and \code{age_yrs} is required. The
#'   former will be used if both are provided. If \code{age_mos} is \emph{not}
#'   provided, it will be calculated based on \code{age_yrs}, assuming 365.2425
#'   days per year and 30.4375 days per month. Depending on how the initial age
#'   calculation was made, rounding error will occur. Thus, use of the
#'   \code{\link{get_age}} function is recommended, with \code{units =
#'   "months"}. If \code{BMI} is provided, there is no need to pass
#'   \code{weight_kg} or \code{height_cm}.
#'
#' @references
#' This function was developed with reference to public domain resources
#' provided by the Centers for Disease Control and Prevention. For more
#' information, see:
#'
#' \url{https://www.cdc.gov/obesity/childhood/defining.html}
#'
#' \url{https://www.cdc.gov/healthyweight/assessing/bmi/childrens_bmi/tool_for_schools.html}
#'
#' @seealso
#'
#' \href{https://www.ahajournals.org/doi/10.1161/CIR.0b013e3182a5cfb3}{Kelly et al. (2013)}
#' \href{https://academic.oup.com/ajcn/article/90/5/1314/4598130}{Flegal et al. (2009)}
#'
#' @examples
#' get_BMI_percentile(39.4, 144.5, 12.35, sex = "Male")
get_BMI_percentile <- function(
  weight_kg, height_cm, age_yrs = NULL,
  age_mos = NULL, sex = c("Male", "Female"), BMI = NULL,
  output = c("percentile", "classification", "both", "summary")
) {

  ## Format variables

    sex %<>% percentile_sex(.)

    reference <- percentile_reference(sex)

    age_mos %<>% percentile_age(age_yrs)

    BMI %<>% percentile_BMI(weight_kg, height_cm)

  ## Prepare for calculations

    increment <-
      floor(age_mos + 0.5) %>%
      {age_mos - . + 0.5}

    greater_index <- percentile_index(reference, age_mos + 1)

    lesser_index <- percentile_index(reference, age_mos)

  ## Get Z score and percentile

    info <- mapply(
      percentile_lms,
      colname = c("L", "M", "S"),
      MoreArgs = list(
        reference = reference,
        lesser_index = lesser_index,
        greater_index = greater_index,
        increment = increment
      ),
      SIMPLIFY = FALSE
    )

    z_score <-
      c(info, BMI = list(BMI)) %>%
      do.call(percentile_z, .)

    percentile <-
      {stats::pnorm(z_score) * 1000} %>%
      floor(.) %>%
      {. / 10} %>%
      unname(.)

    bmi_severe <-
      do.call(percentile_back_calculate, info) %>%
      {. * 1.2}

  ## Get classification

    classification <-
      cut(
        percentile,
        c(-Inf, 5, 85, 95, Inf),
        c("Underweight", "Normal Weight", "Overweight", "Obese"),
        right = FALSE
      ) %>%
      as.character(.) %>%
      ifelse(BMI >= bmi_severe, "Severe_Obese", .) %>%
      factor(c(
        "Underweight", "Normal Weight", "Overweight", "Obese", "Severe_Obese"
      ))

  ## Finish up

    output <- match.arg(output)

    switch(
      output,
      "percentile" = percentile,
      "classification" = classification,
      "both" = stats::setNames(
        list(percentile, classification),
        c("percentile", "classification")
      ),
      "summary" = stats::setNames(
        list(BMI, percentile, classification, bmi_severe),
        c("BMI", "percentile", "classification", "severe_obesity_bmi_cutoff")
      ),
      NULL
    )

}
