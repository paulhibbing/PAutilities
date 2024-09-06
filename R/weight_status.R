#' Determine weight status from body mass index
#'
#' Allows users to determine weight status from body mass index (BMI). The
#' function is designed to classify adult weight status, with default settings
#' yielding weight classes defined by the Centers for Disease Control and
#' Prevention (see reference below). Alternatively, the function can be used as
#' a wrapper for \code{\link{get_BMI_percentile}} to obtain classifications for
#' youth.
#'
#' @usage
#'
#' weight_status(BMI = NULL, breaks = c(-Inf, 18.5, 25, 30, 35, 40, Inf),
#'   labels = c("Underweight", "Healthy Weight", "Overweight", "Class 1 Obese",
#'   "Class 2 Obese", "Class 3 Obese"), right = FALSE, youth = FALSE, ...)
#'
#' #get_BMI_percentile(weight_kg, height_cm, age_yrs = NULL, age_mos = NULL,
#'   #sex = c("Male", "Female"), BMI = NULL, df = NULL,
#'   #output = c("percentile", "classification", "both", "summary"))
#'
#' @param BMI numeric. The participant body mass index
#' @param breaks numeric vector. The boundaries for each weight class; passed to
#'   \code{base::cut}, with warnings if \code{-Inf} and \code{Inf} are not
#'   included in the vector.
#' @param labels character vector. The labels for each weight class; passed to
#'   \code{base::cut}, and should have a length one less than the length of
#'   \code{breaks}
#' @param right logical. See \code{?base::cut}
#' @param youth logical. Use function as a wrapper for
#'   \code{\link{get_BMI_percentile}}?
#' @param ... Arguments passed to \code{\link{get_BMI_percentile}}
#'
#' @return a factor reflecting weight status
#' @export
#'
#' @references
#' \url{https://www.cdc.gov/bmi/adult-calculator/bmi-categories.html}
#'
#' @examples
#' weight_status(17:42)
weight_status <- function(
  BMI = NULL,
  breaks = c(-Inf, 18.5, 25, 30, 35, 40, Inf),
  labels = c(
    "Underweight", "Healthy Weight", "Overweight",
    "Class 1 Obese", "Class 2 Obese", "Class 3 Obese"
  ),
  right = FALSE,
  youth = FALSE,
  ...
) {

  ## Control flow for wrapper case

    if (is.null(BMI) & !youth) stop(
      "Cannot classify weight status for NULL BMI. Did you mean",
      " to set `youth = TRUE`?",
      call. = FALSE
    )

    if (youth) {

      return(
        get_BMI_percentile(BMI = BMI, ...)
      )

    }

  ## Check input formatting

    if (!inherits(BMI, c("numeric", "integer"))) stop(
      "BMI must be numeric"
    )

    if (!-Inf %in% breaks) warning(
      "First element of `breaks` should probably be `-Inf`",
      call. = FALSE
    )

    if (!Inf %in% breaks) warning(
      "Last element of `breaks` should probably be `Inf`",
      call. = FALSE
    )

    bmi_check(BMI)

  ## Execute the cut

    cut(BMI, breaks, labels, right = right)

}
