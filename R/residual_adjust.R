#' Perform residual adjustment on an epidemiologic variable
#'
#' @param d the input data frame on which to perform the adjustment
#' @param variable character. Name of variable needing adjustment
#' @param confounder character. Name of the confounder to adjust for
#' @param label character. Name to give the adjusted variable
#' @param verbose logical. Print updates to console?
#'
#' @return The original \code{d} object, with an extra column reflecting
#'   residual adjustments on the selected variable.
#'
#' @export
#'
#' @examples
#' d <- data.frame(
#'   VARIABLE = rnorm(100, 10, 2),
#'   CONFOUNDER = rnorm(100, 3, 7)
#' )
#' result <- residual_adjust(d, "VARIABLE", "CONFOUNDER", "ADJUSTED")
#'
#' head(d)
#' head(result)
residual_adjust <- function(d, variable, confounder, label, verbose = FALSE) {

  if (exists("zznewvariable", d)) stop(
    "`d` cannot have a variable called `zznewvariable`"
  )

  if (verbose) {

    cat("\n")
    paste(
      "Performing residual adjustment for", variable,
      "based on", confounder
    ) %>%
    print(.)
  }

  if (!exists(variable, d) | !exists(confounder, d)) {

    if (verbose) message(
      variable, " and/or ", confounder,
      " do not exist in `d`. Skipping residual adjustment."
    )

    d

  } else {

    paste0(variable, " ~ ", confounder) %>%
    stats::as.formula(.) %>%
    stats::lm(d) %>%
    {.$residuals + mean(.$fitted.values)} %>%
    {within(d, {zznewvariable = .})} %>%
    stats::setNames(
      ., gsub("^zznewvariable$", label, names(.))
    )

  }

}
