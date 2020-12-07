# Global variables --------------------------------------------------------

  .sendroy <- structure(
    list(
      sex = c("male", "male", "male", "female", "female", "female"),
      left_break = c(-Inf, 0.1, 0.2,-Inf, 0.1, 0.2),
      b1 = c(57.26, 50.6, 60.2, 60.36, 51.1, 62.9),
      exponent = c(0.494, 0.436, 0.562, 0.507, 0.429, 0.578),
      intercept = c(0.254, 0, 0, 0.254, 0, 0)
    ),
    class = "data.frame", row.names = c(NA, -6L)
  )

# S3 functions ------------------------------------------------------------

#' Calculate body volume, surface area, or similar
#'
#' @param x object to operate on
#' @param method character. The method to use for calculations. Currently only
#'   one method is supported (see \code{\link{sendroy_vsa}})
#' @param ... arguments passed to methods
#'
#' @return The body volume, surface area, or similar desired output
#' @export
#'
#' @examples
#' df <- data.frame(
#'   sex = c("female", "male"),
#'   ht = c(138, 138),
#'   wt = c(57, 57)
#' )
#'
#' get_vsa(sex = "female", ht = 138, wt = 57)
#' get_vsa(sex = "male", ht = 138, wt = 57)
#'
#' get_vsa(df, sex = "sex", ht = "ht", wt = "wt")
get_vsa <- function(x = NULL, method = c("Sendroy1966"), ...) {

  if (is.null(x)) {
    x <- "default"
  }

  method <- match.arg(method)
  UseMethod("get_vsa", x)

}

#' @rdname get_vsa
#' @param sex character. For default method, one of \code{female} or
#'   \code{male}. For data frame method, a column name in \code{x} giving a
#'   vector with all elements in \code{c("female", "male")}
#' @param ht For default method, a numeric scalar giving height in centimeters.
#'   For data frame method, a column name in \code{x} giving a vector of heights
#'   in centimeters.
#' @param wt For default method, a numeric scalar giving weight in kilograms.
#'   For data frame method, a column name in \code{x} giving a vector of weights
#'   in kilograms
#' @export
get_vsa.default <- function(
  x = NULL, method = c("Sendroy1966"), sex, ht, wt, ...
) {

  switch(
    method,
    "Sendroy1966" = sendroy_vsa(sex, ht, wt),
    NA
  )

}

#' @rdname get_vsa
#' @export
get_vsa.data.frame <- function(
  x = NULL, method = c("Sendroy1966"), sex, ht, wt, ...
) {

  nrow(x) %>%
  seq(.) %>%
  split(x, .) %>%
  lapply(
    function(y, method, sex, ht, wt) {
      switch(
        method,
        "Sendroy1966" = sendroy_vsa(
          y[ ,sex], y[ ,ht], y[ ,wt]
        ),
        NA
      )
    },
    method = method, sex = sex, ht = ht, wt = wt
  ) %>%
  c(use.names = FALSE) %>%
  do.call(c, .)

}

# Main functions ----------------------------------------------------------

#' Calculate volume-to-surface-area ratio from height and weight
#'
#' @param sex character. The sex of the individual
#' @param ht numeric. The individual's height in centimeters
#' @param wt numeric. The individual's weight in kilograms
#'
#' @return The volume-to-surface-area ratio
#' @seealso \href{https://journals.physiology.org/doi/pdf/10.1152/jappl.1966.21.1.167}{Sendroy et al. (1966)}
#'
#' @keywords internal
sendroy_vsa <- function(sex = c("female", "male"), ht, wt) {

  sex <- match.arg(sex)

  rat <- wt/ht

  s <-
    .sendroy %>%
    .[.$sex == sex, "left_break"] %>%
    c(Inf) %>%
    cut(rat, ., right = FALSE) %>%
    as.numeric(.) %>%
    {. * match(sex, c("male", "female"))} %>%
    .sendroy[., ]

  ((rat^s$exponent)*s$b1)+s$intercept

}
