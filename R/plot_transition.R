#' Plot the transitions and matchings from a \code{transition} object
#'
#' @param x the object to print
#' @param ... further methods passed to or from methods, currently unused
#'
#' @return A plot of the predicted and actual transitions in a \code{transition}
#'   object, as well as the matchings between them
#' @export
#'
#' @examples
#' predictions <- sample(c(0,1), 100, TRUE, c(3, 1))
#' references  <- sample(c(0,1), 100, TRUE, c(4,1))
#' transitions <- get_transition_info(predictions, references, 10)
#' plot(transitions)
plot.transition <- function(x, ...) {
  x$college_prediction <-
    unname(sapply(as.character(x$college_prediction), function(x)
      switch(x, "0" = 3, "1" = 2)))

  graphics::plot(seq(length(x$student_reference)),
    x$student_reference,
    # "l",
    pch = 16,
    ylab = "",
    yaxt = "n",
    ann = FALSE,
    xlab = "Reference",
    ylim = c(0.5,2.5)
  )

  graphics::mtext("Reference", 1, 3)

  graphics::axis(3)
  graphics::mtext("Prediction", line = 3)

  graphics::points(
    seq(length(x$college_prediction)), x$college_prediction
  )

  sapply(seq(nrow(x$matchings)), function(i) {
    graphics::lines(
      c(x$matchings$Reference_Index[i], x$matchings$Prediction_Index[i]),
      c(1,2),
      col = "blue"
    )}
  )

  invisible()
}
