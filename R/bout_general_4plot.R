#' Visualize the result of a bout analysis
#'
#' @inheritParams plot.transition
#'
#' @return A \code{ggplot2} object that visualizes the bout analysis
#' @export
#'
#' @examples
#' data(ex_data, package = "PAutilities")
#' intensity <- as.character(get_intensity(ex_data$METs))
#' \donttest{
#'   bouts <- get_bouts(intensity, "CRIB", "MVPA", 30, 5, 50, 3)
#'   plot(bouts)
#' }
plot.bouts <- function(x, ...) {

  df <-
    bout_expand(x) %>%
    data.frame(x = seq(.), y = .)

  ggplot(df, aes(x, as.numeric(y))) +
  geom_line() +
  scale_y_continuous(
    "State", breaks = seq(levels(df$y)), labels = levels(df$y)
  ) +
  scale_x_continuous("Time (epochs)") +
  theme_classic() +
  theme(
    axis.line = element_line(size = 0.5),
    axis.title = element_text(face = "bold")
  )

}
