#' Create a Bland-Altman plot
#'
#' @param plotdata dataframe from which to build the plot
#' @param x_var character expression to evaluate for the x-axis
#' @param y_var character expression to evaluate for the y-axis
#' @param x_name axis label for the x-axis
#' @param y_name axis label for the y-axis
#' @param ... further arguments passed to \code{theme}
#'
#' @return a Bland-Altman plot
#' @export
#'
#' @references Bland, J. M., & Altman, D. G. (1986). Statistical methods for
#'   assessing agreement between two methods of clinical measurement. lancet,
#'   1(8476), 307-310.
#'
#' @examples
#' data(total_resting, package = "Rcomps")
#' ba_plot(total_resting, "(rmr_mlkgmin + schofield_basal_VO2_mlkgmin) / 2",
#' "rmr_mlkgmin - schofield_basal_VO2_mlkgmin", "mean(RMR, BMR)", "RMR - BMR")
ba_plot <- function(plotdata, x_var, y_var, x_name, y_name, ...) {

  ggplot(plotdata, aes_string(x = x_var, y = y_var)) +
  geom_point() + theme_classic() +
  theme(axis.line = element_line(size = .5)) +
  scale_y_continuous(
    name = y_name
  ) +
  scale_x_continuous(
    name = x_name
  ) +
  geom_hline(
    yintercept =
      lazyeval::f_eval(
        ~mean(eval(parse(text = y_var))),
        data = plotdata
      ),
    size = 1.2
  ) +
  geom_smooth(
    method = 'lm', se = F, colour = 'black'
  ) +
  geom_hline(
    aes(
      yintercept =
        lazyeval::f_eval(
          ~mean(eval(parse(text = y_var)), na.rm = T) +
            (1.96*stats::sd(eval(parse(text = y_var)), na.rm = T)),
          data = plotdata)
    ), size = 1.3, linetype = 'dashed'
  ) +
  geom_hline(
    aes(
      yintercept =
        lazyeval::f_eval(
          ~mean(eval(parse(text = y_var)), na.rm = T) -
            (1.96*stats::sd(eval(parse(text = y_var)), na.rm = T)),
          data = plotdata
        )
    ), size = 1.3, linetype = 'dashed'
  ) +
  theme(
    axis.title = element_text(size = 14, face = 'bold'),
    axis.text = element_text(size = 12)
  ) +
  theme(...)
}
