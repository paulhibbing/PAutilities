#' Perform Bland-Altman analysis on a data frame
#'
#' @param df the data frame on which to operate
#' @param x_var character. The column name of the X variable
#' @param y_var character. The column name of the Y variable (criterion measure,
#'   if applicable)
#' @param regress_against character. One of \code{"Y"} (to regress bias
#'   against \code{yvar}) or \code{"XY_mean"} (to regress bias against
#'   \code{rowMeans(x_var, y_var)}).
#' @param ... optional arguments passed to \code{data.frame}, e.g. to give the
#'   output results a label
#'
#' @return A data frame that has various summaries (means, standard deviations,
#'   and missing data details) plus mean bias (\code{mean_bias} column) and
#'   limits of agreement (\code{lower_LOA} and \code{upper_LOA} columns)
#' @export
#'
#' @examples
#' data(ex_data, package = "PAutilities")
#' ba_analysis(ex_data, "Axis1", "Vector.Magnitude", "XY_mean")
#' ba_analysis(
#'   ex_data, "Axis1", "Vector.Magnitude", "XY_mean",
#'   an_arbitrary_added_column = "Example of passing an extra column"
#' )
ba_analysis <- function(
  df, x_var, y_var, regress_against = c("Y", "XY_mean"), ...
) {

  stopifnot(
    inherits(df, "data.frame"),
    length(regress_against) == 1,
    regress_against %in% c("Y", "XY_mean")
  )

  df_complete <-
    c(x_var, y_var) %T>%
    {stopifnot(all(. %in% names(df)))} %>%
    df[ ,.] %>%
    .[stats::complete.cases(.), ] %>%
    stats::setNames(c("X", "Y")) %>%
    cbind(., XY_mean = rowMeans(.), bias = .$Y - .$X)

  model_results <-
    paste("bias", regress_against, sep = " ~ ") %>%
    stats::as.formula(.) %>%
    stats::lm(df_complete) %>%
    summary(.) %>%
    {data.frame(
      slope = .$coefficients[regress_against,"Estimate"],
      R2 = .$r.squared
    )}

  bias <-
    data.frame(
      mean_bias = mean(df_complete$bias),
      sd_bias = stats::sd(df_complete$bias)
    ) %>%
    within({
      lim = sd_bias * 1.96
      upper_LOA = mean_bias + lim
      lower_LOA = mean_bias - lim
      lim = NULL
    }) %>%
    within({
      LOA_width = upper_LOA - lower_LOA
    })

  df_complete %>%
  {data.frame(

    n_overall = nrow(df),
    n_x = sum(!is.na(df[ ,x_var])),
    n_y = sum(!is.na(df[ ,y_var])),
    n_complete_cases = nrow(.),

    x_var = x_var,
    y_var = y_var,

    x_mean_overall = mean(df[ ,x_var], na.rm = TRUE),
    x_mean_complete_cases = mean(.$X),

    y_mean_overall = mean(df[ ,y_var], na.rm = TRUE),
    y_mean_complete_cases = mean(.$Y),

    x_sd_overall = stats::sd(df[ ,x_var], na.rm = TRUE),
    x_sd_complete_cases = stats::sd(.$X),

    y_sd_overall = stats::sd(df[ ,y_var], na.rm = TRUE),
    y_sd_complete_cases = stats::sd(.$Y),

    bias,
    model_results,

    ...

  )}

}
