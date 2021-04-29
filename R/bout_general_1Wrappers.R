#' Identify bouts of activity, allowing for interruptions
#'
#' @param x A vector of activity labels, e.g. minute-by-minute classifications
#'   of \code{sedentary behavior}, \code{light physical activity}, or
#'   \code{moderate-to-vigorous physical activity}
#' @param method character. One of \code{sequential} (a traditional approach,
#'   the default) or \code{targeted} (focusing on the class indicated by
#'   \code{target})
#' @param longest_allowable_interruption numeric. The number of consecutive
#'   out-of-class readings that must occur to satisfy the bout termination
#'   criteria
#' @param required_percent numeric (1-100). The minimum percentage of time
#'   within the bout period that must be non-interrupted in order for the bout
#'   to qualify as valid. See details.
#' @param max_n_interruptions numeric. The maximum number of interruption events
#'   that are allowed before a bout will be considered invalid.
#' @param target character. The value of \code{x} for which bout information is
#'   desired (applicable only when \code{method = "targeted"})
#' @param target_buffer numeric. Maximum separation between runs of
#'   \code{target}, beyond which they will not be clustered together
#'
#' @details When \code{method = "sequential"}, only one of the arguments
#'   applies (\code{longest_allowable_interruption}). The other arguments
#'   are for \code{method = "targeted"}. However, the output from both
#'   settings will provide details about interruptions and percentage of
#'   time spent in the behavior, so additional restrictions can be applied
#'   by the user after running this function.
#'
#'   The sequential method runs much faster, and returns information about bouts
#'   of all behaviors in \code{x}. The targeted method runs more slowly
#'   (depending on how many state changes there are in \code{x}), and returns
#'   information only about the class specified by the setting of \code{target}.
#'
#'   The sequential method defines bouts for each state of \code{x}, and each of
#'   the states can register as an 'interruption' to the others. As such, the
#'   sequential method may not produce the same frequencies as \code{table(x)}
#'   when there are more than two possible classes, because within each bout all
#'   of the 'interruption' classes are grouped together. In contrast, the
#'   targeted method defines bouts only for the state identified by
#'   \code{target}, and for that state, the output will reflect the same total
#'   as \code{table(x)}.
#'
#' @note \code{x} must be an atomic vector. Factors should be cast to integer or
#'   character prior to passing into this function.
#'
#' @seealso \code{\link{bout_expand}}
#'
#' @return data frame with bout information
#' @export
#'
#' @examples
#' data(ex_data, package = "PAutilities")
#' intensity <- as.character(get_intensity(ex_data$METs))
#' get_bouts(intensity, "sequential", 5)
#' \donttest{
#' get_bouts(intensity, "targeted", 5, 50, 3, "MVPA", 30)
#' }
get_bouts <- function(
  x, method = c("sequential", "targeted"),
  longest_allowable_interruption = Inf, required_percent = 100,
  max_n_interruptions = Inf, target = NULL, target_buffer = NULL
) {

  method <- match.arg(method)

  if (method == "targeted") {
    check_targeted(x, target, required_percent, target_buffer)
    if (is.null(target_buffer)) target_buffer <- Inf
  }

  switch(
    method,
    "sequential" = group_runs_sequential(
      x, longest_allowable_interruption
    ),
    "targeted" = group_runs_targeted(
      x, target, required_percent,
      longest_allowable_interruption,
      max_n_interruptions, target_buffer
    ),
    NULL
  ) %>%
  within({group = NULL}) %>%
  structure(
    .,
    row.names = seq(nrow(.)),
    class = append(class(.), paste0("bout_", method)),
    input_length = length(x),
    longest_allowable_interruption = longest_allowable_interruption,
    required_percent = required_percent,
    max_n_interruptions = max_n_interruptions,
    target = target,
    target_buffer = target_buffer
  )

}

#' Bout analysis internal functions
#'
#' @inheritParams get_bouts
#'
#' @keywords internal
#'
#' @name bouts_internal
collapse_runs <- function(x) {

  split(x, x$group) %>%
  lapply(function(df) {
    data.frame(
      start_index = df$start_index[1],
      end_index = df$end_index[nrow(df)],
      group = unique(df$group),

      values = df$values[1],
      n_events = nrow(df),
      n_value_events = sum(df$values == df$values[1]),
      n_interruption_events = sum(df$values != df$values[1]),

      total_length = sum(df$lengths),
      value_length = sum(
        ifelse(df$values == df$values[1], df$lengths, 0)
      ),
      interruption_length = sum(
        ifelse(df$values == df$values[1], 0, df$lengths)
      ),
      longest_interruption_event = max(
        ifelse(df$values == df$values[1], 0, df$lengths)
      )
    ) %>%
    within({percent_time_engaged = value_length / total_length * 100})
  }) %>%
  do.call(rbind, .)

}
