#' Identify bouts of activity, allowing for interruptions
#'
#' @param x A vector of activity labels, e.g. minute-by-minute classifications
#'   of \code{sedentary behavior}, \code{light physical activity}, or
#'   \code{moderate-to-vigorous physical activity}
#' @param method character. The bout identification method to use. Currently,
#'   only one option is supported (\code{cluster-based}), but others may be
#'   added in the future
#' @param longest_allowable_interruption numeric. The number of consecutive
#'   out-of-class readings that must occur to satisfy the bout termination
#'   criteria
#' @param required_percent numeric (1-100). The minimum percentage of the full
#'   bout period that must be spent engaged in the target behavior. Stated
#'   differently, interruptions can compose no more than
#'   (100-\code{required_percent})\% of the bout.
#' @param max_n_interruptions numeric. The maximum number of interruption events
#'   that are allowed before a bout will be considered invalid.
#' @param target character. The value of \code{x} for which bout information is
#'   desired.
#' @param target_buffer numeric. Maximum separation between runs of
#'   \code{target}, beyond which they will not be clustered together
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
#' \donttest{
#' get_bouts(intensity, "cluster-based", "MVPA", 30, 5, 50, 3)
#' }
get_bouts <- function(
  x, method = "cluster-based", target, target_buffer,
  longest_allowable_interruption = Inf, required_percent = 100,
  max_n_interruptions = Inf
) {

  method <- match.arg(method)

  if (method == "cluster-based") {
    if (missing(target)) target <- NULL
    if (missing(target_buffer)) target_buffer <- NULL
    check_clusterBased(x, target, required_percent, target_buffer)
    if (is.null(target_buffer)) target_buffer <- Inf
  }

  switch(
    method,
    "cluster-based" = group_runs_clusterBased(
      x, target, target_buffer, required_percent,
      longest_allowable_interruption, max_n_interruptions
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
      n_total_events = nrow(df),
      n_value_events = sum(df$values == df$values[1]),
      n_interruption_events = sum(df$values != df$values[1]),

      length_total = sum(df$lengths),
      length_value = sum(
        ifelse(df$values == df$values[1], df$lengths, 0)
      ),
      length_interruption = sum(
        ifelse(df$values == df$values[1], 0, df$lengths)
      ),
      longest_interruption_event = max(
        ifelse(df$values == df$values[1], 0, df$lengths)
      )
    ) %>%
    within({percent_time_engaged = length_value / length_total * 100})
  }) %>%
  do.call(rbind, .)

}
