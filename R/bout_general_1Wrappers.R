#' Identify bouts of activity, allowing for interruptions
#'
#' @param x A vector of activity labels, e.g. minute-by-minute classifications
#'   of \code{sedentary behavior}, \code{light physical activity}, or
#'   \code{moderate-to-vigorous physical activity}. Must be an atomic vector.
#'   Factors should be cast to integer or character prior to passing into
#'   this function.
#' @param method character. The bout identification method to use. Currently,
#'   only one option is supported (\code{cluster-based}), but others may be
#'   added in the future
#' @param target Value of \code{x} for which bout information is desired
#' @param target_buffer numeric. Maximum separation between runs of
#'   \code{target}, beyond which they will not be clustered together
#' @param longest_allowable_interruption numeric. The maximum length for any
#'   single interruption in a valid bout
#' @param required_percent numeric (1-100). The minimum percentage of the full
#'   bout period that must be spent engaging in the target behavior. Stated
#'   differently, this threshold stipulates that interruptions can compose no
#'   more than \code{100-required_percent} of the bout
#' @param max_n_interruptions numeric. The maximum number of interruption events
#'   that are allowed before a bout will be considered invalid
#' @param minimum_bout_length numeric filtering criterion. Bouts will be
#'   discarded if \code{length_value} (see below) is less than this amount.
#'
#' @return data frame with bout information (one row per bout). The 11 columns
#'   are formatted as follows:
#' \describe{
#'   \item{start_index}{The start index of the bout period}
#'   \item{end_index}{The end index of the bout period}
#'   \item{values}{The target behavior (the name is a vestige of
#'   \code{\link{index_runs}})}
#'   \item{n_total_events}{The number of distinct behavior runs in the
#'   bout period. Equal to the sum of \code{n_value_events}
#'   and \code{n_interruption_events}}
#'   \item{n_value_events}{The number of distinct occurrences of the
#'   target behavior}
#'   \item{n_interruption events}{The number of distinct occurrences
#'   of interruptive behavior}
#'   \item{length_total}{The total number of indices comprising the
#'   bout period}
#'   \item{length_value}{The number of indices spent engaged in the
#'   target behavior}
#'   \item{length_interruption}{The number of indices spent engaged
#'   in interruptive behavior}
#'   \item{longest_interruption_event}{The number of indices
#'   comprising the longest interruption event}
#'   \item{percent_time_engaged}{The percentage (0-100) of \code{length_total}
#'   that was spent engaging in \code{target}, equal to
#'   \code{length_value / length_total * 100}}
#' }
#'
#'
#' @note Users should note that the function (input, code, and output) operates
#'   by index, not duration. That is, the function cannot tell if each data point
#'   represents a 1-s period, a 1-min period, or anything else. Users need to
#'   take this into consideration when deciding which settings to use (e.g.
#'   \code{longest_allowable_interruption = 12} to allow for 1-min interruptions
#'   if input data are in 5-s epochs) and how to interpret the output (e.g.
#'   \code{length_value == 12} corresponds to one minute if data are in 5-s
#'   epochs).
#'
#' @seealso \code{\link{bout_expand}}
#'
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
  max_n_interruptions = Inf, minimum_bout_length = 0
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
  .[.$length_value >= minimum_bout_length, ] %>%
  check_no_bouts(x, target) %>%
  structure(
    .,
    row.names = seq(nrow(.)),
    class = append(
      class(.),
      gsub("[-]+", "_", paste0("bout_", method))
    ),
    input_length = length(x),
    longest_allowable_interruption = longest_allowable_interruption,
    required_percent = required_percent,
    max_n_interruptions = max_n_interruptions,
    minimum_bout_length = minimum_bout_length,
    target = target,
    target_buffer = target_buffer,
    x = x
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
