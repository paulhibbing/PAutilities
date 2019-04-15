#' Calculate resting metabolic rate using a sliding window method
#'
#' @param vo2_values numeric vector of oxygen consumption values
#' @param vo2_timestamps timestamps corresponding to each element of
#'   \code{vo2_values}
#' @param start_time the beginning time of the assessment period
#' @param stop_time the ending time of the assessment period
#' @param window_size_minutes the size of the sliding window, in minutes
#'
#' @return A data frame giving the oxygen consumption from the lowest window, as
#'   well as the time difference from first to last breath in the same window.
#' @export
#'
#' @examples
rmr_sliding <- function(vo2_values, vo2_timestamps, start_time, stop_time, window_size_minutes = 5) {

  stopifnot(length(vo2_values) == length(vo2_timestamps))
  window_size_sec <- window_size_minutes * 60

  ## Identify sliding window end times
  Time <- vo2_timestamps[
    vo2_timestamps >= (vo2_timestamps[1] + window_size_sec)
    ]
  stopifnot(length(Time) != 0)

  ## Create an interval for each end time
  intervals <- sapply(
    Time,
    function(x) lubridate::interval(x - window_size_sec, x),
    simplify = FALSE
  )

  ## Get a calculation for each interval
  window_vo2 <- lapply(
    intervals,
    function(y) {
      # y <- intervals[[1]]
      indices <- lubridate::`%within%`(vo2_timestamps, y)
      indices <- indices & (
        vo2_timestamps != lubridate::int_start(y)
      )
      z_time <- vo2_timestamps[indices]
      z_vals <- vo2_values[indices]
      data.frame(
        range_mins_breaths = as.numeric(difftime(
          z_time[length(z_time)], z_time[1]
        )),
        rmr_window_mlmin = mean(z_vals, na.rm = TRUE))
    }
  )

  window_vo2 <- do.call(rbind, window_vo2)

  window_vo2[which.min(window_vo2$rmr_window_mlmin),
    rev(seq(ncol(window_vo2)))]

}
