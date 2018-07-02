#' Process data from ActiGraph monitors
#'
#' @inheritParams process_wearable
#' @param Count_Args optional. named vector of arguments to be passed to
#'   \code{\link[AGread]{read_AG_counts}}
#' @param RAW_Args optional. named vector of arguments to be passed to
#'   \code{\link[AGread]{read_AG_raw}}
#' @param IMU_Args optional. named vector of arguments to be passed to
#'   \code{\link[AGread]{read_AG_IMU}}
#'
#' @rdname process_wearable
#'
#' @note Processing is currently only supported for files in .csv format.
#'
#' @return A data frame of processed data from the specified ActiGraph file(s)
#' @export
#'
#' @examples
#' \dontrun{
#' # Real examples will be necessary
#' }
process_wearable.ActiGraph <- function(file = NULL, RAW = NULL, IMU = NULL,
  TwoRegression = NULL, CutPoint = NULL, Sojourn = NULL,
  Count_Args = NULL, RAW_Args = NULL, IMU_Args = NULL, ...) {

  if (!is.null(file)) {
    file <-
      do.call(AGread::read_AG_counts,
        c(list(file = file), as.list(Count_Args))
      )
  }

  if (!is.null(RAW)) {
    RAW <-
      do.call(AGread::read_AG_raw,
        c(list(file = RAW), as.list(RAW_Args))
      )
  }

  if (!is.null(IMU)) {
    IMU <-
      do.call(AGread::read_AG_IMU,
        c(list(file = IMU), as.list(IMU_Args))
      )
  }

  values <- list(AG = file, RAW = RAW, IMU = IMU)
  values[sapply(values, is.null)] <- NULL
  return(values)
}
