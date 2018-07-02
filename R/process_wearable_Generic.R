#' Process data obtained from a wearable device
#'
#' @param file character scalar giving path to an activity counts data file
#' @param RAW character scalar giving path to a raw primary accelerometer data
#'   file
#' @param IMU character scalar giving path to an inertial measurement unit data
#'   file
#' @param TwoRegression optional. One or more two-regression algorithms to apply
#'   from \code{\link[TwoRegression]{TwoRegression-package}}
#' @param CutPoint optional. One or more cut-points to apply
#' @param Sojourn optional. One or more Sojourn methods to apply
#' @param ... further arguments passed to methods
#' @param device character scalar defining which device generated the data in
#'   the file
#'
#' @return A data frame of processed data from the specified monitor data file(s).
#' @export
#'
#' @examples
#' \dontrun{
#' # Insert a real example
#' }
process_wearable <- function(
  file = NULL, RAW = NULL, IMU = NULL,
  TwoRegression = NULL, CutPoint = NULL, Sojourn = NULL, ...,
  device = c("ActiGraph", "GENEactiv", "Axivity")) {

  device <- try(
    match.arg(device, c("ActiGraph", "GENEactiv", "Axivity", "Error")),
    silent = TRUE
  )

  if (class(device) == "try-error") {
    stop(paste("You must specify `device = `, and the",
      "value must be exactly one of \"ActiGraph\",",
      "\"GENEactiv\", or \"Axivity\"."))
  } else {
    class(device) <- device
  }

  UseMethod("process_wearable", device)

}
