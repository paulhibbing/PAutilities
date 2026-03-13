#' Reintegrate accelerometer data
#'
#'
#' @param df a data frame to reintegrate
#' @param target_sec the desired epoch length of the output. Starting epoch
#'   length will be determined automatically
#' @param time_var The name of the column containing POSIX-formatted timestamp
#'   information
#'
#' @return A data frame of reintegrated data. Numeric variables will be summed,
#'   and others will be labeled with the first value.
#'
#'
#' @export
#'
#' @examples
#'
#' ##Example data in 60-second epochs
#' data(ex_data, package = "PAutilities")
#' ex_data$Timestamp <- as.POSIXct(
#'   paste(ex_data$Date, ex_data$Time),
#'   tz = "UTC",
#'   format = "%m/%d/%Y %H:%M:%S"
#' )
#'
#' ##Reintegrate to 120-second epochs
#' result <- reintegrate(ex_data, 120)
#'
#' utils::head(result)
#'
reintegrate <- function(
  df, target_sec, time_var = "Timestamp"
) {

  stopifnot(
    inherits(df, "data.frame"),
    exists(time_var, df)
  )

  e <-
    nrow(df) %>%
    {. * 0.1} %>%
    ceiling(.) %>%
    max(2) %>%
    seq(.) %>%
    dplyr::slice(df, .) %>%
    .[[time_var]] %>%
    epoch_length_sec(.)

  if (is.na(e)) stop(
    "Reintegration failed due to indeterminate starting epoch length. ",
    "This can happen if the dataset has only one row. ",
    "If that isn't what's happening, feel free to submit a bug report on GitHub.",
    call. = FALSE
  )

  if (e == target_sec) return(df)

  if (e > target_sec) stop(
    "Cannot reintegrate to a shorter epoch length (`target_sec` is ", target_sec,
    ", but `epoch_length_sec(df)` is ", e, ")", call. = FALSE
  )

  out_unit <- paste(target_sec, "sec")

  if (target_sec > 60) {
    out_unit <-
      {target_sec / 60} %>%
      paste("min")
  }

  if (target_sec > 3600) {
    out_unit <-
      {target_sec / 3600} %>%
      paste("hours")
  }

  df %>%
  dplyr::group_by(
    !!as.name(time_var) := lubridate::floor_date(
      !!as.name(time_var),
      out_unit
    )
  ) %>%
  dplyr::summarise(
    dplyr::across(where(is.numeric), sum),
    dplyr::across(where(function(x) !is.numeric(x)), dplyr::first)
  ) %>%
  dplyr::select(dplyr::all_of(names(df))) %>%
  # as.data.frame(.) %>%
  vm_reformat(verbose = FALSE)

}

vm_reformat <- function(
  df, verbose, triaxial_vars = c("Axis1", "Axis2", "Axis3")
) {

  if (!all(triaxial_vars %in% names(df))) {
    if (verbose) cat(
      "Skipping vector magnitude operation because",
      "`all(triaxial_vars %in% names(df))` is not TRUE"
    )
    return(df)
  }

  df %>%
  dplyr::mutate(
    Vector.Magnitude =
      dplyr::pick(dplyr::all_of(triaxial_vars)) %>%
      {rowSums(.^2)} %>%
      sqrt(.) %>%
      round(2)
  )

}
