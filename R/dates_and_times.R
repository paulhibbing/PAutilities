# Helper(s) ---------------------------------------------------------------

check_df_time <- function(df, time_var, ...) {

  stopifnot(
    is.data.frame(df),
    is.character(time_var),
    length(time_var) == 1,
    exists(time_var, df),
    inherits(df[[time_var]], c("POSIXt", "character"))
  )

  if (is.character(df[[time_var]])) {
    df[[time_var]] %<>% as.POSIXct(...)
    if (!exists("tz", list(...))) warning(
      "Using default time zone -- pass a value for `tz` to silence",
      call. = FALSE
    )
  }

  invisible(df)

}

full_days_continuous <- function(
  df, time_var, digits, check_continuous,
  discontinuous_action, ...
) {

  if (check_continuous) {
    is_continuous <- df_continuous(df, time_var, digits, ...)
  } else {
    is_continuous <- TRUE
  }

  if (!is_continuous) switch(
    discontinuous_action,
    "warn" = warning(
      "`full_days` is returning a discontinuous dataframe -- If you",
      " suspect an error, try setting `digits` to\na smaller number,",
      " or setting `check_continuous` to FALSE",
      call. = FALSE
    ),
    "stop" = stop(
      "`full_days` resulted in a discontinuous dataframe -- If you",
      " suspect an error, try setting `digits` to\na smaller number,",
      " or setting `check_continuous` to FALSE",
      call. = FALSE
    ),
    stop("`discontinuous_action` must be \"warn\" or \"stop\"", call. = FALSE)
  )

  df

}

# Main function(s) ---------------------------------------------------------


#' Determine epoch length in seconds
#'
#' @param timestamps POSIX-formatted input
#' @param digits for rounding. See details
#'
#' @details The function is designed to work even when the epoch length is less
#'   than one second (e.g., for raw accelerometry data). Thus, it is not
#'   possible to base the code on convenient \code{difftime} methods. Instead,
#'   numeric operations are performed after running \code{unclass} on the input.
#'   This sometimes results in miniscule fluctuations of the calculated epoch
#'   length (e.g., +/- 0.0000002). Thus, the code rounds everything to the
#'   precision indicated by \code{digits}. For most applications, the default
#'   value (\code{digits = 6}) should be well past the range of meaningful
#'   fluctuations and lead to a favorable outcome. But the \code{ditits}
#'   argument can also be adjusted if greater assurance is needed.
#'
#'   After rounding, the code checks for the existence of multiple epoch
#'   lengths. If they are detected (e.g., due to a discontinuity in the file), a
#'   warning is issued and the most prevalent epoch length is returned. The
#'   warning will specify all the different epoch lengths that were deteceted,
#'   which may be useful information for troubleshooting.
#'
#' @return The epoch length of the data, in seconds
#' @export
#'
#' @examples
#' epoch_length_sec(Sys.time() + 0:5)
#' epoch_length_sec(Sys.time() + seq(0, 25, 5))
epoch_length_sec <- function(timestamps, digits = 6) {

    stopifnot(inherits(timestamps, "POSIXt"))

    if (length(timestamps) == 1) {

      warning(
        "The `timestamps` object has length == 1.",
        " Returning NA for the epoch length"
      )

      NA

    } else {

      unclass(timestamps) %>%
      diff(.) %>%
      index_runs(.) %>%
      within({values = round(values, digits)}) %>%
      {tapply(.$lengths, .$values, sum)} %>%
      {data.frame(
        value = as.numeric(names(.)),
        n = as.vector(.)
      )} %T>%
      {if (nrow(.) > 1) warning(
        "Detected multiple epoch lengths (",
        paste(.$value, collapse = ", "), ") -- ",
        "returning ", .$value[which.max(.$n)],
        " (", round(max(.$n)/sum(.$n)*100, 2), "% of observations)",
        call. = FALSE
      )} %>%
      {.$value[which.max(.$n)]}

    }

}

#' Drop incomplete days from a dataset
#'
#' @param df the input data frame
#' @param time_var character scalar giving the column name of the variable
#'   containing timestamp information (either \code{character} or \code{POSIXt}
#'   format)
#' @param drop character scalar indicating which incomplete days to drop.
#'   Can be \code{all} (default), \code{leading} (only day/s at the start of the
#'   file), \code{trailing} (only day/s at the end of the file), or
#'   \code{label}. If the latter is selected, the full dataset is returned with
#'   an additional column indicating whether each row of data corresponds with a
#'   complete day (useful for troubleshooting, among other things)
#' @param epoch_length_sec optional. The epoch length of the data. If no value
#'   is passed, \code{\link{epoch_length_sec}} is invoked on the \code{time_var}
#'   column
#' @param label_name character scalar. Name to give the indicator column when
#'   \code{drop == "label"}
#' @param digits see \code{\link{epoch_length_sec}}
#' @param check_continuous logical. Check the dataframe after dropping to see if
#'   it is continuous?
#' @param discontinuous_action character scalar telling what to do if a
#'   discontinuity is expected when \code{check_continuous = TRUE}. Can be
#'   either \code{warn} (the default) or \code{stop}
#' @param ... arguments passed to \code{as.POSIXct}, for use if \code{time_var}
#'   is a character rather than \code{POSIXt} variable
#'
#' @seealso \code{\link{df_continuous}}
#' @export
#'
#' @examples
#' data(ex_data, package = "PAutilities")
#' ex_data <- full_days(
#'   ex_data, "DateTime", "label", 60,
#'   "full_day_indicator", tz = "UTC"
#' )
#' head(ex_data)
full_days <- function(
  df, time_var = "Timestamp", drop = c("all", "leading", "trailing", "label"),
  epoch_length_sec = NULL, label_name = "is_full_day", digits = 6,
  check_continuous = TRUE, discontinuous_action = c("stop", "warn"),
  ...
) {

  #* Check input

    df %<>% check_df_time(time_var, ...)

    stopifnot(

      !is.null(drop), !is.null(discontinuous_action),

      (is.null(epoch_length_sec) | is.numeric(epoch_length_sec)),

      !is.na(check_continuous), !is.null(check_continuous),
      is.logical(check_continuous), length(check_continuous) == 1

    )

    drop <- match.arg(drop)
    discontinuous_action <- match.arg(discontinuous_action)

    if (is.null(epoch_length_sec)) {
      epoch_length_sec <- epoch_length_sec(df[[time_var]], digits)
    } else {
      stopifnot(
        !is.na(epoch_length_sec),
        is.numeric(epoch_length_sec),
        length(epoch_length_sec) == 1
      )
    }

    if (drop == "label") {
      stopifnot(
        !is.na(label_name),
        !is.null(label_name),
        is.character(label_name),
        length(label_name) == 1
      )
    }

  #* Identify full days

    rows_per_day <- 86400 / epoch_length_sec

    dates <-
      df[[time_var]] %>%
      as.Date(.)

    full_dates <-
      dates %>%
      table(.) %>%
      {names(.)[. == rows_per_day]} %>%
      as.Date(.)

    full_indices <- dates %in% full_dates

    if (!any(full_indices)) {

      warning(
        "No full days detected. Returning a 1-row NA dataframe",
        call. = FALSE
      )

      result <-
        rbind(df, NA) %>%
        utils::tail(1)

      return(result)

    }

  #* Apply the operation corresponding to `drop`

    switch(
      drop,
      "all" =
        df[full_indices, ] %>%
        structure(., row.names = seq(nrow(.))),
      "leading" =
        which(full_indices) %>%
        dplyr::first(.) %>%
        seq(., nrow(df)) %>%
        df[., ] %>%
        structure(., row.names = seq(nrow(.))),
      "trailing" =
        which(full_indices) %>%
        dplyr::last(.) %>%
        seq(1, .) %>%
        df[., ] %>%
        structure(., row.names =seq(nrow(.))),
      "label" =
        df %>%
        dplyr::mutate(!!as.name(label_name) := full_indices),
      stop(
        "Failed to match `drop` to a removal procedure",
        call. = FALSE
      )
    ) %>%
    full_days_continuous(
      time_var, digits, check_continuous,
      discontinuous_action, ...
    )

}


#' Check if a dataframe is continuous
#'
#' @inheritParams full_days
#'
#' @export
#'
#' @examples
#' data(ex_data, package = "PAutilities")
#' df_continuous(ex_data, "DateTime", tz = "UTC")
#' df_continuous(ex_data[-c(300:500), ], "DateTime", tz = "UTC")
df_continuous <- function(df, time_var = "Timestamp", digits = 6, ...) {

  #* Run tests

    df %<>% check_df_time(time_var, ...)

    test <- tryCatch(
      epoch_length_sec(df[[time_var]], digits),
      warning = function(w) w
    )

  #* Yield output

    result <- TRUE

    if (is(test, "warning")) {

      if (grepl("^Detected multiple epoch lengths ", test$message)) {
        result <- FALSE
      }

    }

    result

}
