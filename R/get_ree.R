#' Calculate resting energy expenditure
#'
#' @param method character. The equation to use, one of \code{"harris_benedict",
#'   "schofield_wt", "schofield_wt_ht", "fao", "muller_wt_ht", or "muller_ffm"}
#' @param sex character. The participant/patient sex, one of \code{"female" or
#'   "male"}.
#' @param age_yr numeric. The participant age in years. Does not need to be
#'   passed for \code{method = "muller_ffm"}
#' @param ... arguments (e.g. \code{wt_kg} or \code{ht_cm}) for calculations. An
#'   error message will clarify which variables need to be passed if they are
#'   missing.
#' @param df optional data frame. If passed, all prior arguments should be
#'   character scalars pointing to a column in \code{df} that contains the
#'   corresponding information is stored
#'
#' @return Calculated resting energy expenditure
#' @export
#'
#' @examples
#' get_ree("schofield_wt_ht", "female", 57.8, wt_kg = 80, ht_m = 1.50)
get_ree <- function(
  method = c("harris_benedict", "schofield_wt",
    "schofield_wt_ht", "fao", "muller_wt_ht", "muller_ffm"),
  sex, age_yr = NULL, ..., df = NULL
) {

  stopifnot(methods::hasArg(method))

  method <- match.arg(method)

  if (is.null(df)) {

    mapply(
      get_ree_default, sex, age_yr, ...,
      MoreArgs = list(method = method),
      USE.NAMES = FALSE
    )

  } else {

    get_ree_dataframe(df, method, sex, age_yr, ...)

  }

}

#' @keywords internal
#' @rdname get_ree
get_ree_dataframe <- function(df, method, sex, age_yr, ...) {

  stopifnot(inherits(df, "data.frame"))

  environment() %>%
  as.list(.) %>%
  c(list(...)) %>%
  .[setdiff(names(.), c("df", "method"))] %T>%
  {stopifnot(
    all(sapply(., length) == 1),
    all(sapply(., is.character)),
    all(unlist(.) %in% names(df))
  )} %>%
  do.call(c, .) %>%
  df[ ,.] %>%
  as.list(.) %>%
  c(method = method) %>%
  do.call(get_ree, .)

}

#' @keywords internal
#' @rdname get_ree
get_ree_default <- function(method, sex, age_yr, ...) {

  switch(

    method,

    "harris_benedict" = ree_single(
      sex, age_yr, "harris_benedict", 2, c("", ""), ...
    ),

    "schofield_wt" = ree_single(
      sex, age_yr, "schofield_wt", ...
    ),

    "schofield_wt_ht" = ree_single(
      sex, age_yr, "schofield_wt_ht", ...
    ),

    "fao" = ree_single(
      sex, age_yr, "fao", ...
    ),

    "muller_wt_ht" = ree_single(
      sex, age_yr, "muller_wt_ht", 2, c("", ""),
      dummy_sex = 1, ...
    ),

    "muller_ffm" = ree_single(
      sex, .Machine$integer.max, "muller_ffm", 2, c("", ""),
      dummy_sex = 1, ...
    ),

    NULL

  )

}
