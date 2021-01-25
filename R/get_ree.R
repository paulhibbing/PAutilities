#' Calculate resting energy expenditure
#'
#' @param method character. The equation(s) to use, chosen from
#'   \code{"harris_benedict", "schofield_wt", "schofield_wt_ht",
#'   "fao", "muller_wt_ht", or "muller_ffm"}
#' @param sex character. The participant/patient sex, one of \code{"female"} or
#'   \code{"male"}
#' @param age_yr numeric. The participant/patient age in years. Does not need to
#'   be passed for \code{method = "muller_ffm"}
#' @param ... arguments (e.g. \code{wt_kg} or \code{ht_cm}) for calculations. An
#'   error message will clarify which variables need to be passed if they are
#'   missing
#' @param output character. The desired output unit(s), chosen from
#'   \code{"default", "mj_day", "kcal_day", or "vo2_ml_min"}
#' @param calorie character. The desired conversion factor(s) for calculating MJ
#'   from kcal, chosen from \code{"thermochemical", "convenience", or "dry"}
#' @param RER numeric. The respiratory exchange ratio
#' @param kcal_table character. The desired conversion table(s) to use for
#'   converting kcal to oxygen consumption, chosen from \code{"Lusk",
#'   "Peronnet", or "both"}
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
  sex, age_yr = NULL, ...,
  output = c("default", "mj_day", "kcal_day", "vo2_ml_min"),
  calorie = c("thermochemical", "convenience", "dry"),
  RER = 0.86, kcal_table = c("Lusk", "Peronnet", "both"),
  df = NULL
) {

  ## Set up the grid

    stopifnot(methods::hasArg(method))

    if (!methods::hasArg(output)) output <- match.arg(output)
    if (!methods::hasArg(calorie)) calorie <- match.arg(calorie)
    if (!methods::hasArg(kcal_table)) kcal_table <- match.arg(kcal_table)

    settings <- get_ree_grid(
      method, output, calorie, RER, kcal_table
    )

  ## Deal with one-setting case (no frills necessary to
  ## clarify what numbers mean)

    if (nrow(settings) == 1) {
      result <-
        get_ree_single_setting(method, sex, age_yr, ..., df = df) %>%
        {. * settings$conversion}
      return(result)
    }

  ## Otherwise go grid by grid

    if (is.null(df)) {

      nrow(settings) %>%
      seq(.) %>%
      split(settings, .) %>%
      lapply(function(x, sex, age_yr, ..., df) {
        get_ree_single_setting(x$method, sex, age_yr, ..., df = df) %>%
        {. * x$conversion} %>%
        structure(., settings = x)
      }, sex, age_yr, ..., df = df) %>%
      unname(.)

    } else {

      nrow(settings) %>%
      seq(.) %>%
      split(settings, .) %>%
      lapply(function(x, sex, age_yr, ..., df) {
        get_ree_single_setting(x$method, sex, age_yr, ..., df = df) %>%
        {. * x$conversion} %>%
        data.frame(x, ree = ., stringsAsFactors = FALSE, row.names = NULL)
      }, sex, age_yr, ..., df = df) %>%
      unname(.)

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
