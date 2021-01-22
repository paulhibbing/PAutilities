#' Internal functions for calculating resting energy expenditure
#'
#' @inheritParams get_ree
#' @param breaks age breaks to use (passed to \code{base::cut})
#' @param labels age labels to use (passed to \code{base::cut})
#'
#' @keywords internal
#' @name get_ree_internal
ree_single <- function(
  sex = c("male", "female"), age_yr, method,
  breaks = c(-Inf, 3, 10, 18, 30, 60, Inf),
  labels = c("less3", "3to10", "10to18", "18to30", "30to60", "over60"),
  ...
) {

  s <- get_stratum(method, sex, age_yr, breaks, labels)

  variables <-
    names(s) %>%
    setdiff(c("method", "unit", "stratum", "intercept"))

  for (x in variables) {
    check_arg <- paste0("methods::hasArg(", x, ")")
    if (!eval(parse(text = check_arg))) stop(
      "`", method, "` requires passing values for all",
      " of the following: ", paste(variables, collapse = ", "),
      call. = FALSE
    )
  }

  xvals <-
    environment() %>%
    as.list(.) %>%
    c(list(...)) %>%
    .[variables] %>%
    c(intercept = 1, .)

  names(xvals) %>%
  s[ ,.] %>%
  as.list(.) %>%
  {mapply(function(b, x) b * x, ., xvals, USE.NAMES = FALSE)} %>%
  sum(.)

}

#' @rdname get_ree_internal
#' @keywords internal
get_stratum <- function(method, sex, age_yr, breaks, labels) {

  cut(age_yr, breaks, labels) %>%
  as.character(.) %>%
  paste(sex, ., sep = "_") %>%
  gsub("_$", "", .) %>%
  {equations[equations$stratum == ., ]} %>%
  {.[.$method == method, ]} %T>%
  {stopifnot(nrow(.) == 1)} %>%
  .[ ,!sapply(., is.na)]

}
