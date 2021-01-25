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

#' @keywords internal
#' @rdname get_ree_internal
get_ree_single_setting <- function(method, sex, age_yr, ..., df) {

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
#' @rdname get_ree_internal
get_ree_grid <- function(
  method, output, calorie, RER, kcal_table
) {

  method <- match.arg(
    method,
    c(
      "harris_benedict", "schofield_wt",
      "schofield_wt_ht", "fao",
      "muller_wt_ht", "muller_ffm"
    ),
    TRUE
  )

  output <- match.arg(
    output,
    c("default", "mj_day", "kcal_day", "vo2_ml_min"),
    TRUE
  )

  calorie <- match.arg(
    calorie,
    c("thermochemical", "convenience", "dry"),
    TRUE
  )

  kcal_table <- match.arg(
    kcal_table,
    c("Lusk", "Peronnet"),
    TRUE
  )

  result <-
    expand.grid(
      method = method,
      to = output,
      calorie = calorie,
      RER = RER,
      kcal_table = kcal_table,
      stringsAsFactors = FALSE,
      KEEP.OUT.ATTRS = FALSE
    ) %>%
    within({
      from = equations$unit[
        sapply(method, match, equations$method, USE.NAMES = FALSE)
      ]
      to = ifelse(to == "default", from, to)
    }) %>%
    .[!duplicated(.), ] %>%
    within({
      mj_day__mj_day = 1
      kcal_day__kcal_day = 1
      kcal_day__vo2_ml_min = unname(mapply(
        get_kcal_vo2_conversion, RER, kcal_table
      ))
      kcal_day__vo2_ml_min = 1000/kcal_day__vo2_ml_min/1440
      #^^ 1000 in numerator to convert L to ml
      mj_day__kcal_day = sapply(calorie, function(x) {
        switch(
          x,
          "thermochemical" = 239.006,
          "convenience" = 239,
          "dry" = 238.846,
          NA_real_
        )
      })
      kcal_day__mj_day = 1 / mj_day__kcal_day
      mj_day__vo2_ml_min = mj_day__kcal_day * kcal_day__vo2_ml_min
    }) %>%
    .[ ,c(
      c("method", "from", "to"),
      setdiff(names(.), c("method", "from", "to"))
    )]

  paste(result$from, result$to, sep = "__") %>%
    mapply(
      function(x, y, df) df[x,y],
      seq(.), ., MoreArgs = list(df = result)
    ) %>%
    {within(result, {conversion = .})} %>%
    .[ ,!grepl("__", names(.))] %>%
    structure(., row.names = seq(nrow(.)))

}
