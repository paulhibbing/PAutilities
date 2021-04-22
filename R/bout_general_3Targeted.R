group_runs_targeted <- function(x, threshold = 10) {

  x %<>%
    index_runs(x) %>%
    within({group = 1})

  for (i in 2:nrow(x)) {

    ## Begin by assuming this bout/run
    ## is a continuation of the last one
    x$group[i] <- x$group[i - 1]

    ## If the run length does not exceed the minimum,
    ## no further work is needed
    if (x$lengths[i] < threshold) {

      next

    }

    ## Otherwise, we have to check if a new
    ## behavior is starting -- first identify
    ## the expected behavior for this run
    run_class <-
      seq(i) %>%
      x$group[.] %>%
      {. == x$group[i]} %>%
      which(.) %>%
      .[1] %>%
      x$values[.]

    ## If the current behavior is different,
    ## then label it as a new run
    if (x$values[i] != run_class) {

      x$group[i] %<>% {. + 1}

    }

  }

  df_reorder(x, "group", "end_index")

}

check_targeted <- function(x, target = NULL) {

  if (is.null(target)) stop(
    "Value must be supplied for `target` argument\n  when",
    " using method = \"targeted\"", call. = FALSE
  )

  stopifnot(
    length(target) == 1,
    target %in% x
  )

}
