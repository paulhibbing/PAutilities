get_bouts <- function(
  x, method = c("sequential", "targeted"),
  threshold = 10, target = NULL
) {

  method <- match.arg(method)

  if (method == "targeted") check_targeted(x, target)

  switch(
    method,
    "sequential" = group_runs_sequential(x, threshold),
    "targeted" = group_runs_targeted(x, threshold, target),
    NULL
  ) %>%
  collapse_runs(threshold)

}

collapse_runs <- function(x, threshold = 10) {

  group_runs_sequential(x, threshold) %>%
  split(., .$group) %>%
  lapply(function(df) {
    data.frame(
      start_index = df$start_index[1],
      end_index = df$end_index[nrow(df)],
      group = unique(df$group),
      values = df$values[1],
      lengths = sum(df$lengths),
      interruption_length = sum(
        ifelse(df$values == df$values[1], 0, df$lengths)
      )
    )
  }) %>%
  do.call(rbind, .)

}
