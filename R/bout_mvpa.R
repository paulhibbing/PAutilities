#' Classify moderate-to-vigorous physical activity in bouts of a specific
#' minimum length
#'
#' @param intensity a vector of intensity classifications to be re-classified
#'   according to the bout definition
#' @param var_type character scalar indicating whether the \code{intensity}
#'   variable is a numeric vector of metabolic equivalents, or a factor variable
#'   giving activity intensity classification
#' @param min_duration numeric scalar: minimum duration of a qualifying bout, in
#'   minutes
#' @param termination numeric scalar: consecutive minutes of non-MVPA required
#'   to terminate the bout
#' @param MoreArgs required arguments to pass to \code{cut}
#' @param ... optional arguments passed to \code{cut} for converting METs to
#'   intensity classification
#'
#' @examples
#' data(ex_data, package = "PAutilities")
#' bout_mvpa(intensity = ex_data$METs, var_type = "METs")
#'
#' @export
#'
bout_mvpa <-
  function(intensity, var_type = c("METs", "Intensity"),
    min_duration = 10, termination = 3, MoreArgs =
      list(breaks = c(-Inf, 1.51, 3, Inf),
           labels = c("SB", "LPA", "MVPA"),
           right = FALSE), ...) {

  # Set up ####
    var_type <- match.arg(var_type, c("METs", "Intensity", "Error"))
    if (var_type == "METs") {
      # intensity <-
      #   do.call(cut, c(list(intensity), MoreArgs))
      intensity <-
        do.call(cut, c(list(x = intensity), MoreArgs, ...))
    } else {
      stopifnot(is.factor(intensity))
    }
    stopifnot("MVPA" %in% levels(intensity))

  # Label all bouts of `MVPA` or `Other`
    intensity <-
      sapply(
        as.character(intensity),
        function(x) if (x == "MVPA") "MVPA" else "Other"
      )

    bout_label <- rle(as.character(intensity))
    bout_label <- rep(seq(bout_label$lengths), bout_label$lengths)

  # Get starting indices of all `Other` bouts with length >= `termination`
    stop_indices <- unlist(
      sapply(unique(bout_label), function(x) {
        index <- which(bout_label == x)[1]
        intensity <- intensity[index]
        length_test <- sum(bout_label == x) >= termination
        if (any(!length_test, intensity == "MVPA")) return(NULL)
        index
      })
    )

  # Get starting indices of all `MVPA` bouts
    start_indices <- unlist(
      sapply(unique(bout_label), function(x) {
        index <- which(bout_label == x)[1]
        intensity <- intensity[index]
        if (intensity != "MVPA") return(NULL)
        index
      })
    )

  # Match each `MVPA` bout's starting index to the next termination bout's
  # starting index
    matched_stops <- sapply(
      start_indices, function(x) {
        index <- stop_indices[stop_indices > x][1]
        if (length(index) == 0) return(NA)
        return(index)
    })

  # Assemble a data frame and use it to track bouts and determine if they meet
  # the criteria
    bout_tracker <- tapply(start_indices, matched_stops, min)
    #^It's possible for multiple bouts to be matched to the same stopping index,
    #which happens anytime there is an interruption < the value of
    #`termination`. Therefore, things need to be collapsed so that there is a
    #1:1 occurrence of MVPA bouts starting and ending (i.e., separate spurts of
    #MVPA in the bout need to be combined so that we can just focus on the
    #start/stop index of the overall bout. The next step will be to see how many
    #minutes of MVPA occur in that span)
    bout_tracker <- data.frame(
      mvpa_start = as.integer(unname(bout_tracker)),
      bout_terminates = as.integer(names(bout_tracker)),
      stringsAsFactors = FALSE,
      row.names = NULL
    )

    bout_tracker$MVPA_mins <- sapply(
      seq(nrow(bout_tracker)),
      function(x) {
        indices <-
          seq(
            bout_tracker$mvpa_start[x],
            bout_tracker$bout_terminates[x]
            #^Technically should be x - 1 for termination, but it doesn't matter
            #because this index is automatically non-MVPA
          )
        sum(intensity[indices] == "MVPA")
    })

    bout_tracker$bout_MVPA_proportion <-
      with(bout_tracker, MVPA_mins / (bout_terminates - mvpa_start))

    # Reduce the data frame to only cases where there are at least
    # `min_duration` minutes of MVPA in the bout
    bout_tracker <-
      bout_tracker[bout_tracker$MVPA_mins >= min_duration, ]

    # Now prepare a vector to return, which indicates numerically (0 for no, 1
    # for yes) whether the minute-by-minute values are part of a valid bout of
    # MVPA
    valid_indices <- unlist(apply(
      bout_tracker, 1,
      function(x)
        seq(
          x["mvpa_start"],
          x["bout_terminates"]
      )
    ))

    is_MVPA <- sapply(
      seq(intensity),
      function(x) {
        if (all(intensity[x] == "MVPA", x %in% valid_indices)) 1 else 0
    })

    # View(data.frame(intensity, bout_label, is_MVPA))
    return(is_MVPA)
  }
