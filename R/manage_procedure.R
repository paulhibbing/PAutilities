#' Printing and timing utility for managing processes
#'
#' @param part character scalar, either \code{Start} or \code{End}.
#' @param ... character strings to print
#' @param verbose logical. Print to console?
#'
#' @return For \code{part = "Start"}, a proc_time object; for \code{pard =
#'   "End"}, invisible
#' @export
#'
#' @examples
#'
#' timer <- manage_procedure("Start", "String will be printed")
#' get_duration(timer)
#'
#' manage_procedure("Start")
#' manage_procedure("Start", "String will not be printed", verbose = FALSE)
#'
#' manage_procedure(
#'   "End",
#'   "Processing complete. Elapsed time",
#'   AGread::get_duration(timer),
#'   "minutes."
#' )
manage_procedure <- function(
  part = c("Start", "End"),
  ..., verbose = TRUE
) {

  part <- match.arg(part)
  if (verbose) cat(...)
  if (part == "Start") return(proc.time()) else invisible()

}

#' @rdname manage_procedure
#'
#' @param timer a proc_time object
#'
#' @export
get_duration <- function (timer) {

  format(
    (proc.time() - timer)[3]/60,
    digits = 1,
    nsmall = 1
  )

}
