#' Reorder the columns of a data frame
#'
#' @param df the data frame
#' @param columns the column(s) to move (either as character names or numeric
#'   indices)
#' @param after the column after which to insert \code{columns} (must be a
#'   scalar, either a character name or a numeric index)
#'
#' @return The reordered data frame
#' @export
#'
#' @examples
#' df <- data.frame(a = 1:10, b = 11:20, c = 21:30, d = 31:40)
#' df_reorder(df, 2:3, "d")
#' df_reorder(df, c("c", "d"), "a")
df_reorder <- function(df, columns, after) {

  stopifnot(
    inherits(df, "data.frame"),
    length(after) == 1
  )

  columns <- validate_df_reorder_input(df, columns, "columns")
  after <- validate_df_reorder_input(df, after, "after")

  names(df) %>%
  setdiff(columns) %>%
  append(., columns, match(after, .)) %>%
  df[ ,.]

}

#' @rdname df_reorder
#' @param arg internal parameter (one of \code{columns} or \code{after})
#' @param label internal parameter (one of \code{"columns"} or \code{"after"})
validate_df_reorder_input <- function(df, arg, label) {

  if (is.numeric(arg)) {
    stopifnot(all(arg %in% seq(ncol(df))))
    return(names(df)[arg])
  }

  if (is.character(arg)) {
    stopifnot(all(arg %in% names(df)))
    return(arg)
  }

  stop(label, " must be numeric or character")

}
