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

  df %>%
  dplyr::relocate(
    dplyr::all_of(columns),
    .after = dplyr::all_of(after)
  )

}
