#' Convert tables into a single column in a dataframe.
#'
#' Accepts a table or matrix or a list of tables and matrices
#' and converts them into dataframe columns.
#'
#' @param tab A table/matrix or a list of tables/matrices.
#'
#' @return A dataframe column.
#'
#' @importFrom tidyr gather
#' @importFrom dplyr select
#'
#' @examples
#' tab <- organize_tables(file)
#' convert_tables(tab)
#'
#'
#' @export
convert_tables <- function(tab) {
  df_list <- data.frame()
  if (is.vector(tab)) {
    for (i in 1:length(tab)) {
      print(names(tab[i]))
      column <- tab[[i]] |>
        t() |>
        as.data.frame() |>
        tidyr::gather() |>
        dplyr::select("value")
      df_list <- append(df_list, column)
    }
    df_ <- as.data.frame(df_list)
    colnames(df_) <- names(tab)
    return(df_)
  }
}
