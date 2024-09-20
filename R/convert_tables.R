#'  Convert tables into a single column in a dataframe.
#'
#'  convert_tables accepts a table or matrix or a list of tables and matrices
#'  and converts them into dataframe columns.
#'
#'  @param tab A table/matrix or a list of tables/matrices.
#'
#'  @importFrom tidyr gather
#'  @importFrom dplyr select
#'
#'  @export
convert_tables <- function(tab) {
  df_list <- data.frame()
  if (is.vector(tab)) {
    for (i in 1:length(tab)) {
      print(names(tab[i]))
      column <- tab[[i]] |>
        t() |>
        as.data.frame() |>
        tidyr::gather() |>
        dplyr::select(value)
      print(i)
      df_list <- append(df_list, column)
    }
  }
  df_ <- as.data.frame(df_list)
  colnames(df_) <- names(tab)
  return(df_)
}
