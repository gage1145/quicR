#' Convert tables into a single column in a dataframe.
#'
#' Accepts a table or matrix or a list of tables and matrices
#' and converts them into dataframe columns.
#'
#' @param tab A table/matrix or a list of tables/matrices.
#' @param na_omit Logical; if true, will remove rows with NA.
#' 
#' @return A dataframe column.
#'
#' @examples
#' file <- system.file(
#'   "extdata/input_files",
#'   file = "test.xlsx",
#'   package = "quicR"
#' )
#' tabs <- organize_tables(file)
#' convert_tables(tabs)
#'
#' @export
convert_tables <- function(tab, na_omit = TRUE) {

  lifecycle::deprecate_warn(
    when = "3.2.0", 
    what = "convert_tables()",
    details = "This function has become deprecated given that get_real() and get_quic() can pull the metadata all at once."
  )

  if (!is.vector(tab)) stop("Input should be of type vector.")

  df_ <- lapply(
    seq_along(tab),
    function(i) {
      message(paste0(i, ": ", names(tab)[i]))
      data.frame(as.vector(t(tab[[i]])), stringsAsFactors = FALSE)
    }
  )
  df_ <- do.call(cbind, df_)
  colnames(df_) <- names(tab)

  if (na_omit) df_ <- na.omit(df_)

  return(df_)
}
