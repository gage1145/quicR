#' Calculate the Maxpoint Ratio
#'
#' Maxpoint ratio is defined as the maximum relative fluorescence divided by the
#' background fluorescence.
#'
#' @param data A data frame output from 'get_quic()'.
#' @param col The column containing the normalized fluorescence data.
#' @param .by Grouping factor. Should typically be by individual wells.
#' @return A data frame containing well-matched MPR values.
#'
#' @importFrom dplyr summarize
#'
#' @examples
#' file <- system.file(
#'   "extdata/input_files",
#'   file = "test.xlsx",
#'   package = "quicR"
#' )
#' get_quic(file) |>
#'   calculate_MPR()
#'
#' @export
calculate_MPR <- function(data, col="Norm", .by="Wells") {
  summarize(data, MPR = max(!!sym(col)), .by=.by)
}
