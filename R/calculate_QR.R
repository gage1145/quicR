#' Calculate the Quenching Ratio
#' 
#' The quenching ratio (QR) is defined as the ratio between the maximum initial value
#' after the exponential phase divided by the last measured value.
#' 
#' @param data A data frame output from 'get_quic()'.
#' @param col The column containing the normalized fluorescence data.
#' @param time_col The column containing the time points.
#' @param .by Grouping factor. Should typically be by individual wells.
#' @return A data frame containing well-matched quenching ratio values.
#'
#' @importFrom dplyr summarize
#' @importFrom dplyr sym
#' @importFrom dplyr syms
#' @importFrom dplyr group_by
#' @importFrom dplyr is_grouped_df
#' @importFrom dplyr last
#' @importFrom dplyr %>%
#'
#' @examples
#' file <- system.file(
#'   "extdata/input_files",
#'   file = "test2.xlsx",
#'   package = "quicR"
#' )
#' get_quic(file) |>
#'   calculate_QR()
#'
#' @export
calculate_QR <- function(data, col="Norm", time_col="Time", .by="Wells") {
  col <- sym(col)
  time_col <- sym(time_col)
  .by <- syms(c(.by))
  
  data %>%
    {if (is_grouped_df(.)) . else group_by(., !!!.by)} %>%
    summarize(
      MPR = max(!!col, na.rm=TRUE),
      QR = last(!!col, !!!time_col) / MPR
    ) 
}
