#' Calculate the Area Under the Curve
#'
#' Maxpoint ratio is defined as the maximum relative fluorescence divided by the
#' background fluorescence.
#'
#' @param data A data frame output from 'get_quic()'.
#' @param x The column name containing the time data.
#' @param y The column containing the normalized fluorescence data.
#' @param .by Grouping factor. Should typically be by individual wells.
#' @return A data frame containing well-matched AUC values.
#'
#' @importFrom dplyr summarize
#' @importFrom dplyr sym
#' @importFrom dplyr group_by
#' @importFrom dplyr is_grouped_df
#' @importFrom dplyr %>%
#' @importFrom pracma trapz
#'
#' @examples
#' file <- system.file(
#'   "extdata/input_files",
#'   file = "test.xlsx",
#'   package = "quicR"
#' )
#' get_quic(file) |>
#'   calculate_AUC()
#'
#' @export
calculate_AUC <- function(data, x="Time", y="Norm", .by="Wells") {
  x <- sym(x)
  y <- sym(y)
  .by <- syms(c(.by))
  data %>%
    {if (is_grouped_df(.)) . else group_by(., !!!.by)} %>%
    summarize(AUC = trapz(!!x, !!y))
}

