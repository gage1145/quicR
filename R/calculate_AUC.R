#' Calculate the Area Under the Curve
#'
#' Maxpoint ratio is defined as the maximum relative fluorescence divided by the
#' background fluorescence.
#'
#' @param data A data frame output from 'get_quic()'.
#' @param x The column name containing the time data.
#' @param y The column containing the normalized fluorescence data.
#' @param .by `r lifecycle::badge("deprecated")` Use "by" instead.
#' @param by Grouping factor. Should typically be by individual wells.
#' @return A data frame containing well-matched AUC values.
#'
#' @import dplyr 
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
calculate_AUC <- function(data, x="Time", y="Norm", .by=lifecycle::deprecated(), by="Well") {

  if (lifecycle::is_present(.by)) {
    lifecycle::deprecate_warn(
      when = "3.2.0", 
      what = "get_quic(smooth)"
    )
    by <- .by
  }

  data %>%
    {if (is_grouped_df(.)) . else group_by(., across(all_of(by)))} %>%
    summarize(AUC = trapz(!!sym(x), !!sym(y)))
}

