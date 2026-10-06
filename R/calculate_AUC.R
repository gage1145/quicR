#' Calculate the Area Under the Curve
#'
#' Maxpoint ratio is defined as the maximum relative fluorescence divided by the
#' background fluorescence.
#'
#' @param data A data frame output from 'get_quic()'.
#' @param x `r lifecycle::badge("deprecated")`
#' @param y `r lifecycle::badge("deprecated")`
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
calculate_AUC <- function(data, x=lifecycle::deprecated(), y=lifecycle::deprecated(), .by=lifecycle::deprecated(), by="well") {

  if (lifecycle::is_present(.by)) {
    lifecycle::deprecate_warn(
      when = "3.2.0", 
      what = "calculate_AUC(.by)"
    )
    by <- .by
  }

  data %>%
    {if (is_grouped_df(.)) . else group_by(., across(all_of(by)))} %>%
    summarize(auc = trapz(time, norm))
}

