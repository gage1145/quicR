#' Calculate Maximum Slope
#'
#' Uses a sliding window to calculate the slope of real-time reads.
#'
#' @param data A dataframe containing real-time reads. It is recommended to use a dataframe made from normalize_RFU.
#' @param col Character, defines the column containing the derivative curve.
#' @param .by `r lifecycle::badge("deprecated")` Use "by" instead.
#' @param by Grouping factor. Should typically be by individual wells.
#'
#' @return A dataframe containing the real-time slope values as change in RFU/sec.
#'
#' @import dplyr
#'
#' @export
calculate_MS <- function(data, col="Deriv", .by=lifecycle::deprecated(), by="Well") {

  if (lifecycle::is_present(.by)) {
    lifecycle::deprecate_warn(
      when = "3.2.0", 
      what = "get_quic(smooth)"
    )
    by <- .by
  }

  data %>%
    {if (is_grouped_df(.)) . else group_by(., across(all_of(by)))} %>%
    summarize(MS = max(!!sym(col), na.rm=TRUE))
}
