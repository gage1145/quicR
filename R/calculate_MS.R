#' Calculate Maximum Slope
#'
#' Uses a sliding window to calculate the slope of real-time reads.
#'
#' @param data A dataframe containing real-time reads. It is recommended to use a dataframe made from normalize_RFU.
#' @param col `r lifecycle::badge("deprecated")`
#' @param .by `r lifecycle::badge("deprecated")` Use "by" instead.
#' @param by Grouping factor. Should typically be by individual wells.
#'
#' @return A dataframe containing the real-time slope values as change in RFU/sec.
#'
#' @import dplyr
#'
#' @export
calculate_MS <- function(data, col=lifecycle::deprecated(), .by=lifecycle::deprecated(), by="well") {

  if (lifecycle::is_present(.by)) {
    lifecycle::deprecate_warn(
      when = "3.2.0", 
      what = "calculate_MS(.by)"
    )
    by <- .by
  }

  data %>%
    {if (is_grouped_df(.)) . else group_by(., across(all_of(by)))} %>%
    summarize(ms = max(deriv, na.rm=TRUE))
}
