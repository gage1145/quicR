#' Calculate Maximum Slope
#'
#' Uses a sliding window to calculate the slope of real-time reads.
#'
#' @param data A dataframe containing real-time reads. It is recommended to use a dataframe made from normalize_RFU.
#' @param col Character, defines the column containing the derivative curve.
#' @param .by Grouping factor. Should typically be by individual wells.
#'
#' @return A dataframe containing the real-time slope values as change in RFU/sec.
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr summarize
#' @importFrom dplyr group_by
#' @importFrom dplyr is_grouped_df
#' @importFrom dplyr sym
#'
#' @export
calculate_MS <- function(data, col="Deriv", .by="Wells") {
  data %>%
    {if (is_grouped_df(.)) . else group_by(., !!!.by)} %>%
    summarize(MS = max(!!col, na.rm=TRUE))
}
