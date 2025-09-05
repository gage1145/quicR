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
#' @importFrom slider slide_dbl
#' @importFrom purrr map_dbl
#' @importFrom dplyr mutate_all
#' @importFrom dplyr mutate
#' @importFrom dplyr relocate
#' @importFrom dplyr %>%
#' @importFrom dplyr select
#'
#' @export
calculate_MS <- function(data, col="Deriv", .by="Wells") {
  data %>%
    group_by(!!sym(.by)) %>%
    summarize(MS = max(!!sym(col), na.rm=TRUE))
}
