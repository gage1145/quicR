#' Generate a data frame with calculated metrics.
#'
#' Uses functions from the "calculate" family of quicR functions to generate an analyzed dataframe.
#'
#' @param data A data frame containing the raw RT-QuIC data.
#' @param ... A list of grouping factors. If left empty, function groups by "Sample IDs", "Dilutions", and "Wells".
#' @param threshold Float; the threshold applied to the calculation of time-to-threshold.
#' @param time_col String; column name containing the time values.
#' @param ttt_values String; column name containing values to use for calculating time-to-threshold.
#' @param auc_values String; column name containing values to use for calculating the area under the curve.
#' @param norm_col String; column name containing the normalized fluorescent values.
#' @param deriv_col String; column name containing the estimated derivative values.
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr group_by
#' @importFrom dplyr left_join
#' @importFrom dplyr reframe
#' @importFrom dplyr syms
#' @importFrom dplyr %>%
#' @importFrom purrr reduce
#' @importFrom purrr is_empty
#'
#' @return A data frame of calculated metrics.
#'
#' @examples
#' file <- system.file(
#'   "extdata/input_files",
#'   file = "test.xlsx",
#'   package = "quicR"
#' )
#' get_quic(file) |>
#'  calculate_metrics()
#'
#' @export
calculate_metrics <- function(data, ..., threshold = 2, time_col = "Time", ttt_values = "Norm", 
                              auc_values = "Norm", norm_col = "Norm", deriv_col = "Deriv") 
{
  groupings <- c(...)
  if (is_empty(groupings)) {
    groupings <- c("Sample IDs", "Dilutions", "Wells")
  }
  groupings <- syms(groupings)
  data <- group_by(data, !!!groupings)
  list(
    reframe(data),
    calculate_MPR(data, col=norm_col, .by=groupings),
    calculate_MS(data, col=deriv_col, .by=groupings),
    calculate_TtT(data, threshold, time=time_col, values=ttt_values, .by=groupings),
    calculate_AUC(data, x=time_col, y=auc_values, .by=groupings)
  ) %>%
    reduce(left_join) %>%
    suppressMessages()
}
