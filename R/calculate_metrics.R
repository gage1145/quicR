#' Generate a data frame with calculated metrics.
#'
#' Uses functions from the "calculate" family of quicR functions to generate an analyzed dataframe.
#'
#' @param data A data frame containing the raw RT-QuIC data.
#' @param ... A list of grouping factors. If left empty, function groups by "Sample IDs", "Dilutions", and "Wells".
#' @param threshold Float; the threshold applied to the calculation of time-to-threshold.
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
calculate_metrics <- function(data, ..., threshold = 2) {
  groupings <- c(...)
  if (is_empty(groupings)) {
    groupings <- c("Sample IDs", "Dilutions", "Wells")
  }
  groupings <- syms(groupings)
  data <- group_by(data, !!!groupings)
  list(
    reframe(data),
    calculate_MPR(data, .by=groupings),
    calculate_MS(data, .by=groupings),
    calculate_TtT(data, threshold, .by=groupings)
  ) %>%
    reduce(left_join) %>%
    suppressMessages()
}

