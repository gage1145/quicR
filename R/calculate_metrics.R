#' Generate a dataframe with calculated metrics.
#'
#' Uses functions from the "calculate" family of quicR functions to generate an analyzed dataframe.
#'
#' @param data A dataframe containing the raw RT-QuIC data.
#' @param ... A list of grouping factors.
#' @param threshold Float; the threshold applied to the calculation of time-to-threshold.
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr group_by
#' @importFrom dplyr left_join
#' @importFrom dplyr reframe
#'
#' @return A dataframe of calculated metrics.
#'
#' #' @examples
#' file <- system.file(
#'   "extdata/input_files",
#'   file = "test.xlsx",
#'   package = "quicR"
#' )
#' get_quic(file) |>
#'  calculate_metrics(`Sample IDs`, Dilutions, Wells)
#'
#' @export
calculate_metrics <- function(data, ..., threshold = 2) {
  data %>%
    group_by(...) %>%
    reframe() %>%
    left_join(calculate_MPR(raw)) %>%
    left_join(calculate_TtT(raw, threshold)) %>%
    left_join(calculate_MS(raw)) %>%
    mutate(RAF = 1/.data$TtT) %>%
    suppressMessages()
}
