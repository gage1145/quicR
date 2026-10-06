#' Generate a data frame with calculated metrics.
#'
#' Uses functions from the "calculate" family of quicR functions to generate an analyzed dataframe.
#'
#' @param data A S3 object of class "quic".
#' @param ... A list of grouping factors. If left empty, function groups by "Sample IDs", "Dilutions", and "Wells".
#' @param threshold Float; the threshold applied to the calculation of time-to-threshold.
#' @param time_col `r lifecycle::badge("deprecated")`
#' @param ttt_values `r lifecycle::badge("deprecated")`
#' @param auc_values `r lifecycle::badge("deprecated")`
#' @param norm_col `r lifecycle::badge("deprecated")`
#' @param deriv_col `r lifecycle::badge("deprecated")`
#' @param flip_ratio Logical; should the quenching ratio be calculated as max / last (default) or last / max?
#'
#' @import dplyr
#' @import purrr 
#'
#' @return An S3 object of class "quic_metrics" containing relevant kinetic metrics.
#'
#' @examples
#' file <- system.file(
#'   "extdata/input_files",
#'   file = "test2.xlsx",
#'   package = "quicR"
#' )
#' get_quic(file) |>
#'  calculate_metrics(threshold = 3)
#'
#' @export
calculate_metrics <- function(data, ..., threshold = 2, time_col = lifecycle::deprecated(), ttt_values = lifecycle::deprecated(), 
                              auc_values = lifecycle::deprecated(), norm_col = lifecycle::deprecated(), deriv_col = lifecycle::deprecated(), 
                              flip_ratio = FALSE) {
  
  c(time_col, ttt_values, auc_values, norm_col, deriv_col) %>%
  sapply(function(x) {
    if (lifecycle::is_present(x)) {
      lifecycle::deprecate_warn(
      when = "3.2.0", 
      what = paste0("calculate_metrics(", x, ")"),
      details = paste0(x, " is automatically detected now.")
      )
    }
  })
  
  groupings <- c(...)
  if (is_empty(groupings)) {
    groupings <- c("sample", "dilution", "well")
  }

  df <- data$data %>%
    unnest(where(is.list)) 

  df_ <- list(
    reframe(df, .by = all_of(groupings)),
    calculate_QR(df, by=groupings, flip_ratio = flip_ratio, zeroed = data$zero), # does both MPR and QR
    calculate_MS(df, by=groupings),
    calculate_AUC(df, by=groupings),
    calculate_TtT(df, threshold, by=groupings, zeroed = data$zero)
  ) %>%
    reduce(left_join) %>%
    suppressMessages()

  quic_obj <- list(
    data = df_, by = groupings, threshold = threshold, flip_ratio = flip_ratio,
    plate = data$plate, norm_point = data$norm_point, zero = data$zero, smooth = data$smooth,
    smooth_factor = data$smooth_factor, window_size = data$window_size
  )
  class(quic_obj) <- "quic_metrics"
  return(quic_obj)
}
