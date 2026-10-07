#' Generate a data frame with calculated metrics.
#'
#' Uses functions from the "calculate" family of quicR functions to generate an analyzed dataframe.
#'
#' @param data Either an S3 object of class "quic" returned by [get_quic()], or a
#'   long-format data frame with `time`, `norm` and `deriv` columns.
#' @param ... A list of grouping factors. If left empty, function groups by
#'   "sample", "dilution", and "well" (whichever are present).
#' @param threshold Float; the threshold applied to the calculation of time-to-threshold.
#' @param time_col `r lifecycle::badge("deprecated")`
#' @param ttt_values `r lifecycle::badge("deprecated")`
#' @param auc_values `r lifecycle::badge("deprecated")`
#' @param norm_col `r lifecycle::badge("deprecated")`
#' @param deriv_col `r lifecycle::badge("deprecated")`
#' @param flip_ratio Logical; should the quenching ratio be calculated as max / last (default) or last / max?
#' @param zeroed Logical; was the data zeroed in [get_quic()]? Only used by the
#'   data frame method, since a "quic" object already records this.
#'
#' @import dplyr
#' @import purrr
#' @importFrom tidyr unnest
#'
#' @return For a "quic" object, an S3 object of class "quic_metrics": a list with
#'   `data`, a tibble of kinetic metrics with one row per group, and `params`,
#'   the settings carried forward from [get_quic()] plus `by`, `threshold` and
#'   `flip_ratio`. For a data frame, a data frame of the metrics.
#'
#' @examples
#' file <- system.file(
#'   "extdata/input_files",
#'   file = "test2.xlsx",
#'   package = "quicR"
#' )
#' metrics <- get_quic(file) |>
#'   calculate_metrics(threshold = 3)
#' metrics
#' summary(metrics)
#'
#' @export
calculate_metrics <- function(data, ...) {
  UseMethod("calculate_metrics")
}

#' @rdname calculate_metrics
#' @export
calculate_metrics.quic <- function(data, ..., threshold = 2, flip_ratio = FALSE) {

  df <- calculate_metrics(
    as.data.frame(data), ...,
    threshold = threshold, flip_ratio = flip_ratio, zeroed = data$params$zero
  )

  groupings <- c(...)
  if (is_empty(groupings)) {
    groupings <- intersect(c("sample", "dilution", "well"), names(data$data))
  }

  params <- data$params
  params[c("by", "threshold", "flip_ratio")] <- list(groupings, threshold, flip_ratio)

  validate_quic_metrics(new_quic_metrics(as_tibble(df), params))
}

#' @rdname calculate_metrics
#' @export
calculate_metrics.data.frame <- function(data, ..., threshold = 2, time_col = lifecycle::deprecated(), ttt_values = lifecycle::deprecated(),
                                         auc_values = lifecycle::deprecated(), norm_col = lifecycle::deprecated(), deriv_col = lifecycle::deprecated(),
                                         flip_ratio = FALSE, zeroed = FALSE) {

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
    groupings <- intersect(c("sample", "dilution", "well"), names(data))
  }

  list(
    reframe(data, .by = all_of(groupings)),
    calculate_QR(data, by=groupings, flip_ratio = flip_ratio, zeroed = zeroed), # does both MPR and QR
    calculate_MS(data, by=groupings),
    calculate_AUC(data, by=groupings),
    calculate_TtT(data, threshold, by=groupings, zeroed = zeroed)
  ) %>%
    reduce(left_join) %>%
    suppressMessages()
}
