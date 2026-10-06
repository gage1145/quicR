#' Calculate the Quenching Ratio
#' 
#' The quenching ratio (QR) is defined as the ratio between the maximum initial value
#' after the exponential phase divided by the last measured value.
#' 
#' @param data A data frame output from 'get_quic()'.
#' @param col `r lifecycle::badge("deprecated")`
#' @param time_col `r lifecycle::badge("deprecated")`
#' @param flip_ratio Logical; Should the ratio be calculated as max / last (default), or last / max?
#' @param .by `r lifecycle::badge("deprecated")` Use "by" instead.
#' @param by Grouping factor. Should typically be by individual wells.
#' @return A data frame containing well-matched quenching ratio values.
#'
#' @importFrom dplyr summarize
#' @importFrom dplyr sym
#' @importFrom dplyr syms
#' @importFrom dplyr group_by
#' @importFrom dplyr is_grouped_df
#' @importFrom dplyr last
#' @importFrom dplyr %>%
#'
#' @examples
#' file <- system.file(
#'   "extdata/input_files",
#'   file = "test2.xlsx",
#'   package = "quicR"
#' )
#' get_quic(file) |>
#'   calculate_QR()
#'
#' @export
calculate_QR <- function(data, col=lifecycle::deprecated(), time_col=lifecycle::deprecated(), 
                         .by=lifecycle::deprecated(), by="well", flip_ratio=FALSE, zeroed=FALSE) {

  if (lifecycle::is_present(.by)) {
    lifecycle::deprecate_warn(
      when = "3.2.0", 
      what = "calculate_QR(.by)"
    )
    by <- .by
  }

  data$norm <- data$norm + zeroed

  data %>%
    {if (is_grouped_df(.)) . else group_by(., across(all_of(by)))} %>%
    summarize(
      mpr = max(norm, na.rm=TRUE),
      qr = mpr / last(norm, time),
      qr = ifelse(flip_ratio, 1 / qr, qr)
    )
}
