#' Calculate a Threshold for Rate Determination
#'
#' Calculates a threshold for determining time-to-threshold and rate of amyloid formation.
#'
#' @param data A dataframe output from get_real.
#' @param values Column containing fluorescent values.
#' @param time Column containing time values.
#' @param background_time Float; the time point used for background fluorescence.
#' @param method Method for determining threshold; default is "stdev".
#' @param multiplier For some methods, will add a multiplier for more conservative thresholds.
#'
#' @importFrom dplyr select
#' @importFrom stats sd
#'
#' @return A float value.
#'
#' @examples
#' file <- system.file(
#'   "extdata/input_files",
#'   file = "test2.xlsx",
#'   package = "quicR"
#' )
#' threshold <- get_quic(file) |>
#'   calculate_threshold(multiplier=10)
#'
#' @export
calculate_threshold <- function(data, values="RFU", time="Time",
                                background_time=0, method=list("stdev", "none"),
                                multiplier=1) {

  if (is.list(method)) method <- "stdev"
  if (method == "none") return(NA)

  if (method == "stdev") {
    (
      data %>%
       filter(!!sym(time) == background_time) %>%
       summarize(
         avg = mean(!!sym(values)),
         std = sd(!!sym(values)),
         thr = .data$avg + .data$std * multiplier
       )
    )$thr
  }

}
