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
#' @import dplyr
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
calculate_threshold <- function(data, values="rfu", time=lifecycle::deprecated(),
                                background_time=0, method=c("stdev"),
                                multiplier=1) {

  values <- sym(values)
  if (is.array(method)) method <- method[1]
  # if (method == "none") return(NA_real_)
  if (method == "stdev") {
    data %>%
      as.data.frame() %>%
      filter(time == background_time) %>%
      summarize(
        avg = mean(!!values),
        std = sd(!!values),
        thr = avg + std * multiplier
      ) %>%
      pull(thr) %>%
      pluck(1)
  }
}
