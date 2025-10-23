#' Calculate Time to Threshold
#'
#' Calculates the time required to reach a defined threshold.
#'
#' @param data A dataframe containing real-time RT-QuIC data.
#' @param threshold A numeric value defining the threshold.
#' @param time Column containing your time values.
#' @param values Column containing your fluorescence values.
#' @param .by Grouping factor(s). Should typically be by individual wells. Can be supplied a vector as an argument.
#'
#' @return A vector containing the times to threshold
#'
#' @importFrom dplyr group_by
#' @importFrom dplyr rename
#' @importFrom dplyr mutate_at
#' @importFrom dplyr summarize
#' @importFrom dplyr select
#' @importFrom dplyr sym
#' @importFrom dplyr %>%
#' @importFrom purrr is_empty
#'
#' @examples
#' file <- system.file(
#'   "extdata/input_files",
#'   file = "test2.xlsx",
#'   package = "quicR"
#' )
#' get_quic(file) |>
#'   calculate_TtT(threshold = 2)
#'
#' @export
calculate_TtT <- function(data, threshold, time="Time", values="Norm", .by="Wells") {

  dt <- data$Time[2] - data$Time[1]

  .by <- syms(c(.by))
  values <- sym(values)
  time <- sym(time)

  data <- if (is_grouped_df(data)) data else group_by(data, !!!.by)

  data %>%
    rename(y = !!values, x = !!time) %>%
    mutate_at(c("x", "y"), as.numeric) %>%
    summarize(
      crossed = max(y) > threshold,
      x2 = ifelse(
        !is_empty(x[y > threshold]),
        min(x[y > threshold]),
        max(x)
      ),
      x1  = x2 - dt,
      y2  = y[x == x2],
      y1  = y[x == x1],
      TtT = ifelse(
        crossed,
        x1 + (threshold - y1) * (x2 - x1) / (y2 - y1),
        x2
      )
    ) %>%
    mutate(RAF = 1/TtT) %>%
    select(!!!.by, "TtT", "RAF", "crossed")
}
