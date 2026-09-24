#' Calculate Time to Threshold
#'
#' Calculates the time required to reach a defined threshold.
#'
#' @param data A dataframe containing real-time RT-QuIC data.
#' @param threshold A positive numeric value defining the threshold.
#' @param time Column containing your time values.
#' @param values Column containing your fluorescence values.
#' @param .by `r lifecycle::badge("deprecated")` Use "by" instead.
#' @param by Grouping factor(s). Should typically be by individual wells. Can be supplied a vector as an argument.
#' @param zero Logical; has the data been zeroed by `get_quic()`? Translates the background fluorescence back to 1.
#'
#' @return A vector containing the times to threshold
#'
#' @import dplyr
#' @importFrom purrr is_empty
#'
#' @examples
#' file <- system.file(
#'   "extdata/input_files",
#'   file = "test2.xlsx",
#'   package = "quicR"
#' )
#' get_quic(file) |>
#'   calculate_TtT(threshold = 3)
#'
#' @export
calculate_TtT <- function(data, threshold, time="Time", values="Norm", .by=lifecycle::deprecated(), by="Well", zero=FALSE) {

  if (lifecycle::is_present(.by)) {
    lifecycle::deprecate_warn(
      when = "3.2.0", 
      what = "calculate_TtT(.by)"
    )
    by <- .by
  }

  check_positive <- function(threshold, min_y) {
    stopifnot(threshold > min_y)
    return("Threshold must be a positive number")
  }

  values <- sym(values)
  time <- sym(time)
  min_y = min(data[[values]])

  check_positive(threshold, min_y)
  
  if (zero) threshold <- threshold - 1

  data %>%
    ungroup() %>%
    rename(y = !!values, x = !!time) %>%
    mutate(across(c("x", "y"), as.numeric)) %>%
    summarize(
      max_y    = max(y),
      crossed  = max_y > threshold,
      x2       = ifelse(!is_empty(x[y > threshold]), min(x[y > threshold]), max(x)),
      x2_index = row_number(x)[x == x2][1],
      x1       = x[x2_index - 1],
      y2       = y[x2_index],
      y1       = y[x2_index - 1],
      TtT      = ifelse(crossed, x1 + (threshold - y1) * (x2 - x1) / (y2 - y1), x2),
      .by = all_of(by)
    ) %>%
    mutate(RAF = 1/TtT) %>%
    select(all_of(by), "TtT", "RAF", "crossed")
}
