#' Calculate Time to Threshold
#'
#' Calculates the time required to reach a defined threshold.
#'
#' @param data A dataframe containing real-time RT-QuIC data.
#' @param threshold A positive numeric value defining the threshold.
#' @param time `r lifecycle::badge("deprecated")`
#' @param values `r lifecycle::badge("deprecated")`
#' @param .by `r lifecycle::badge("deprecated")` Use "by" instead.
#' @param by Grouping factor(s). Should typically be by individual wells. Can be supplied a vector as an argument.
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
calculate_TtT <- function(data, threshold, time=lifecycle::deprecated(), values=lifecycle::deprecated(), .by=lifecycle::deprecated(), 
                          by="well", zeroed=FALSE, use_normalized=TRUE) {

  if (lifecycle::is_present(.by)) {
    lifecycle::deprecate_warn(
      when = "3.2.0", 
      what = "calculate_TtT(.by)"
    )
    by <- .by
  }

  # check_positive <- function(threshold, min_y) {
  #   stopifnot(threshold > min_y)
  #   return("Threshold must be a positive number")
  # }
  # min_y = min(data$norm, na.rm=TRUE)

  # check_positive(threshold, min_y)
  
  if (zeroed) threshold <- threshold - 1

  data %>%
    {if (is_grouped_df(.)) . else group_by(., across(all_of(by)))} %>%
    mutate(across(c(time, norm), as.numeric)) %>%
    summarize(
      crossed  = max(norm) > threshold,
      x2       = ifelse(!is_empty(time[norm > threshold]), min(time[norm > threshold]), max(time)),
      x2_index = row_number(time)[time == x2][1],
      x1       = time[x2_index - 1],
      y2       = norm[x2_index],
      y1       = norm[x2_index - 1],
      ttt      = ifelse(crossed, x1 + (threshold - y1) * (x2 - x1) / (y2 - y1), x2)
    ) %>%
    mutate(raf = 1/ttt) %>%
    select(all_of(by), "ttt", "raf", "crossed")
}
