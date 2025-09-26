#' Calculate Time to Threshold
#'
#' Calculates the time required to reach a defined threshold.
#'
#' @param data A dataframe containing real-time RT-QuIC data.
#' @param threshold A numeric value defining the threshold.
#' @param time Column containing your time values.
#' @param values Column containing your fluorescence values.
#' @param .by Grouping factor. Should typically be by individual wells.
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
  grouping <- sym(.by)

  crossings <- data %>%
    {if (is_grouped_df(.)) . else group_by(., !!grouping)} %>%
    filter(.data[[values]] > threshold) %>%
    reframe()

  data %>%
    {if (is_grouped_df(.)) . else group_by(., !!grouping)} %>%
    rename("y" = !!sym(values), "x" = !!sym(time)) %>%
    mutate_at(c("x", "y"), as.numeric) %>%
    summarize(
      "x2" = ifelse(
        !is_empty(.data$x[.data$y > threshold]),
        min(.data$x[.data$y > threshold]),
        max(.data$x)
      ),
      x1 = .data$x2 - dt,
      y2 = .data$y[.data$x == .data$x2],
      y1 = .data$y[.data$x == .data$x1],
      TtT = ifelse(
        .data$crossed,
        .data$x1 + (threshold - .data$y1) * (.data$x2 - .data$x1) / (.data$y2 - .data$y1),
        .data$x2
      )
    ) %>%
    mutate(RAF = 1/.data$TtT) %>%
    select(!!sym(.by), "TtT", "RAF")
}
