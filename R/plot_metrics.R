#' Plot metrics generated from the "calculate" family of quicR functions.
#'
#' Generates a faceted figure of boxplots.
#'
#' @param data Either an S3 object of class "quic_metrics" returned by
#'   [calculate_metrics()], or a data frame of metrics.
#' @param ... A list of faceting columns. If empty, groups are set to "mpr", "ms", "ttt", and "raf".
#' @param sample_col The name of the column containing the sample IDs.
#' @param color The color used for the boxplot outline.
#' @param fill The column containing the fill aesthetic. Usually the dilution column.
#' @param dilution_bool Logical; should dilution factors be included in the plot?
#' @param nrow Integer; number of rows to output in the plot.
#' @param ncol Integer; number of columns to output in the plot.
#'
#' @importFrom dplyr %>%
#' @importFrom tidyr pivot_longer
#' @import ggplot2
#'
#' @return A ggplot object
#'
#' @examples
#' file <- system.file(
#'   "extdata/input_files",
#'   file = "test2.xlsx",
#'   package = "quicR"
#' )
#'
#' get_quic(file) |>
#'   calculate_metrics(threshold = 3) |>
#'   plot_metrics()
#'
#' @export
plot_metrics <- function(data, ...) {
  UseMethod("plot_metrics")
}

#' @rdname plot_metrics
#' @export
plot_metrics.quic_metrics <- function(data, ...) {
  plot_metrics(data$data, ...)
}

#' @rdname plot_metrics
#' @export
plot_metrics.data.frame <- function(data, ..., sample_col = "sample", color="black", fill = "dilution", dilution_bool = TRUE, nrow = 2, ncol = 2) {

  groupings <- c(...)
  groupings <- {if (is_empty(groupings)) c("mpr", "ms", "ttt", "raf") else groupings}
  data %>%
    pivot_longer(all_of(groupings)) %>%
    ggplot(
      aes(!!sym(sample_col), .data$value,
        fill = if (dilution_bool) as.factor(!!sym(fill))
      )
    ) +
    geom_boxplot(position = position_dodge2(preserve = "single"), color=color) +
    {if (length(groupings) > 1) {
      facet_wrap(~.data$name, scales = "free_y", nrow = nrow, ncol = ncol)
    }} +
    {if (dilution_bool) {
      labs(fill = fill)
    }} +
    theme(
      legend.position = "bottom",
      strip.text = element_text(face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
      axis.title = if(length(groupings) > 1) element_blank() else element_text()
    )
}
