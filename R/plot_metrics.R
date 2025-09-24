#' Plot metrics generated from the "calculate" family of quicR functions.
#'
#' Generates a faceted figure of boxplots.
#'
#' @param data A dataframe containing the calculated metrics from the "calculate" family of quicR functions.
#' @param ... A list of faceting columns.
#' @param sample_col The name of the column containing the sample IDs.
#' @param color The color used for the boxplot outline.
#' @param fill The column containing the fill aesthetic. Usually the dilutions column.
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
#' \donttest{
#' file <- system.file(
#'   "extdata/input_files",
#'   file = "raw.csv",
#'   package = "quicR"
#' )
#'
#' df <- read.csv(file, check.names=FALSE)
#'
#' calculate_metrics(df, "Sample IDs", "Dilutions", "Wells") |>
#'   plot_metrics("MPR", "MS", "TtT", "RAF")
#' }
#'
#' @export
plot_metrics <- function(data, ..., sample_col = "Sample IDs", color="black", fill = "Dilutions", dilution_bool = TRUE, nrow = 2, ncol = 2) {

  data %>%
    pivot_longer(c(...)) %>%
    ggplot(
      aes(!!sym(sample_col), .data$value,
        fill = if (dilution_bool) as.factor(!!sym(fill))
      )
    ) +
    geom_boxplot(position = position_dodge2(preserve = "single"), color=color) +
    {if (length(c(...)) > 1) {
      facet_wrap(~.data$name, scales = "free_y", nrow = nrow, ncol = ncol)
    }} +
    {if (dilution_bool) {
      labs(fill = fill)
    }} +
    theme(
      legend.position = "bottom",
      strip.text = element_text(face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
      axis.title = if(length(c(...)) > 1) element_blank() else element_text()
    )
}
