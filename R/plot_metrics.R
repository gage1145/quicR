#' Plot metrics generated from the "calculate" family of quicR functions.
#'
#' Generates a faceted figure of boxplots.
#'
#' @param data A dataframe containing the calculated metrics from the "calculate" family of quicR functions.
#' @param ... A selection of columns to be used as faceting variables. Typically this includes the sample IDs and dilution factors.
#' @param dilution_bool Logical; should dilution factors be included in the plot?
#'
#' @importFrom dplyr %>%
#' @importFrom tidyr gather
#' @import ggplot2
#'
#' @return A ggplot object
#'
#' @examples
#' file <- system.file(
#'   "extdata/input_files",
#'   file = "test4.xlsx",
#'   package = "quicR"
#' )
#'
#' raw <- quicR::get_real(file)[[1]] |>
#'   quicR::normalize_RFU()
#'
#' meta <- quicR::organize_tables(file) |>
#'   quicR::convert_tables()
#'
#' data.frame(
#'   `Sample IDs` = meta$`Sample IDs`,
#'   `Dilutions`  = meta$Dilutions,
#'   check.names = FALSE
#' ) |>
#'   dplyr::mutate(
#'     MPR = quicR::calculate_MPR(raw),
#'     MS  = quicR::calculate_MS(raw),
#'     TtT = quicR::calculate_TtT(raw, 2),
#'     RAF = 1 / TtT
#'   ) |>
#'   plot_metrics("Sample IDs", "Dilutions")
#'
#' @export
plot_metrics <- function(data, ..., dilution_bool = TRUE, nrow = 2, ncol = 2) {

  data %>%
    gather("variable", "value", -c(...)) %>%
    ggplot(
      aes(!!sym(c(...)[1]),
          .data$value,
          fill = if (dilution_bool) as.factor(!!sym(c(...)[2])))
    ) +
    geom_boxplot(
      position = position_dodge2(preserve = "single")
    ) +
    facet_wrap(~variable, scales = "free_y", nrow = nrow, ncol = ncol) +
    labs(fill = if (dilution_bool) "Dilutions") +
    theme(
      legend.position = "bottom",
      strip.text = element_text(face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
      axis.title = element_blank()
  )
}
