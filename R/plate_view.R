#' Real-Time Plate View
#'
#' Converts the real-time data into a ggplot figure. The layout is either 8x12
#' or 16x24 for 96- and 384-well plates, respectively.
#'
#' @param data Either an S3 object of class "quic" returned by [get_quic()], or a
#'   long-format data frame with `well`, `sample`, `dilution`, `time`, `norm`
#'   and `deriv` columns.
#' @param ... Arguments passed on to the data frame method.
#' @param plate Integer either 96 or 384 to denote microplate type. For "quic"
#'   objects, defaults to the plate size recorded by [get_quic()].
#' @param sep A string defining how sample IDs and dilutions should be separated.
#' @param plot_deriv Logical; should the derivative be plotted?
#' @param flu_color Line color of the fluorescent data.
#' @param der_color Line color of the derivative data.
#'
#' @return A ggplot object
#'
#' @import ggplot2
#' @import dplyr
#' @importFrom tidyr replace_na
#' @importFrom tidyr separate
#' @importFrom tidyr unite
#' @importFrom stringr str_length
#' @importFrom stats setNames
#'
#' @examples
#' file <- system.file(
#'   "extdata/input_files",
#'   file = "test2.xlsx",
#'   package = "quicR"
#' )
#'
#' get_quic(file) |>
#'   plate_view(plot_deriv = FALSE)
#'
#' @export
plate_view <- function(data, ...) {
  UseMethod("plate_view")
}

#' @rdname plate_view
#' @export
plate_view.quic <- function(data, plate = data$params$plate, ...) {
  plate_view(as.data.frame(data), plate = plate, ...)
}

#' @rdname plate_view
#' @export
plate_view.data.frame <- function(data, plate=96, sep="\n", plot_deriv=TRUE,
                                  flu_color="black", der_color="blue", ...) {

  if (plate != 96 & plate != 384) {
    return("Invalid plate layout. Format should be either 96 or 384. ")
  }

  wells <- expand.grid(
    {if (plate == 96) LETTERS[1:8] else LETTERS[1:16]},
    {if (plate == 96) sprintf("%02d", 1:12) else sprintf("%02d", 1:24)}
  ) %>%
    unite("well", 1, 2, sep="") %>%
    arrange(well)

  labels_lookup <- data %>%
    reframe(.by = c("well", "sample", "dilution")) %>%
    right_join(wells, by = "well") %>%
    mutate(
      across(everything(), ~ replace_na(as.character(.x), " ")),
      well = setNames(paste(sample, dilution, sep=sep), well)
    ) %>%
    pull(well)

  data %>%
    right_join(wells, by = "well") %>%
    mutate(
      dilution = as.factor(dilution),
      well = factor(well, levels = wells$well)
    ) %>%
    arrange(well, time) %>%
    ggplot(aes(time)) +
    geom_line(aes(y=norm), color=flu_color, na.rm=TRUE) +
    {if (plot_deriv) geom_line(aes(y=deriv), color=der_color, na.rm=TRUE)} +
    facet_wrap(
      vars(well),
      nrow = ifelse(plate == 96, 8, 16),
      ncol = ifelse(plate == 96, 12, 24),
      labeller = as_labeller(labels_lookup)
    ) +
    labs(
      y = "RFU",
      x = "Time (h)"
    ) +
    theme_classic() +
    theme(
      panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5),
      strip.background = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank()
    )
}
