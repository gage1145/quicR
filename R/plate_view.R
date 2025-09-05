#' Real-Time Plate View
#'
#' Converts the real-time data into a ggplot figure. The layout is either 8x12
#' or 16x24 for 96- and 384-well plates, respectively.
#'
#' @param data Real-time dataframe
#' @param plate Integer either 96 or 384 to denote microplate type.
#'
#' @return A ggplot object
#'
#' @import ggplot2
#' @import dplyr
#' @importFrom tidyr replace_na
#' @importFrom tidyr separate
#' @importFrom stringr str_length
#' @importFrom reshape2 melt
#'
#' @examples
#' file <- system.file(
#'   "extdata/input_files",
#'   file = "test2.xlsx",
#'   package = "quicR"
#' )
#'
#' # Get the real-time data.
#' df_ <- get_quic(file)
#'
#' plate_view(df_)
#'
#' @export
plate_view <- function(data, plate = 96) {

  if (plate != 96 & plate != 384) {
    return("Invalid plate layout. Format should be either 96 or 384. ")
  }

  unique_cols <- data %>%
    select(1:2) %>%
    unique()

  # Create the labeller function for the facet plot.
  id_labeller <- function(variable, value) {
    i <- unique_cols["Sample IDs"][unique_cols["Wells"] == value]
    ifelse(is.na(i), " ", i)
  }

  data %>%
    ggplot(aes(.data$Time)) +
    geom_line(aes(y=.data$Norm), color="black") +
    geom_line(aes(y=.data$Deriv), color="blue") +
    facet_wrap(
      vars(.data$Wells),
      nrow = ifelse(plate == 96, 8, 16),
      ncol = ifelse(plate == 96, 12, 24),
      labeller = id_labeller
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
    ) %>%
    suppressWarnings()
}
