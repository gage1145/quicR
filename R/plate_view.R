#' Real-Time Plate View
#'
#' Converts the real-time data into a ggplot figure. The layout is either 8x12
#' or 16x24 for 96- and 384-well plates, respectively.
#'
#' @param data Real-time dataframe
#' @param plate Integer either 96 or 384 to denote microplate type.
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
#' @importFrom stringr str_length
#' @importFrom stats setNames
#'
#' @examples
#' \donttest{
#' file <- system.file(
#'   "extdata/input_files",
#'   file = "raw.csv",
#'   package = "quicR"
#' )
#'
#' # Get the real-time data.
#' df_ <- read.csv(file, check.names=FALSE)
#'
#' plate_view(df_)
#' }
#'
#' @export
plate_view <- function(data, plate=96, sep="\n", plot_deriv=TRUE,
                       flu_color="black", der_color="blue") {

  if (plate != 96 & plate != 384) {
    return("Invalid plate layout. Format should be either 96 or 384. ")
  }

  wells <- data %>%
    select("Sample IDs", "Dilutions", "Wells") %>%
    mutate_at("Dilutions", as.character) %>%
    group_by_at(1:3) %>%
    reframe() %>%
    full_join(
      expand.grid(
        {if (plate == 96) LETTERS[1:8] else LETTERS[1:16]},
        {if (plate == 96) sprintf("%02d", 1:12) else sprintf("%02d", 1:24)}
      ) %>%
        unite("Wells", 1,2, sep="")
    ) %>%
    mutate_all(function(x) replace_na(x, " ")) %>%
    suppressMessages()

  labels_lookup <- setNames(
    paste(wells$`Sample IDs`, wells$Dilutions, sep=sep),
    wells$Wells
  )

  p <- data %>%
    mutate_at("Dilutions", as.character) %>%
    full_join(wells) %>%
    suppressMessages() %>%
    ggplot(aes(.data$Time)) +
    geom_line(aes(y=.data$Norm), color=flu_color) +
    {if (plot_deriv) geom_line(aes(y=.data$Deriv), color=der_color)} +
    facet_wrap(
      vars(.data$Wells),
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

  print(p) %>%
    suppressWarnings() %>%
    suppressMessages()
}
