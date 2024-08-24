#' Real-Time Plate View
#'
#' Converts the real-time data into a ggplot figure. The layout is either 8x12
#' or 16x24 for 96- and 384-well plates, respectively.
#'
#' @param df Real-time dataframe
#' @param meta Dataframe containing well IDs and Sample IDs to title each facet.
#' @param plate Integer either 96 or 384 to denote microplate type.
#'
#' @return A ggplot object
#'
#' @import ggplot2
#' @import dplyr
#' @importFrom tidyr replace_na
#' @importFrom tidyr separate
#'
#' @export
plate_view <- function(df, meta, plate=96) {

  if (plate != 96 & plate != 384) {
    return ("Invalid plate layout. Format should be either 96 or 384. ")
  }

  # Ensures that the input is a dataframe.
  df <- data.frame(df)

  colnames(df) <- paste(meta$B, meta$A, sep=".")

  # Create a template of all possible columns
  template_columns <- expand.grid(
    if (plate == 96) {Var1 = LETTERS[1:8]}
    else {Var1 = LETTERS[1:16]},
    if (plate == 96) {Var2 = sprintf("%02d", 1:12)}
    else {Var2 = sprintf("%02d", 1:24)}
  )
  template_columns <- sort(paste0(template_columns$Var1, template_columns$Var2))
  rm(Var1, Var2)

  # Add columns with NAs if they do not exist.
  for (col in template_columns) {
    if (!(col %in% meta$A)) {
      df[[col]] <- NA
    }
  }

  # Add a "Time" column. This is important for the melt function.
  df <- cbind(Time = rownames(df), df)

  # Combine the template_columns and sample_locations.
  template_columns <- as.data.frame(template_columns)
  colnames(template_columns) <- "A"

  # Create a data.frame with all the wells and IDs, even if some are missing.
  full <- sample_locations %>%
    full_join(as.data.frame(template_columns)) %>%
    arrange(A)

  # Create the labeller function for the facet plot.
  ID_labeller <- function(variable, value) {
    i <- full$B[full$A == value]
    ifelse(is.na(i), " ", i)
  }

  df %>%
    # Melt the data to help with the faceting.
    reshape2::melt(id.vars = "Time") %>%
    # Separate the wells from the IDs.
    separate(variable, c("ID", "Well"), "\\.", fill="left") %>%
    # Ensures that Time and observations are numeric.
    mutate(Time = as.numeric(Time),
           value = as.numeric(value),
           ID = as.character(ID),
           Well = as.factor(Well)) %>%
    mutate(ID = replace_na(ID, "none")) %>%
    # Create the facet plot.
    ggplot(aes(x=Time, y=value)) +
    geom_line() +
    labs(y = "RFU",
         x = "Time (h)") +
    theme(
      panel.border = element_rect(colour="black", fill=NA, linewidth=0.5),
      strip.background = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank()
    ) +
    facet_wrap(vars(Well),
               nrow=ifelse(plate == 96, 8, 16),
               ncol=ifelse(plate == 96, 12, 24),
               labeller=ID_labeller) %>%
    suppressWarnings()
}
