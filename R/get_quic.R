#' Get all time-series and metadata.
#'
#' Accepts an Excel file or a data frame of real-time RT-QuIC data.
#'
#' @param file An Excel file exported by BMG.
#' @param transpose_table Logical, if true, will transpose the table(s). This should almost always be true.
#' @param norm_point Integer, defines the cycle to use as background fluorescence.
#' @param which_table Integer, defines which table in the Excel sheet contains the real-time data. Should usually be set to 1.
#' @param window_size Integer, defines the window size for estimating the derivative.
#' @param .by Grouping factor. Should typically be by individual wells.
#'
#' @return A data frame containing all time-series data and sample/plate metadata.
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr mutate_at
#' @importFrom dplyr group_by
#' @importFrom dplyr lag
#' @importFrom dplyr lead
#' @importFrom tidyr pivot_longer
#' @importFrom magrittr %>%
#'
#' @examples
#' file <- system.file(
#'   "extdata/input_files",
#'   file = "test.xlsx",
#'   package = "quicR"
#' )
#' get_quic(file)
#'
#' @export
get_quic <- function(file, transpose_table=TRUE, norm_point=2, which_table=1,
                     window_size=2, .by="Wells") {

  sheets <- lapply(1:2, function(x) suppressMessages(read_xlsx(file, sheet=x)))

  meta <- organize_tables(sheets[[1]]) %>%
    convert_tables()

  get_real(sheets[[2]], transpose_table=transpose_table)[[which_table]] %>%
    mutate(
      "Sample IDs" = meta[["Sample IDs"]],
      "Wells" = meta[["Wells"]],
      "Dilutions" = ifelse("Dilutions" %in% colnames(meta), meta[["Dilutions"]], NA),
      .after = "Sample IDs"
    ) %>%
    pivot_longer(4:ncol(.), names_to="Time", values_to="RFU") %>%
    mutate_at(c("Time", "RFU"), as.numeric) %>%
    group_by(., !!sym(.by)) %>%
      mutate(
        Norm = .data$RFU/.data$RFU[norm_point],
        Deriv = (lead(.data$Norm, window_size) - lag(.data$Norm, window_size)) / (lead(.data$Time, window_size) - lag(.data$Time, window_size))
      ) %>%
      ungroup()
}
