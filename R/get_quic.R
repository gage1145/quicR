#' Get all time-series and metadata.
#'
#' Accepts an Excel file or a data frame of real-time RT-QuIC data.
#'
#' @param file An Excel file exported by BMG.
#' @param transpose_table `r lifecycle::badge("deprecated")` This argument no longer has any effect.
#' @param norm_point Integer, defines the cycle to use as background fluorescence.
#' @param which_table Integer, defines which table in the Excel sheet contains the real-time data. Should usually be set to 1.
#' @param window_size Integer, defines the window size for estimating the derivative.
#' @param .by `r lifecycle::badge("deprecated")` Use "by" instead.
#' @param by Grouping factor. Should typically be by individual wells.
#' @param plate Integer; either 96 or 384 to denote the type of well plate being used.
#'
#' @return A data frame containing all time-series data and sample/plate metadata.
#'
#' @import dplyr
#' @importFrom tidyr pivot_longer
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
get_quic <- function(file, transpose_table=lifecycle::deprecated(), norm_point=2, which_table=1,
                     window_size=2, smooth = FALSE, smooth_factor = 10, zero=FALSE, 
                     .by=lifecycle::deprecated(), by=c("Well"), plate=96, sheet = 2) {

  if (lifecycle::is_present(transpose_table)) {
    lifecycle::deprecate_warn(
      when = "3.0.8", 
      what = "get_quic(transpose_table)"
    )
  }

  if (lifecycle::is_present(.by)) {
    lifecycle::deprecate_warn(
      when = "3.0.8", 
      what = "get_quic(smooth)"
    )
    by <- .by
  }
  
  data <- read_xlsx(file, sheet=sheet, col_names=FALSE) %>%
    suppressMessages()

  get_real(sheets[[2]], transpose_table=transpose_table)[[which_table]] %>%
    mutate(
      "Sample IDs" = meta[["Sample IDs"]],
      "Wells" = meta[["Wells"]],
      "Dilutions" = {if ("Dilutions" %in% colnames(meta)) meta[["Dilutions"]] else NA},
      .after = "Sample IDs"
    ) %>%
    pivot_longer(4:ncol(.), names_to="Time", values_to="RFU") %>%
    mutate_at(c("Time", "RFU"), as.numeric) %>%
    group_by(across(all_of(.by))) %>%
    mutate(
      Norm = RFU/RFU[norm_point],
      Deriv = (lead(Norm, window_size) - lag(Norm, window_size)) / (lead(Time, window_size) - lag(Time, window_size))
    ) %>%
    ungroup()
}
