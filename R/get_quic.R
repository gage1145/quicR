#' Get all time-series and metadata.
#'
#' Accepts an Excel file or a data frame of real-time RT-QuIC data.
#'
#' @param file An Excel file exported by BMG.
#' @param transpose_table Logical, if true, will transpose the table(s). This should almost always be true.
#' @param normalize Logical, if true, will normalize the fluorescent data against the values at a certain cycle defined by 'norm_point'.
#' @param norm_point Integer, defines the cycle to use as background fluorescence.
#' @param which_table Integer, defines which table in the Excel sheet contains the real-time data. Should usually be set to 1.
#' @param .by Grouping factor. Should typically be by individual wells.
#'
#' @return A data frame containing all time-series data and sample/plate metadata.
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr mutate_at
#' @importFrom dplyr group_by
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
get_quic <- function(file, transpose_table=TRUE, normalize=TRUE,
                     norm_point=2, which_table=1, .by="Wells") {

  meta <- organize_tables(file) %>%
    convert_tables()

  # normalize_real <- function(x) {
  #   group_by(x, !!sym(.by)) %>%
  #     mutate(Norm = RFU/RFU[norm_point]) %>%
  #     ungroup()
  # }

  get_real(file, transpose_table=transpose_table)[[which_table]] %>%
    mutate(
      "Sample IDs" = meta[["Sample IDs"]],
      "Wells" = meta[["Wells"]],
      "Dilutions" = ifelse("Dilutions" %in% colnames(meta), meta[["Dilutions"]], NA),
      .after = `Sample IDs`
    ) %>%
    # relocate(Wells, Dilutions, .after=`Sample IDs`) %>%
    pivot_longer(4:ncol(.), names_to="Time", values_to="RFU") %>%
    mutate_at(c("Time", "RFU"), as.numeric) %>%
    {
      if (normalize) {
        group_by(., !!sym(.by)) %>%
          mutate(Norm = RFU/RFU[norm_point]) %>%
          ungroup()
      } else {
        .
      }
    }
}
