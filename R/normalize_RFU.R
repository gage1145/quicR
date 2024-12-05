#' Normalize Fluorescence
#'
#' Normalizes the real-time RT-QuIC data against the background fluorescence of
#' a defined cycle. All cycles are divided by the fluorescent value of the
#' defined cycle.
#'
#' @param data A dataframe generated from get_real.
#' @param bg_cycle The cycle used for background fluorescence
#'
#' @return A dataframe containing real-time normalized fluorescence values.
#'
#' @examples
#' # This test takes >5 sec
#' \donttest{
#'   file <- system.file(
#'     "extdata/input_files",
#'     file = "test2.xlsx",
#'     package = "quicR"
#'   )
#'   df_ <- get_real(file)[[1]]
#'
#'   # Export the tables in the first sheet of the file.
#'   dic <- quicR::organize_tables(file)
#'
#'   # Apply the column names.
#'   sample_ids <- convert_tables(dic)$`Sample IDs`
#'   colnames(df_) <- c("Time", sample_ids)
#'
#'   # Transpose the table
#'   df_ <- quicR::transpose_real(df_)
#'
#'   # Normalize the raw data against the background reading.
#'   normalize_RFU(df_)
#' }
#'
#'
#' @export
normalize_RFU <- function(data, bg_cycle = 4) {
    df_norm <- data
    for (i in 1:nrow(data)) {
      for (j in 2:ncol(data)) {
        raw_value <- df_norm[i, j]
        df_norm[i, j] <- raw_value / data[i, bg_cycle + 1]
      }
    }
  return(df_norm)
}
