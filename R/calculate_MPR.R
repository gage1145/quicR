#' Calculate the Maxpoint Ratio
#'
#' Maxpoint ratio is defined as the maximum relative fluorescence divided by the
#' background fluorescence.
#'
#' @param data A dataframe containing the real-time fluorescence data.
#' @param start_col Integer, the column at which the background fluorescence should be read.
#' @param data_is_norm Logical, if the data has not been normalized, will make a call to normalize_RFU.
#' @return A vector containing MPR values.
#'
#' @examples
#' # This test takes >5 sec
#' \donttest{
#'   file <- system.file(
#'     "extdata/input_files",
#'     file = "test.xlsx",
#'     package = "quicR"
#'     )
#'   df_ <- quicR::get_real(file)[[1]]
#'   print(calculate_MPR(df_))
#' }
#'
#' @export
calculate_MPR <- function(data, start_col = 3, data_is_norm = FALSE) {
  if (data_is_norm == FALSE) {
    # Calculate the normalized real-time data.
    data <- normalize_RFU(data)
  }

  # Initialize the list containing the maxpoint ratios.
  MPR_list <- c(rep(NA, nrow(data)))

  # Identify the maxpoint ratio.
  for (i in 1:nrow(data)) {
    maximum <- max(data[i, start_col:(ncol(data))])
    MPR_list[i] <- maximum
  }
  return(MPR_list)
}
