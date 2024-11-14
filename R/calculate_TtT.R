#' Calculate Time to Threshold
#'
#' Calculates the time required to reach a defined threshold.
#'
#' @param data A dataframe containing real-time RT-QuIC data.
#' @param threshold A numeric value defining the threshold.
#' @param start_col The column containing the starting position of the real-time data.
#' @param run_time The time in hours that the assay ran.
#'
#' @return A vector containing the times to threshold
#'
#' @examples
#' # This test takes >5 sec
#' \donttest{
#' file <- system.file(
#' "extdata/input_files",
#' file = "test.xlsx",
#' package = "quicR"
#' )
#' df_ <- get_real(file)[[1]] |>
#'   normalize_RFU()
#' calculate_TtT(df_, threshold = 2)
#' }
#'
#'
#' @export
calculate_TtT <- function(data, threshold, start_col = 3, run_time = 48) {
  # Initialize the list containing the times-to-threshold.
  TtT_list <- c(rep(run_time, nrow(data)))

  # Set the cycle interval.
  cycle_interval <- diff(as.numeric(colnames(data[, start_col:(start_col + 1)])))

  # Calculate the Time to Threshold
  for (i in 1:nrow(data)) {
    for (j in start_col:(ncol(data))) {
      # Use the threshold argument.
      current_read <- data[i, j]

      if (TtT_list[i] == run_time & current_read >= threshold) {
        previous_read <- data[i, j - 1]

        delta_rfu <- current_read - previous_read
        slope <- delta_rfu / cycle_interval

        delta_threshold <- threshold - previous_read
        delta_t <- delta_threshold / slope

        previous_cycle <- as.numeric(colnames(data[j - 1]))
        TtT_list[i] <- previous_cycle + delta_t

        break
      }
    }
  }
  return(TtT_list)
}
