#' Calculate Time to Threshold
#'
#' Calculates the time required to reach a defined threshold.
#'
#' @param data A dataframe containing real-time RT-QuIC data.
#' @param threshold A numeric value defining the threshold.
#' @param start_col The column containing the starting position of the real-time data.
#'
#' @return A vector containing the times to threshold
#'
#' @examples
#' # This test takes >5 sec
#' \donttest{
#' file <- system.file(
#'   "extdata/input_files",
#'   file = "test2.xlsx",
#'   package = "quicR"
#' )
#' get_real(file)[[1]] |>
#'   quicR::normalize_RFU() |>
#'   calculate_TtT(threshold = 2)
#' }
#'
#' @export
calculate_TtT <- function(data, threshold, start_col = 3) {
  # Get numeric cycle times
  cycles <- as.numeric(colnames(data)[start_col:ncol(data)])
  cycle_interval <- diff(cycles[1:2])  # assumes uniform intervals

  # Extract just the numeric matrix portion
  readings <- as.matrix(data[, start_col:ncol(data)])

  # Apply function across rows
  TtT <- apply(
    readings,
    1,
    function(row) {
      # Find first index where threshold is crossed
      cross_idx <- which(row >= threshold)[1]

      if (!is.na(cross_idx)) {
        if (cross_idx == 1) {
          return(cycles[1])  # threshold met at first timepoint
        }

        # Linear interpolation
        prev <- row[cross_idx - 1]
        curr <- row[cross_idx]
        slope <- curr - prev

        if (slope == 0) return(cycles[cross_idx])  # avoid division by zero

        delta_threshold <- threshold - prev
        delta_time <- delta_threshold / slope

        return(cycles[cross_idx - 1] + delta_time)
      } else {
        # If never crossed, return last cycle time
        return(cycles[length(cycles)])
      }
    }
  )

  return(TtT)
}
