#' Normalize Fluorescence
#'
#' Normalizes the real-time RT-QuIC data against the background fluorescence of
#' a defined cycle. All cycles are divided by the fluorescent value of the
#' defined cycle.
#'
#' @param data A dataframe generated from get_real.
#' @param bg_cycle The cycle used for background fluorescence
#' @param transposed Logical, TRUE if cycle values are shown as column names.
#'
#' @return A dataframe containing real-time normalized fluorescence values.
#'
#' @examples
#' # This test takes >5 sec
#' \donttest{
#' file <- system.file(
#'   "extdata/input_files",
#'   file = "test2.xlsx",
#'   package = "quicR"
#' )
#' df_ <- get_real(file)[[1]]
#'
#' # Normalize the raw data against the background reading.
#' normalize_RFU(df_)
#' }
#'
#' @export
normalize_RFU <- function(data, bg_cycle = 4, transposed = FALSE) {

  curate <- function(x) {
    colnames(x) %>%
      rbind(x) %>%
      unname() %>%
      t() %>%
      as.data.frame() %>%
      row_to_names(1) %>%
      rename("Sample IDs" = "Time") %>%
      mutate_at(-c(1), function(y) as.numeric(as.character(y)))
  }

  normalize <- function(x) {
    cbind(x[1], sweep(x[-1], 1, x[[bg_cycle + 1]], FUN = "/"))
  }

  if (transposed) normalize(data) else normalize(curate(data))
}
