#' Calculate Maximum Slope
#'
#' Uses a sliding window to calculate the slope of real-time reads.
#'
#' @param data A dataframe containing real-time reads. It is recommended to use a dataframe made from normalize_RFU.
#' @param window Integer designating how wide you want the sliding window to be for calculating the moving average slope.
#' @param data_is_norm Logical; if FALSE, will make a call to normalize_RFU.
#'
#' @return A dataframe containing the real-time slope values.
#'
#' @importFrom slider slide
#' @importFrom dplyr slice
#' @importFrom dplyr mutate_all
#' @importFrom dplyr %>%
#' @importFrom dplyr select_at
#' @importFrom stats as.formula
#'
#' @examples
#' # This test takes >5 sec
#' \donttest{
#' file <- system.file(
#'   "extdata/input_files",
#'   file = "rt_data.csv",
#'   package = "quicR"
#' )
#' df_ <- read.csv(file, check.names = FALSE)
#' calculate_MS(df_)
#' }
#'
#' @export
calculate_MS <- function(data, window = 3, data_is_norm = TRUE) {
  curate <- function(x) {
    x %>%
      {if (data_is_norm) . else normalize_RFU(.)} %>%
      t() %>%
      as.data.frame() %>%
      mutate_all(~ as.numeric(as.character(.))) %>%
      suppressWarnings() %>%
      na.omit() %>%
      mutate(Time = rownames(.))
  }

  slope <- function(x) {
    max_slopes <- c()

    for (i in colnames(x)[-ncol(x)]) {
      max_slopes <- max_slopes %>%
        rbind(
          x %>%
            slide(
              ~ lm(
                as.formula(paste0("`", i, "`", " ~ Time")),
                data = .x
              )[[1]][[2]] / 3600,
              .before = window,
              .complete = TRUE
            ) %>%
            replace(. == "NULL", NA) %>%
            as.numeric() %>%
            max(na.rm = TRUE)
        )
    }

    return(max_slopes)
  }

  return(slope(curate(data)))
}
