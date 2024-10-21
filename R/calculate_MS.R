#' Calculate Maximum Slope
#'
#' Uses a sliding window to calculate the slope of real-time reads.
#'
#' @param data A dataframe containing real-time reads. It is recommended to use a dataframe made from normalize_RFU.
#' @param window Integer designating how wide you want the sliding window to be for calculating the moving average slope.
#'
#' @return A dataframe containing the real-time slope values.
#'
#' @importFrom slider slide
#' @importFrom dplyr %>%
#' @importFrom dplyr slice
#' @importFrom dplyr mutate_all
#' @importFrom dplyr select_at
#'
#' @export
calculate_MS <- function(data, window = 3) {
  # Calculate the slope using a moving window linear regression.
  df_norm_t <- t(data)
  colnames(df_norm_t) <- df_norm_t[1, ]
  df_norm_t <- df_norm_t[-1, ]
  df_norm_t <- cbind(Time = as.numeric(rownames(df_norm_t)), df_norm_t)
  df_norm_t <- as.data.frame(df_norm_t)

  unique_cols <- colnames(df_norm_t)[1]

  x <- 1
  for (i in colnames(df_norm_t)[-1]) {
    unique_cols <- cbind(unique_cols, paste0(i, "_", x))
    x <- x + 1
  }

  unique_cols <- gsub("-", "_", unique_cols, fixed = TRUE)

  colnames(df_norm_t) <- unique_cols

  df_deriv <- df_norm_t$Time

  for (row_start in 1:length(df_deriv)) {
    if (!is.na(df_deriv[row_start] == "0")) {
      row_start <- row_start - 1
      break
    }
  }

  # Make sure there are no "-" in the sample IDs. This affects the formula below.
  for (i in colnames(df_norm_t)[-1]) {
    slope_column <- slide(
      df_norm_t[-(1:row_start),],
      ~lm(as.formula(paste0("`", i, "`", " ~ Time")),
          data = .x
      )[[1]][[2]] / 3600,
      .before = window,
      .complete = TRUE
    )
    df_deriv <- cbind(df_deriv, slope_column)
  }

  # Reformat df_deriv to match data formatting.
  df_deriv_1 <- df_deriv %>%
    as.data.frame() %>%
    t() %>%
    as.data.frame() %>%
    slice(-1)
  df_deriv_1 <- select(df_deriv_1, 1:(ncol(df_deriv_1) - window))

  df_deriv_1 <- cbind(data[1:(row_start + 1)], df_deriv_1)
  colnames(df_deriv_1) <- colnames(data)

  df_deriv_1 <- mutate_all(
    df_deriv_1,
    as.character
  )

  MS_list <- apply(df_deriv_1[-(1:(row_start + window + 1))], 1, max)

  return(MS_list)
}
