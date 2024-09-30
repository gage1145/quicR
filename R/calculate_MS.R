#' Calculate Maximum Slope
#'
#' Uses a sliding window to calculate the slope of real-time reads.
#'
#' @param data A dataframe containing real-time reads. It is recommended to use a dataframe made from normalize_RFU.
#' @param start_col The column containing the starting position of the real-time data.
#'
#' @return A dataframe containing the real-time slope values.
#'
#' @importFrom slider slide
#' @importFrom dplyr %>%
#' @importFrom dplyr slice
#' @importFrom dplyr mutate_all
#'
#' @export
calculate_MS <- function(data, start_col = 3) {
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

  # Make sure there are no "-" in the sample IDs. This affects the formula below.
  for (i in colnames(df_norm_t)[-1]) {
    slope_column <- slide(df_norm_t,
      ~ lm(as.formula(paste0("`", i, "`", " ~ Time")),
        data = .x
      )[[1]][[2]] / 3600,
      .before = 3,
      .complete = TRUE
    )
    df_deriv <- cbind(df_deriv, slope_column)
  }

  # Reformat df_deriv to match data formatting.
  df_deriv <- df_deriv %>%
    as.data.frame() %>%
    t() %>%
    as.data.frame() %>%
    slice(-c(1))

  df_deriv <- cbind(data$`Sample ID`, df_deriv)
  colnames(df_deriv) <- colnames(data)

  # Convert df_deriv to numeric values.
  df_deriv[, -1] <- mutate_all(
    df_deriv[, -1],
    function(x) as.numeric(as.character(x))
  )

  # Initialize the list containing the max slope.
  MS_list <- c(rep(NA, nrow(data)))

  for (i in 1:nrow(df_deriv)) {
    MS_list[i] <- max(df_deriv[i, 5:(ncol(df_deriv))])
  }
  return(MS_list)
}
