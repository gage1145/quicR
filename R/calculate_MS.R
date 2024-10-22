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

  df_norm_t <- data %>%
    t() %>%
    as.data.frame() %>%
    mutate_all(
      ~ as.numeric(as.character(.x))
    ) %>%
    suppressWarnings() %>%
    na.omit()

  df_norm_t$Time <- rownames(df_norm_t)
  df_deriv <- data.frame(Time = df_norm_t$Time)

  for (i in colnames(df_norm_t)[-ncol(df_norm_t)]) {

    formula <- as.formula(paste0("`", i, "`", " ~ Time"))

    slope_column <- df_norm_t %>%
      slide(
        ~lm(formula, data = .x)[[1]][[2]] / 3600,
        .before = window,
        .complete = TRUE
      ) %>%
      as.character()
    df_deriv <- cbind(df_deriv, slope_column)
  }

  df_deriv <- df_deriv %>%
    t() %>%
    as.data.frame() %>%
    slice(-1) %>%
    select(-all_of(1:window)) %>%
    mutate_all(as.numeric)

  MS_list <- apply(df_deriv, 1, function(x) max(as.numeric(x)))

  return(MS_list)
}
