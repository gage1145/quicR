#' Normalize Fluorescence
#'
#' Normalizes the real-time RT-QuIC data against the background fluorescence of
#' a defined cycle. All cycles are divided by the fluorescent value of the
#' defined cycle.
#'
#' @param df A dataframe made from get_real.
#' @param bg_cycle The cycle used for background fluorescence
#'
#' @return A dataframe containing real-time normalized fluorescence values.
#'
#' @export
normalize_RFU <- function (df, bg_cycle=4) {
  # Accepts output from get_real function.
  # Apply the column names as the first row instead.
  df <- rbind(colnames(df), df)
  df <- df %>%
    unname() %>%
    t() %>%
    as.data.frame()

  # Rename first cell as "Sample ID".
  df[1,1] <- "Sample ID"

  # Make all columns numeric except for the first column with sample IDs.
  # Combine the sample ID column with the numeric columns
  df <- cbind(df[,1],
              mutate_all(df[, -1],
                         function(x) as.numeric(as.character(x))))

  # Make the first row the column names for the df and remove first row.
  colnames(df) <- df[1,]
  df <- df[-1,]

  df_norm <- df
  for (i in 1:nrow(df)) {
    for (j in 2:ncol(df)) {
      raw_value <- df_norm[i,j]
      df_norm[i,j] <- raw_value / df[i, bg_cycle + 1]
    }
  }
  return (df_norm)
}
