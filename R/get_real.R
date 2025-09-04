#' Get Real-Time RT-QuIC Fluorescence Data
#'
#' Accepts an Excel file or a dataframe of real-time RT-QuIC data.
#'
#' @param data Either an Excel file or a dataframe.
#' @param order_by_sample Logical, if true, will organize the columns by sample ID rather than by well.
#' @param transpose_table Logical, if true, will transpose the table(s).
#'
#' @return A list of dataframes containing the formatted real-time data.
#'
#' @importFrom dplyr select
#' @importFrom dplyr %>%
#' @importFrom dplyr rename
#' @importFrom readxl read_xlsx
#' @importFrom janitor row_to_names
#' @importFrom janitor clean_names
#'
#' @examples
#' file <- system.file(
#'   "extdata/input_files",
#'   file = "test.xlsx",
#'   package = "quicR"
#' )
#' get_real(file)
#'
#' @export
get_real <- function(data, order_by_sample = FALSE, transpose_table = TRUE) {
  check_format <- function(x) {
    if (is.character(x)) {
      return(suppressMessages(read_xlsx(x, sheet = 2, col_names = FALSE)))
    } else if (is.data.frame(x)) {
      return(x)
    } else {
      stop("Please enter either .xlsx file path or data frame. ")
    }
  }

  curate <- function(x) {
    x %>%
      na.omit() %>%
      select(-1) %>%
      row_to_names(1) %>%
      clean_names() %>%
      rename("Time" = 1) %>%
      {
        if (order_by_sample) {
          select(., "Time", order(colnames(.[colnames(.) != "Time"])))
        } else {
          .
        }
      } %>%
      suppressWarnings()
  }

  split_real_time <- function(x) {
    # Number of types of data (e.g. Raw, Normalized, or Derivative)
    reads <- length(which(x[["Time"]] == 0))
    if (reads == 1) {
      return(list(x))
    }

    # Designate the integers used to calculate how the data will be cut
    num_rows <- cycles <- length(unique(x[["Time"]]))

    # Create separate data frames for different read types
    df_list <- list()
    for (i in 1:reads) {
      df_list <- append(df_list, list(x[(1 + num_rows - cycles):num_rows, ]))
      num_rows <- num_rows + cycles
    }
    return(df_list)
  }

  transpose_real <- function(data) {
    colnames(data) %>%
      rbind(data) %>%
      unname() %>%
      t() %>%
      as.data.frame() %>%
      row_to_names(1) %>%
      rename("Sample IDs" = "Time") %>%
      mutate_at(-c(1), function(y) as.numeric(as.character(y)))
  }

  return(
    data %>%
      check_format() %>%
      curate() %>%
      split_real_time() %>%
      {if (isTRUE(transpose_table)) lapply(., transpose_real) else .}
  )
  # return(split_real_time(curate(check_format(data))))
}
