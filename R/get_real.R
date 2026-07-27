#' Get Real-Time RT-QuIC Fluorescence Data
#'
#' Accepts an Excel file or a dataframe of real-time RT-QuIC data.
#'
#' @param data Either an Excel file or a dataframe.
#' @param order_by_sample `r lifecycle::badge("deprecated")` This argument no longer has any effect.
#' @param transpose_table `r lifecycle::badge("deprecated")` This argument no longer has any effect.
#'
#' @return A list of dataframes containing the formatted real-time data.
#'
#' @import dplyr
#' @import janitor
#' @importFrom readxl read_xlsx
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
get_real <- function(data, order_by_sample = lifecycle::deprecated(), transpose_table = lifecycle::deprecated()) {
  
  if (lifecycle::is_present(order_by_sample)) {
    lifecycle::deprecate_warn(
      when = "3.0.8", 
      what = "get_real(order_by_sample)"
    )
  }

  if (lifecycle::is_present(transpose_table)) {
    lifecycle::deprecate_warn(
      when = "3.0.8", 
      what = "get_real(transpose_table)"
    )
  }
  
  check_format <- function(x) {
    if (is.character(x)) {
      return(suppressMessages(read_xlsx(x, sheet = 2, col_names = FALSE)))
    } else if (is.data.frame(x)) {
      return(x)
    } else {
      stop("Please enter either .xlsx file path or data frame. ")
    }
  }

  get_locs <- function(x) {
    x %>% 
      filter(!is.na(`...3`) & is.na(`...2`)) %>%
      select(where(~ any(!is.na(.)))) %>% 
      t() %>%
      as.data.frame() %>%
      row_to_names(1) 
  }

  curate <- function(x) {
    x %>%
      select(-1) %>%
      filter(!is.na(`...3`)) %>%
      row_to_names(1) %>%
      rename("Time" = 1) %>%
      mutate(Time = as.numeric(Time)) %>%
      filter(!is.na(Time)) %>%
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

  make_long <- function(x, locs) {
    x %>%
      pivot_longer(-Time, names_to = "Well", values_to = "RFU") %>%
      right_join(locs, by = "Well") %>%
      relocate(Time, RFU, .after = last_col()) %>%
      mutate(across(c(Time, RFU), as.numeric)) %>%
      arrange(Well, Time)
  }

  data <- check_format(data)

  locs <- get_locs(data)

  data %>%
    curate() %>%
    split_real_time() %>%
    map(~ make_long(.x, locs)) 
}
