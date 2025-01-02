#' Get the well locations of the samples used in the RT-QuIC run.
#'
#' Returns a dataframe with the sample IDs and well IDs used in the plate.
#'
#' @param file Excel file exported from MARS
#' @param tab_name Table name containing the sample IDs.
#' @param dilution_bool Logical; is there a table containing dilution factors? If so, will add a newline and the log of the dilution factor to the ID column.
#' @param plate Integer; either 96 or 384 to denote microplate type.
#'
#' @return A vector containing well IDs.
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr %>%
#' @importFrom stats na.omit
#' @importFrom stringr str_length
#'
#' @examples
#' file <- system.file(
#'   "extdata/input_files",
#'   file = "test.xlsx",
#'   package = "quicR"
#' )
#' get_sample_locations(file, "Sample IDs", TRUE, 96)
#'
#' @export
get_sample_locations <- function(file, tab_name, dilution_bool = FALSE, plate = 96) {

  return(
    data.frame(
      wells = get_wells(file) %>%
        suppressMessages(),
      IDs = (
        organize_tables(file, plate = plate) %>%
          convert_tables() %>%
          suppressMessages()
      )[[tab_name]] %>%
        na.omit()
    ) %>%
      mutate(
        "IDs" = ifelse(
          str_length(.[["IDs"]]) > 12,
          gsub(" ", "\n", .[["IDs"]]),
          .[["IDs"]]
        ),
        "IDs" = if (dilution_bool) {
          paste(
            as.character(.[["IDs"]]),
            "\n",
            (organize_tables(file) %>%
               convert_tables() %>%
               suppressMessages())[["Dilutions"]] %>%
              as.numeric() %>%
              log10() * -1 %>%
              na.omit()
          )
        } else {
          .[["IDs"]]
        }
    )
  )
}
