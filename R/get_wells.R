get_wells <- function (file) {

  if (is.character(file)) {
    df <- read_excel(file, sheet = 2, col_name = FALSE)
  }
  else if (is.data.frame(file)) {
    df <- file
  }
  else {
    return ("Please enter either .xlsx string or dataframe. ")
  }


  # Get the wells used in the run.
  for (i in 1: nrow(df)) {
    while (is.na(df[i, 1])) {
      i <- i + 1
    }
    if (df[i, 1] == "Well") {
      wells <- c(df[i, ])
      break
    }
  }
  return (wells[-(1:2)])
}
