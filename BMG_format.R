source("C:/Users/rowde002/Box/Scripts/R scripts/functions/functions.R")

df <- BMG_format("plate_layout.csv")

writeLines(df_, "formatted.txt")
