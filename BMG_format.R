source("functions/functions.R")

df_ <- BMG_format("BMG formatting/plate_layout.csv")

writeLines(df_, "formatted.txt")
