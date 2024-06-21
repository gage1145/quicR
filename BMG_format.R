# This script takes an .csv file plate layout and formats it into a .txt file that can be imported into the BMG control software.
# See "formatted.txt" in the BMG formatting folder for the output example.
# See "plate_layout.csv" in the BMG formatting folder for input file example.

source("functions/functions.R")

df <- BMG_format("plate_layout.csv")

writeLines(df_, "formatted.txt")
