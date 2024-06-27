<<<<<<< HEAD
=======
# This script takes an .csv file plate layout and formats it into a .txt file that can be imported into the BMG control software.
# See "formatted.txt" in the BMG formatting folder for the output example.
# See "plate_layout.csv" in the BMG formatting folder for input file example.

>>>>>>> e35b6d582f0b715e928cee84dac15246e4f97dab
source("functions/functions.R")

df_ <- BMG_format("BMG formatting/plate_layout.csv")

writeLines(df_, "formatted.txt")
