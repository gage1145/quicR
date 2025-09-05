# This script takes an .csv file plate layout and formats it into a .txt file
# that can be imported into the BMG control software.

# See "formatted.txt" in the BMG formatting folder for the output example.
# See "plate_layout.csv" in the BMG formatting folder for input file example.



library(quicR)



# Use plate_layout.csv in the Desktop.
setwd("C:/Users/Priogen/Desktop")
file <- "plate_layout.csv"

BMG_format(file) |> writeLines("formatted.txt")
