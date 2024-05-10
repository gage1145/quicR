source("C:/Users/rowde002/Box/Scripts/R scripts/functions/functions.R")

df <- BMG_format("plate_layout.csv")

write.table(df,
            file="formatted.txt", 
            quote=FALSE, sep=" ", 
            col.names = FALSE, 
            row.names=FALSE)
