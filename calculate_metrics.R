library(slider)




source("functions/functions.R")


# For testing, use "test".
file <- paste0(
  "input files/",
  readline(
    "Please input the Excel file name without .xlsx extension: "
    ),
  ".xlsx"
  )

df <- get_real(file, ordered=FALSE)

df_id <- as.integer(
  readline(
    paste(
      "There are",
      length(df),
      "real-time data sets. Please enter a number in that range: "
      )
    )
  )

df <- df[[df_id]]

dic <- organize_tables(file)
column_names <- c("Time")
for (i in t(dic[["Sample IDs"]])) {
  for (j in i) {
    if (!is.na(j)) {
      column_names <- cbind(column_names, j)
    }
  }
}

colnames(df) <- column_names

################################################################################

df_norm <- normalize_RFU(df)
hours <- as.numeric(colnames(df_norm)[ncol(df_norm)])

df_analyzed <- data.frame(`Sample_ID` = df_norm$`Sample ID`)

df_analyzed$TtT <- hours
df_analyzed$RAF <- NA
df_analyzed$MPR <- NA
df_analyzed$MS  <- NA

# Set the cycle interval.
cycle_interval <- diff(as.numeric(colnames(df_norm[,2:3])))
threshold <- as.numeric(readline("Please enter your preferred threshold: "))

################################################################################

# Calculate the Time to Threshold
for (i in 1: nrow(df_norm))  {
  for (j in 3: (ncol(df_norm))) {
    
    # Use the sample-unique threshold calculated in "summary".
    current_read <- df_norm[i, j]
    
    if (df_analyzed[i, "TtT"] == hours & current_read > threshold) {
      
      previous_read <- df_norm[i, j-1]
      
      delta_rfu <-  current_read - previous_read
      slope <- delta_rfu / cycle_interval
      
      delta_threshold <- threshold - previous_read
      delta_t <- delta_threshold / slope
      
      previous_cycle <- as.numeric(colnames(df_norm[j-1]))
      df_analyzed[i, "TtT"] <- previous_cycle + delta_t
    }
  }
}

################################################################################

# Calculate the rate of amyloid formation.
df_analyzed$RAF <- 1 / (3600 * df_analyzed$TtT)

################################################################################

# Identify the maxpoint ratio.
for (i in 1: nrow(df_norm)) {
  maximum <- max(df_norm[i,3:(ncol(df_norm))])
  df_analyzed[i,"MPR"] <- maximum
}

################################################################################

# Calculate the slope using a moving window linear regression.
df_norm_t <- t(df_norm)
colnames(df_norm_t) <- df_norm_t[1,]
df_norm_t <- df_norm_t[-1,]
df_norm_t <- cbind(Time = as.numeric(rownames(df_norm_t)), df_norm_t)
df_norm_t <- as.data.frame(df_norm_t)

unique_cols <- colnames(df_norm_t)[1]

x <- 1
for (i in colnames(df_norm_t)[-1]) {
  unique_cols <- cbind(unique_cols, paste0(i, "_", x))
  x <- x + 1
}

unique_cols <- gsub("-", "_", unique_cols, fixed = TRUE)

colnames(df_norm_t) <- unique_cols

df_deriv <- df_norm_t$Time

# Make sure there are no "-" in the sample IDs. This affects the formula below.

for (i in colnames(df_norm_t)[-1]) {
  slope_column <- slide(df_norm_t,
                        ~ lm(as.formula(paste(i, "~ Time")),
                             data = .x)[[1]][[2]] / 3600,
                        .before = 3,
                        .complete = TRUE)
  df_deriv <- cbind(df_deriv, slope_column)
  
}

df_deriv <- as.data.frame(df_deriv)
df_deriv <- t(df_deriv)
df_deriv <- as.data.frame(df_deriv)[-1,]
df_deriv <- cbind(df_norm$`Sample ID`, df_deriv)
colnames(df_deriv) <- colnames(df_norm)
df_deriv[,-1] <- mutate_all(df_deriv[,-1], 
                            function(x) as.numeric(as.character(x)))

# Identify the max slope.
for (i in 1: nrow(df_deriv)) {
  maximum <- max(df_deriv[i,5:(ncol(df_deriv))])
  df_analyzed[i,"MS"] <- maximum
}

################################################################################

df_analyzed <- df_analyzed[order(df_analyzed$Sample_ID),]
summary <- df_analyzed %>%
  group_by(Sample_ID) %>%
  summarise(mean_TtT = mean(TtT), 
            mean_RAF = mean(RAF), 
            mean_MPR = mean(MPR), 
            mean_MS = mean(MS))

################################################################################

# Initialize the workbook for Excel.
wb <- createWorkbook()

# Add the sheets.
addWorksheet(wb, "Total")
addWorksheet(wb, "Summary")

# Write the "summary" df to the "Summary" sheet.
writeData(wb, "Total", df_analyzed)
writeData(wb, "Summary", summary)


# Save the Excel file.

saveWorkbook(wb, "output files/output.xlsx", overwrite = TRUE)








