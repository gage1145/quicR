library(slider)
library(ggpubr)
library(agricolae)


# Users will need to adjust for their own file directory.
source("C:/Users/rowde002/Box/Scripts/R scripts/functions/functions.R")



# For testing, use "test".
file <- ""
while (file == "") {
  file <- paste0(
    readline(
      "Please input the Excel file name without .xlsx extension: "
    ),
    ".xlsx"
  )
  if (!file.exists(file)) {
    print(paste(file, "does not exist. Please enter a valid file name."))
    file <- ""
  }
}


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

# Reformat df_deriv to match df_norm formatting.
df_deriv <- df_deriv %>%
  as.data.frame() %>%
  t() %>%
  as.data.frame()
df_deriv <- df_deriv[-1,]
df_deriv <- cbind(df_norm$`Sample ID`, df_deriv)
colnames(df_deriv) <- colnames(df_norm)
df_deriv[,-1] <- mutate_all(df_deriv[,-1], 
                            function(x) as.numeric(as.character(x)))

# Identify the max slope.
for (i in 1: nrow(df_deriv)) {
  df_analyzed[i,"MS"] <- max(df_deriv[i,5:(ncol(df_deriv))])
}

################################################################################

df_analyzed <- df_analyzed[order(df_analyzed$Sample_ID),]

# Reorganize the columns of df_analyzed.
df_analyzed <- data.frame(Sample_ID = df_analyzed$Sample_ID,
                          MPR = df_analyzed$MPR,
                          RAF = df_analyzed$RAF,
                          MS  = df_analyzed$MS,
                          TtT = df_analyzed$TtT)

summary <- df_analyzed %>%
  group_by(Sample_ID) %>%
  summarise(mean_TtT   = mean(TtT), 
            mean_RAF   = mean(RAF),
            mean_MPR   = mean(MPR),
            mean_MS    = mean(MS))

################################################################################

# Calculate the statistical comparisons for MPR.
model <- aov(MPR ~ Sample_ID, data=df_analyzed)
stats <- LSD.test(model, "Sample_ID", p.adj = "bonferroni")

stats <- stats$groups[order(row.names(stats$groups)),]
neg_group <- stats["N", "groups"]
  
summary$MPR_Result <- c(ifelse(stats$groups == neg_group, "ns", "*"))


# Calculate the statistical comparisons for Max Slope.
model <- aov(MS ~ Sample_ID, data=df_analyzed)
stats <- LSD.test(model, "Sample_ID")

stats <- stats$groups[order(row.names(stats$groups)),]
neg_group <- stats["N", "groups"]

summary$MS_Result <- c(ifelse(stats$groups == neg_group, "ns", "*"))

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
saveWorkbook(wb, "output.xlsx", overwrite = TRUE)

################################################################################

# Plot the summary metrics
df_analyzed_melted <- reshape2::melt(df_analyzed, id.vars = "Sample_ID")
p <- ggplot(df_analyzed_melted, aes(Sample_ID, value, fill=Sample_ID)) +
  scale_fill_discrete() +
  geom_boxplot() +
  # stat_compare_means() +
  facet_wrap(vars(variable),
             scales = "free",
             labeller = as_labeller(c(MPR = "MPR (Max RFU / Initial RFU)",
                                      RAF = "RAF (1/s)",
                                      MS = "Max Slope (RFU/s)",
                                      TtT = "Time to Threshold (h)")),
             strip.position = "left") +
  xlab(NULL) +
  ylab(NULL) +
  theme(axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.background = element_blank(),
        panel.grid = element_line(colour = "lightgrey"),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        legend.position = "none",
        strip.background = element_blank(),
        strip.placement = "outside")
p

ggsave("Summary.png", p, width = 3600, height = 2400, units = "px")







