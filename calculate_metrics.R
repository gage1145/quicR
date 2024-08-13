library(slider)
library(ggpubr)
library(agricolae)



# Users will need to adjust for their own file directory.
source("~/RTQ_analysis/functions/functions.R")



# Initialize the file and read in the data --------------------------------



# For testing, use "test".
# Request the user to input the file name for analysis.
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



# Identify the raw real-time data -----------------------------------------



# Get the real-time data from the BMG export file.
df <- get_real(file, ordered=FALSE)

# Ask the user which real-time data set they want to use.
df_id <- as.integer(
  readline(
    paste(
      "There are",
      length(df),
      "real-time data sets. Please enter a number in that range: "
      )
    )
  )

# Ask the user for the run-time in hours.
run_time <- as.integer(
  readline(
    "Please enter the number of hours in the run: "
  )
)

# Ask the user for the run-time in hours.
threshold <- as.integer(
  readline(
    "Please enter the desired threshold for RAF calculation: "
  )
)

# Select the real-time data set that the user signified.
df <- df[[df_id]]

# Export the tables in the first sheet of the file.
dic <- organize_tables(file)
column_names <- c("Time")
for (i in t(dic[["Sample IDs"]])) {
  for (j in i) {
    if (!is.na(j)) {
      column_names <- cbind(column_names, j)
    }
  }
}
# Apply the column names.
colnames(df) <- column_names

# Determine if there is a dilutions table.
dilution_bool <- "Dilutions" %in% names(dic)



# Calculate the normalized real-time data ---------------------------------



# Calculate the normalized real-time data.
df_norm <- normalize_RFU(df)

# Add dilution factors if applicable.
if (dilution_bool) {
  dilutions <- c()
  for (i in t(dic[["Dilutions"]])) {
    for (j in i) {
      if (!is.na(j)) {
        dilutions <- rbind(dilutions, j)
      }
    }
  }
}



# Calculate the relevant metrics ------------------------------------------



# Define the number of hours that the rxn ran for.
hours <- as.numeric(colnames(df_norm)[ncol(df_norm)])

# Initialized the dataframe with the calculated metrics.
df_analyzed <- data.frame(`Sample_ID` = df_norm$`Sample ID`) %>%
  mutate(
    # Add dilutions if applicable.
    Dilutions = if (dilution_bool) -log10(as.numeric(dilutions)),
    # Maxpoint Ratio
    MPR = calculate_MPR(df_norm, start_col=3, data_is_norm=TRUE),
    # Max Slope
    MS  = calculate_MS(df_norm, start_col=3),
    # Time to Threshold
    TtT = calculate_TtT(df_norm, threshold=threshold, start_col=3, run_time=run_time)
  ) %>%
  mutate(
    # Rate of Amyloid Formation
    RAF = ifelse(TtT == run_time, 0, 1 / (3600 * TtT)),
    # Crossed threshold?
    crossed = TtT != run_time
  ) %>%
  # Order the data frame based on Sample_ID.
  arrange(Sample_ID)

################################################################################

# Create a summary data frame.
summary <- (if (dilution_bool) {
  summary <- df_analyzed %>%
    group_by(Sample_ID, Dilutions)
} else {
  summary <- df_analyzed %>%
    group_by(Sample_ID)
}) %>%
  summarise(
    mean_TtT  = mean(TtT), 
    mean_RAF  = mean(RAF),
    mean_MPR  = mean(MPR),
    mean_MS   = mean(MS),
    thres_pos = sum(crossed) / n() > 0.5
  )

################################################################################

metrics <- c("MPR", "MS")
for (metric in metrics) {
  
  # Create a dataframe of the individual comparisons.
  comps <- LSD.test( # Perform the post-hoc multiple comparisons test.
    # Create the statistical model using ANOVA.
    aov(as.formula(paste0(metric, " ~ ", "Sample_ID")), 
        data = df_analyzed),
    "Sample_ID",  p.adj = "holm", group = F
  )[["comparison"]]
  
  # Initialize columns which will hold unique IDs for each sample compared.
  comps <- comps %>%
    cbind(rownames(comps) %>%
            strsplit(" - ") %>%
            as.data.frame() %>%
            t() %>%
            as.data.frame()) %>%
    select(-difference) %>%
    # Remove all comparisons that are not against "N".
    subset(V1 == "N" | V2 == "N") %>%
    rename("{metric}_pvalue" := pvalue,
           "{metric}_significance" := signif.) %>%
    mutate(V1 = replace(V1, V1=="N", NA),
           V2 = replace(V2, V2=="N", NA)) %>%
    unite(Sample_ID, c("V1", "V2"), sep="", na.rm=T) %>%
    rbind(c(NA, NA, "N"))
  
  summary <- left_join(summary, comps)
}

summary <- summary %>%
  mutate(Positive = thres_pos & MPR_pvalue <= 0.05 & MS_pvalue <= 0.05)



# Save the data to an Excel workbook --------------------------------------



# Initialize the workbook for Excel.
wb <- createWorkbook()

# Add the sheets.
addWorksheet(wb, "Total")
addWorksheet(wb, "Summary")

# Write the "summary" df to the "Summary" sheet.
writeData(wb, "Total", df_analyzed)
writeData(wb, "Summary", summary)

# Save the Excel file.
saveWorkbook(wb, "summary.xlsx", overwrite = TRUE)



# Plot the metrics in a facet plot ----------------------------------------



df_analyzed %>%
  select(-crossed) %>%
  
  {
    if (dilution_bool) {
      reshape2::melt(., id.vars = c("Sample_ID", "Dilutions")) %>%
        mutate(Dilutions = as.factor(desc(Dilutions))) %>%
        ggplot(aes(Sample_ID, value, fill = Dilutions))
    } else {
      reshape2::melt(., id.vars = "Sample_ID") %>%
        ggplot(aes(Sample_ID, value))
    }
  } +
  
  geom_boxplot(aes(fill=Dilutions), outlier.shape = NA, position="dodge") +
  geom_dotplot(binaxis="y", stackdir="center", dotsize=0.5, position="dodge",
               stackratio=0.5) +
  facet_wrap(
    vars(variable),
    scales = "free",
    labeller = as_labeller(
      c(
        MPR = "MPR (Max RFU / Initial RFU)",
        RAF = "RAF (1/s)",
        MS  = "Max Slope (RFU/s)",
        TtT = "Time to Threshold (h)"
      )
    ),
    strip.position = "left"
  ) +
  ylim(0, NA) +
  xlab(NULL) +
  ylab(NULL) +
  theme(
    axis.line = element_line(colour = "black"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.background = element_blank(),
    panel.grid = element_line(colour = "lightgrey"),
    panel.border = element_rect(colour = "black", fill=NA, size=1),
    # legend.position = "none",
    strip.background = element_blank(),
    strip.placement = "outside"
  )

ggsave("summary.png", width = 4000, height = 2500, units = "px")
