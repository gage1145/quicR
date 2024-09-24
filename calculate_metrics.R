library(slider)
library(ggpubr)
library(openxlsx)
library(agricolae)
library(tidyr)
library(stringr)
library(quicR)



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
df <- quicR::get_real(file, ordered = FALSE)

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

# Ask the user for the threshold.
threshold <- as.integer(
  readline(
    "Please enter the desired threshold for RAF calculation: "
  )
)

# Select the real-time data set that the user signified.
df <- df[[df_id]]



# Get the metadata of each sample -----------------------------------------



# Export the tables in the first sheet of the file.
dic <- quicR::organize_tables(file)

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
df_norm <- quicR::normalize_RFU(df)

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
    MPR = quicR::calculate_MPR(df_norm, start_col = 3, data_is_norm = TRUE),
    # Max Slope
    MS = quicR::calculate_MS(df_norm, start_col = 3),
    # Time to Threshold
    TtT = quicR::calculate_TtT(df_norm, threshold = threshold, start_col = 3, run_time = hours)
  ) %>%
  mutate(
    # Rate of Amyloid Formation
    RAF = ifelse(TtT == hours, 0, 1 / (3600 * TtT)),
    # Crossed threshold?
    crossed = TtT != hours
  ) %>%
  # Order the data frame based on Sample_ID.
  arrange(Sample_ID)



# Summarize the data ------------------------------------------------------



# Create a summary data frame.
summary <- (
  if (dilution_bool) {
    summary <- df_analyzed %>%
      group_by(Sample_ID, Dilutions)
  } else {
    summary <- df_analyzed %>%
      group_by(Sample_ID)
  }) %>%
  summarise(
    reps      = n(),
    mean_TtT  = mean(TtT),
    # sd_TtT    = sd(TtT),
    mean_RAF  = mean(RAF),
    # sd_RAF    = sd(RAF),
    mean_MPR  = mean(MPR),
    # sd_MPR    = sd(MPR),
    mean_MS   = mean(MS),
    # sd_MS     = sd(MS),
    thres_pos = sum(crossed) / n() > 0.5
  )



# Run the statistical analysis against the negative control ---------------



metrics <- c("MPR", "MS")
for (metric in metrics) {
  # Create a dataframe of the individual comparisons.
  comps <- LSD.test( # Perform the post-hoc multiple comparisons test.
    # Create the statistical model using ANOVA.
    aov(as.formula(paste0(metric, " ~ ", "Sample_ID")),
      data = df_analyzed
    ),
    "Sample_ID",
    p.adj = "holm", group = F
  )[["comparison"]]

  # Initialize columns which will hold unique IDs for each sample compared.
  comps <- comps %>%
    cbind(
      rownames(comps) %>%
        strsplit(" - ") %>%
        as.data.frame() %>%
        t() %>%
        as.data.frame()
    ) %>%
    select(-difference) %>%
    # Remove all comparisons that are not against "N".
    subset(
      V1 == "N" |
        V2 == "N" |
        str_detect(V1, "N_") |
        str_detect(V2, "N_")
    ) %>%
    rename(
      "{metric}_pvalue" := pvalue,
      "{metric}_significance" := signif.
    ) %>%
    mutate(
      V1 = replace(V1, V1 == "N" | str_detect(V1, "N_"), NA),
      V2 = replace(V2, V2 == "N" | str_detect(V2, "N_"), NA)
    ) %>%
    unite(
      Sample_ID,
      c("V1", "V2"),
      sep = "",
      na.rm = T
    ) %>%
    rbind(c(NA, NA, "N"))

  summary <- left_join(summary, comps)
}

summary <- summary %>%
  mutate(
    MPR_pvalue = as.numeric(MPR_pvalue),
    MS_pvalue = as.numeric(MS_pvalue),
    Positive = thres_pos & MPR_pvalue <= 0.05 & MS_pvalue <= 0.05
  )



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

  geom_boxplot(
    outlier.shape = NA,
    position = "dodge",
    fill = "lightgrey"
  ) +

  geom_dotplot(
    binaxis = "y",
    stackdir = "center",
    dotsize = 0.5,
    position = "dodge",
    stackratio = 0.5
  ) +

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
    panel.border = element_rect(colour = "black", fill = NA, size = 1),
    strip.background = element_blank(),
    strip.placement = "outside"
  )

ggsave("summary.png", width = 4000, height = 2500, units = "px")
