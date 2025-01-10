library(tidyverse)
library(quicR)
library(slider)
library(ggpubr)
library(ggrepel)


data_is_norm <- FALSE
file <- "../../inst/extdata/input_files/test2.xlsx"
window <- 4

curate <- function(x) {
  x %>%
    {if (data_is_norm) . else normalize_RFU(.)} %>%
    t() %>%
    as.data.frame() %>%
    mutate_all(~ as.numeric(as.character(.))) %>%
    suppressWarnings() %>%
    na.omit() %>%
    mutate(Time = as.numeric(rownames(.))) %>%
    relocate("Time", .before = 1)
}


df_1 <- get_real(file, ordered = FALSE)[[1]][1:2] %>%
  curate()


slope <- slide(
  df_1,
  ~lm(`2` ~ Time, data = .x)[[1]][[2]],
  .before = window,
  .complete = TRUE
) %>%
  unlist() %>%
  as.data.frame()

slope <- rbind(NA, NA, slope, NA, NA)

df_2 <- cbind(df_1, slope) %>%
  na.omit() %>%
  mutate_all(as.numeric)
colnames(df_2) <- c("Time", "Raw", "Slope")

ms <- max(df_2$Slope)
ms_time <- df_2[[which(df_2$Slope == max(df_2$Slope)), "Time"]]

mpr <- max(df_2$Raw)
mpr_time <- df_2[[which(df_2$Raw == max(df_2$Raw)), "Time"]]
threshold <- 1.2
ttt <- min(df_2[which(df_2$Raw > threshold),][["Time"]])

ggplot(df_2, aes(Time)) +
  geom_line(aes(y = Slope), color = "blue", size = 1.2) +
  geom_line(aes(y = Raw - 1), color = "red", size = 1.2) +
  # geom_smooth(aes(y = Slope), color = "blue", span = 0.1, se = F, size = 1.2) +
  # geom_smooth(aes(y = Raw - 1), color = "red", span = 0.1, se = F, size = 1.2) +

  geom_hline(yintercept = ms, color = "darkblue", size = 1.2, linetype = "dashed") +
  # geom_vline(xintercept = ms_time, color = "darkblue", size = 1.2, linetype = "dashed") +
  geom_hline(yintercept = mpr - 1, color = "darkred", size = 1.2, linetype = "dashed") +
  # geom_vline(xintercept = mpr_time, color = "darkred", size = 1.2, linetype = "dashed") +
  geom_hline(yintercept = threshold - 1, color = "black", size = 1.2, linetype = "dashed") +
  geom_vline(xintercept = ttt, color = "darkgreen", size = 1.2, linetype = "dashed") +

  geom_label_repel(
    label = "Maxpoint Ratio",
    hjust = "inward",
    x = mpr_time,
    vjust = "outward",
    y = mpr - 1,
    data = subset(df_2, Raw == mpr),
    force_pull = -200,
    color = "darkred",
    size = 6,
    segment.size = 1.2
  ) +
  geom_label_repel(
    label = "Max Slope",
    hjust = "inward",
    x = ms_time,
    vjust = "outward",
    y = ms,
    data = subset(df_2, Slope == ms),
    force_pull = -200,
    color = "darkblue",
    size = 6,
    segment.size = 1.2
  ) +
  geom_label_repel(
    label = "Threshold",
    x = 15,
    vjust = "outward",
    y = threshold - 1,
    data = subset(df_2, Time == 20.25),
    force_pull = -100,
    color = "black",
    size = 6,
    segment.size = 1.5
  ) +
  geom_label_repel(
    label = "Time-to-threshold",
    hjust = "inward",
    x = ttt,
    vjust = "outward",
    y = threshold - 1,
    data = subset(df_2, Time == ttt),
    force_pull = -300,
    color = "darkgreen",
    size = 6,
    segment.size = 1.2
  ) +
  geom_text(
    label = "-- Normalized RFU",
    x = 15,
    y = 4,
    hjust = 0,
    vjust = 0,
    color = "red",
    size = 6
  ) +
  geom_text(
    label = "-- Derivative",
    x = 15,
    y = 3.5,
    hjust = 0,
    vjust = 0,
    color = "blue",
    size = 6
  ) +

  labs(
    x = "Time (h)"
  ) +
  theme_classic2() +
  scale_x_continuous(breaks = seq(0, 48, 2)) +
  coord_cartesian(xlim = c(15, 35)) +
  theme(
    axis.title.y = element_blank(),
    axis.title.x = element_text(size = 20),
    axis.text = element_text(size = 16)
  )
ggsave("images/metric_example.png", width = 12, height = 6)
