library(tidyverse)
library(quicR)
library(slider)
library(ggpubr)
library(ggrepel)



# Smoothed Functions ------------------------------------------------------



S <- 5.13534
a <- 314730
b <- -1.61059
E <- -1.43142
c <- 78.01494
d <- -0.304958

raw_func <- function(x) {
  ratio1 <- S/(1 + a * exp(b * x))
  ratio2 <- E/(1 + c * exp(d * x))
  return(ratio1 + ratio2)
}

deriv_func <- function(x) {
  ratio1 <- -S * a * b * exp(b * x) / (a * exp(b * x) + 1)^2
  ratio2 <- -E * c * d * exp(d * x) / (c * exp(d * x) + 1)^2
  ratio1 + ratio2
}

df_ <- data.frame(x = seq(0, 48, 0.1)) %>%
  mutate(raw = raw_func(x), der = deriv_func(x))

threshold <- 0.2
ttt <- min(df_[which(df_$raw > threshold), "x"])
mpr <- max(df_$raw)
mpr_time <- df_[which(df_$raw == max(df_$raw)), "x"]
ms <- max(df_$der)
ms_time <- df_[which(df_$der == max(df_$der)), "x"]

blue_color <- "lightblue"
red_color <- "red"


ggplot(df_, aes(x)) +
  geom_line(aes(y = raw), color = red_color, linewidth = 1.2) +
  geom_line(aes(y = der), color = blue_color, linewidth = 1.2) +
  geom_segment(
    x = 0,
    xend = mpr_time,
    y = mpr,
    yend = mpr,
    color = red_color,
    linewidth = 1.2,
    linetype = "dashed"
  ) +
  geom_segment(
    x = mpr_time,
    xend = mpr_time,
    y = -0.15,
    yend = mpr,
    color = red_color,
    linewidth = 1.2,
    linetype = "dashed"
  ) +
  geom_segment(
    x = 0,
    xend = ms_time,
    y = ms,
    yend = ms,
    color = blue_color,
    linewidth = 1.2,
    linetype = "dashed"
  ) +
  geom_segment(
    x = ms_time,
    xend = ms_time,
    y = -0.15,
    yend = ms,
    color = blue_color,
    linewidth = 1.2,
    linetype = "dashed"
  ) +
  geom_segment(
    x = 0,
    xend = ttt,
    y = threshold,
    yend = threshold,
    color = red_color,
    linewidth = 1.2,
    linetype = "dashed"
  ) +
  geom_segment(
    x = ttt,
    xend = ttt,
    y = -0.15,
    yend = threshold,
    color = red_color,
    linewidth = 1.2,
    linetype = "dashed"
  ) +
  geom_label_repel(
    label = "Maxpoint Ratio",
    x = mpr_time,
    y = mpr,
    hjust = "inward",
    data = subset(df_, raw == mpr),
    force_pull = -200,
    color = red_color,
    fill = "transparent",
    size = 5,
    segment.size = 1.2
  ) +
  geom_label_repel(
    label = "Max Slope",
    x = ms_time,
    y = ms,
    hjust = "outward",
    vjust = "outward",
    data = subset(df_, der == ms),
    force_pull = -100,
    color = blue_color,
    fill = "transparent",
    size = 5,
    segment.size = 1.2
  ) +
  geom_label_repel(
    label = "Threshold",
    x = 1,
    y = threshold,
    vjust = "outward",
    data = subset(df_, x == 0),
    force_pull = -50,
    color = red_color,
    fill = "transparent",
    size = 5,
    segment.size = 1.5
  ) +
  geom_label_repel(
    label = "Time-to-threshold",
    x = ttt,
    y = threshold,
    hjust = "inward",
    vjust = "outward",
    data = subset(df_, x == ttt),
    force_pull = -250,
    color = red_color,
    fill = "transparent",
    size = 5,
    segment.size = 1.2
  ) +
  geom_text(
    label = "-- Normalized RFU",
    x = 1,
    y = 4,
    hjust = 0,
    vjust = 0,
    color = red_color,
    size = 5
  ) +
  geom_text(
    label = "-- Derivative",
    x = 1,
    y = 3.5,
    hjust = 0,
    vjust = 0,
    color = "lightblue",
    size = 5
  ) +
  labs(x = "Time (h)") +
  theme_classic() +
  scale_x_continuous(breaks = seq(0, 48, 2)) +
  coord_cartesian(xlim = c(0, 21), ylim = c(-0.15, 4.8), expand = FALSE) +
  theme(
    line=element_line(color="white"),
    plot.background = element_rect(fill="#1D1D1D"),
    panel.background = element_rect(fill="#1D1D1D"),
    panel.border = element_rect(color="white"),
    panel.grid = element_blank(),
    strip.text = element_text(color="white"),
    axis.title = element_blank(),
    axis.text = element_text(color="white"),
    axis.text.x = element_text(angle=0, hjust=0.5),
    strip.background = element_blank(),
    legend.background = element_blank(),
    legend.text = element_text(color="white"),
    legend.title = element_text(color="white")
  )
ggsave("images/metric_example.png", width = 12, height = 6)



# Boxplots ----------------------------------------------------------------



file <- "../../inst/extdata/input_files/test5.xlsx"
df_raw <- get_real(file)[[1]]
df_norm <- normalize_RFU(df_raw)

meta <- organize_tables(file) %>%
  convert_tables()

df_analyzed <- calculate_metrics(df_norm, meta)

plot_metrics(df_analyzed, nrow = 2, ncol = 2) +
  guides(fill = guide_legend(nrow = 1))
ggsave("images/boxplot.png", width = 8, height = 4)



# Plate View --------------------------------------------------------------



locations <- get_sample_locations(file, dilution_bool = TRUE, sep = " ")
plate_view(df_raw, locations) +
  theme(
    strip.text = element_text(face = "bold", size = 12)
  )
ggsave("images/plate_view.png", width = 12, height = 8)
