required_pkgs <- c("readxl", "dplyr", "tidyr", "ggplot2")
install_if_missing <- function(pkgs) {
  for (p in pkgs) {
    if (!requireNamespace(p, quietly = TRUE)) {
      install.packages(p, repos = "https://cloud.r-project.org")
    }
  }
}
install_if_missing(required_pkgs)

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)

get_script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) > 0) {
    return(dirname(normalizePath(sub("^--file=", "", file_arg))))
  }
  if (!is.null(sys.frames()[[1]]$ofile)) {
    return(dirname(normalizePath(sys.frames()[[1]]$ofile)))
  }
  return(getwd())
}

script_dir <- get_script_dir()
data_dir <- file.path(dirname(dirname(script_dir)), "data")

output_dir <- file.path(dirname(dirname(script_dir)), "latex", "peerj", "images")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

input_excel <- file.path(data_dir, "benchmark_subjective_scores_ori_image_level.xlsx")
output_pdf  <- file.path(output_dir, "figure3.pdf")

language_target <- "chinese"
recommended_threshold <- 3.5
acceptable_threshold  <- 3.0

model_levels <- c("qwen", "zimage", "sdxl")
metric_labels <- c(
  "CompositeScore" = "Composite score",
  "MeanAdherence" = "Prompt adherence",
  "MeanClarity" = "Clarity",
  "MeanAesthetics" = "Aesthetics"
)
metric_order <- c("CompositeScore", "MeanAdherence", "MeanClarity", "MeanAesthetics")
metric_order_en <- unname(metric_labels[metric_order])

df <- read_excel(input_excel, sheet = 1)

df_ch <- df %>%
  mutate(
    Language = trimws(as.character(`Language`)),
    ModelShortName = as.character(`ModelShortName`),
    CompositeScore = as.numeric(`CompositeScore`),
    MeanAdherence = as.numeric(`MeanAdherence`),
    MeanClarity = as.numeric(`MeanClarity`),
    MeanAesthetics = as.numeric(`MeanAesthetics`)
  ) %>%
  filter(Language == language_target) %>%
  filter(ModelShortName %in% model_levels) %>%
  mutate(ModelShortName = factor(ModelShortName, levels = model_levels))

if (nrow(df_ch) == 0) {
  stop("filteringLanguagegroup yieldedno data, please check language_target Excel Medium'Language'Infoactual valuesfor consistency.")
}

plot_df <- df_ch %>%
  select(ModelShortName, CompositeScore, MeanAdherence, MeanClarity, MeanAesthetics) %>%
  pivot_longer(
    cols = c(CompositeScore, MeanAdherence, MeanClarity, MeanAesthetics),
    names_to = "Metric",
    values_to = "Score"
  ) %>%
  filter(!is.na(ModelShortName), !is.na(Metric), !is.na(Score)) %>%
  mutate(
    Metric = factor(Metric, levels = metric_order, labels = metric_order_en),
    ModelShortName = factor(ModelShortName, levels = model_levels)
  )

if (nrow(plot_df) == 0) {
  stop("plot_df is empty, please checkfour metric columnsInfosuccessfully converted to numeric values.")
}

threshold_df <- data.frame(
  Metric = factor(rep("Prompt adherence", 2), levels = metric_order_en),
  yint = c(recommended_threshold, acceptable_threshold),
  linetype = c("Recommended", "Acceptable")
)

threshold_key_segments <- data.frame(
  Metric = factor(c("Prompt adherence", "Prompt adherence"), levels = metric_order_en),
  x = c(0.70, 0.70),
  xend = c(1.02, 1.02),
  y = c(1.46, 1.22),
  yend = c(1.46, 1.22),
  linetype = c("Recommended", "Acceptable")
)

threshold_key_labels <- data.frame(
  Metric = factor(c("Prompt adherence", "Prompt adherence"), levels = metric_order_en),
  x = c(1.08, 1.08),
  y = c(1.46, 1.22),
  label = c("3.5 recommended", "3.0 acceptable")
)

plot_width_in  <- 3.45
plot_height_in <- 4.10
base_family <- "sans"

# Okabe--Ito colorblind-friendly palette, kept consistent across model figures.
model_colors <- c(
  "qwen" = "#0072B2",
  "zimage" = "#E69F00",
  "sdxl" = "#009E73"
)
model_fills <- c(
  "qwen" = "#D7EAF7",
  "zimage" = "#FCE4B6",
  "sdxl" = "#D8F0E5"
)

p <- ggplot(plot_df, aes(x = ModelShortName, y = Score)) +
  geom_hline(
    data = threshold_df,
    aes(yintercept = yint, linetype = linetype),
    linewidth = 0.42,
    color = "black",
    show.legend = FALSE
  ) +
  geom_boxplot(
    aes(fill = ModelShortName),
    width = 0.54,
    outlier.shape = NA,
    linewidth = 0.44,
    color = "grey25",
    alpha = 0.85,
    show.legend = FALSE
  ) +
  geom_point(
    aes(colour = ModelShortName),
    position = position_jitter(width = 0.18, height = 0, seed = 2026),
    alpha = 0.55,
    size = 0.90,
    shape = 16,
    show.legend = FALSE
  ) +
  geom_segment(
    data = threshold_key_segments,
    aes(x = x, xend = xend, y = y, yend = yend, linetype = linetype),
    inherit.aes = FALSE,
    linewidth = 0.42,
    color = "black",
    show.legend = FALSE
  ) +
  geom_text(
    data = threshold_key_labels,
    aes(x = x, y = y, label = label),
    inherit.aes = FALSE,
    hjust = 0,
    vjust = 0.5,
    size = 2.05,
    family = base_family,
    color = "black"
  ) +
  facet_wrap(~ Metric, ncol = 2, scales = "fixed") +
  scale_x_discrete(labels = c("qwen" = "Qwen", "zimage" = "Z-Image", "sdxl" = "SDXL")) +
  scale_y_continuous(
    limits = c(1, 5),
    breaks = seq(1, 5, by = 1),
    expand = expansion(mult = c(0.03, 0.10))
  ) +
  scale_linetype_manual(
    values = c(
      "Recommended" = "dashed",
      "Acceptable" = "dotdash"
    ),
    guide = "none"
  ) +
  scale_fill_manual(values = model_fills) +
  scale_colour_manual(values = model_colors) +
  coord_cartesian(clip = "off") +
  labs(
    x = "Model",
    y = "Score"
  ) +
  theme_classic(base_size = 9.0, base_family = base_family) +
  theme(
    text = element_text(color = "black"),
    axis.title.x = element_text(size = 9.2, margin = margin(t = 5)),
    axis.title.y = element_text(size = 9.2, margin = margin(r = 5)),
    axis.text.x = element_text(size = 8.0, margin = margin(t = 1)),
    axis.text.y = element_text(size = 8.0),
    axis.line = element_line(linewidth = 0.42, color = "black"),
    axis.ticks = element_line(linewidth = 0.36, color = "black"),
    axis.ticks.length = unit(1.5, "mm"),
    strip.background = element_blank(),
    strip.text = element_text(size = 9.2, face = "bold", margin = margin(b = 2.5)),
    panel.spacing.x = unit(0.70, "lines"),
    panel.spacing.y = unit(0.85, "lines"),
    panel.grid = element_blank(),
    plot.margin = margin(t = 4, r = 5, b = 4, l = 4)
  )

ggsave(
  filename = output_pdf,
  plot = p,
  width = plot_width_in,
  height = plot_height_in,
  units = "in",
  device = cairo_pdf,
  bg = "white"
)

print(p)

cat("\nSCI single-column 2×2 figure exported(wider boxes, threshold notes in lower-left version):\n")
cat("PDF: ", output_pdf, "\n", sep = "")
