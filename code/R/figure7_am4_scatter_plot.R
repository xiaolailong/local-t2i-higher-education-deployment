suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(ggplot2)
})

get_script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) > 0) return(dirname(normalizePath(sub("^--file=", "", file_arg))))
  if (!is.null(sys.frames()[[1]]$ofile)) return(dirname(normalizePath(sys.frames()[[1]]$ofile)))
  getwd()
}

script_dir <- get_script_dir()
data_dir <- file.path(dirname(dirname(script_dir)), "data")

output_dir <- file.path(dirname(dirname(script_dir)), "latex", "peerj", "images")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

input_excel <- file.path(data_dir, "benchmark_am4_deployment_summary.xlsx")
output_pdf  <- file.path(output_dir, "figure7.pdf")

df <- read_excel(input_excel, sheet = "ModelSummary") %>%
  mutate(
    model = factor(ModelShortName, levels = c("qwen", "zimage", "sdxl"),
                   labels = c("Qwen", "Z-Image", "SDXL")),
    time_s = MeanInferenceTime,
    vram_gb = MeanVRAM / 1024,
    label = paste0(
      as.character(model), "\n",
      sprintf("%.1f s, %.1f GB", time_s, vram_gb)
    ),
    label_x = case_when(
      model == "Qwen" ~ time_s - 12,
      model == "Z-Image" ~ time_s + 11,
      model == "SDXL" ~ time_s + 14,
      TRUE ~ time_s
    ),
    label_y = case_when(
      model == "SDXL" ~ MeanComposite - 0.28,
      TRUE ~ MeanComposite - 0.34
    ),
    label_hjust = 0.5
  )

legend_breaks <- c(8, 10, 12, 14)

# Okabe--Ito colorblind-friendly palette, consistent across model figures.
model_colors <- c(
  "Qwen" = "#0072B2",
  "Z-Image" = "#E69F00",
  "SDXL" = "#009E73"
)
model_fills <- c(
  "Qwen" = "#D7EAF7",
  "Z-Image" = "#FCE4B6",
  "SDXL" = "#D8F0E5"
)

p <- ggplot(df, aes(x = time_s, y = MeanComposite)) +
  geom_point(
    aes(size = vram_gb, fill = model, colour = model),
    shape = 21,
    stroke = 0.8,
    alpha = 0.95,
    show.legend = c(size = TRUE, fill = FALSE, colour = FALSE)
  ) +
  geom_text(
    aes(x = label_x, y = label_y, label = label, hjust = label_hjust),
    size = 4.0,
    lineheight = 0.95
  ) +
  scale_size_area(
    name = "Mean VRAM (GB)",
    breaks = legend_breaks,
    max_size = 24,
    limits = c(min(legend_breaks), max(legend_breaks))
  ) +
  scale_fill_manual(values = model_fills) +
  scale_colour_manual(values = model_colors) +
  scale_x_continuous(
    limits = c(0, 195),
    breaks = c(0, 10, 30, 60, 90, 120, 150, 180),
    expand = expansion(mult = c(0.08, 0.03))
  ) +
  scale_y_continuous(
    limits = c(2.35, 4.45),
    breaks = seq(2.5, 4.25, by = 0.25),
    expand = expansion(mult = c(0.03, 0.08))
  ) +
  labs(
    x = "Mean generation time (s)",
    y = "Mean composite score"
  ) +
  guides(
    size = guide_legend(
      title.position = "top",
      title.hjust = 0.5,
      label.position = "right",
      keyheight = unit(0.75, "cm"),
      keywidth = unit(1.0, "cm"),
      override.aes = list(shape = 21, fill = "grey85", color = "grey25", alpha = 1)
    )
  ) +
  theme_bw(base_size = 13) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(linewidth = 0.3, colour = "grey85"),
    axis.title = element_text(size = 13, face = "bold"),
    axis.text = element_text(size = 11.5, color = "black"),
    legend.position = "right",
    legend.direction = "vertical",
    legend.box = "vertical",
    legend.background = element_blank(),
    legend.key = element_rect(fill = "white", color = "white"),
    legend.title = element_text(size = 12.5, face = "bold"),
    legend.text = element_text(size = 11.5),
    plot.margin = margin(10, 8, 8, 8)
  )

ggsave(output_pdf, p, width = 6.5, height = 4.85, units = "in", device = cairo_pdf, bg = "white")