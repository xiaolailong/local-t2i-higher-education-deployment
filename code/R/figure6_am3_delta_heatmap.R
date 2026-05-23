suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
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

input_excel <- file.path(data_dir, "benchmark_am3_bilingual_results.xlsx")
output_pdf  <- file.path(output_dir, "figure6.pdf")

comparison_tbl <- read_excel(input_excel, sheet = "cn_en_comparison")

plot_df <- comparison_tbl %>%
  transmute(
    Model,
    Composite = delta_composite,
    Adherence = delta_adherence,
    Clarity = delta_clarity,
    Aesthetic = delta_aesthetic
  ) %>%
  pivot_longer(
    cols = c(Composite, Adherence, Clarity, Aesthetic),
    names_to = "Metric",
    values_to = "Delta"
  ) %>%
  mutate(
    Model = factor(
      Model,
      levels = c("Qwen", "Z-Image", "SDXL-Turbo"),
      labels = c("Qwen", "Z-Image", "SDXL")
    ),
    Metric = factor(Metric, levels = c("Composite", "Adherence", "Clarity", "Aesthetic"))
  )

delta_lim <- max(abs(plot_df$Delta), na.rm = TRUE)

p <- ggplot(plot_df, aes(x = Metric, y = Model, fill = Delta)) +
  geom_tile(colour = "white", linewidth = 0.5) +
  geom_text(aes(label = sprintf("%.3f", Delta)), size = 3.2) +
  scale_fill_gradient2(
    low = "#3B6FB6",
    mid = "white",
    high = "#C03A2B",
    midpoint = 0,
    limits = c(-delta_lim, delta_lim),
    name = "English - Chinese"
  ) +
  labs(
    x = NULL,
    y = NULL
  ) +
  theme_bw(base_size = 11) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(linewidth = 0.6),
    axis.text = element_text(size = 10, colour = "black"),
    legend.position = "right",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9),
    plot.margin = margin(8, 8, 8, 8)
  )

ggsave(output_pdf, p, width = 6.5, height = 3.8, units = "in", device = cairo_pdf, bg = "white")