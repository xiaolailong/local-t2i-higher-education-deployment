required_pkgs <- c("readxl", "dplyr", "ggplot2", "stringr", "grid")
for (p in required_pkgs) {
  if (!requireNamespace(p, quietly = TRUE)) {
    install.packages(p, repos = "https://cloud.r-project.org")
  }
}

library(readxl)
library(dplyr)
library(ggplot2)
library(stringr)
library(grid)

get_script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) > 0) return(dirname(normalizePath(sub("^--file=", "", file_arg))))
  if (!is.null(sys.frames()[[1]]$ofile)) return(dirname(normalizePath(sys.frames()[[1]]$ofile)))
  getwd()
}

script_dir <- get_script_dir()
data_dir <- file.path(dirname(dirname(script_dir)), "data")

input_excel <- file.path(data_dir, "benchmark_am2_summary_results.xlsx")
output_png  <- file.path(data_dir, "benchmark_am2_profile_plot.png")
output_pdf  <- file.path(data_dir, "benchmark_am2_profile_plot.pdf")

summary_content <- read_excel(input_excel, sheet = "summary_content")
summary_context <- read_excel(input_excel, sheet = "summary_context")
summary_style <- read_excel(input_excel, sheet = "summary_style")
summary_complexity <- read_excel(input_excel, sheet = "summary_complexity")

plot_df <- bind_rows(
  summary_context %>%
    filter(level %in% c("Educational", "Chinese-cultural")) %>%
    mutate(scene = level),

  summary_style %>%
    filter(level %in% c("Realistic", "Stylized")) %>%
    mutate(scene = level),

  summary_complexity %>%
    filter(level %in% c("High")) %>%
    mutate(scene = level),

  summary_content %>%
    filter(level %in% c("Object–Concept", "Object-Concept")) %>%
    mutate(scene = "Object–Concept")
) %>%
  mutate(
    scene = factor(
      scene,
      levels = c("Educational", "Chinese-cultural", "Realistic", "Stylized", "High", "Object–Concept"),
      labels = c(
        "Context: Educational",
        "Context: Chinese-cultural",
        "Style: Realistic",
        "Style: Stylized",
        "Complexity: High",
        "Content: Object–Concept"
      )
    ),
    model = factor(
      as.character(model),
      levels = c("qwen", "zimage", "sdxl"),
      labels = c("Qwen", "Z-Image", "SDXL-Turbo")
    ),
    follow_status = factor(
      follow_status,
      levels = c("Recommended", "Acceptable", "Below")
    )
  )

p <- ggplot(
  plot_df,
  aes(x = scene, y = overall_mean_mean, group = model, linetype = model)
) +
  geom_line(linewidth = 0.7, color = "black") +
  geom_point(
    aes(shape = follow_status),
    size = 2.8,
    stroke = 0.9,
    color = "black"
  ) +
  scale_linetype_manual(
    values = c("Qwen" = "solid", "Z-Image" = "dashed", "SDXL-Turbo" = "dotdash"),
    name = "Model"
  ) +
  scale_shape_manual(
    values = c("Recommended" = 16, "Acceptable" = 1, "Below" = 4),
    name = "Adherence status"
  ) +
  scale_y_continuous(
    limits = c(1.5, 4.5),
    breaks = seq(1.5, 4.5, by = 0.5),
    expand = expansion(mult = c(0.02, 0.04))
  ) +
  labs(
    x = NULL,
    y = "Mean composite score"
  ) +
  guides(
    linetype = guide_legend(order = 1, nrow = 1, byrow = TRUE),
    shape = guide_legend(order = 2, nrow = 1, byrow = TRUE)
  ) +
  theme_classic(base_size = 9.5, base_family = "sans") +
  theme(
    axis.text.x = element_text(angle = 20, hjust = 1, size = 8.5),
    axis.text.y = element_text(size = 8.5),
    axis.title.y = element_text(size = 10),
    legend.position = "top",
    legend.box = "vertical",
    legend.direction = "horizontal",
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 8.5),
    legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
    legend.box.margin = margin(t = 0, r = 0, b = 2, l = 0),
    panel.spacing = unit(0.8, "lines")
  )

ggsave(output_png, p, width = 7.8, height = 4.9, dpi = 600, bg = "white")
