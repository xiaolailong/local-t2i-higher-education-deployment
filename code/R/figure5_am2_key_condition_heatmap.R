required_pkgs <- c("readxl", "dplyr", "ggplot2", "grid")
for (p in required_pkgs) {
  if (!requireNamespace(p, quietly = TRUE)) {
    install.packages(p, repos = "https://cloud.r-project.org")
  }
}

library(readxl)
library(dplyr)
library(ggplot2)
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

output_dir <- file.path(dirname(dirname(script_dir)), "latex", "peerj", "images")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

input_excel <- file.path(data_dir, "benchmark_am2_summary_results.xlsx")
output_pdf  <- file.path(output_dir, "figure5.pdf")

summary_content <- read_excel(input_excel, sheet = "summary_content")
summary_context <- read_excel(input_excel, sheet = "summary_context")
summary_style <- read_excel(input_excel, sheet = "summary_style")
summary_complexity <- read_excel(input_excel, sheet = "summary_complexity")

plot_df <- bind_rows(
  summary_context %>%
    filter(level %in% c("Educational", "Chinese-cultural")) %>%
    mutate(condition_raw = level),

  summary_style %>%
    filter(level %in% c("Realistic", "Stylized")) %>%
    mutate(condition_raw = level),

  summary_complexity %>%
    filter(level %in% c("High")) %>%
    mutate(condition_raw = level),

  summary_content %>%
    filter(level %in% c("Object–Concept", "Object-Concept")) %>%
    mutate(condition_raw = "Object–Concept")
) %>%
  mutate(
    condition = factor(
      condition_raw,
      levels = c(
        "Educational",
        "Chinese-cultural",
        "Realistic",
        "Stylized",
        "High",
        "Object–Concept"
      ),
      labels = c(
        "Educational\ncontext",
        "Chinese-\ncultural",
        "Realistic\ndisplay",
        "Stylized\ndisplay",
        "High\ncomplexity",
        "Object--\nConcept"
      )
    ),
    model = factor(
      as.character(model),
      levels = c("qwen", "zimage", "sdxl"),
      labels = c("Qwen", "Z-Image", "SDXL")
    ),
    status_code = case_when(
      follow_status == "Recommended" ~ "R",
      follow_status == "Acceptable"  ~ "A",
      TRUE ~ "B"
    ),
    cell_label = sprintf("%.2f\n%s", overall_mean_mean, status_code),
    text_colour = if_else(overall_mean_mean >= 3.55, "white", "black")
  )

plot_df$model <- factor(
  plot_df$model,
  levels = rev(c("Qwen", "Z-Image", "SDXL"))
)

p <- ggplot(plot_df, aes(x = condition, y = model, fill = overall_mean_mean)) +
  geom_tile(color = "white", linewidth = 0.55) +
  geom_text(
    aes(label = cell_label, colour = text_colour),
    size = 2.25,
    lineheight = 0.86,
    family = "sans"
  ) +
  scale_colour_identity() +
  scale_fill_gradientn(
    colours = c("#F7FBFF", "#DEEBF7", "#9ECAE1", "#4292C6", "#084594"),
    name = "Composite score",
    limits = c(2.2, 4.3),
    breaks = c(2.5, 3.0, 3.5, 4.0)
  ) +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_size = 8.0, base_family = "sans") +
  theme(
    panel.grid = element_blank(),

    axis.text.x = element_text(
      size = 6.6,
      color = "black",
      lineheight = 0.88,
      margin = margin(t = 2)
    ),
    axis.text.y = element_text(
      size = 7.8,
      color = "black"
    ),

    legend.position = "top",
    legend.direction = "horizontal",
    legend.title = element_text(size = 7.2, hjust = 0.5),
    legend.text = element_text(size = 6.8),
    legend.margin = margin(0, 0, 0, 0),
    legend.box.margin = margin(0, 0, 1, 0),

    plot.margin = margin(2, 3, 3, 2)
  ) +
  guides(
    fill = guide_colorbar(
      title.position = "top",
      title.hjust = 0.5,
      direction = "horizontal",
      barwidth = unit(34, "mm"),
      barheight = unit(3.0, "mm"),
      ticks.colour = "black",
      frame.colour = "black"
    )
  )

ggsave(output_pdf, p, width = 3.45, height = 2.45, device = cairo_pdf, bg = "white")

cat("Output completed:\n")
cat(output_pdf, "\n")
