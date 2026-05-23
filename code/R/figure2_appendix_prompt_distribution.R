library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)

get_script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) > 0) return(dirname(normalizePath(sub("^--file=", "", file_arg))))
  if (!is.null(sys.frames()[[1]]$ofile)) return(dirname(normalizePath(sys.frames()[[1]]$ofile)))
  getwd()
}

script_dir <- get_script_dir()

data_dir <- file.path(dirname(dirname(script_dir)), "data")
input_excel <- file.path(data_dir, "prompts.xlsx")

if (!file.exists(input_excel)) {
  input_excel <- file.path(script_dir, "prompts.xlsx")
}

output_dir <- file.path(dirname(dirname(script_dir)), "latex", "peerj", "images")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

output_pdf <- file.path(output_dir, "figure2.pdf")

prompt_df <- read_excel(input_excel, sheet = "prompt") %>%
  mutate(
    Set = str_trim(as.character(Set)),
    Content = str_trim(as.character(Content)),
    Style = str_trim(as.character(Style)),
    Complexity = str_trim(as.character(Complexity)),
    Context = str_trim(as.character(Context)),
    Content = str_replace_all(Content, "[–—]", "-")
  )

main_sets <- c("Calibration", "Benchmark", "Extended")

level_order <- c(
  "Landscape", "Portrait", "Object-Concept", "Complex Scene",
  "Realistic", "Stylized",
  "Simple", "Moderate", "High",
  "General", "Educational", "Chinese-cultural"
)

plot_df <- prompt_df %>%
  filter(Set %in% main_sets) %>%
  select(Set, Content, Style, Complexity, Context) %>%
  pivot_longer(
    cols = c(Content, Style, Complexity, Context),
    names_to = "Dimension",
    values_to = "Level"
  ) %>%
  mutate(
    Set = factor(Set, levels = main_sets),
    Dimension = factor(Dimension, levels = c("Content", "Style", "Complexity", "Context")),
    Level = str_replace_all(Level, "[–—]", "-"),
    Level = factor(Level, levels = level_order)
  ) %>%
  count(Set, Dimension, Level, name = "n") %>%
  group_by(Set, Dimension) %>%
  mutate(
    prop = n / sum(n),
    label = as.character(n)
  ) %>%
  ungroup()

level_colors <- c(
  "Landscape" = "#4E79A7",
  "Portrait" = "#F28E2B",
  "Object-Concept" = "#59A14F",
  "Complex Scene" = "#E15759",
  "Realistic" = "#76B7B2",
  "Stylized" = "#EDC948",
  "Simple" = "#B07AA1",
  "Moderate" = "#FF9DA7",
  "High" = "#9C755F",
  "General" = "#BAB0AC",
  "Educational" = "#86BCB6",
  "Chinese-cultural" = "#D37295"
)

p <- ggplot(plot_df, aes(x = Dimension, y = prop, fill = Level)) +
  geom_col(width = 0.72, linewidth = 0.25, color = "white") +
  geom_text(
    aes(label = label),
    position = position_stack(vjust = 0.5),
    size = 2.7,
    color = "black"
  ) +
  facet_wrap(~ Set, nrow = 1) +
  scale_fill_manual(values = level_colors, drop = FALSE) +
  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, 0.25),
    labels = function(x) paste0(round(x * 100), "%"),
    expand = expansion(mult = c(0, 0.02))
  ) +
  labs(
    x = NULL,
    y = "Proportion of prompts within each dimension",
    fill = "Level"
  ) +
  theme_bw(base_size = 10) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 8),
    legend.key.size = unit(0.35, "cm"),
    strip.background = element_rect(fill = "grey92", linewidth = 0.3),
    strip.text = element_text(face = "bold", size = 10),
    axis.text.x = element_text(angle = 35, hjust = 1, vjust = 1),
    axis.title.y = element_text(size = 10),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.margin = margin(6, 6, 6, 6)
  ) +
  guides(fill = guide_legend(nrow = 3, byrow = TRUE))

ggsave(output_pdf, p, width = 6.5, height = 4.3, units = "in", device = cairo_pdf, bg = "white")

message("Saved: ", output_pdf)
