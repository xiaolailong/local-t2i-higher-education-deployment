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

output_dir <- file.path(dirname(dirname(script_dir)), "latex", "peerj", "images")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

input_file <- file.path(file.path(data_dir, "calibration_analysis"), "summary_statistics.xlsx")
output_file <- file.path(output_dir, "figureC1.pdf")

df <- read_excel(input_file, sheet = "ParameterGroupRank") %>%
  mutate(
    Model = dplyr::case_when(
      stringr::str_trim(stringr::str_to_lower(ModelShortName)) == "qwen" ~ "Qwen-Image-2512",
      stringr::str_trim(stringr::str_to_lower(ModelShortName)) == "zimage" ~ "Z-Image-Turbo",
      stringr::str_trim(stringr::str_to_lower(ModelShortName)) == "sdxl" ~ "SDXL-Turbo",
      TRUE ~ as.character(ModelShortName)
    ),
    Resolution = str_replace(Resolution, "\\*", "×"),
    Steps = as.numeric(Steps)
  )

plot_df <- df %>%
  select(
    Model, Resolution, Steps,
    Chinese_Adjusted = ChineseStabilityAdjustedScore,
    English_Adjusted = EnglishStabilityAdjustedScore
  ) %>%
  pivot_longer(
    cols = c(Chinese_Adjusted, English_Adjusted),
    names_to = "Language",
    values_to = "AdjustedScore"
  ) %>%
  mutate(
    Language = dplyr::case_when(
      Language == "Chinese_Adjusted" ~ "Chinese",
      Language == "English_Adjusted" ~ "English",
      TRUE ~ as.character(Language)
    ),
    Model = factor(
      Model,
      levels = c("Qwen-Image-2512", "Z-Image-Turbo", "SDXL-Turbo")
    ),
    Language = factor(Language, levels = c("Chinese", "English")),
    Resolution = factor(Resolution)
  )

# Colorblind-friendly palette for resolution traces. Line type and point shape are
# retained so that the figure remains interpretable in grayscale printing. The
# palette is mapped dynamically to the resolution levels found in the data.
resolution_palette <- c("#0072B2", "#E69F00", "#009E73", "#CC79A7", "#56B4E9")
resolution_colors <- setNames(
  resolution_palette[seq_along(levels(plot_df$Resolution))],
  levels(plot_df$Resolution)
)

p <- ggplot(
  plot_df,
  aes(
    x = Steps,
    y = AdjustedScore,
    group = Resolution,
    colour = Resolution,
    linetype = Resolution,
    shape = Resolution
  )
) +
  geom_line(linewidth = 0.50) +
  geom_point(size = 2.1) +
  facet_grid(Language ~ Model, scales = "free_x") +
  labs(
    x = "Sampling steps",
    y = "Stability-adjusted weighted objective composite score",
    colour = "Resolution",
    linetype = "Resolution",
    shape = "Resolution"
  ) +
  scale_colour_manual(values = resolution_colors) +
  theme_bw(base_size = 9) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 8),
    strip.text = element_text(size = 8.5, face = "bold"),
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 9),
    panel.grid.minor = element_blank()
  )

ggsave(
  filename = output_file,
  plot = p,
  width = 6.5,
  height = 4.6,
  units = "in",
  device = cairo_pdf,
  dpi = 600,
  bg = "white"
)