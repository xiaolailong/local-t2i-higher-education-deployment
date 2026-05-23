library(readxl)
library(dplyr)
library(ggplot2)


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

objective_excel <- file.path(file.path(data_dir, "extended_analysis"), "normalized_scores.xlsx")
prompt_excel <- file.path(data_dir, "prompts.xlsx")
output_excel <- file.path(file.path(data_dir, "extended_analysis"), "r_analysis_result.xlsx")

input_excel <- file.path(file.path(data_dir, "extended_analysis"), "r_analysis_result.xlsx")
output_pdf  <- file.path(output_dir, "figure8.pdf")

emm <- read_excel(input_excel, sheet = "LMM_bilingual_emmeans") %>%
  mutate(
    model = factor(
      model,
      levels = c("qwen", "zimage", "sdxl"),
      labels = c("Qwen", "Z-Image", "SDXL")
    ),
    language = factor(
      language,
      levels = c("chinese", "english"),
      labels = c("Chinese", "English")
    )
  )

# Okabe--Ito colorblind-friendly palette, consistent across model figures.
model_colors <- c(
  "Qwen" = "#0072B2",
  "Z-Image" = "#E69F00",
  "SDXL" = "#009E73"
)

p <- ggplot(
  emm,
  aes(
    x = language,
    y = emmean,
    group = model,
    colour = model,
    linetype = model,
    shape = model
  )
) +
  geom_line(linewidth = 0.75) +
  geom_point(size = 3.0) +
  geom_errorbar(
    aes(ymin = lower.CL, ymax = upper.CL),
    width = 0.08,
    linewidth = 0.55
  ) +
  labs(
    x = "Prompt language",
    y = "EMM of weighted objective score",
    colour = "Model",
    linetype = "Model",
    shape = "Model"
  ) +
  scale_colour_manual(values = model_colors) +
  theme_classic(base_size = 13) +
  theme(
    legend.position = "right",
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 11),
    legend.key.width = unit(1.2, "lines"),
    legend.key.height = unit(1.0, "lines")
  )

ggsave(output_pdf, p, width = 6.5, height = 4.2, device = cairo_pdf, bg = "white")