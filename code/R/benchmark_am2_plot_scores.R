required_pkgs <- c(
  "readxl", "dplyr", "tidyr", "stringr", "ggplot2",
  "forcats", "purrr", "writexl", "rlang"
)

to_install <- required_pkgs[!required_pkgs %in% installed.packages()[, "Package"]]
if (length(to_install) > 0) {
  install.packages(to_install, repos = "https://cloud.r-project.org")
}

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(forcats)
library(purrr)
library(writexl)
library(rlang)

script_dir <- if (exists("get_script_dir")) get_script_dir() else getwd()
data_dir <- file.path(dirname(dirname(script_dir)), "data")

input_excel <- file.path(data_dir, "benchmark_subjective_scores_ori_image_level.xlsx")
output_dir  <- data_dir
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

recommended_threshold <- 3.5
acceptable_threshold  <- 3.0

raw_df <- read_excel(input_excel, sheet = 1)

rename_by_match <- function(df) {
  old_names <- names(df)
  old_names <- stringr::str_replace_all(old_names, "\u3000", " ")
  old_names <- stringr::str_trim(old_names)
  names(df) <- old_names

  name_map <- c(
    "ImageName" = "image_name",
    "ModelShortName" = "model",
    "PromptID" = "prompt_id",
    "Language" = "language",
    "ContentAttribute" = "content",
    "StyleAttribute" = "style",
    "ComplexityAttribute" = "complexity",
    "ContextAttribute" = "context",
    "MeanAdherence" = "follow_mean",
    "AdherenceSD" = "follow_sd",
    "MeanClarity" = "clarity_mean",
    "ClaritySD" = "clarity_sd",
    "MeanAesthetics" = "aesthetic_mean",
    "AestheticsSD" = "aesthetic_sd",
    "CompositeScore" = "overall_mean",
    "ValidRaterCount" = "n_raters",
    "ElapsedSeconds" = "time_sec",
    "VRAMUsage" = "vram"
  )

  matched_old <- intersect(names(name_map), names(df))
  names(df)[match(matched_old, names(df))] <- unname(name_map[matched_old])

  return(df)
}

df <- rename_by_match(raw_df)

required_cols <- c(
  "image_name", "model", "prompt_id", "language",
  "content", "style", "complexity", "context",
  "follow_mean", "clarity_mean", "aesthetic_mean", "overall_mean",
  "n_raters", "time_sec", "vram"
)

missing_cols <- setdiff(required_cols, names(df))
if (length(missing_cols) > 0) {
  stop(
    paste0(
      "Missing required columns: ", paste(missing_cols, collapse = ", "),
      "\nColumns read from the Excel file:\n",
      paste(names(df), collapse = " | "),
      "\nPlease check whether the first-row headers in the Excel file match the script settings."
    )
  )
}

df <- df %>%
  mutate(
    language   = str_trim(as.character(language)),
    model      = str_trim(as.character(model)),
    content    = str_trim(as.character(content)),
    style      = str_trim(as.character(style)),
    complexity = str_trim(as.character(complexity)),
    context    = str_trim(as.character(context))
  )

df_chinese <- df %>%
  filter(tolower(language) == "chinese")

if (nrow(df_chinese) == 0) {
  stop("No data remained after filtering language == 'chinese'. Please check the values in the Language column.")
}

content_levels    <- c("Landscape", "Portrait", "Object–Concept", "Object-Concept", "Complex Scene")
style_levels      <- c("Realistic", "Stylized")
complexity_levels <- c("Simple", "Moderate", "High")
context_levels    <- c("General", "Educational", "Chinese-cultural")

df_chinese <- df_chinese %>%
  mutate(
    content = str_replace_all(content, fixed("Object-Concept"), "Object–Concept")
  )

df_chinese <- df_chinese %>%
  mutate(
    content = factor(content, levels = c("Landscape", "Portrait", "Object–Concept", "Complex Scene")),
    style = factor(style, levels = style_levels),
    complexity = factor(complexity, levels = complexity_levels),
    context = factor(context, levels = context_levels)
  )

model_levels <- sort(unique(df_chinese$model))
df_chinese <- df_chinese %>%
  mutate(model = factor(model, levels = model_levels))

summarise_by_dimension <- function(data, dim_col) {
  dim_sym <- rlang::sym(dim_col)

  data %>%
    group_by(model, !!dim_sym) %>%
    summarise(
      n = n(),
      overall_mean_mean = mean(overall_mean, na.rm = TRUE),
      overall_mean_sd   = sd(overall_mean, na.rm = TRUE),
      follow_mean_mean  = mean(follow_mean, na.rm = TRUE),
      follow_mean_sd    = sd(follow_mean, na.rm = TRUE),
      clarity_mean_mean = mean(clarity_mean, na.rm = TRUE),
      clarity_mean_sd   = sd(clarity_mean, na.rm = TRUE),
      aesthetic_mean_mean = mean(aesthetic_mean, na.rm = TRUE),
      aesthetic_mean_sd   = sd(aesthetic_mean, na.rm = TRUE),
      follow_status = case_when(
        mean(follow_mean, na.rm = TRUE) >= recommended_threshold ~ "Recommended",
        mean(follow_mean, na.rm = TRUE) >= acceptable_threshold  ~ "Acceptable",
        TRUE ~ "Below"
      ),
      .groups = "drop"
    ) %>%
    rename(level = !!dim_sym) %>%
    mutate(dimension = dim_col) %>%
    select(
      dimension, level, model, n,
      overall_mean_mean, overall_mean_sd,
      follow_mean_mean, follow_mean_sd, follow_status,
      clarity_mean_mean, clarity_mean_sd,
      aesthetic_mean_mean, aesthetic_mean_sd
    )
}

summary_content    <- summarise_by_dimension(df_chinese, "content")
summary_style      <- summarise_by_dimension(df_chinese, "style")
summary_complexity <- summarise_by_dimension(df_chinese, "complexity")
summary_context    <- summarise_by_dimension(df_chinese, "context")

summary_all <- bind_rows(
  summary_content,
  summary_style,
  summary_complexity,
  summary_context
)

make_heatmap <- function(summary_df, dim_name, out_png, out_pdf) {

  if (dim_name == "content") {
    summary_df$level <- factor(summary_df$level,
                               levels = c("Landscape", "Portrait", "Object–Concept", "Complex Scene"))
    plot_title <- "Content"
  } else if (dim_name == "style") {
    summary_df$level <- factor(summary_df$level,
                               levels = c("Realistic", "Stylized"))
    plot_title <- "Style"
  } else if (dim_name == "complexity") {
    summary_df$level <- factor(summary_df$level,
                               levels = c("Simple", "Moderate", "High"))
    plot_title <- "Complexity"
  } else if (dim_name == "context") {
    summary_df$level <- factor(summary_df$level,
                               levels = c("General", "Educational", "Chinese-cultural"))
    plot_title <- "Context"
  } else {
    plot_title <- dim_name
  }

  summary_df <- summary_df %>%
    mutate(
      status_label = case_when(
        follow_status == "Recommended" ~ "R",
        follow_status == "Acceptable"  ~ "A",
        TRUE ~ "B"
      ),
      label_text = paste0(
        sprintf("%.2f", overall_mean_mean),
        "\n",
        "n=", n, " | ", status_label
      )
    )

  p <- ggplot(summary_df, aes(x = model, y = level, fill = overall_mean_mean)) +
    geom_tile(color = "black", linewidth = 0.45) +
    geom_text(aes(label = label_text), size = 3.4, lineheight = 0.95, family = "sans") +
    scale_fill_gradient(
      low = "#F7FBFF",
      high = "#08519C",
      name = "Composite\nScore"
    ) +
    labs(
      title = plot_title,
      x = "Model",
      y = NULL,
      caption = paste0(
        "Cell label: mean composite score; n = sample size; ",
        "R = adherence ≥ ", recommended_threshold,
        "; A = ", acceptable_threshold, "–", recommended_threshold - 0.01,
        "; B = < ", acceptable_threshold
      )
    ) +
    theme_minimal(base_size = 12, base_family = "sans") +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5, size = 13),
      axis.title.x = element_text(face = "bold", size = 11),
      axis.text.x = element_text(face = "bold", color = "black", size = 10),
      axis.text.y = element_text(color = "black", size = 10),
      panel.grid = element_blank(),
      panel.border = element_rect(fill = NA, color = "black", linewidth = 0.6),
      legend.title = element_text(face = "bold", size = 10),
      legend.text = element_text(size = 9),
      plot.caption = element_text(size = 9, hjust = 0)
    )

  ggsave(out_png, p, width = 7.2, height = 4.8, dpi = 600, bg = "white")

  return(p)
}

p_content <- make_heatmap(
  summary_content,
  "content",
  file.path(output_dir, "benchmark_am2_heatmap_content.png"),
  file.path(output_dir, "benchmark_am2_heatmap_content.pdf")
)

p_style <- make_heatmap(
  summary_style,
  "style",
  file.path(output_dir, "benchmark_am2_heatmap_style.png"),
  file.path(output_dir, "benchmark_am2_heatmap_style.pdf")
)

p_complexity <- make_heatmap(
  summary_complexity,
  "complexity",
  file.path(output_dir, "benchmark_am2_heatmap_complexity.png"),
  file.path(output_dir, "benchmark_am2_heatmap_complexity.pdf")
)

p_context <- make_heatmap(
  summary_context,
  "context",
  file.path(output_dir, "benchmark_am2_heatmap_context.png"),
  file.path(output_dir, "benchmark_am2_heatmap_context.pdf")
)


rank_with_focus <- function(df, extra_focus = c("none", "clarity", "aesthetic")) {
  extra_focus <- match.arg(extra_focus)

  if (extra_focus == "clarity") {
    df %>% arrange(desc(overall_mean_mean), clarity_mean_sd, overall_mean_sd, desc(follow_mean_mean))
  } else if (extra_focus == "aesthetic") {
    df %>% arrange(desc(overall_mean_mean), aesthetic_mean_sd, overall_mean_sd, desc(follow_mean_mean))
  } else {
    df %>% arrange(desc(overall_mean_mean), overall_mean_sd, desc(follow_mean_mean))
  }
}

rank_models_for_level <- function(summary_df, target_level, extra_focus = c("none", "clarity", "aesthetic")) {
  extra_focus <- match.arg(extra_focus)

  sub <- summary_df %>%
    filter(level == target_level)

  rec_sub <- sub %>% filter(follow_status == "Recommended")
  acc_sub <- sub %>% filter(follow_status == "Acceptable")

  if (nrow(rec_sub) > 0) {
    ranked <- rank_with_focus(rec_sub, extra_focus)
    candidate_group <- "Recommended"
  } else if (nrow(acc_sub) > 0) {
    ranked <- rank_with_focus(acc_sub, extra_focus)
    candidate_group <- "Acceptable"
  } else {
    ranked <- sub %>% arrange(desc(follow_mean_mean), desc(overall_mean_mean), overall_mean_sd)
    candidate_group <- "Below"
  }

  top_score <- ranked$overall_mean_mean[1]
  ranked <- ranked %>%
    mutate(
      score_gap_from_top = top_score - overall_mean_mean
    )

  list(
    ranked_table = ranked,
    candidate_group = candidate_group
  )
}

build_recommendation_text <- function(summary_df, target_level, scene_title, scene_desc,
                                      extra_focus = c("none", "clarity", "aesthetic")) {
  extra_focus <- match.arg(extra_focus)

  res <- rank_models_for_level(summary_df, target_level, extra_focus)
  ranked <- res$ranked_table
  candidate_group <- res$candidate_group

  extra_text <- switch(
    extra_focus,
    "clarity" = "with additional consideration of clarity stability, where a smaller clarity SD is preferred",
    "aesthetic" = "with additional consideration of aesthetic stability, where a smaller aesthetics SD is preferred",
    "none" = "with additional consideration of composite-score stability"
  )

  if (candidate_group == "Recommended") {
    top1 <- ranked[1, , drop = FALSE]
    backup_models <- ranked %>% slice(-1)
    backup_text <- if (nrow(backup_models) > 0) {
      paste0("Other models reaching the Recommended tier include: ", paste(as.character(backup_models$model), collapse = ", "), ".")
    } else {
      ""
    }

    recommendation <- paste0(
      "[", scene_title, "]\n",
      "Priority attribute level: ", target_level, "\n",
      "Conclusion type: Preferred recommendation\n",
      "Recommended model: ", as.character(top1$model), "\n",
      "Rationale: At this attribute level, the model reached the recommended deployment threshold (mean adherence >= ", recommended_threshold,
      ") and had the highest composite score; ", extra_text, ".\n",
      "Statistics: mean composite score = ", sprintf("%.3f", top1$overall_mean_mean),
      ", composite-score SD = ", sprintf("%.3f", top1$overall_mean_sd),
      ", mean adherence = ", sprintf("%.3f", top1$follow_mean_mean),
      ", sample size n = ", top1$n, ".\n",
      backup_text, ifelse(backup_text == "", "", "\n"),
      "Applicable scenarios: ", scene_desc, "\n"
    )
  } else if (candidate_group == "Acceptable") {
    top1 <- ranked[1, , drop = FALSE]
    candidate_text <- if (nrow(ranked) > 1) {
      paste0("Other models reaching the Acceptable tier include: ", paste(as.character(ranked$model[-1]), collapse = ", "), ".")
    } else {
      ""
    }

    recommendation <- paste0(
      "[", scene_title, "]\n",
      "Priority attribute level: ", target_level, "\n",
      "Conclusion type: Acceptable with manual review\n",
      "Preferred candidate model: ", as.character(top1$model), "\n",
      "Rationale: No model reached the recommended deployment threshold at this attribute level (mean adherence >= ", recommended_threshold,
      "). The current top-ranked model by composite score was ", as.character(top1$model),
      "; its mean adherence was in the acceptable range (", acceptable_threshold, "-", recommended_threshold - 0.01,
      "), so it can be used as a conditional candidate. Manual review is recommended before classroom use.\n",
      candidate_text, ifelse(candidate_text == "", "", "\n"),
      "Applicable scenarios: ", scene_desc, "\n"
    )
  } else {
    recommendation <- paste0(
      "[", scene_title, "]\n",
      "Priority attribute level: ", target_level, "\n",
      "Conclusion type: No priority deployment model\n",
      "Rationale: At this attribute level, the mean adherence scores of all models were below the acceptable threshold (", acceptable_threshold,
      "), so no model is recommended as a priority deployment option.\n",
      "Applicable scenarios: ", scene_desc, "\n"
    )
  }

  list(
    ranked_table = ranked,
    candidate_group = candidate_group,
    text = recommendation
  )
}

rec_educational <- build_recommendation_text(
  summary_context,
  target_level = "Educational",
  scene_title = "EducationalContextTask",
  scene_desc = "TeachingCaseFiguresAndEducationalContextIllustrations",
  extra_focus = "none"
)

rec_chinese_culture <- build_recommendation_text(
  summary_context,
  target_level = "Chinese-cultural",
  scene_title = "ChineseCulturalContextTask",
  scene_desc = "ChineseCulturalTeachingAndIdiomOrPoetryIllustrations",
  extra_focus = "none"
)

rec_realistic <- build_recommendation_text(
  summary_style,
  target_level = "Realistic",
  scene_title = "RealisticDisplayTask",
  scene_desc = "CoursewareIllustrationsProjectionDisplayTeachingDiagramsAndDetailObservation",
  extra_focus = "clarity"
)

rec_stylized <- build_recommendation_text(
  summary_style,
  target_level = "Stylized",
  scene_title = "ArtisticIntroAndCreativeDisplayTask",
  scene_desc = "ClassroomIntroductoryImagesCreativeTeachingLiteratureAndArtCoursesAndStudentCreationExamples",
  extra_focus = "aesthetic"
)

rec_high_complexity <- build_recommendation_text(
  summary_complexity,
  target_level = "High",
  scene_title = "ComplexPromptTask",
  scene_desc = "MultiObjectTeachingScenesComplexRelationDiagramsSituatedTeachingFiguresAndMultiPersonScenes",
  extra_focus = "none"
)

rec_object_concept <- build_recommendation_text(
  summary_content,
  target_level = "Object–Concept",
  scene_title = "ObjectConceptExpressionTask",
  scene_desc = "KnowledgePointFiguresConceptExplanationFiguresObjectConceptRelationExpressionAndInstructionalExplanatoryImages",
  extra_focus = "none"
)

recommendation_rankings <- bind_rows(
  rec_educational$ranked_table %>% mutate(scene = "EducationalContextTask", level_target = "Educational", candidate_group = rec_educational$candidate_group),
  rec_chinese_culture$ranked_table %>% mutate(scene = "ChineseCulturalContextTask", level_target = "Chinese-cultural", candidate_group = rec_chinese_culture$candidate_group),
  rec_realistic$ranked_table %>% mutate(scene = "RealisticDisplayTask", level_target = "Realistic", candidate_group = rec_realistic$candidate_group),
  rec_stylized$ranked_table %>% mutate(scene = "ArtisticIntroAndCreativeDisplayTask", level_target = "Stylized", candidate_group = rec_stylized$candidate_group),
  rec_high_complexity$ranked_table %>% mutate(scene = "ComplexPromptTask", level_target = "High", candidate_group = rec_high_complexity$candidate_group),
  rec_object_concept$ranked_table %>% mutate(scene = "ObjectConceptExpressionTask", level_target = "Object–Concept", candidate_group = rec_object_concept$candidate_group)
) %>%
  group_by(scene) %>%
  mutate(rank = row_number()) %>%
  ungroup() %>%
  select(
    scene, rank, candidate_group, model, level_target, n,
    overall_mean_mean, overall_mean_sd,
    follow_mean_mean, follow_status,
    clarity_mean_mean, clarity_mean_sd,
    aesthetic_mean_mean, aesthetic_mean_sd,
    score_gap_from_top
  )

write_xlsx(
  list(
    chinese_data = df_chinese,
    summary_content = summary_content,
    summary_style = summary_style,
    summary_complexity = summary_complexity,
    summary_context = summary_context,
    summary_all = summary_all,
    recommendation_rankings = recommendation_rankings
  ),
  path = file.path(output_dir, "benchmark_am2_summary_results.xlsx")
)

recommendation_text <- paste(
  rec_educational$text,
  "
",
  rec_chinese_culture$text,
  "
",
  rec_realistic$text,
  "
",
  rec_stylized$text,
  "
",
  rec_high_complexity$text,
  "
",
  rec_object_concept$text,
  sep = ""
)

# writeLines(
#   recommendation_text,
#   con = file.path(output_dir, "benchmark_am2_recommendations.txt"),
#   useBytes = TRUE
# )

cat("========================================\n")
cat("Benchmark AM2 analysis completed.\n")
cat("Chinese group sample size: ", nrow(df_chinese), "\n")
cat("Models: ", paste(levels(df_chinese$model), collapse = ", "), "\n")
cat("Output directory: ", normalizePath(output_dir), "\n")
cat("Recommended threshold: ", recommended_threshold, "\n")
cat("Acceptable threshold: ", acceptable_threshold, "\n")
cat("========================================\n\n")

cat("[Automatically generated use recommendations]\n\n")
# cat(recommendation_text)
