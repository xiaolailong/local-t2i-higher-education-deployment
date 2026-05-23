suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(stringr)
  library(tidyr)
  library(openxlsx)
  library(ggplot2)
  library(forcats)
  library(lme4)
  library(lmerTest)
  library(emmeans)
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

input_excel <- file.path(data_dir, "benchmark_subjective_scores_ori_image_level.xlsx")
output_dir  <- data_dir

manual_default_model <- NULL

normalize_language <- function(x) {
  x <- tolower(trimws(as.character(x)))
  case_when(
    x %in% c("chinese", "cn", "zh", "Chinese") ~ "Chinese",
    x %in% c("english", "en", "English") ~ "English",
    TRUE ~ NA_character_
  )
}

normalize_model <- function(x) {
  x <- trimws(as.character(x))
  x_low <- tolower(x)
  case_when(
    x_low %in% c("qwen", "qwen-image", "qwen-image-2512") ~ "Qwen",
    x_low %in% c("z-image", "zimage", "z-image-turbo", "zimage-turbo") ~ "Z-Image",
    x_low %in% c("sdxl", "sdxl-turbo", "sdxl turbo") ~ "SDXL-Turbo",
    TRUE ~ x
  )
}

imbalance_label <- function(abs_delta, rank_position, total_n) {
  if (is.na(abs_delta) || is.na(rank_position) || is.na(total_n)) return(NA_character_)
  if (total_n <= 1) return("Single model only")
  if (rank_position == 1) return("Smallest cross-language shift among models")
  if (rank_position == total_n) return("Largest cross-language shift among models")
  return("Intermediate cross-language shift")
}

default_model_reference_judgement <- function(rank_en, delta_comp, en_mean, best_en_mean) {
  if (is.na(rank_en) || is.na(delta_comp) || is.na(en_mean) || is.na(best_en_mean)) {
    return("Insufficient data")
  }
  gap_to_best <- best_en_mean - en_mean

  if (rank_en == 1 && delta_comp > -0.30) {
    return("Still highly competitive in English")
  } else if (rank_en <= 2 && gap_to_best <= 0.20 && delta_comp > -0.50) {
    return("Still has strong reference value in English")
  } else if (rank_en <= 2 && gap_to_best <= 0.40) {
    return("Still has some reference value in English")
  } else {
    return("Reference value in English is weakened")
  }
}

fmt_p <- function(p) {
  ifelse(is.na(p), NA_character_, ifelse(p < 0.001, "< 0.001", sprintf("%.3f", p)))
}

if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

raw_df <- read_excel(input_excel, sheet = 1)

required_cols <- c(
  "ImageName", "ModelShortName", "PromptID", "Language",
  "ContentAttribute", "StyleAttribute", "ComplexityAttribute", "ContextAttribute",
  "MeanAdherence", "AdherenceSD",
  "MeanClarity", "ClaritySD",
  "MeanAesthetics", "AestheticsSD",
  "CompositeScore", "ValidRaterCount", "ElapsedSeconds", "VRAMUsage"
)

missing_cols <- setdiff(required_cols, names(raw_df))
if (length(missing_cols) > 0) {
  stop("Input data is missing the following columns:\n", paste(missing_cols, collapse = "\n"))
}

df <- raw_df %>%
  mutate(
    Language_std = normalize_language(`Language`),
    ModelStd = normalize_model(`ModelShortName`)
  ) %>%
  filter(!is.na(Language_std), !is.na(ModelStd))

if (nrow(df) == 0) {
  stop("Infothere is no usable data after standardization, please check‘Language’Info‘ModelShortName’fields.")
}

analysis_df <- df %>%
  transmute(
    ImageName = `ImageName`,
    prompt = factor(`PromptID`),
    Model = factor(ModelStd, levels = c("Qwen", "Z-Image", "SDXL-Turbo")),
    Language = factor(Language_std, levels = c("Chinese", "English")),
    ContentAttribute = `ContentAttribute`,
    StyleAttribute = `StyleAttribute`,
    ComplexityAttribute = `ComplexityAttribute`,
    ContextAttribute = `ContextAttribute`,
    CompositeScore = as.numeric(`CompositeScore`),
    MeanAdherence = as.numeric(`MeanAdherence`),
    MeanClarity = as.numeric(`MeanClarity`),
    MeanAesthetics = as.numeric(`MeanAesthetics`),
    ValidRaterCount = as.numeric(`ValidRaterCount`),
    ElapsedSeconds = as.numeric(`ElapsedSeconds`),
    VRAMUsage = as.numeric(`VRAMUsage`)
  )

lang_count <- analysis_df %>% count(Language, Model, name = "n")

english_summary <- analysis_df %>%
  filter(Language == "English") %>%
  group_by(Model) %>%
  summarise(
    n_images = n(),
    mean_composite = mean(CompositeScore, na.rm = TRUE),
    sd_composite   = sd(CompositeScore, na.rm = TRUE),
    mean_adherence = mean(MeanAdherence, na.rm = TRUE),
    sd_adherence   = sd(MeanAdherence, na.rm = TRUE),
    mean_clarity   = mean(MeanClarity, na.rm = TRUE),
    sd_clarity     = sd(MeanClarity, na.rm = TRUE),
    mean_aesthetic = mean(MeanAesthetics, na.rm = TRUE),
    sd_aesthetic   = sd(MeanAesthetics, na.rm = TRUE),
    mean_n_raters  = mean(ValidRaterCount, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(mean_composite)) %>%
  mutate(rank_english = row_number())

chinese_summary <- analysis_df %>%
  filter(Language == "Chinese") %>%
  group_by(Model) %>%
  summarise(
    n_images = n(),
    mean_composite = mean(CompositeScore, na.rm = TRUE),
    sd_composite   = sd(CompositeScore, na.rm = TRUE),
    mean_adherence = mean(MeanAdherence, na.rm = TRUE),
    mean_clarity   = mean(MeanClarity, na.rm = TRUE),
    mean_aesthetic = mean(MeanAesthetics, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(mean_composite)) %>%
  mutate(rank_chinese = row_number())

comparison_tbl <- chinese_summary %>%
  select(
    Model,
    n_images_chinese = n_images,
    composite_chinese = mean_composite,
    adherence_chinese = mean_adherence,
    clarity_chinese   = mean_clarity,
    aesthetic_chinese = mean_aesthetic,
    rank_chinese
  ) %>%
  full_join(
    english_summary %>%
      select(
        Model,
        n_images_english = n_images,
        composite_english = mean_composite,
        adherence_english = mean_adherence,
        clarity_english   = mean_clarity,
        aesthetic_english = mean_aesthetic,
        rank_english
      ),
    by = "Model"
  ) %>%
  mutate(
    delta_composite = composite_english - composite_chinese,
    delta_adherence = adherence_english - adherence_chinese,
    delta_clarity   = clarity_english - clarity_chinese,
    delta_aesthetic = aesthetic_english - aesthetic_chinese,
    rank_change     = rank_english - rank_chinese,
    abs_delta_composite = abs(delta_composite)
  ) %>%
  arrange(rank_chinese) %>%
  mutate(
    shift_rank = min_rank(abs_delta_composite),
    language_balance = mapply(imbalance_label, abs_delta_composite, shift_rank, n())
  )

modeling_df <- analysis_df %>%
  select(prompt, Model, Language, CompositeScore) %>%
  filter(!is.na(prompt), !is.na(Model), !is.na(Language), !is.na(CompositeScore))

contrasts(modeling_df$Model) <- contr.sum(length(levels(modeling_df$Model)))
contrasts(modeling_df$Language) <- contr.sum(length(levels(modeling_df$Language)))

m_comp <- lmer(CompositeScore ~ Model * Language + (1 | prompt), data = modeling_df, REML = FALSE)
mm_anova <- as.data.frame(anova(m_comp, type = 3, ddf = "Satterthwaite"))
mm_anova$Effect <- rownames(mm_anova)
rownames(mm_anova) <- NULL
mm_anova <- mm_anova %>%
  select(Effect, NumDF, DenDF, `F value`, `Pr(>F)`) %>%
  rename(
    numerator_df = NumDF,
    denominator_df = DenDF,
    F_value = `F value`,
    p_value = `Pr(>F)`
  )

fixed_effects_tbl <- as.data.frame(summary(m_comp)$coefficients) %>%
  mutate(term = rownames(.), .before = 1)
rownames(fixed_effects_tbl) <- NULL

emm_model_by_language <- as.data.frame(emmeans(m_comp, ~ Model | Language)) %>%
  rename(
    model = Model,
    language = Language,
    emmean_composite = emmean,
    SE = SE,
    df = df,
    lower_CL = lower.CL,
    upper_CL = upper.CL
  )

english_model_order <- emm_model_by_language %>%
  filter(language == "English") %>%
  arrange(desc(emmean_composite)) %>%
  mutate(rank_english_emm = row_number())

english_pairwise_models <- as.data.frame(pairs(emmeans(m_comp, ~ Model | Language), adjust = "BH")) %>%
  rename(
    language = Language,
    estimate_diff = estimate,
    SE = SE,
    df = df,
    t_ratio = t.ratio,
    p_value = p.value
  )

language_within_model <- as.data.frame(pairs(emmeans(m_comp, ~ Language | Model), adjust = "BH")) %>%
  rename(
    model = Model,
    estimate_diff = estimate,
    SE = SE,
    df = df,
    t_ratio = t.ratio,
    p_value = p.value
  ) %>%
  mutate(
    contrast_direction = contrast,
    interpretation = case_when(
      is.na(p_value) ~ NA_character_,
      p_value < 0.05 & estimate_diff > 0 ~ "Chinese significantly higher than English",
      p_value < 0.05 & estimate_diff < 0 ~ "English significantly higher than Chinese",
      TRUE ~ "No significant Chinese-English difference"
    )
  )

interaction_row <- mm_anova %>% filter(Effect == "Model:Language")
interaction_text <- if (nrow(interaction_row) == 1) {
  if (!is.na(interaction_row$p_value) && interaction_row$p_value < 0.05) {
    paste0(
      "The model × language interaction was significant (F = ",
      sprintf("%.3f", interaction_row$F_value), ", p = ", fmt_p(interaction_row$p_value),
      "), indicating that cross-language shifts differed across models."
    )
  } else {
    paste0(
      "The model × language interaction was not significant (F = ",
      sprintf("%.3f", interaction_row$F_value), ", p = ", fmt_p(interaction_row$p_value),
      "), suggesting that the overall rank pattern was broadly stable across languages."
    )
  }
} else {
  "The model × language interaction could not be extracted from the composite mixed model."
}

if (is.null(manual_default_model)) {
  default_model <- chinese_summary %>%
    arrange(desc(mean_composite)) %>%
    slice(1) %>%
    pull(Model)
} else {
  default_model <- manual_default_model
}

best_english_mean <- max(english_summary$mean_composite, na.rm = TRUE)

default_validation <- comparison_tbl %>%
  filter(Model == default_model) %>%
  mutate(
    default_model = default_model,
    english_reference_value = mapply(
      default_model_reference_judgement,
      rank_en = rank_english,
      delta_comp = delta_composite,
      en_mean = composite_english,
      best_en_mean = best_english_mean
    ),
    adherence_shift = case_when(
      is.na(delta_adherence) ~ NA_character_,
      delta_adherence >= 0 ~ "Adherence is stable or improves slightly in English",
      TRUE ~ "Adherence declines after switching to English"
    ),
    overall_transfer_judgement = case_when(
      english_reference_value %in% c("Still highly competitive in English",
                                     "Still has strong reference value in English") &
        delta_adherence > -0.30 ~ "The Chinese default model demonstrates good bilingual transfer ability",
      english_reference_value %in% c("Still highly competitive in English",
                                     "Still has strong reference value in English",
                                     "Still has some reference value in English") ~
        "The Chinese default model retains partial bilingual transfer value",
      TRUE ~ "The Chinese default model shows limited transferability to English prompts"
    )
  )

aux_interpretation <- comparison_tbl %>%
  mutate(
    main_shift_dimension = case_when(
      abs(delta_adherence) >= abs(delta_clarity) & abs(delta_adherence) >= abs(delta_aesthetic) ~ "Adherence",
      abs(delta_clarity)   >= abs(delta_adherence) & abs(delta_clarity) >= abs(delta_aesthetic) ~ "Clarity",
      abs(delta_aesthetic) >= abs(delta_adherence) & abs(delta_aesthetic) >= abs(delta_clarity) ~ "Aesthetic",
      TRUE ~ "Mixed"
    ),
    overall_shift_index = rowMeans(cbind(
      abs(delta_composite), abs(delta_adherence), abs(delta_clarity), abs(delta_aesthetic)
    ), na.rm = TRUE)
  ) %>%
  arrange(overall_shift_index) %>%
  mutate(
    stability_rank = row_number(),
    stability_judgement = case_when(
      stability_rank == 1 ~ "Highest cross-language stability among models",
      stability_rank == n() ~ "Largest cross-language fluctuation among models",
      TRUE ~ "Intermediate cross-language stability"
    )
  ) %>%
  select(
    Model,
    delta_composite, delta_adherence, delta_clarity, delta_aesthetic,
    main_shift_dimension, overall_shift_index, stability_judgement, language_balance
  )

english_ranking_text <- english_summary %>%
  arrange(rank_english) %>%
  mutate(txt = paste0(rank_english, ". ", Model, " (", sprintf("%.3f", mean_composite), ")")) %>%
  pull(txt) %>%
  paste(collapse = "; ")

comparison_rank_tbl <- comparison_tbl %>%
  mutate(
    rank_change_text = case_when(
      is.na(rank_change) ~ "NA",
      rank_change == 0   ~ "unchanged",
      rank_change > 0    ~ paste0("down ", rank_change),
      rank_change < 0    ~ paste0("up ", abs(rank_change))
    )
  )

all_rank_small_change <- all(abs(comparison_rank_tbl$rank_change) <= 1, na.rm = TRUE)
overall_rank_stability <- if (all_rank_small_change) {
  "Model rankings changed little between Chinese and English conditions, indicating good cross-language stability."
} else {
  "Some rank changes were observed between Chinese and English conditions, suggesting partial cross-language fluctuation."
}

largest_shift_model <- comparison_tbl %>%
  arrange(desc(abs_delta_composite)) %>%
  slice(1)

overall_balance_text <- paste0(
  "The largest descriptive composite-score shift was observed for ", largest_shift_model$Model,
  " (|English - Chinese| = ", sprintf("%.3f", largest_shift_model$abs_delta_composite), ")."
)

default_text <- if (nrow(default_validation) > 0) {
  paste0(
    "The Chinese default model was ", default_model, ". ",
    "Its English composite mean was ", sprintf("%.3f", default_validation$composite_english),
    ", rank = ", default_validation$rank_english,
    ", delta (English - Chinese) = ", sprintf("%.3f", default_validation$delta_composite),
    ". ",
    default_validation$english_reference_value, ". ",
    default_validation$adherence_shift, ". ",
    default_validation$overall_transfer_judgement, "."
  )
} else {
  "Default model validation could not be completed due to missing paired Chinese-English summary information."
}

language_contrast_lines <- language_within_model %>%
  mutate(
    txt = paste0(
      model, ": ", contrast_direction,
      ", estimate = ", sprintf("%.3f", estimate_diff),
      ", p = ", fmt_p(p_value),
      "; ", interpretation, "."
    )
  ) %>%
  pull(txt)

aux_lines <- aux_interpretation %>%
  mutate(
    txt = paste0(
      Model, ": composite delta = ", sprintf("%.3f", delta_composite),
      ", adherence delta = ", sprintf("%.3f", delta_adherence),
      ", clarity delta = ", sprintf("%.3f", delta_clarity),
      ", aesthetic delta = ", sprintf("%.3f", delta_aesthetic),
      "; dominant shift dimension = ", main_shift_dimension,
      "; stability = ", stability_judgement, "."
    )
  ) %>%
  pull(txt)

conclusion_text <- c(
  "Goal 3: Bilingual transfer validation",
  "--------------------------------------------------",
  paste0("English ranking by composite score: ", english_ranking_text),
  overall_rank_stability,
  overall_balance_text,
  interaction_text,
  default_text,
  "Formal language contrasts within each model:",
  language_contrast_lines,
  "Auxiliary-dimension interpretation:",
  aux_lines
)

writeLines(conclusion_text, file.path(output_dir, "benchmark_am3_conclusion.txt"))

wb <- createWorkbook()

addWorksheet(wb, "data_check")
writeData(wb, "data_check", lang_count)

addWorksheet(wb, "english_summary")
writeData(wb, "english_summary", english_summary)

addWorksheet(wb, "chinese_summary")
writeData(wb, "chinese_summary", chinese_summary)

addWorksheet(wb, "cn_en_comparison")
writeData(wb, "cn_en_comparison", comparison_tbl)

addWorksheet(wb, "mixed_model_anova")
writeData(wb, "mixed_model_anova", mm_anova)

addWorksheet(wb, "mixed_model_fixed_effects")
writeData(wb, "mixed_model_fixed_effects", fixed_effects_tbl)

addWorksheet(wb, "emm_model_by_language")
writeData(wb, "emm_model_by_language", emm_model_by_language)

addWorksheet(wb, "english_model_order_emm")
writeData(wb, "english_model_order_emm", english_model_order)

addWorksheet(wb, "english_pairwise_models")
writeData(wb, "english_pairwise_models", english_pairwise_models)

addWorksheet(wb, "language_within_model")
writeData(wb, "language_within_model", language_within_model)

addWorksheet(wb, "default_model_validation")
writeData(wb, "default_model_validation", default_validation)

addWorksheet(wb, "aux_interpretation")
writeData(wb, "aux_interpretation", aux_interpretation)

saveWorkbook(wb, file.path(output_dir, "benchmark_am3_bilingual_results.xlsx"), overwrite = TRUE)

plot1_df <- comparison_tbl %>%
  select(Model, composite_chinese, composite_english) %>%
  pivot_longer(
    cols = c(composite_chinese, composite_english),
    names_to = "Language",
    values_to = "Composite"
  ) %>%
  mutate(
    Language = case_when(
      Language == "composite_chinese" ~ "Chinese",
      Language == "composite_english" ~ "English",
      TRUE ~ Language
    ),
    Model = factor(Model, levels = chinese_summary$Model)
  )

p1 <- ggplot(plot1_df, aes(x = Model, y = Composite, fill = Language)) +
  geom_col(position = position_dodge(width = 0.72), width = 0.64) +
  labs(
    x = "Model",
    y = "Mean composite score",
    title = "Composite score comparison between Chinese and English prompts"
  ) +
  theme_bw(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.grid.major.x = element_blank(),
    legend.title = element_blank()
  )

ggsave(
  filename = file.path(output_dir, "benchmark_am3_plot_composite_cn_en.png"),
  plot = p1, width = 8, height = 5, dpi = 300
)

plot2_df <- comparison_tbl %>%
  select(Model, delta_adherence, delta_clarity, delta_aesthetic) %>%
  pivot_longer(
    cols = c(delta_adherence, delta_clarity, delta_aesthetic),
    names_to = "Metric",
    values_to = "Delta"
  ) %>%
  mutate(
    Metric = case_when(
      Metric == "delta_adherence" ~ "Adherence",
      Metric == "delta_clarity"   ~ "Clarity",
      Metric == "delta_aesthetic" ~ "Aesthetic",
      TRUE ~ Metric
    ),
    Model = factor(Model, levels = chinese_summary$Model)
  )

p2 <- ggplot(plot2_df, aes(x = Model, y = Delta, fill = Metric)) +
  geom_hline(yintercept = 0, linewidth = 0.4) +
  geom_col(position = position_dodge(width = 0.72), width = 0.64) +
  labs(
    x = "Model",
    y = "English - Chinese",
    title = "Cross-language shifts in auxiliary dimensions"
  ) +
  theme_bw(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.grid.major.x = element_blank(),
    legend.title = element_blank()
  )

ggsave(
  filename = file.path(output_dir, "benchmark_am3_plot_submetrics_delta.png"),
  plot = p2, width = 8, height = 5, dpi = 300
)

cat("\n============================================\n")
cat("Goal 3 analysis completed.\n")
cat("Output directory:", normalizePath(output_dir), "\n")
cat("Default Chinese model:", default_model, "\n")
cat(interaction_text, "\n")
cat("============================================\n\n")
