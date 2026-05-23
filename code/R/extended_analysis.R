suppressPackageStartupMessages({
  library(readxl)
  library(openxlsx)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(lme4)
  library(lmerTest)
  library(emmeans)
  library(broom)
  library(broom.mixed)
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

objective_excel <- file.path(file.path(data_dir, "extended_analysis"), "normalized_scores.xlsx")
prompt_excel <- file.path(data_dir, "prompts.xlsx")
output_excel <- file.path(file.path(data_dir, "extended_analysis"), "r_analysis_result.xlsx")

model_levels <- c("qwen", "zimage", "sdxl")
language_levels <- c("chinese", "english")

expected_order <- c("qwen", "zimage", "sdxl")

required_objective_cols <- c(
  "ModelShortName", "PromptID", "Language", "Resolution", "Steps", "SeedNo",
  "OpenCLIPScore", "AestheticScore", "MUSIQScore", "ImageSizeBytes",
  "ElapsedSeconds", "IterPerSecond", "VRAMUsage", "GPUUsage", "MemoryUsage", "CPUUsage", "FileName",
  "OpenCLIPNormalizedScore", "AestheticNormalizedScore", "MUSIQNormalizedScore",
  "WeightedObjectiveScore", "EqualObjectiveScore"
)

required_prompt_cols <- c(
  "ID", "Set", "Chinese Prompt", "English Prompt", "Content", "Style", "Complexity", "Context"
)

check_required_cols <- function(df, required_cols, df_name) {
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    stop(df_name, " is missing the following required columns:\n", paste(missing_cols, collapse = "\n"))
  }
}

safe_mean <- function(x) mean(x, na.rm = TRUE)
safe_sd <- function(x) sd(x, na.rm = TRUE)

fmt_order <- function(model_order) {
  model_order <- unlist(model_order, use.names = FALSE)
  model_order <- as.character(model_order)
  model_order <- model_order[!is.na(model_order) & nzchar(model_order)]

  if (length(model_order) == 0) {
    return(NA_character_)
  }

  label_map <- c(
    qwen = "Qwen",
    zimage = "Z-Image",
    sdxl = "SDXL-Turbo"
  )

  mapped <- ifelse(
    model_order %in% names(label_map),
    unname(label_map[model_order]),
    model_order
  )

  paste(mapped, collapse = " > ")
}

judge_stability <- function(model_order) {
  model_order <- as.character(model_order)
  if (length(model_order) < 3) {
    return("InsufficientSampleSize")
  }
  if (identical(model_order[1:3], expected_order)) {
    return("FullySupportsBenchmarkFinding")
  }
  if (setequal(model_order[1:2], c("qwen", "zimage")) && model_order[3] == "sdxl") {
    return("BasicallySupportsBenchmarkFindingQwenZImageTopTwoSDXLLowest")
  }
  return("DoesNotSupportOrRequiresCautiousInterpretation")
}

rank_support <- function(rank_qwen, rank_zimage, rank_sdxl) {
  if (is.na(rank_qwen) || is.na(rank_zimage) || is.na(rank_sdxl)) {
    return("InsufficientSampleSize")
  }
  if (rank_qwen == 1 && rank_zimage == 2 && rank_sdxl == 3) {
    return("SupportsMainFinding")
  }
  if (rank_qwen <= 2 && rank_zimage <= 2 && rank_sdxl == 3) {
    return("BasicallySupportsMainFindingQwenZImageTopTwoSDXLLowest")
  }
  return("DoesNotSupportOrRequiresCautiousInterpretation")
}

make_desc <- function(df, group_vars, score_col) {
  df %>%
    group_by(across(all_of(group_vars))) %>%
    summarise(
      n = n(),
      mean = mean(.data[[score_col]], na.rm = TRUE),
      sd = sd(.data[[score_col]], na.rm = TRUE),
      median = median(.data[[score_col]], na.rm = TRUE),
      min = min(.data[[score_col]], na.rm = TRUE),
      max = max(.data[[score_col]], na.rm = TRUE),
      .groups = "drop"
    )
}

extract_anova <- function(model_fit) {
  anova(model_fit, type = 3) %>%
    as.data.frame() %>%
    tibble::rownames_to_column("term") %>%
    as_tibble()
}

extract_emmeans <- function(model_fit, specs_formula) {
  emm <- emmeans(model_fit, specs_formula)
  as.data.frame(emm) %>% as_tibble()
}

extract_pairs <- function(model_fit, specs_formula, adjust_method = "BH") {
  emm <- emmeans(model_fit, specs_formula)
  pairs(emm, adjust = adjust_method) %>%
    as.data.frame() %>%
    as_tibble()
}

objective_raw <- read_excel(objective_excel)
prompt_raw <- read_excel(prompt_excel, sheet = "prompt")

check_required_cols(objective_raw, required_objective_cols, "Objective score Excel")
check_required_cols(prompt_raw, required_prompt_cols, "prompt sheet in the prompt Excel file")

objective_df <- objective_raw %>%
  mutate(
    model = str_to_lower(str_trim(as.character(ModelShortName))),
    language = str_to_lower(str_trim(as.character(Language))),
    prompt_id = as.character(PromptID),
    weighted_objective_score = as.numeric(WeightedObjectiveScore),
    equal_objective_score = as.numeric(EqualObjectiveScore),
    openclip_norm = as.numeric(OpenCLIPNormalizedScore),
    aesthetic_norm = as.numeric(AestheticNormalizedScore),
    musiq_norm = as.numeric(MUSIQNormalizedScore)
  ) %>%
  mutate(
    model = factor(model, levels = model_levels),
    language = factor(language, levels = language_levels)
  )

prompt_df <- prompt_raw %>%
  mutate(
    prompt_id = as.character(ID),
    Set = as.character(Set),
    Content = as.character(Content),
    Style = as.character(Style),
    Complexity = as.character(Complexity),
    Context = as.character(Context)
  ) %>%
  select(prompt_id, Set, `Chinese Prompt`, `English Prompt`, Content, Style, Complexity, Context)

analysis_df <- objective_df %>%
  left_join(prompt_df, by = "prompt_id")

unmatched_prompt <- analysis_df %>%
  filter(is.na(Content) | is.na(Style) | is.na(Complexity) | is.na(Context)) %>%
  distinct(prompt_id)

if (nrow(unmatched_prompt) > 0) {
  warning("Some prompt IDs could not be matched with attribute information in the prompt sheet: ", paste(unmatched_prompt$prompt_id, collapse = ", "))
}

overall_desc <- make_desc(
  analysis_df,
  group_vars = c("language", "model"),
  score_col = "weighted_objective_score"
) %>%
  group_by(language) %>%
  arrange(language, desc(mean), .by_group = TRUE) %>%
  mutate(rank = rank(-mean, ties.method = "min")) %>%
  ungroup()

chinese_desc <- analysis_df %>%
  filter(language == "chinese") %>%
  make_desc(group_vars = c("model"), score_col = "weighted_objective_score") %>%
  arrange(desc(mean)) %>%
  mutate(rank = rank(-mean, ties.method = "min"))

chinese_df <- analysis_df %>%
  filter(language == "chinese") %>%
  filter(!is.na(weighted_objective_score), !is.na(model), !is.na(prompt_id))

model_chinese <- lmer(
  weighted_objective_score ~ model + (1 | prompt_id),
  data = chinese_df,
  REML = FALSE
)

chinese_model_anova <- extract_anova(model_chinese)
chinese_model_fixed_effects <- broom.mixed::tidy(model_chinese, effects = "fixed", conf.int = TRUE)
chinese_model_random_effects <- broom.mixed::tidy(model_chinese, effects = "ran_pars")
chinese_model_emmeans <- extract_emmeans(model_chinese, ~ model)
chinese_model_pairs <- extract_pairs(model_chinese, ~ model, adjust_method = "BH")

bilingual_df <- analysis_df %>%
  filter(!is.na(weighted_objective_score), !is.na(model), !is.na(language), !is.na(prompt_id))

model_bilingual <- lmer(
  weighted_objective_score ~ model * language + (1 | prompt_id),
  data = bilingual_df,
  REML = FALSE
)

bilingual_model_anova <- extract_anova(model_bilingual)
bilingual_model_fixed_effects <- broom.mixed::tidy(model_bilingual, effects = "fixed", conf.int = TRUE)
bilingual_model_random_effects <- broom.mixed::tidy(model_bilingual, effects = "ran_pars")
bilingual_model_emmeans <- extract_emmeans(model_bilingual, ~ model | language)
bilingual_model_pairs_by_language <- extract_pairs(model_bilingual, ~ model | language, adjust_method = "BH")
bilingual_language_pairs_by_model <- extract_pairs(model_bilingual, ~ language | model, adjust_method = "BH")

attribute_base_df <- analysis_df %>%
  filter(language == "chinese")

make_attribute_stability <- function(df, attr_col) {
  temp <- df %>%
    filter(!is.na(.data[[attr_col]]), !is.na(weighted_objective_score), !is.na(model)) %>%
    mutate(model = as.character(model)) %>%
    group_by(attribute_level = .data[[attr_col]], model) %>%
    summarise(
      mean_score = mean(weighted_objective_score, na.rm = TRUE),
      sd_score = sd(weighted_objective_score, na.rm = TRUE),
      n = n(),
      .groups = "drop"
    )

  if (nrow(temp) == 0) {
    return(tibble(
      AttributeDimension = character(),
      AttributeLevel = character(),
      QwenMean = numeric(),
      ZImageMean = numeric(),
      SDXLMean = numeric(),
      QwenN = integer(),
      ZImageN = integer(),
      SDXLN = integer(),
      Order = character(),
      StabilityJudgment = character()
    ))
  }

  wide <- temp %>%
    select(attribute_level, model, mean_score) %>%
    pivot_wider(names_from = model, values_from = mean_score)

  n_wide <- temp %>%
    select(attribute_level, model, n) %>%
    mutate(model = paste0(model, "_n")) %>%
    pivot_wider(names_from = model, values_from = n)

  for (col in c("qwen", "zimage", "sdxl")) {
    if (!col %in% names(wide)) wide[[col]] <- NA_real_
  }
  for (col in c("qwen_n", "zimage_n", "sdxl_n")) {
    if (!col %in% names(n_wide)) n_wide[[col]] <- NA_integer_
  }

  out <- wide %>%
    left_join(n_wide, by = "attribute_level") %>%
    rowwise() %>%
    mutate(
      model_order = list({
        vals <- c(qwen = qwen, zimage = zimage, sdxl = sdxl)
        vals <- vals[!is.na(vals)]
        names(sort(vals, decreasing = TRUE))
      }),
      Order = fmt_order(model_order),
      StabilityJudgment = judge_stability(model_order)
    ) %>%
    ungroup() %>%
    transmute(
      AttributeDimension = attr_col,
      AttributeLevel = attribute_level,
      QwenMean = qwen,
      ZImageMean = zimage,
      SDXLMean = sdxl,
      QwenN = qwen_n,
      ZImageN = zimage_n,
      SDXLN = sdxl_n,
      Order = Order,
      StabilityJudgment = StabilityJudgment
    )

  return(out)
}

attribute_stability <- bind_rows(
  make_attribute_stability(attribute_base_df, "Content"),
  make_attribute_stability(attribute_base_df, "Style"),
  make_attribute_stability(attribute_base_df, "Complexity"),
  make_attribute_stability(attribute_base_df, "Context")
)

robust_metrics <- tibble::tribble(
  ~metric_name, ~score_col,
  "WeightedObjectiveScore", "weighted_objective_score",
  "EqualObjectiveScore", "equal_objective_score",
  "OpenCLIPNormalizedScore", "openclip_norm",
  "AestheticNormalizedScore", "aesthetic_norm",
  "MUSIQNormalizedScore", "musiq_norm"
)

make_robust_rank <- function(df, score_col, metric_name) {
  temp <- df %>%
    filter(language == "chinese") %>%
    filter(!is.na(.data[[score_col]]), !is.na(model)) %>%
    group_by(model) %>%
    summarise(
      mean_score = mean(.data[[score_col]], na.rm = TRUE),
      n = n(),
      .groups = "drop"
    ) %>%
    mutate(
      rank = rank(-mean_score, ties.method = "min")
    )

  rank_qwen <- temp %>% filter(model == "qwen") %>% pull(rank)
  rank_zimage <- temp %>% filter(model == "zimage") %>% pull(rank)
  rank_sdxl <- temp %>% filter(model == "sdxl") %>% pull(rank)

  mean_qwen <- temp %>% filter(model == "qwen") %>% pull(mean_score)
  mean_zimage <- temp %>% filter(model == "zimage") %>% pull(mean_score)
  mean_sdxl <- temp %>% filter(model == "sdxl") %>% pull(mean_score)

  tibble(
    Metric = metric_name,
    QwenMean = ifelse(length(mean_qwen) == 0, NA_real_, mean_qwen),
    ZImageMean = ifelse(length(mean_zimage) == 0, NA_real_, mean_zimage),
    SDXLMean = ifelse(length(mean_sdxl) == 0, NA_real_, mean_sdxl),
    QwenRank = ifelse(length(rank_qwen) == 0, NA_real_, rank_qwen),
    ZImageRank = ifelse(length(rank_zimage) == 0, NA_real_, rank_zimage),
    SDXLRank = ifelse(length(rank_sdxl) == 0, NA_real_, rank_sdxl),
    SupportForBenchmarkFinding = rank_support(
      ifelse(length(rank_qwen) == 0, NA_real_, rank_qwen),
      ifelse(length(rank_zimage) == 0, NA_real_, rank_zimage),
      ifelse(length(rank_sdxl) == 0, NA_real_, rank_sdxl)
    )
  )
}

robustness_rank <- purrr::map2_dfr(
  robust_metrics$score_col,
  robust_metrics$metric_name,
  ~ make_robust_rank(analysis_df, .x, .y)
)

chinese_order <- chinese_desc %>% arrange(rank) %>% pull(model) %>% as.character()
chinese_stability <- judge_stability(chinese_order)

interaction_row <- bilingual_model_anova %>%
  filter(term == "model:language")
interaction_text <- if (nrow(interaction_row) > 0) {
  pval <- interaction_row$`Pr(>F)`[1]
  ifelse(
    is.na(pval),
    "The model x language interaction in the bilingual model did not produce a valid p value.",
    ifelse(
      pval < 0.05,
      paste0("In the bilingual supplementary analysis, the model x language interaction was significant (p = ", signif(pval, 3), "), indicating that different models may show different objective-score shift patterns across Chinese and English conditions."),
      paste0("In the bilingual supplementary analysis, the model x language interaction was not significant (p = ", signif(pval, 3), "), indicating that no clear between-model difference in language-related score shift was observed in the Extended subset.")
    )
  )
} else {
  "The model x language interaction term was not found in the bilingual model output."
}

conclusion_text <- tibble(
  section = c(
    "ChineseExtendedMainAnalysisNote",
    "BilingualSupplementaryAnalysisNote",
    "TaskAttributeStratificationNote",
    "RobustnessCheckNote"
  ),
  text = c(
    paste0(
      "In the Chinese Extended subset, the weighted objective score order was: ", fmt_order(chinese_order), ". Stability judgment: ", chinese_stability,
      ". This result examines whether the objective performance of the three models under Chinese Extended prompts still follows the model stratification observed in the Benchmark stage."
    ),
    paste0(
      interaction_text,
      " The bilingual supplementary analysis is used to examine whether the model stratification observed in the Chinese Extended analysis remains informative under English prompts and whether the models show language sensitivity."
    ),
    "Task attribute stratification is used only for descriptive stability assessment, without complex significance testing. The focus is whether Qwen and Z-Image remain the top two models and whether SDXL-Turbo remains the lowest under each attribute level.",
    "The equal-weight composite and the three single metrics are used only as robustness checks, not as new deployment recommendations. If most metrics show Qwen/Z-Image as the top two models and SDXL-Turbo as the lowest, the main finding has some stability across weighting schemes and individual metric choices."
  )
)

wb <- createWorkbook()

addWorksheet(wb, "merged_data")
writeData(wb, "merged_data", analysis_df)

addWorksheet(wb, "weighted_desc_by_lang_model")
writeData(wb, "weighted_desc_by_lang_model", overall_desc)

addWorksheet(wb, "weighted_desc_chinese")
writeData(wb, "weighted_desc_chinese", chinese_desc)

addWorksheet(wb, "LMM_chinese_ANOVA")
writeData(wb, "LMM_chinese_ANOVA", chinese_model_anova)

addWorksheet(wb, "LMM_chinese_fixed_effects")
writeData(wb, "LMM_chinese_fixed_effects", chinese_model_fixed_effects)

addWorksheet(wb, "LMM_chinese_random")
writeData(wb, "LMM_chinese_random", chinese_model_random_effects)

addWorksheet(wb, "LMM_chinese_emmeans")
writeData(wb, "LMM_chinese_emmeans", chinese_model_emmeans)

addWorksheet(wb, "LMM_chinese_pairwise")
writeData(wb, "LMM_chinese_pairwise", chinese_model_pairs)

addWorksheet(wb, "LMM_bilingual_ANOVA")
writeData(wb, "LMM_bilingual_ANOVA", bilingual_model_anova)

addWorksheet(wb, "LMM_bilingual_fixed")
writeData(wb, "LMM_bilingual_fixed", bilingual_model_fixed_effects)

addWorksheet(wb, "LMM_bilingual_random")
writeData(wb, "LMM_bilingual_random", bilingual_model_random_effects)

addWorksheet(wb, "LMM_bilingual_emmeans")
writeData(wb, "LMM_bilingual_emmeans", bilingual_model_emmeans)

addWorksheet(wb, "LMM_model_pairs_by_lang")
writeData(wb, "LMM_model_pairs_by_lang", bilingual_model_pairs_by_language)

addWorksheet(wb, "LMM_lang_pairs_by_model")
writeData(wb, "LMM_lang_pairs_by_model", bilingual_language_pairs_by_model)

addWorksheet(wb, "attribute_stability")
writeData(wb, "attribute_stability", attribute_stability)

addWorksheet(wb, "robustness_rank")
writeData(wb, "robustness_rank", robustness_rank)

addWorksheet(wb, "conclusion_notes")
writeData(wb, "conclusion_notes", conclusion_text)

for (s in names(wb)) {
  setColWidths(wb, sheet = s, cols = 1:50, widths = "auto")
}

saveWorkbook(wb, output_excel, overwrite = TRUE)

cat("Stage 3 Extended validation analysis completed.\n")
cat("Input objective score file: ", objective_excel, "\n")
cat("Input prompt file: ", prompt_excel, "\n")
cat("Output result file: ", output_excel, "\n")
cat("Model order in the Chinese Extended subset: ", fmt_order(chinese_order), "\n")
cat("Stability judgment: ", chinese_stability, "\n")
cat(interaction_text, "\n")
