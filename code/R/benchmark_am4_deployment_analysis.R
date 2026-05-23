required_pkgs <- c(
  "readxl", "dplyr", "writexl",
  "stringr", "scales"
)

to_install <- required_pkgs[!required_pkgs %in% installed.packages()[, "Package"]]
if (length(to_install) > 0) {
  install.packages(to_install, repos = "https://cloud.r-project.org")
}

library(readxl)
library(dplyr)
library(writexl)
library(stringr)
library(scales)

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
output_summary_excel <- file.path(data_dir, "benchmark_am4_deployment_summary.xlsx")
output_text_file     <- file.path(data_dir, "benchmark_am4_deployment_recommendations.txt")

adherence_recommended_threshold <- 3.5
adherence_acceptable_threshold  <- 3.0

vram_small_gap_abs_mb  <- 512
vram_small_gap_pct     <- 0.05
substantial_speed_gain <- 0.30   # At least 30% faster

raw_df <- read_excel(input_excel, sheet = 1)

normalize_names <- function(x) {
  x %>%
    str_replace_all("\\s+", "") %>%
    str_replace_all("（", "(") %>%
    str_replace_all("）", ")")
}

names(raw_df) <- normalize_names(names(raw_df))

required_cols <- c(
  "ImageName",
  "ModelShortName",
  "PromptID",
  "Language",
  "ContentAttribute",
  "StyleAttribute",
  "ComplexityAttribute",
  "ContextAttribute",
  "MeanAdherence",
  "AdherenceSD",
  "MeanClarity",
  "ClaritySD",
  "MeanAesthetics",
  "AestheticsSD",
  "CompositeScore",
  "ValidRaterCount",
  "ElapsedSeconds",
  "VRAMUsage"
)

missing_cols <- setdiff(required_cols, names(raw_df))
if (length(missing_cols) > 0) {
  stop(
    paste0(
      "The input Excel file is missing the following required columns:\n",
      paste(missing_cols, collapse = "\n")
    )
  )
}

df_cn <- raw_df %>%
  mutate(
    Language = as.character(Language),
    ModelShortName = as.character(ModelShortName)
  ) %>%
  filter(str_to_lower(Language) %in% c("chinese", "cn"))

if (nrow(df_cn) == 0) {
  stop("No Chinese-group samples were found. Please check the values in the Language column.")
}

numeric_cols <- c(
  "MeanAdherence", "MeanClarity", "MeanAesthetics", "CompositeScore",
  "ElapsedSeconds", "VRAMUsage", "ValidRaterCount"
)

df_cn <- df_cn %>%
  mutate(across(all_of(numeric_cols), as.numeric))

if (any(is.na(df_cn$MeanAdherence))) stop("The MeanAdherence column contains values that cannot be converted to numeric.")
if (any(is.na(df_cn$CompositeScore))) stop("The CompositeScore column contains values that cannot be converted to numeric.")
if (any(is.na(df_cn$ElapsedSeconds))) stop("The ElapsedSeconds column contains values that cannot be converted to numeric.")
if (any(is.na(df_cn$VRAMUsage)))      stop("The VRAMUsage column contains values that cannot be converted to numeric.")

model_summary <- df_cn %>%
  group_by(ModelShortName) %>%
  summarise(
    n_images          = n(),
    mean_composite    = mean(CompositeScore, na.rm = TRUE),
    sd_composite      = sd(CompositeScore, na.rm = TRUE),
    mean_adherence    = mean(MeanAdherence, na.rm = TRUE),
    sd_adherence      = sd(MeanAdherence, na.rm = TRUE),
    mean_time         = mean(ElapsedSeconds, na.rm = TRUE),
    sd_time           = sd(ElapsedSeconds, na.rm = TRUE),
    mean_vram         = mean(VRAMUsage, na.rm = TRUE),
    sd_vram           = sd(VRAMUsage, na.rm = TRUE),
    mean_clarity      = mean(MeanClarity, na.rm = TRUE),
    mean_aesthetics   = mean(MeanAesthetics, na.rm = TRUE),
    mean_valid_raters = mean(ValidRaterCount, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    deployment_tier = case_when(
      mean_adherence >= adherence_recommended_threshold ~ "recommended",
      mean_adherence >= adherence_acceptable_threshold  ~ "acceptable_review",
      TRUE                                             ~ "not_recommended"
    )
  )

if (nrow(model_summary) < 2) {
  stop("Fewer than two models were found, so deployment comparison cannot be performed.")
}

recommended_models <- model_summary %>% filter(deployment_tier == "recommended")
if (nrow(recommended_models) == 0) {
  stop("No model reached the recommended deployment threshold, so AM4 deployment recommendations cannot be generated.")
}

format_model_list <- function(x) paste(x, collapse = ", ")

get_metric_sentence <- function(df_row) {
  paste0(
    " (mean composite score = ", sprintf("%.3f", df_row$mean_composite[1]),
    ", mean adherence = ", sprintf("%.3f", df_row$mean_adherence[1]),
    ", mean inference time = ", sprintf("%.3f", df_row$mean_time[1]), " s",
    ", mean VRAM usage = ", sprintf("%.1f", df_row$mean_vram[1]), " MB)"
  )
}

get_tier_label <- function(x) {
  case_when(
    x == "recommended"       ~ "Recommended",
    x == "acceptable_review" ~ "AcceptableWithManualReview",
    TRUE                      ~ "NotRecommendedForPriorityDeployment"
  )
}

speed_ranked <- recommended_models %>%
  arrange(mean_time, desc(mean_composite), mean_vram)
speed_primary <- speed_ranked %>% slice(1)
speed_backups <- speed_ranked %>% slice(-1)

vram_ranked <- recommended_models %>%
  arrange(mean_vram, desc(mean_composite), mean_time)
vram_primary <- vram_ranked %>% slice(1)
vram_backups <- vram_ranked %>% slice(-1)

has_second_recommended <- nrow(vram_ranked) >= 2
if (has_second_recommended) {
  vram_second <- vram_ranked %>% slice(2)
  vram_gap_abs <- abs(vram_second$mean_vram[1] - vram_primary$mean_vram[1])
  vram_gap_pct <- vram_gap_abs / min(vram_primary$mean_vram[1], vram_second$mean_vram[1])
  time_gain_second_vs_primary <- (vram_primary$mean_time[1] - vram_second$mean_time[1]) / vram_primary$mean_time[1]
  vram_difference_is_small <- (vram_gap_abs <= vram_small_gap_abs_mb) || (vram_gap_pct <= vram_small_gap_pct)
  second_is_much_faster <- time_gain_second_vs_primary >= substantial_speed_gain
} else {
  vram_second <- NULL
  vram_gap_abs <- NA_real_
  vram_gap_pct <- NA_real_
  time_gain_second_vs_primary <- NA_real_
  vram_difference_is_small <- FALSE
  second_is_much_faster <- FALSE
}

quality_ranked <- recommended_models %>%
  arrange(desc(mean_composite), mean_time, mean_vram)
quality_primary <- quality_ranked %>% slice(1)

if (speed_primary$ModelShortName[1] == quality_primary$ModelShortName[1]) {
  balanced_primary <- speed_primary
  balanced_reason  <- "The quality-first and speed-first criteria identify the same model, so this model naturally becomes the balanced default option."
} else {
  speed_gain_vs_quality <- (quality_primary$mean_time[1] - speed_primary$mean_time[1]) / quality_primary$mean_time[1]
  if (vram_difference_is_small && speed_gain_vs_quality >= substantial_speed_gain) {
    balanced_primary <- speed_primary
    balanced_reason <- paste0(
      "Among models that reached the recommended deployment threshold, ", speed_primary$ModelShortName[1],
      " had a slightly lower composite score than ", quality_primary$ModelShortName[1],
      ", but reduced inference time by approximately ", percent(speed_gain_vs_quality, accuracy = 0.1),
      ", while the VRAM difference was small. Therefore, it is more suitable as the balanced default option that considers both quality and efficiency."
    )
  } else {
    balanced_primary <- quality_primary
    balanced_reason <- paste0(
      "Although ", speed_primary$ModelShortName[1],
      " was faster, its efficiency advantage was not sufficient to offset the overall quality advantage of the leading quality model. Therefore, ",
      quality_primary$ModelShortName[1], " remains the balanced default option."
    )
  }
}

balanced_backups <- recommended_models %>%
  filter(ModelShortName != balanced_primary$ModelShortName[1]) %>%
  arrange(mean_time, desc(mean_composite), mean_vram)

threshold_sentence <- paste0(
  "This analysis uses the same deployment decision rule as AM1 and AM2: ",
  "mean adherence >= ", sprintf("%.1f", adherence_recommended_threshold),
  " is treated as Recommended; ",
  sprintf("%.1f", adherence_acceptable_threshold), "-",
  sprintf("%.2f", adherence_recommended_threshold - 0.01),
  " is treated as Acceptable with Manual Review; and values below ",
  sprintf("%.1f", adherence_acceptable_threshold),
  " are treated as Not Recommended for Priority Deployment. Among candidate models that meet the quality threshold, resource constrained deployment recommendations are then formed by considering inference time and VRAM cost."
)

speed_sentence <- paste0(
  "Recommendation 1: Speed-first scenario\n",
  "Recommended model: ", speed_primary$ModelShortName[1], "\n",
  "Applicable use cases: classroom real-time generation, student interaction, rapid trial-and-error, and draft preview.\n",
  "Decision rule: compare mean inference time only among models that reached the Recommended threshold, and prioritize the model with lower inference time.\n",
  "Rationale: ", speed_primary$ModelShortName[1], get_metric_sentence(speed_primary), ".",
  if (nrow(speed_backups) > 0) paste0(" Alternative model(s): ", format_model_list(speed_backups$ModelShortName), ".") else ""
)

vram_sentence <- if (!has_second_recommended) {
  paste0(
    "Recommendation 2: VRAM-constrained scenario\n",
    "Recommended model: ", vram_primary$ModelShortName[1], "\n",
    "Applicable use cases: environments with extremely tight VRAM budgets that still require basic instructional usability.\n",
    "Rationale: only one model reached the Recommended threshold, so it naturally becomes the preferred option under the VRAM-constrained scenario."
  )
} else if (vram_difference_is_small && second_is_much_faster) {
  paste0(
    "Recommendation 2: VRAM-constrained scenario\n",
    "Conclusion type: cautious interpretation\n",
    "Model with nominally lower VRAM usage: ", vram_primary$ModelShortName[1], "\n",
    "Applicable use cases: situations where VRAM is exactly at the single-GPU feasibility boundary and the primary concern is whether the model can run.\n",
    "Rationale: among models that reached the Recommended threshold, the average VRAM usage of ", vram_primary$ModelShortName[1],
    " and ", vram_second$ModelShortName[1],
    " differed by only ", sprintf("%.1f", vram_gap_abs), " MB (",
    percent(vram_gap_pct, accuracy = 0.1), "), indicating a small overall difference. At the same time, ",
    vram_second$ModelShortName[1], " reduced mean inference time by approximately ",
    percent(time_gain_second_vs_primary, accuracy = 0.1),
    ". Therefore, VRAM did not become the main factor that changed the recommendation order in this study. Unless deployment is at the strict VRAM boundary, the speed-first or balanced default option is more practically informative."
  )
} else {
  paste0(
    "Recommendation 2: VRAM-constrained scenario\n",
    "Recommended model: ", vram_primary$ModelShortName[1], "\n",
    "Applicable use cases: environments with limited VRAM, shared laboratory machines, or concurrent use of other programs.\n",
    "Decision rule: compare VRAM usage only among models that reached the Recommended threshold. Models below the quality threshold are not prioritized even if their VRAM usage is lower.\n",
    "Rationale: ", vram_primary$ModelShortName[1], get_metric_sentence(vram_primary), ".",
    if (nrow(vram_backups) > 0) paste0(" Alternative model(s): ", format_model_list(vram_backups$ModelShortName), ".") else ""
  )
}

balanced_sentence <- paste0(
  "Recommendation 3: Balanced default option\n",
  "Recommended model: ", balanced_primary$ModelShortName[1], "\n",
  "Applicable use cases: routine teaching laboratory environments that require relatively high subjective quality while also considering interaction efficiency and resource use.\n",
  "Decision rule: among models that reached the Recommended threshold, jointly consider overall quality, inference time, and VRAM demand. When the VRAM difference is small but the speed difference is large, prioritize the more efficient model.\n",
  "Rationale: ", balanced_primary$ModelShortName[1], get_metric_sentence(balanced_primary), ". ",
  balanced_reason,
  if (nrow(balanced_backups) > 0) paste0(" Alternative model(s): ", format_model_list(balanced_backups$ModelShortName), ".") else ""
)

model_tier_lines <- model_summary %>%
  arrange(desc(mean_composite)) %>%
  mutate(
    deployment_label = get_tier_label(deployment_tier),
    line = paste0(
      ModelShortName, ": ", deployment_label,
      " (composite score = ", sprintf("%.3f", mean_composite),
      ", adherence = ", sprintf("%.3f", mean_adherence),
      ", inference time = ", sprintf("%.3f", mean_time), " s",
      ", VRAM = ", sprintf("%.1f", mean_vram), " MB)"
    )
  ) %>%
  pull(line)

all_recommendation_text <- paste(
  "AM4: Resource Constrained Deployment Recommendations (Revised Version)",
  threshold_sentence,
  "",
  "Model tier results:",
  paste(model_tier_lines, collapse = "\n"),
  "",
  speed_sentence,
  "",
  vram_sentence,
  "",
  balanced_sentence,
  sep = "\n"
)

cat(all_recommendation_text)

summary_export <- model_summary %>%
  mutate(
    DeploymentLabel = get_tier_label(deployment_tier)
  ) %>%
  mutate(across(where(is.numeric), ~ round(.x, 4))) %>%
  rename(
    ModelShortName = ModelShortName,
    ImageCount = n_images,
    MeanComposite = mean_composite,
    SDComposite = sd_composite,
    MeanAdherence = mean_adherence,
    SDAdherence = sd_adherence,
    MeanInferenceTime = mean_time,
    SDInferenceTime = sd_time,
    MeanVRAM = mean_vram,
    SDVRAM = sd_vram,
    MeanClarity = mean_clarity,
    MeanAesthetics = mean_aesthetics,
    MeanValidRaters = mean_valid_raters,
    DeploymentTier = deployment_tier
  )

recommendation_export <- tibble::tibble(
  Scenario = c("SpeedFirstScenario", "VRAMConstrainedScenario", "BalancedDefaultOption"),
  RecommendedModel = c(
    speed_primary$ModelShortName[1],
    if (has_second_recommended && vram_difference_is_small && second_is_much_faster) paste0(vram_primary$ModelShortName[1], "Nominal") else vram_primary$ModelShortName[1],
    balanced_primary$ModelShortName[1]
  ),
  ConclusionType = c(
    "PreferredOption",
    if (has_second_recommended && vram_difference_is_small && second_is_much_faster) "CautiousInterpretation" else "PreferredOption",
    "PreferredOption"
  ),
  Explanation = c(
    "Among models that reached the Recommended threshold, models are sorted by mean inference time.",
    if (has_second_recommended && vram_difference_is_small && second_is_much_faster) {
      paste0("The VRAM difference between recommended models was small (", sprintf("%.1f", vram_gap_abs), " MB, ", percent(vram_gap_pct, accuracy = 0.1), "), so VRAM did not become the main factor that changed the recommendation order.")
    } else {
      "Among models that reached the Recommended threshold, models are sorted by mean VRAM usage."
    },
    "Overall quality, inference time, and VRAM cost are considered jointly. When the VRAM difference is small, efficiency is emphasized."
  )
)

cn_export <- df_cn %>%
  mutate(across(any_of(c("MeanAdherence", "CompositeScore", "ElapsedSeconds", "VRAMUsage")), ~ round(.x, 4)))

write_xlsx(
  list(
    ChineseData = cn_export,
    ModelSummary = summary_export,
    Recommendations = recommendation_export
  ),
  path = output_summary_excel
)

writeLines(all_recommendation_text, con = output_text_file, useBytes = TRUE)

message("Analysis completed. Output files:")
message("1) ", output_summary_excel)
message("2) ", output_text_file)
