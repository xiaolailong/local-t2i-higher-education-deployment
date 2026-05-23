required_pkgs <- c(
  "readxl", "dplyr", "tidyr", "stringr",
  "car", "effectsize", "writexl"
)

install_if_missing <- function(pkgs) {
  for (p in pkgs) {
    if (!requireNamespace(p, quietly = TRUE)) {
      install.packages(p, repos = "https://cloud.r-project.org")
    }
  }
}
install_if_missing(required_pkgs)

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(car)
library(effectsize)
library(writexl)

script_dir <- dirname(sys.frame(1)$ofile)
data_dir <- file.path(dirname(dirname(script_dir)), "data")

input_excel  <- file.path(data_dir, "benchmark_subjective_scores_ori_image_level.xlsx")
output_excel <- file.path(data_dir, "benchmark_am1_model_deploy_analysis.xlsx")

group_col <- "ModelShortName"
language_col <- "Language"
language_value <- "chinese"

dv_cols <- c("CompositeScore", "MeanAdherence", "MeanClarity", "MeanAesthetics")

recommended_threshold <- 3.5
acceptable_threshold  <- 3.0

safe_as_numeric <- function(x) {
  suppressWarnings(as.numeric(x))
}

fmt_p <- function(p) {
  if (is.na(p)) return(NA_character_)
  if (p < 0.001) return("< 0.001")
  sprintf("%.3f", p)
}

fmt_num <- function(x, digits = 3) {
  ifelse(is.na(x), NA_character_, format(round(x, digits), nsmall = digits))
}

sig_mark <- function(p) {
  if (is.na(p)) return("")
  if (p < 0.001) return("***")
  if (p < 0.01)  return("**")
  if (p < 0.05)  return("*")
  return("ns")
}

interpret_d <- function(d) {
  if (is.na(d)) return(NA_character_)
  ad <- abs(d)
  if (ad < 0.2) return("Negligible")
  if (ad < 0.5) return("Small")
  if (ad < 0.8) return("Medium")
  return("Large")
}

interpret_eta2 <- function(x) {
  if (is.na(x)) return(NA_character_)
  if (x < 0.01) return("Very small")
  if (x < 0.06) return("Small")
  if (x < 0.14) return("Medium")
  return("Large")
}

interpret_rbc <- function(x) {
  if (is.na(x)) return(NA_character_)
  ax <- abs(x)
  if (ax < 0.10) return("Negligible")
  if (ax < 0.30) return("Small")
  if (ax < 0.50) return("Medium")
  return("Large")
}

mean_sd_text <- function(m, s, digits = 3) {
  paste0(fmt_num(m, digits), " ± ", fmt_num(s, digits))
}

pooled_sd <- function(x, y) {
  nx <- length(na.omit(x))
  ny <- length(na.omit(y))
  sx <- sd(x, na.rm = TRUE)
  sy <- sd(y, na.rm = TRUE)
  sqrt(((nx - 1) * sx^2 + (ny - 1) * sy^2) / (nx + ny - 2))
}

calc_cohens_d_manual <- function(x, y) {
  x <- x[!is.na(x)]
  y <- y[!is.na(y)]
  if (length(x) < 2 || length(y) < 2) return(NA_real_)
  psd <- pooled_sd(x, y)
  if (is.na(psd) || psd == 0) return(NA_real_)
  (mean(x) - mean(y)) / psd
}

calc_rank_biserial <- function(x, y) {
  x <- x[!is.na(x)]
  y <- y[!is.na(y)]
  if (length(x) < 1 || length(y) < 1) return(NA_real_)
  cmp <- outer(x, y, FUN = "-")
  wins <- sum(cmp > 0)
  losses <- sum(cmp < 0)
  (wins - losses) / (length(x) * length(y))
}

calc_partial_eta2_from_f <- function(F_value, df1, df2) {
  if (is.na(F_value) || is.na(df1) || is.na(df2)) return(NA_real_)
  (F_value * df1) / (F_value * df1 + df2)
}

pairwise_parametric_tests <- function(data, dv_name, equal_var = TRUE, p_adjust = "BH") {
  lv <- levels(data$group)
  pairs <- combn(lv, 2, simplify = FALSE)

  res <- lapply(pairs, function(pp) {
    g1 <- pp[1]
    g2 <- pp[2]
    x <- data$y[data$group == g1]
    y <- data$y[data$group == g2]

    tt <- tryCatch(
      t.test(x, y, var.equal = equal_var),
      error = function(e) NULL
    )

    d <- calc_cohens_d_manual(x, y)

    data.frame(
      DependentVariable = dv_name,
      group1 = g1,
      group2 = g2,
      ComparisonMethod = ifelse(equal_var, "Student t-test", "Welch t-test"),
      Statistic = ifelse(is.null(tt), NA_real_, as.numeric(tt$statistic)),
      PValue = ifelse(is.null(tt), NA_real_, tt$p.value),
      MeanGroup1 = mean(x, na.rm = TRUE),
      MeanGroup2 = mean(y, na.rm = TRUE),
      MeanDifference = mean(x, na.rm = TRUE) - mean(y, na.rm = TRUE),
      CILower = ifelse(is.null(tt), NA_real_, tt$conf.int[1]),
      CIUpper = ifelse(is.null(tt), NA_real_, tt$conf.int[2]),
      Cohen_d = d,
      EffectSizeInterpretation = interpret_d(d)
    )
  })

  out <- bind_rows(res)
  out$AdjustedPValue <- p.adjust(out$PValue, method = p_adjust)
  out$Significance <- sapply(out$AdjustedPValue, sig_mark)
  out
}

pairwise_nonparametric_tests <- function(data, dv_name, p_adjust = "BH") {
  lv <- levels(data$group)
  pairs <- combn(lv, 2, simplify = FALSE)

  res <- lapply(pairs, function(pp) {
    g1 <- pp[1]
    g2 <- pp[2]
    x <- data$y[data$group == g1]
    y <- data$y[data$group == g2]

    wt <- tryCatch(
      wilcox.test(x, y, exact = FALSE, correct = FALSE),
      error = function(e) NULL
    )

    rbc <- calc_rank_biserial(x, y)

    data.frame(
      DependentVariable = dv_name,
      group1 = g1,
      group2 = g2,
      ComparisonMethod = "Wilcoxon rank-sum",
      Statistic = ifelse(is.null(wt), NA_real_, as.numeric(wt$statistic)),
      PValue = ifelse(is.null(wt), NA_real_, wt$p.value),
      MedianGroup1 = median(x, na.rm = TRUE),
      MedianGroup2 = median(y, na.rm = TRUE),
      MedianDifference = median(x, na.rm = TRUE) - median(y, na.rm = TRUE),
      rank_biserial = rbc,
      EffectSizeInterpretation = interpret_rbc(rbc)
    )
  })

  out <- bind_rows(res)
  out$AdjustedPValue <- p.adjust(out$PValue, method = p_adjust)
  out$Significance <- sapply(out$AdjustedPValue, sig_mark)
  out
}

pairwise_summary_sentence <- function(pair_df, dv_name, use_nonparametric = FALSE) {
  if (nrow(pair_df) == 0) {
    return(paste0("No significant pairwise differences were found for ", dv_name, "."))
  }

  sig_df <- pair_df %>% filter(!is.na(AdjustedPValue), AdjustedPValue < 0.05)

  if (nrow(sig_df) == 0) {
    return(paste0("No significant pairwise differences were found for ", dv_name, "."))
  }

  txts <- c()
  for (i in seq_len(nrow(sig_df))) {
    r <- sig_df[i, ]
    if (!use_nonparametric) {
      better <- ifelse(r$MeanGroup1 >= r$MeanGroup2, r$group1, r$group2)
      worse  <- ifelse(r$MeanGroup1 <  r$MeanGroup2, r$group1, r$group2)
      d_show <- abs(r$Cohen_d)
      txts <- c(
        txts,
        paste0(
          better, " was significantly higher than ", worse,
          " (adjusted p = ", fmt_p(r$AdjustedPValue),
          ", Cohen's d = ", fmt_num(d_show, 3), ")"
        )
      )
    } else {
      better <- ifelse(r$MedianGroup1 >= r$MedianGroup2, r$group1, r$group2)
      worse  <- ifelse(r$MedianGroup1 <  r$MedianGroup2, r$group1, r$group2)
      rb_show <- abs(r$rank_biserial)
      txts <- c(
        txts,
        paste0(
          better, " was significantly higher than ", worse,
          " (adjusted p = ", fmt_p(r$AdjustedPValue),
          ", rank-biserial = ", fmt_num(rb_show, 3), ")"
        )
      )
    }
  }

  paste0("Post-hoc comparisons showed that ", paste(txts, collapse = "; "), ".")
}

get_sorted_model_sentence <- function(stats_df, metric_col) {
  ord <- stats_df %>% arrange(desc(.data[[metric_col]]))
  paste(
    paste0(ord$ModelShortName, "(", fmt_num(ord[[metric_col]], 3), ")"),
    collapse = ", "
  )
}

status_from_follow <- function(x) {
  case_when(
    x >= recommended_threshold ~ "Recommended",
    x >= acceptable_threshold  ~ "Acceptable",
    TRUE ~ "Below"
  )
}

status_label_text <- function(x) {
  dplyr::case_when(
    x == "Recommended" ~ "Recommended",
    x == "Acceptable"  ~ "AcceptableWithManualReview",
    TRUE ~ "BelowThreshold"
  )
}

rank_with_focus_am1 <- function(df, score_col, tie_break_cols = character()) {
  score_sym <- rlang::sym(score_col)
  tie_syms  <- rlang::syms(tie_break_cols)

  arrange_exprs <- c(
    list(rlang::expr(desc(!!score_sym))),
    lapply(tie_syms, function(sym_obj) rlang::expr(!!sym_obj))
  )

  dplyr::arrange(df, !!!arrange_exprs)
}

select_candidates_am1 <- function(stats_df, score_col, tie_break_cols = character()) {
  rec_sub <- stats_df %>% filter(follow_status == "Recommended")
  acc_sub <- stats_df %>% filter(follow_status == "Acceptable")

  if (nrow(rec_sub) > 0) {
    ranked <- rank_with_focus_am1(rec_sub, score_col, tie_break_cols)
    candidate_group <- "Recommended"
  } else if (nrow(acc_sub) > 0) {
    ranked <- rank_with_focus_am1(acc_sub, score_col, tie_break_cols)
    candidate_group <- "Acceptable"
  } else {
    ranked <- stats_df %>% arrange(desc(MeanAdherence), desc(.data[[score_col]]), CompositeScore_SD)
    candidate_group <- "Below"
  }

  top_score <- ranked[[score_col]][1]
  ranked <- ranked %>%
    mutate(
      score_metric = score_col,
      score_gap_from_top = top_score - .data[[score_col]]
    )

  list(
    ranked_table = ranked,
    candidate_group = candidate_group,
    score_col = score_col
  )
}

build_scene_summary_am1 <- function(res, scene_name, scene_rule, scene_desc, score_label) {
  ranked <- res$ranked_table
  candidate_group <- res$candidate_group

  if (candidate_group == "Recommended") {
    top1 <- ranked[1, , drop = FALSE]
    backup_models <- ranked %>% slice(-1)
    backup_text <- if (nrow(backup_models) > 0) {
      paste0("Other models reaching the Recommended threshold include: ", paste(as.character(backup_models$ModelShortName), collapse = ", "), ".")
    } else {
      ""
    }

    list(
      ConclusionType = "PreferredOption",
      RecommendedModel = as.character(top1$ModelShortName),
      Rationale = paste0(
        "This model reached the recommended deployment threshold (MeanAdherence >= ", recommended_threshold,
        "), and ranked first by ", score_label, ".",
        "MeanAdherence=", fmt_num(top1$MeanAdherence, 3),
        "; ", score_label, "=", fmt_num(top1[[res$score_col]], 3), ".",
        backup_text
      ),
      ScenarioDescription = scene_desc
    )
  } else if (candidate_group == "Acceptable") {
    top1 <- ranked[1, , drop = FALSE]
    candidate_text <- if (nrow(ranked) > 1) {
      paste0("Other models reaching the Acceptable tier include: ", paste(as.character(ranked$ModelShortName[-1]), collapse = ", "), ".")
    } else {
      ""
    }

    list(
      ConclusionType = "AcceptableWithManualReview",
      RecommendedModel = as.character(top1$ModelShortName),
      Rationale = paste0(
        "No model reached the recommended deployment threshold (MeanAdherence >= ", recommended_threshold,
        ") in this scenario. The highest ranked model by ", score_label, " was ", as.character(top1$ModelShortName),
        "; its MeanAdherence was within the acceptable range (", acceptable_threshold, "-", recommended_threshold - 0.01,
        "), so it can be treated as a conditional candidate and should be manually reviewed before classroom use.",
        candidate_text
      ),
      ScenarioDescription = scene_desc
    )
  } else {
    list(
      ConclusionType = "NoPriorityDeploymentModel",
      RecommendedModel = "None",
      Rationale = paste0(
        "All models in this scenario had MeanAdherence below the acceptable threshold (", acceptable_threshold,
        "), so no model is recommended as a priority deployment option."
      ),
      ScenarioDescription = scene_desc
    )
  }
}

df_raw <- read_excel(input_excel, sheet = 1)

names(df_raw) <- names(df_raw) %>%
  str_replace_all("\\u3000", " ") %>%
  str_trim()

# The input Excel file is expected to use the English public schema.
# Required columns:
# ImageName, ModelShortName, PromptID, Language, ContentAttribute, StyleAttribute,
# ComplexityAttribute, ContextAttribute, MeanAdherence, AdherenceSD, MeanClarity,
# ClaritySD, MeanAesthetics, AestheticsSD, CompositeScore, ValidRaterCount,
# ElapsedSeconds, and VRAMUsage.

required_cols <- c(
  "ImageName", "ModelShortName", "PromptID", "Language",
  "ContentAttribute", "StyleAttribute", "ComplexityAttribute", "ContextAttribute",
  "MeanAdherence", "AdherenceSD", "MeanClarity", "ClaritySD",
  "MeanAesthetics", "AestheticsSD", "CompositeScore", "ValidRaterCount",
  "ElapsedSeconds", "VRAMUsage"
)

missing_cols <- setdiff(required_cols, names(df_raw))
if (length(missing_cols) > 0) {
  stop(paste0("The input Excel file is missing the following columns:\n", paste(missing_cols, collapse = "\n")))
}

df <- df_raw %>%
  mutate(
    ModelShortName = as.factor(.data[[group_col]]),
    Language = str_trim(as.character(.data[[language_col]])),
    CompositeScore = safe_as_numeric(`CompositeScore`),
    MeanAdherence = safe_as_numeric(`MeanAdherence`),
    MeanClarity = safe_as_numeric(`MeanClarity`),
    MeanAesthetics = safe_as_numeric(`MeanAesthetics`),
    ValidRaterCount = safe_as_numeric(`ValidRaterCount`),
    ElapsedSeconds = safe_as_numeric(`ElapsedSeconds`),
    VRAMUsage = safe_as_numeric(`VRAMUsage`)
  ) %>%
  filter(!is.na(ModelShortName)) %>%
  filter(tolower(Language) == tolower(language_value))

if (nrow(df) == 0) {
  stop(paste0("No data remained after filtering Language == '", language_value, "'."))
}

if (nlevels(droplevels(df$ModelShortName)) < 2) {
  stop("The Chinese group contains fewer than two models, so between-model comparison cannot be performed.")
}

df$ModelShortName <- droplevels(df$ModelShortName)

desc_stats <- df %>%
  group_by(ModelShortName) %>%
  summarise(
    n = n(),
    CompositeScoreMean = mean(CompositeScore, na.rm = TRUE),
    CompositeScore_SD = sd(CompositeScore, na.rm = TRUE),
    CompositeScoreMedian = median(CompositeScore, na.rm = TRUE),

    MeanAdherenceMean = mean(MeanAdherence, na.rm = TRUE),
    MeanAdherenceSD = sd(MeanAdherence, na.rm = TRUE),
    MeanAdherenceMedian = median(MeanAdherence, na.rm = TRUE),

    MeanClarityMean = mean(MeanClarity, na.rm = TRUE),
    MeanClaritySD = sd(MeanClarity, na.rm = TRUE),
    MeanClarityMedian = median(MeanClarity, na.rm = TRUE),

    MeanAestheticsMean = mean(MeanAesthetics, na.rm = TRUE),
    MeanAestheticsSD = sd(MeanAesthetics, na.rm = TRUE),
    MeanAestheticsMedian = median(MeanAesthetics, na.rm = TRUE),

    ElapsedSecondsMean = mean(ElapsedSeconds, na.rm = TRUE),
    VRAMUsageMean = mean(VRAMUsage, na.rm = TRUE),
    ValidRaterCountMean = mean(ValidRaterCount, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    CompositeScore = mean_sd_text(CompositeScoreMean, CompositeScore_SD),
    MeanAdherence = mean_sd_text(MeanAdherenceMean, MeanAdherenceSD),
    MeanClarity = mean_sd_text(MeanClarityMean, MeanClaritySD),
    MeanAesthetics = mean_sd_text(MeanAestheticsMean, MeanAestheticsSD)
  )

analyze_one_dv <- function(data, dv_name, group_name = "ModelShortName") {
  subdat <- data %>%
    select(all_of(group_name), all_of(dv_name)) %>%
    rename(group = all_of(group_name), y = all_of(dv_name)) %>%
    filter(!is.na(group), !is.na(y)) %>%
    mutate(group = droplevels(as.factor(group)))

  fit <- aov(y ~ group, data = subdat)
  residuals_fit <- residuals(fit)

  shapiro_p <- tryCatch(
    shapiro.test(residuals_fit)$p.value,
    error = function(e) NA_real_
  )

  levene_p <- tryCatch(
    car::leveneTest(y ~ group, data = subdat)[1, "Pr(>F)"],
    error = function(e) NA_real_
  )

  normal_ok <- !is.na(shapiro_p) && shapiro_p > 0.05
  homo_ok   <- !is.na(levene_p)  && levene_p > 0.05

  if (normal_ok && homo_ok) {
    chosen_method <- "ANOVA"
  } else if (normal_ok && !homo_ok) {
    chosen_method <- "Welch ANOVA"
  } else {
    chosen_method <- "Kruskal-Wallis"
  }

  assumption_df <- data.frame(
    DependentVariable = dv_name,
    NormalityP = shapiro_p,
    HomogeneityP = levene_p,
    NormalitySatisfied = ifelse(is.na(shapiro_p), NA, ifelse(normal_ok, "Yes", "No")),
    HomogeneitySatisfied = ifelse(is.na(levene_p), NA, ifelse(homo_ok, "Yes", "No")),
    RecommendedOmnibusTest = chosen_method
  )

  omnibus_df <- NULL
  pairwise_df <- NULL
  narrative_df <- NULL

  if (chosen_method == "ANOVA") {
    aov_sum <- summary(fit)[[1]]
    df1 <- aov_sum[1, "Df"]
    df2 <- aov_sum[2, "Df"]
    Fv  <- aov_sum[1, "F value"]
    pv  <- aov_sum[1, "Pr(>F)"]

    eta_val <- calc_partial_eta2_from_f(Fv, df1, df2)

    omnibus_df <- data.frame(
      DependentVariable = dv_name,
      TestMethod = "One-way ANOVA",
      StatisticName = "F",
      StatisticValue = Fv,
      Df1 = df1,
      Df2 = df2,
      PValue = pv,
      EffectSize = eta_val,
      EffectSizeName = "partial_eta2",
      EffectSizeInterpretation = interpret_eta2(eta_val)
    )

    pairwise_df <- pairwise_parametric_tests(subdat, dv_name, equal_var = TRUE, p_adjust = "BH")

    sentence1 <- paste0(
      "In the Chinese-group samples, a one-way ANOVA was conducted with ModelShortName as the independent variable and ", dv_name,
      " as the dependent variable. The model main effect was ",
      ifelse(pv < 0.05, "significant", "not significant"),
      ", F(", df1, ", ", df2, ") = ", fmt_num(Fv, 3),
      ", p ", ifelse(pv < 0.001, "< 0.001", paste0("= ", fmt_p(pv))),
      ", partial η² = ", fmt_num(eta_val, 3),
      ", indicating a ",
      ifelse(is.na(eta_val), "certain", interpret_eta2(eta_val)),
      " effect of ModelShortName on ", dv_name, "."
    )

    sentence2 <- pairwise_summary_sentence(pairwise_df, dv_name, use_nonparametric = FALSE)

    narrative_df <- data.frame(
      DependentVariable = dv_name,
      ResultParagraph = paste(sentence1, sentence2)
    )
  }

  if (chosen_method == "Welch ANOVA") {
    wel <- tryCatch(
      oneway.test(y ~ group, data = subdat, var.equal = FALSE),
      error = function(e) NULL
    )

    statv <- ifelse(is.null(wel), NA_real_, as.numeric(wel$statistic))
    df1   <- ifelse(is.null(wel), NA_real_, as.numeric(wel$parameter[1]))
    df2   <- ifelse(is.null(wel), NA_real_, as.numeric(wel$parameter[2]))
    pv    <- ifelse(is.null(wel), NA_real_, wel$p.value)

    omnibus_df <- data.frame(
      DependentVariable = dv_name,
      TestMethod = "Welch ANOVA",
      StatisticName = "F",
      StatisticValue = statv,
      Df1 = df1,
      Df2 = df2,
      PValue = pv,
      EffectSize = NA_real_,
      EffectSizeName = NA_character_,
      EffectSizeInterpretation = NA_character_
    )

    pairwise_df <- pairwise_parametric_tests(subdat, dv_name, equal_var = FALSE, p_adjust = "BH")

    sentence1 <- paste0(
      "In the Chinese-group samples, residual normality for ", dv_name,
      " was approximately satisfied, but the homogeneity of variance assumption was not. Therefore, Welch ANOVA was used. The between-model difference was ",
      ifelse(pv < 0.05, "significant", "not significant"),
      ",F(", fmt_num(df1, 3), ", ", fmt_num(df2, 3), ") = ",
      fmt_num(statv, 3), ", p ",
      ifelse(pv < 0.001, "< 0.001", paste0("= ", fmt_p(pv))), "."
    )

    sentence2 <- pairwise_summary_sentence(pairwise_df, dv_name, use_nonparametric = FALSE)

    narrative_df <- data.frame(
      DependentVariable = dv_name,
      ResultParagraph = paste(sentence1, sentence2)
    )
  }

  if (chosen_method == "Kruskal-Wallis") {
    kw <- tryCatch(
      kruskal.test(y ~ group, data = subdat),
      error = function(e) NULL
    )

    H <- ifelse(is.null(kw), NA_real_, as.numeric(kw$statistic))
    dfkw <- ifelse(is.null(kw), NA_real_, as.numeric(kw$parameter))
    pv <- ifelse(is.null(kw), NA_real_, kw$p.value)

    n_total <- nrow(subdat)
    eps2 <- if (!is.na(H) && !is.na(dfkw) && n_total > 0) {
      (H - dfkw + 1) / (n_total - dfkw)
    } else {
      NA_real_
    }

    omnibus_df <- data.frame(
      DependentVariable = dv_name,
      TestMethod = "Kruskal-Wallis",
      StatisticName = "H",
      StatisticValue = H,
      Df1 = dfkw,
      Df2 = NA_real_,
      PValue = pv,
      EffectSize = eps2,
      EffectSizeName = "epsilon_squared",
      EffectSizeInterpretation = interpret_eta2(eps2)
    )

    pairwise_df <- pairwise_nonparametric_tests(subdat, dv_name, p_adjust = "BH")

    sentence1 <- paste0(
      "In the Chinese-group samples, ", dv_name,
      " did not satisfy the normality assumption. Therefore, the Kruskal--Wallis test was used. The between-model difference was ",
      ifelse(pv < 0.05, "significant", "not significant"),
      ",H(", dfkw, ") = ", fmt_num(H, 3),
      ", p ", ifelse(pv < 0.001, "< 0.001", paste0("= ", fmt_p(pv))),
      ", ε² = ", fmt_num(eps2, 3), "."
    )

    sentence2 <- pairwise_summary_sentence(pairwise_df, dv_name, use_nonparametric = TRUE)

    narrative_df <- data.frame(
      DependentVariable = dv_name,
      ResultParagraph = paste(sentence1, sentence2)
    )
  }

  list(
    assumption = assumption_df,
    omnibus = omnibus_df,
    pairwise = pairwise_df,
    narrative = narrative_df
  )
}

analysis_results <- lapply(dv_cols, function(dv) analyze_one_dv(df, dv, group_col))
names(analysis_results) <- dv_cols

assumption_table <- bind_rows(lapply(analysis_results, `[[`, "assumption")) %>%
  mutate(
    NormalityPFormatted = sapply(NormalityP, fmt_p),
    HomogeneityPFormatted = sapply(HomogeneityP, fmt_p)
  )

omnibus_table <- bind_rows(lapply(analysis_results, `[[`, "omnibus")) %>%
  mutate(
    StatisticValueFormatted = fmt_num(StatisticValue, 3),
    PValueFormatted = sapply(PValue, fmt_p),
    EffectSizeFormatted = fmt_num(EffectSize, 3)
  )

pairwise_table <- bind_rows(lapply(analysis_results, `[[`, "pairwise"))
if (nrow(pairwise_table) > 0) {
  pairwise_table <- pairwise_table %>%
    mutate(
      PValueFormatted = sapply(PValue, fmt_p),
      AdjustedPValueFormatted = sapply(AdjustedPValue, fmt_p)
    )
}

narrative_table <- bind_rows(lapply(analysis_results, `[[`, "narrative"))

deploy_stats <- df %>%
  group_by(ModelShortName) %>%
  summarise(
    CompositeScore = mean(CompositeScore, na.rm = TRUE),
    MeanAdherence = mean(MeanAdherence, na.rm = TRUE),
    MeanClarity = mean(MeanClarity, na.rm = TRUE),
    MeanAesthetics = mean(MeanAesthetics, na.rm = TRUE),
    CompositeScore_SD = sd(CompositeScore, na.rm = TRUE),
    AdherenceSD = sd(MeanAdherence, na.rm = TRUE),
    ClaritySD = sd(MeanClarity, na.rm = TRUE),
    AestheticsSD = sd(MeanAesthetics, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  mutate(
    follow_status = status_from_follow(MeanAdherence),
    follow_status_label = status_label_text(follow_status)
  ) %>%
  arrange(desc(CompositeScore))

default_res   <- select_candidates_am1(deploy_stats, "CompositeScore", c("CompositeScore_SD", "MeanClarity", "MeanAesthetics"))
content_res   <- select_candidates_am1(deploy_stats, "MeanAdherence", c("AdherenceSD", "CompositeScore_SD"))
clarity_res   <- select_candidates_am1(deploy_stats, "MeanClarity", c("ClaritySD", "CompositeScore_SD"))
aesthetic_res <- select_candidates_am1(deploy_stats, "MeanAesthetics", c("AestheticsSD", "CompositeScore_SD"))

top_follow <- deploy_stats %>% arrange(desc(MeanAdherence), CompositeScore_SD) %>% slice(1)
top_clarity <- deploy_stats %>% arrange(desc(MeanClarity), ClaritySD, CompositeScore_SD) %>% slice(1)
top_aesthetic <- deploy_stats %>% arrange(desc(MeanAesthetics), AestheticsSD, CompositeScore_SD) %>% slice(1)

default_scene   <- build_scene_summary_am1(default_res, "DefaultModelSelection", "CheckCompositeScoreFirst", "GeneralTeachingScenario", "CompositeScore")
content_scene   <- build_scene_summary_am1(content_res, "ContentAccuracyFirst", "CheckAdherenceFirst", "KnowledgePointFiguresAndConceptExplanationFigures", "MeanAdherence")
clarity_scene   <- build_scene_summary_am1(clarity_res, "DisplayClarityFirst", "CheckClarityFirst", "ProjectionCoursewareAndDetailObservation", "MeanClarity")
aesthetic_scene <- build_scene_summary_am1(aesthetic_res, "VisualAppealFirst", "CheckAestheticsFirst", "IntroductoryPagesPromotionalImagesAndCreativeAssignments", "MeanAesthetics")

overall_intro <- paste0(
  "Based on the image-level samples in the Chinese group, this section first compared between-model differences in CompositeScore, MeanAdherence, MeanClarity, and MeanAesthetics. The descriptive statistics showed that ",
  "for CompositeScore, the models were ordered from highest to lowest as follows: ", get_sorted_model_sentence(deploy_stats, "CompositeScore"), "; ",
  "for MeanAdherence, the models were ordered from highest to lowest as follows: ", get_sorted_model_sentence(deploy_stats, "MeanAdherence"), "; ",
  "for MeanClarity, the models were ordered from highest to lowest as follows: ", get_sorted_model_sentence(deploy_stats, "MeanClarity"), "; ",
  "and for MeanAesthetics, the models were ordered from highest to lowest as follows: ", get_sorted_model_sentence(deploy_stats, "MeanAesthetics"), "."
)

ability_summary <- paste0(
  "In terms of capability characteristics, ",
  top_follow$ModelShortName, " performed best in prompt adherence, ",
  top_clarity$ModelShortName, " performed best in clarity, and ",
  top_aesthetic$ModelShortName, " performed best in aesthetics."
)

deploy_framework_text <- paste0(
  "In generating deployment recommendations, this study adopts a tiered filtering strategy. Models are first classified by MeanAdherence into Recommended (>= ",
  recommended_threshold,
  "), AcceptableWithManualReview (", acceptable_threshold, "-", recommended_threshold - 0.01,
  "), and BelowThreshold (< ", acceptable_threshold,
  ") tiers. Then, among candidate models meeting the quality threshold, CompositeScore, MeanAdherence, MeanClarity, or MeanAesthetics is compared separately, and the top-ranked model is identified as the preferred option for the corresponding scenario. Other models reaching the same quality tier are retained as alternatives."
)

deploy_paragraph <- paste(
  paste0(
    "For DefaultModelSelection, ",
    default_scene$ConclusionType, ":", default_scene$RecommendedModel, ".",
    default_scene$Rationale
  ),
  paste0(
    "For ContentAccuracyFirst tasks, ",
    content_scene$ConclusionType, ":", content_scene$RecommendedModel, ".",
    content_scene$Rationale
  ),
  paste0(
    "For DisplayClarityFirst tasks, ",
    clarity_scene$ConclusionType, ":", clarity_scene$RecommendedModel, ".",
    clarity_scene$Rationale
  ),
  paste0(
    "For VisualAppealFirst tasks, ",
    aesthetic_scene$ConclusionType, ":", aesthetic_scene$RecommendedModel, ".",
    aesthetic_scene$Rationale
  )
)

full_result_paragraph <- paste(
  overall_intro,
  paste(narrative_table$ResultParagraph, collapse = " "),
  ability_summary,
  deploy_framework_text,
  deploy_paragraph
)

full_result_table <- data.frame(
  PaperResultSection = c(
    "OverallDescription",
    paste(narrative_table$DependentVariable),
    "CapabilitySummary",
    "DeploymentDecisionFramework",
    "DeploymentRecommendations",
    "MergedResultParagraph"
  ),
  Content = c(
    overall_intro,
    narrative_table$ResultParagraph,
    ability_summary,
    deploy_framework_text,
    deploy_paragraph,
    full_result_paragraph
  )
)

method_text <- paste0(
  "Based on the image-level samples in the Chinese group, ModelShortName was used as the independent variable, and CompositeScore, MeanAdherence, MeanClarity, and MeanAesthetics were used as dependent variables for between-model comparisons. Shapiro--Wilk tests were first used to examine the normality of model residuals for each dependent variable, and Levene tests were used to examine homogeneity of variance. If both normality and homogeneity assumptions were satisfied, one-way ANOVA was used. If normality was satisfied but homogeneity of variance was not, Welch ANOVA was used. If the normality assumption was not satisfied, the Kruskal--Wallis test was used. For dependent variables with significant omnibus differences, post-hoc pairwise comparisons were conducted: independent-samples t tests with BH correction were used in the parametric path, and Wilcoxon rank-sum tests with BH correction were used in the non-parametric path. Cohen's d was additionally reported for parametric comparisons, and rank-biserial correlation was reported for non-parametric comparisons. Deployment recommendations were generated using a tiered filtering strategy. Models were first classified as Recommended, AcceptableWithManualReview, or BelowThreshold according to MeanAdherence. Then, among candidate models meeting the quality threshold, CompositeScore, MeanAdherence, MeanClarity, or MeanAesthetics was compared, and the top-ranked model was identified as the preferred option for the corresponding scenario. Other models reaching the same quality tier were retained as alternatives."
)

method_text_table <- data.frame(
  PaperMethodDescription = method_text
)

scene_to_table <- function(scene_name, rule_text, scene_obj) {
  data.frame(
    Scenario = scene_name,
    DecisionRule = rule_text,
    ConclusionType = scene_obj$ConclusionType,
    RecommendedModel = scene_obj$RecommendedModel,
    Rationale = scene_obj$Rationale,
    ApplicableTasks = scene_obj$ScenarioDescription
  )
}

deploy_recommendation_table <- bind_rows(
  scene_to_table(
    "DefaultModelSelection",
    paste0("First classify models by MeanAdherence (Recommended >= ", recommended_threshold, "; Acceptable >= ", acceptable_threshold,
           "), then compare CompositeScore to identify the preferred option"),
    default_scene
  ),
  scene_to_table(
    "ContentAccuracyFirst",
    paste0("First classify models by MeanAdherence, then compare MeanAdherence to identify the preferred option"),
    content_scene
  ),
  scene_to_table(
    "DisplayClarityFirst",
    paste0("First classify models by MeanAdherence, then compare MeanClarity to identify the preferred option"),
    clarity_scene
  ),
  scene_to_table(
    "VisualAppealFirst",
    paste0("First classify models by MeanAdherence, then compare MeanAesthetics to identify the preferred option"),
    aesthetic_scene
  )
)

deploy_model_rankings <- bind_rows(
  default_res$ranked_table %>% mutate(Scenario = "DefaultModelSelection"),
  content_res$ranked_table %>% mutate(Scenario = "ContentAccuracyFirst"),
  clarity_res$ranked_table %>% mutate(Scenario = "DisplayClarityFirst"),
  aesthetic_res$ranked_table %>% mutate(Scenario = "VisualAppealFirst")
) %>%
  group_by(Scenario) %>%
  mutate(rank = row_number()) %>%
  ungroup() %>%
  select(
    Scenario, rank, ModelShortName, follow_status, follow_status_label,
    CompositeScore, CompositeScore_SD, MeanAdherence, AdherenceSD,
    MeanClarity, ClaritySD, MeanAesthetics, AestheticsSD,
    score_metric, score_gap_from_top, n
  )

output_list <- list(
  "01_ChineseGroupDescStatistics" = desc_stats,
  "02_AssumptionChecks" = assumption_table,
  "03_OmnibusTests" = omnibus_table,
  "04_PostHocComparisons" = pairwise_table,
  "05_SingleMetricResultParagraphs" = narrative_table,
  "06_FullPaperResults" = full_result_table,
  "07_DeploymentRecommendations" = deploy_recommendation_table,
  "08_DeploymentCandidateRanking" = deploy_model_rankings,
  "09_PaperMethodDescription" = method_text_table
)

write_xlsx(output_list, output_excel)

cat("\n============================\n")
cat("Analysis completed. Results were written to: ", output_excel, "\n")
cat("Recommended threshold: ", recommended_threshold, "\n")
cat("Acceptable threshold: ", acceptable_threshold, "\n")
cat("============================\n\n")

cat("[Full paper result paragraph]\n")
cat(full_result_paragraph)
cat("\n\n")
