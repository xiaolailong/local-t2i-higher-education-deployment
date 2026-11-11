#!/usr/bin/env Rscript

options(stringsAsFactors = FALSE)
script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
if (length(script_arg) != 1L) stop("Run this file with Rscript.", call. = FALSE)
script_path <- normalizePath(sub("^--file=", "", script_arg[1]), winslash = "/", mustWork = TRUE)
root <- normalizePath(file.path(dirname(script_path), "..", ".."), winslash = "/", mustWork = TRUE)
result_dir <- file.path(root, "data", "paper_results")
gap_file <- file.path(result_dir, "Task_attribute_prompt_gaps.csv")
rating_file <- file.path(root, "data", "source", "subjective_scores_human120.xlsx")
out_file <- file.path(result_dir, "Supplementary_Table_S4_task_conditioned_gaps.md")

if (!file.exists(gap_file)) stop("Missing input: ", gap_file, call. = FALSE)
if (!file.exists(rating_file)) stop("Missing input: ", rating_file, call. = FALSE)
if (!requireNamespace("readxl", quietly = TRUE)) {
  stop("Package 'readxl' is required.", call. = FALSE)
}

p <- read.csv(gap_file, check.names = FALSE, stringsAsFactors = FALSE)
required <- c("PromptID", "Context", "Content", "QwenComposite", "ZImageComposite",
              "QwenAdherence", "ZImageAdherence", "QZGap", "QZAdherenceGap")
if (!all(required %in% names(p))) {
  stop("Missing columns: ", paste(setdiff(required, names(p)), collapse = ", "), call. = FALSE)
}
if (nrow(p) != 20L || anyDuplicated(p$PromptID) ||
    !setequal(p$PromptID, paste0("B", 1:20))) {
  stop("Expected one row for each PromptID B1–B20.", call. = FALSE)
}
if (!identical(sort(unique(p$Context)), sort(c("Educational", "General", "Chinese-cultural"))) ||
    !identical(sort(unique(p$Content)), sort(c("Object-Concept", "Other content")))) {
  stop("Unexpected Context or Content labels.", call. = FALSE)
}
num_cols <- c("QwenComposite", "ZImageComposite", "QwenAdherence", "ZImageAdherence",
              "QZGap", "QZAdherenceGap")
for (nm in num_cols) p[[nm]] <- suppressWarnings(as.numeric(p[[nm]]))
if (any(!is.finite(as.matrix(p[num_cols])))) stop("Non-finite score values.", call. = FALSE)
if (max(abs(p$QZGap - (p$QwenComposite - p$ZImageComposite))) > 1e-8 ||
    max(abs(p$QZAdherenceGap - (p$QwenAdherence - p$ZImageAdherence))) > 1e-8) {
  stop("Prompt-level paired differences do not match score columns.", call. = FALSE)
}

mean_ci <- function(x) {
  x <- as.numeric(x)
  c(mean(x), stats::t.test(x, conf.level = 0.95)$conf.int[1:2])
}
paired_loo <- function(x) {
  vapply(seq_along(x), function(i) mean(x[-i]), numeric(1))
}
contrast_summary <- function(x, y) {
  fit <- stats::t.test(x, y, var.equal = FALSE, conf.level = 0.95)
  drops <- c(vapply(seq_along(x), function(i) mean(x[-i]) - mean(y), numeric(1)),
             vapply(seq_along(y), function(i) mean(x) - mean(y[-i]), numeric(1)))
  c(estimate = mean(x) - mean(y), lower = unname(fit$conf.int[1]), upper = unname(fit$conf.int[2]),
    p = fit$p.value, loo_min = min(drops), loo_max = max(drops))
}
fm <- function(x) sprintf("%+.3f", x)
interval <- function(x) sprintf("%+.3f [%+.3f, %+.3f]", x[1], x[2], x[3])
mdrow <- function(...) paste0("| ", paste(c(...), collapse = " | "), " |")

subset_items <- list(
  "All prompts" = rep(TRUE, nrow(p)),
  "Educational" = p$Context == "Educational",
  "General" = p$Context == "General",
  "Chinese-cultural" = p$Context == "Chinese-cultural",
  "Object–Concept" = p$Content == "Object-Concept",
  "Other content" = p$Content == "Other content"
)
panel_a <- c(
  "**Panel A. Paired differences by task category**", "",
  "| Task factor | Category | Prompts | Composite difference (95% CI) | Adherence difference | Qwen ahead |",
  "|:--|:--|--:|--:|--:|--:|"
)
for (i in seq_along(subset_items)) {
  select <- subset_items[[i]]
  x <- p$QZGap[select]
  a <- p$QZAdherenceGap[select]
  factor_name <- if (i == 1L) "Overall" else if (i <= 4L) "Context" else "Content"
  category <- names(subset_items)[i]
  panel_a <- c(panel_a, mdrow(factor_name, category, length(x), interval(mean_ci(x)),
                              fm(mean(a)), paste0(sum(x > 0), "/", length(x))))
}

joint_items <- list(
  c("Educational", "Object–Concept"),
  c("Educational", "Other content"),
  c("Other contexts", "Object–Concept"),
  c("Other contexts", "Other content")
)
panel_b <- c(
  "**Panel B. Joint context and content categories**", "",
  "| Context | Content | Prompts | Composite difference | Adherence difference | Qwen ahead |",
  "|:--|:--|--:|--:|--:|--:|"
)
for (z in joint_items) {
  select <- (if (z[1] == "Educational") p$Context == "Educational" else p$Context != "Educational") &
            (if (z[2] == "Object–Concept") p$Content == "Object-Concept" else p$Content == "Other content")
  x <- p$QZGap[select]
  panel_b <- c(panel_b, mdrow(z[1], z[2], length(x), fm(mean(x)),
                              fm(mean(p$QZAdherenceGap[select])), paste0(sum(x > 0), "/", length(x))))
}

contrast_items <- list(
  list(name = "Educational − General", a = p$Context == "Educational", b = p$Context == "General"),
  list(name = "Educational − Other contexts", a = p$Context == "Educational", b = p$Context != "Educational"),
  list(name = "Object–Concept − Other content", a = p$Content == "Object-Concept", b = p$Content == "Other content")
)
contrasts <- lapply(contrast_items, function(z) contrast_summary(p$QZGap[z$a], p$QZGap[z$b]))
pvals <- stats::p.adjust(vapply(contrasts, function(x) unname(x["p"]), numeric(1)), method = "BH")
panel_c <- c(
  "**Panel C. Between-category composite differences**", "",
  "| Contrast | Prompts (A/B) | Difference (95% CI) | LOO range | BH-adjusted p |",
  "|:--|--:|--:|--:|--:|"
)
for (i in seq_along(contrast_items)) {
  z <- contrast_items[[i]]
  x <- contrasts[[i]]
  panel_c <- c(panel_c, mdrow(z$name, paste0(sum(z$a), "/", sum(z$b)),
                              interval(x[c("estimate", "lower", "upper")]),
                              sprintf("[%+.3f, %+.3f]", x["loo_min"], x["loo_max"]),
                              sprintf("%.4f", pvals[i])))
}

norm_header <- function(x) tolower(gsub("[^a-z0-9]", "", x))
get_col <- function(d, possible, sheet) {
  k <- match(norm_header(possible), norm_header(names(d)))
  k <- k[!is.na(k)]
  if (!length(k)) stop("Missing column on ", sheet, ": ", paste(possible, collapse = "/"), call. = FALSE)
  d[[k[1]]]
}
normalize_prompt <- function(x) {
  x <- toupper(gsub("[[:space:]]+", "", as.character(x)))
  good <- grepl("^B0*[0-9]+$", x)
  x[good] <- paste0("B", as.integer(sub("^B", "", x[good])))
  x
}
normalize_model <- function(x) {
  x <- tolower(trimws(as.character(x)))
  out <- rep(NA_character_, length(x))
  out[grepl("qwen", x)] <- "Qwen"
  out[grepl("z[-_ ]?image|zimage", x)] <- "Z-Image"
  out[grepl("sdxl|stable.diffusion.xl", x)] <- "SDXL"
  out
}
normalize_language <- function(x) {
  x <- tolower(gsub("[[:space:]_]+", "", trimws(as.character(x))))
  x %in% c("chinese", "ch", "cn", "zh", "zh-cn", "zh-hans", "中文", "中文提示词", "汉语", "汉语提示词")
}
raw_sheets <- c("teacher", "student_A", "student_B")
if (!all(raw_sheets %in% readxl::excel_sheets(rating_file))) {
  stop("Missing one or more Chinese-prompt questionnaire sheets.", call. = FALSE)
}
rating_parts <- lapply(raw_sheets, function(sh) {
  d <- as.data.frame(readxl::read_excel(rating_file, sheet = sh))
  data.frame(
    PromptID = normalize_prompt(get_col(d, c("prompt_id", "PromptID"), sh)),
    Model = normalize_model(get_col(d, c("model", "ModelShortName"), sh)),
    Chinese = normalize_language(get_col(d, c("language", "PromptLanguage"), sh)),
    Score = suppressWarnings(as.numeric(get_col(d, c("score", "Rating"), sh))),
    RaterGroup = if (sh == "teacher") "Teachers" else "Students",
    stringsAsFactors = FALSE
  )
})
r <- do.call(rbind, rating_parts)
r <- r[r$Chinese, , drop = FALSE]
if (nrow(r) != 18450L || anyNA(r[c("PromptID", "Model", "Score")]) ||
    !all(r$PromptID %in% paste0("B", 1:20)) ||
    !all(r$Model %in% c("Qwen", "Z-Image", "SDXL")) ||
    any(r$Score < 1 | r$Score > 5)) {
  stop("Unexpected records in the Chinese-prompt rating subset.", call. = FALSE)
}
by_group <- aggregate(Score ~ PromptID + Model + RaterGroup, data = r, FUN = mean)
wide <- reshape(by_group, idvar = c("PromptID", "RaterGroup"), timevar = "Model", direction = "wide")
if (nrow(wide) != 40L || anyNA(wide[c("Score.Qwen", "Score.Z-Image")])) {
  stop("Teacher/student paired image summaries are incomplete.", call. = FALSE)
}
wide$Gap <- wide[["Score.Qwen"]] - wide[["Score.Z-Image"]]
profiles <- list(
  "All prompts" = rep(TRUE, nrow(p)),
  "Educational" = p$Context == "Educational",
  "General" = p$Context == "General",
  "Chinese-cultural" = p$Context == "Chinese-cultural"
)
panel_d <- c(
  "**Panel D. Composite differences by rater group**", "",
  "| Context | Prompts | Teachers | Students | Student − teacher |",
  "|:--|--:|--:|--:|--:|"
)
for (i in seq_along(profiles)) {
  ids <- p$PromptID[profiles[[i]]]
  t <- mean(wide$Gap[wide$PromptID %in% ids & wide$RaterGroup == "Teachers"])
  s <- mean(wide$Gap[wide$PromptID %in% ids & wide$RaterGroup == "Students"])
  nt <- sum(wide$PromptID %in% ids & wide$RaterGroup == "Teachers")
  ns <- sum(wide$PromptID %in% ids & wide$RaterGroup == "Students")
  if (nt != length(ids) || ns != length(ids)) stop("Incomplete ratings for ", names(profiles)[i], call. = FALSE)
  panel_d <- c(panel_d, mdrow(names(profiles)[i], length(ids), fm(t), fm(s), fm(s-t)))
}

note <- paste0(
  "**Note.** Differences represent Qwen-Image-2512 minus Z-Image-Turbo on the 1–5 rating scale. ",
  "Each prompt contributes one rated image per model. ",
  "95% intervals use prompt-level t estimates in Panel A and Welch estimates in Panel C. ",
  "Panel C p-values use Benjamini–Hochberg adjustment across the three contrasts; ",
  "LOO ranges omit one prompt at a time. Context and content labels overlap."
)
text <- c(
  "# Supplementary Table S4. Task-conditioned paired human-rating differences", "",
  panel_a, "", panel_b, "", panel_c, "", panel_d, "", note
)
dir.create(result_dir, recursive = TRUE, showWarnings = FALSE)
writeLines(text, out_file, useBytes = TRUE)
cat("Wrote: ", out_file, "\n", sep = "")
