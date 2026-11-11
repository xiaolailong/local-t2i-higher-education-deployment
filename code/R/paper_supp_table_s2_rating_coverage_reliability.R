#!/usr/bin/env Rscript
# Chinese-prompt questionnaire coverage and variance-component reliability.

root <- normalizePath(getwd(), winslash = '/', mustWork = TRUE)
raw_file <- file.path(root, 'data', 'source', 'subjective_scores_human120.xlsx')
rel_file <- file.path(root, 'data', 'paper_results', 'Rating_reliability_variance_components.csv')
out <- file.path(root, 'data', 'paper_results', 'Supplementary_Table_S2_rating_coverage_reliability.md')

stop_with <- function(...) stop(paste0(...), call. = FALSE)
if (!requireNamespace('readxl', quietly = TRUE))
  stop_with('The R package readxl is required.')
if (!file.exists(raw_file)) stop_with('Missing project-local questionnaire workbook: ', raw_file)
if (!file.exists(rel_file)) stop_with('Missing reliability results: ', rel_file,
                                      '\nRun paper_rating_reliability.R first.')

norm_language <- function(x) {
  z <- tolower(trimws(as.character(x)))
  z %in% c('chinese', 'cn', 'zh', 'zh-cn', '中文', '中文提示词')
}
norm_id <- function(x) {
  z <- toupper(trimws(as.character(x)))
  matched <- grepl('^B0*[0-9]+$', z)
  z[matched] <- paste0('B', as.integer(sub('^B', '', z[matched])))
  z
}
norm_model <- function(x) {
  z <- tolower(trimws(as.character(x)))
  out <- rep(NA_character_, length(z))
  out[grepl('qwen', z)] <- 'Qwen'
  out[grepl('z[-_ ]?image|zimage', z)] <- 'Z-Image'
  out[grepl('sdxl|stable.diffusion.xl', z)] <- 'SDXL'
  out
}
norm_question <- function(x) {
  z <- toupper(gsub('[[:space:]_.-]+', '', as.character(x)))
  sub('^Q([123])$', '\\1', z)
}
forms <- c('teacher', 'student_A', 'student_B')
if (!all(forms %in% readxl::excel_sheets(raw_file)))
  stop_with('Questionnaire workbook is missing one or more Chinese-form worksheets.')

raws <- lapply(forms, function(sh) {
  d <- as.data.frame(readxl::read_excel(raw_file, sheet = sh))
  fields <- c('respondent_id','prompt_id','model','language','q_no','score')
  if (!all(fields %in% names(d))) stop_with('Missing columns in ', sh, ': ',
                                             paste(setdiff(fields, names(d)), collapse = ', '))
  d <- d[norm_language(d$language), , drop = FALSE]
  data.frame(Form = sh,
             Rater = paste0(sh, '::', trimws(as.character(d$respondent_id))),
             Image = paste(norm_id(d$prompt_id), norm_model(d$model), sep = '|'),
             Question = norm_question(d$q_no),
             Score = suppressWarnings(as.numeric(d$score)),
             stringsAsFactors = FALSE)
})
raw <- do.call(rbind, raws)
if (anyNA(raw) || any(!is.finite(raw$Score)) ||
    any(!raw$Question %in% as.character(1:3)) ||
    any(raw$Score < 1 | raw$Score > 5) ||
    any(!nzchar(raw$Rater)) ||
    anyDuplicated(paste(raw$Rater, raw$Image, raw$Question, sep = '|')))
  stop_with('Invalid or duplicated Chinese questionnaire ratings.')
if (!setequal(unique(raw$Image), as.vector(outer(paste0('B', 1:20),
                                c('Qwen','Z-Image','SDXL'), paste, sep = '|'))))
  stop_with('Expected 60 prompt-model image keys for Chinese ratings.')
units <- table(paste(raw$Rater, raw$Image, sep = '|'))
if (any(units != 3L)) stop_with('Incomplete Q1–Q3 response-image records.')

counts <- lapply(forms, function(f) {
  d <- raw[raw$Form == f, , drop = FALSE]
  c(Respondents = length(unique(d$Rater)),
    Images = length(unique(d$Image)),
    Units = length(unique(paste(d$Rater, d$Image, sep = '|'))),
    Scores = nrow(d))
})
counts <- do.call(rbind, counts)
rownames(counts) <- forms
if (!identical(unname(as.integer(counts[, 'Respondents'])), c(25L,84L,82L)))
  stop_with('Chinese questionnaire counts differ from the expected 25/84/82.')
if (sum(counts[, 'Units']) != 6150L || sum(counts[, 'Scores']) != 18450L ||
    length(unique(raw$Image)) != 60L)
  stop_with('The Chinese rating coverage does not match the 60-image analysis.')
per_image <- tapply(raw$Rater, raw$Image, function(v) length(unique(v)))
if (length(per_image) != 60L || min(per_image) != 100L || max(per_image) != 105L)
  stop_with('Unexpected image coverage. Inspect questionnaire selections.')

rel <- read.csv(rel_file, check.names = FALSE, stringsAsFactors = FALSE)
needed <- c('Dimension', 'N_Images', 'N_Raters', 'N_Scores',
            'ReliabilitySingle', 'ReliabilityAverageK', 'SingularFit')
if (!all(needed %in% names(rel)))
  stop_with('Variance-component results contain missing columns.')
order <- c('Adherence', 'Clarity', 'Aesthetics', 'Composite')
if (nrow(rel) != 4L || !setequal(rel$Dimension, order) ||
    any(rel$N_Images != 60L) || any(rel$N_Raters != 191L) ||
    any(rel$N_Scores != 6150L) || any(rel$SingularFit) ||
    anyNA(rel[, needed]) ||
    any(rel$ReliabilitySingle < 0 | rel$ReliabilitySingle > 1) ||
    any(rel$ReliabilityAverageK < 0 | rel$ReliabilityAverageK > 1))
  stop_with('Variance-component results are inconsistent with the Chinese 60-image sample.')
rel <- rel[match(order, rel$Dimension), , drop = FALSE]
comma <- function(x) format(x, big.mark=',', scientific=FALSE, trim=TRUE)
label <- c(teacher='Teacher', student_A='Student A', student_B='Student B')

lines <- c('# Supplementary Table S2. Human-rating coverage and score reliability',
           '', '**Panel A. Questionnaire coverage**', '',
           '| Questionnaire form | Valid respondents | Distinct images | Complete rater–image units | Q1–Q3 scores |',
           '|:--|--:|--:|--:|--:|')
for (f in forms) {
  d <- counts[f, ]
  lines <- c(lines, sprintf('| %s | %s | %s | %s | %s |', label[f],
    comma(d['Respondents']), comma(d['Images']), comma(d['Units']), comma(d['Scores'])))
}
lines <- c(lines, sprintf('| **Total** | **%s** | **%s** | **%s** | **%s** |',
  comma(sum(counts[, 'Respondents'])), comma(length(unique(raw$Image))),
  comma(sum(counts[, 'Units'])), comma(sum(counts[, 'Scores']))),
  '', '**Panel B. Variance-component reliability**', '',
  '| Dimension | Single-rater reliability | Image-mean reliability |',
  '|:--|--:|--:|')
for (i in seq_len(nrow(rel))) {
  lines <- c(lines, sprintf('| %s | %.3f | %.3f |',
    rel$Dimension[i], rel$ReliabilitySingle[i], rel$ReliabilityAverageK[i]))
}
lines <- c(lines, '', sprintf(paste0(
  '**Note.** Panel A counts distinct images across forms only once in the total row. ',
  'All 60 images received %d–%d ratings (mean %.1f). ',
  'Panel B is estimated from image and rater random-intercept variance components; ',
  'image-mean reliability uses the mean number of ratings per image.'),
  min(per_image), max(per_image), mean(per_image)), '')
dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
writeLines(lines, con = out, useBytes = TRUE)
message('Written: ', out)
