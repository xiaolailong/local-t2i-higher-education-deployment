#!/usr/bin/env Rscript
# Compare the task-attribute contributions to the Chinese prompt-matched model gaps.
# Inputs: data/source/subjective_scores_human120*.xlsx
# Outputs: data/paper_results/Task_attribute_incremental_diagnostic.{md,csv}

options(stringsAsFactors = FALSE)

fail <- function(...) stop(paste0(...), call. = FALSE)
args <- grep('^--file=', commandArgs(trailingOnly = FALSE), value = TRUE)
if (length(args) != 1L) fail('Execute with Rscript.')
script <- normalizePath(sub('^--file=', '', args[[1L]]), winslash = '/', mustWork = TRUE)
root <- normalizePath(file.path(dirname(script), '..', '..'), winslash = '/', mustWork = TRUE)
source_dir <- file.path(root, 'data', 'source')
image_path <- file.path(source_dir, 'subjective_scores_human120_image_level.xlsx')
ratings_path <- file.path(source_dir, 'subjective_scores_human120.xlsx')
out_dir <- file.path(root, 'data', 'paper_results')
for (p in c(image_path, ratings_path)) if (!file.exists(p)) fail('Missing project-local input: ', p)
if (!requireNamespace('readxl', quietly = TRUE)) fail('Install R package: readxl')
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

normalize_id <- function(x) {
  z <- toupper(gsub('[[:space:]]+', '', as.character(x)))
  ok <- grepl('^B0*[0-9]+$', z)
  z[ok] <- paste0('B', as.integer(sub('^B', '', z[ok])))
  z
}
normalize_model <- function(x) {
  z <- tolower(trimws(as.character(x)))
  out <- rep(NA_character_, length(z))
  out[grepl('qwen', z)] <- 'Qwen'
  out[grepl('z[-_ ]?image|zimage', z)] <- 'Z-Image'
  out[grepl('sdxl|stable.diffusion.xl', z)] <- 'SDXL'
  out
}
normalize_language <- function(x) {
  z <- tolower(trimws(as.character(x)))
  out <- rep(NA_character_, length(z))
  out[z %in% c('chinese', 'cn', 'zh', 'zh-cn', '中文', '中文提示词')] <- 'Chinese'
  out[z %in% c('english', 'en', 'eng', '英文', '英文提示词')] <- 'English'
  out
}
normalized_label <- function(x) {
  tolower(gsub('[[:space:]_./–—-]+', '', trimws(as.character(x))))
}
context_label <- function(x) {
  z <- normalized_label(x)
  out <- rep(NA_character_, length(z))
  out[grepl('educat|teaching|教育|教学', z)] <- 'Educational'
  out[grepl('general|通用|一般', z)] <- 'General'
  out[grepl('chinesecultur|culturechinese|culturalchinese|中国文化|中华文化|中国传统', z)] <- 'Chinese-cultural'
  out
}
style_label <- function(x) {
  z <- normalized_label(x)
  out <- rep(NA_character_, length(z))
  out[grepl('realistic|photoreal|写实|真实风格', z)] <- 'Realistic'
  out[grepl('stylized|stylised|艺术化|插画|风格化', z)] <- 'Stylized'
  out
}
complexity_label <- function(x) {
  z <- normalized_label(x)
  out <- rep(NA_character_, length(z))
  out[grepl('^simple$|^low$|简单|简易', z)] <- 'Simple'
  out[grepl('^moderate$|^medium$|中等|适中', z)] <- 'Moderate'
  out[grepl('^high$|^highcomplexity$|高复杂|复杂度高', z)] <- 'High'
  out
}
fmt <- function(x, digits = 3L) {
  ifelse(is.finite(x), sprintf(paste0('%.', digits, 'f'), x), 'NA')
}
signed <- function(x, digits = 3L) {
  ifelse(is.finite(x), sprintf(paste0('%+.', digits, 'f'), x), 'NA')
}
mdrow <- function(...) paste0('| ', paste(as.character(c(...)), collapse = ' | '), ' |')

ids <- paste0('B', 1:20)
image <- as.data.frame(readxl::read_excel(image_path, sheet = 'image_level'))
cols <- c('PromptID','Language','ModelShortName','CompositeScore','MeanAdherence')
if (!all(cols %in% names(image))) fail('image_level missing columns: ', paste(setdiff(cols, names(image)), collapse = ', '))
image$PromptID <- normalize_id(image$PromptID)
image$Language <- normalize_language(image$Language)
image$Model <- normalize_model(image$ModelShortName)
image$CompositeScore <- suppressWarnings(as.numeric(image$CompositeScore))
image$MeanAdherence <- suppressWarnings(as.numeric(image$MeanAdherence))
image <- image[image$Language == 'Chinese' & !is.na(image$Language), , drop = FALSE]
key <- paste(image$PromptID, image$Model)
if (nrow(image) != 60L || anyDuplicated(key) || anyNA(image[, c('PromptID','Model','CompositeScore','MeanAdherence')]) ||
    !setequal(image$PromptID, ids) || !setequal(image$Model, c('Qwen','Z-Image','SDXL')) ||
    any(table(image$PromptID) != 3L) ||
    any(image$CompositeScore < 1 | image$CompositeScore > 5) ||
    any(image$MeanAdherence < 1 | image$MeanAdherence > 5)) {
  fail('Expected 60 unique Chinese-prompt images (B1-B20 x three models).')
}
latest <- c(Qwen = 4.1344, 'Z-Image' = 4.0030, SDXL = 2.9123)
for (m in names(latest)) {
  obs <- mean(image$CompositeScore[image$Model == m])
  if (abs(obs - latest[[m]]) > 0.005) fail('Image-level model mean differs from verified Chinese scores: ', m)
}

forms <- c('teacher', 'student_A', 'student_B')
if (!all(forms %in% readxl::excel_sheets(ratings_path))) fail('Missing Chinese rating source sheets.')
meta_raw <- lapply(forms, function(sh) {
  x <- as.data.frame(readxl::read_excel(ratings_path, sheet = sh))
  required <- c('prompt_id', 'language', 'Context', 'Style', 'Complexity')
  if (!all(required %in% names(x))) fail('Missing task fields in sheet ', sh, ': ', paste(setdiff(required, names(x)), collapse = ', '))
  x$PromptID <- normalize_id(x$prompt_id)
  x$PromptLanguage <- normalize_language(x$language)
  x <- x[!is.na(x$PromptLanguage) & x$PromptLanguage == 'Chinese' & x$PromptID %in% ids, , drop = FALSE]
  if (!nrow(x)) fail('No Chinese-prompt metadata found in sheet ', sh)
  data.frame(PromptID = x$PromptID, Context = context_label(x$Context),
             Style = style_label(x$Style), Complexity = complexity_label(x$Complexity),
             stringsAsFactors = FALSE)
})
meta <- unique(do.call(rbind, meta_raw))
if (nrow(meta) != 20L || anyDuplicated(meta$PromptID) || !setequal(meta$PromptID, ids) || anyNA(meta)) {
  sources <- lapply(forms, function(sh) {
    x <- as.data.frame(readxl::read_excel(ratings_path, sheet = sh))
    x[c('prompt_id','Context','Style','Complexity')]
  })
  samples <- unique(do.call(rbind, sources))
  samples <- utils::head(samples, 16L)
  print(samples)
  fail('Task labels cannot be assigned uniquely to B1-B20. Check source values printed above.')
}
meta <- meta[order(match(meta$PromptID, ids)), , drop = FALSE]
check_count <- function(v, levels, expected) {
  got <- as.integer(table(factor(v, levels = levels)))
  if (!identical(got, as.integer(expected))) fail('Unexpected task counts for ', paste(levels, collapse = '/'), ': ', paste(got, collapse = '/'))
}
check_count(meta$Context, c('Educational','General','Chinese-cultural'), c(5,10,5))
check_count(meta$Style, c('Realistic','Stylized'), c(10,10))
check_count(meta$Complexity, c('Simple','Moderate','High'), c(4,12,4))
meta$Content <- ifelse(meta$PromptID %in% c('B7','B8','B9','B16','B17','B18'), 'Object-Concept', 'Other content')
check_count(meta$Content, c('Object-Concept','Other content'), c(6,14))
if (sum(meta$Context == 'Educational' & meta$Content == 'Object-Concept') != 3L) fail('Context/content labels disagree with Figure 3 taxonomy.')

model_rows <- function(model, prefix) {
  z <- image[image$Model == model, c('PromptID','CompositeScore','MeanAdherence'), drop = FALSE]
  names(z)[2:3] <- paste0(prefix, c('Composite','Adherence'))
  z
}
pairs <- Reduce(function(a, b) merge(a, b, by = 'PromptID', sort = FALSE),
                list(model_rows('Qwen','Qwen'), model_rows('Z-Image','ZImage'), model_rows('SDXL','SDXL')))
pairs <- merge(pairs, meta, by = 'PromptID', sort = FALSE)
pairs <- pairs[order(match(pairs$PromptID, ids)), , drop = FALSE]
pairs$QZGap <- pairs$QwenComposite - pairs$ZImageComposite
pairs$QZAdherenceGap <- pairs$QwenAdherence - pairs$ZImageAdherence
pairs$QSDXLGap <- pairs$QwenComposite - pairs$SDXLComposite
pairs$ZSDXLGap <- pairs$ZImageComposite - pairs$SDXLComposite
if (nrow(pairs) != 20L || abs(mean(pairs$QZGap) - 0.1314365) > 0.005 || sum(pairs$QZGap > 0) != 16L) {
  fail('Prompt-matched quality-gap validation failed.')
}

summarise_group <- function(attr, level) {
  z <- pairs[pairs[[attr]] == level, , drop = FALSE]
  data.frame(Attribute = attr, Group = level, N = nrow(z),
             MeanQZ = mean(z$QZGap), MeanQZAdherence = mean(z$QZAdherenceGap),
             QwenAhead = sum(z$QZGap > 0),
             stringsAsFactors = FALSE)
}
group_specs <- list(Context = c('Educational','General','Chinese-cultural'),
                    Content = c('Object-Concept','Other content'),
                    Style = c('Realistic','Stylized'),
                    Complexity = c('Simple','Moderate','High'))
groups <- do.call(rbind, lapply(names(group_specs), function(attr) {
  do.call(rbind, lapply(group_specs[[attr]], function(level) summarise_group(attr, level)))
}))
rownames(groups) <- NULL

compare_groups <- function(attr, a, b) {
  av <- pairs$QZGap[pairs[[attr]] == a]
  bv <- pairs$QZGap[pairs[[attr]] == b]
  estimate <- mean(av) - mean(bv)
  loo <- c(vapply(seq_along(av), function(i) mean(av[-i]) - mean(bv), numeric(1)),
           vapply(seq_along(bv), function(i) mean(av) - mean(bv[-i]), numeric(1)))
  data.frame(Comparison = paste0(attr, ': ', a, ' minus ', b),
             N_A = length(av), N_B = length(bv),
             GapDifference = estimate,
             LOOmin = min(loo), LOOmax = max(loo),
             AllLOOSameSign = all(sign(loo) == sign(estimate)),
             stringsAsFactors = FALSE)
}
contrasts <- do.call(rbind, list(
  compare_groups('Context', 'Educational', 'General'),
  compare_groups('Content', 'Object-Concept', 'Other content'),
  compare_groups('Style', 'Stylized', 'Realistic'),
  compare_groups('Complexity', 'High', 'Moderate'),
  compare_groups('Complexity', 'High', 'Simple')
))

pairs$Context <- factor(pairs$Context, levels = group_specs$Context)
pairs$Content <- factor(pairs$Content, levels = group_specs$Content)
pairs$Style <- factor(pairs$Style, levels = group_specs$Style)
pairs$Complexity <- factor(pairs$Complexity, levels = group_specs$Complexity)
model_formulas <- list(
  'Context + content' = QZGap ~ Context + Content,
  '+ Style' = QZGap ~ Context + Content + Style,
  '+ Complexity' = QZGap ~ Context + Content + Complexity,
  '+ Style + complexity' = QZGap ~ Context + Content + Style + Complexity
)
loocv <- function(fml) {
  full_rank <- qr(stats::model.matrix(fml, data = pairs))$rank
  full_p <- ncol(stats::model.matrix(fml, data = pairs))
  if (full_rank != full_p) return(c(RMSE = NA_real_, MAE = NA_real_, Rank = full_rank, Columns = full_p))
  predictions <- vapply(seq_len(nrow(pairs)), function(i) {
    train <- pairs[-i, , drop = FALSE]
    matrix_i <- stats::model.matrix(fml, data = train)
    if (qr(matrix_i)$rank < ncol(matrix_i)) return(NA_real_)
    fit <- stats::lm(fml, data = train)
    tryCatch(as.numeric(stats::predict(fit, newdata = pairs[i, , drop = FALSE])), error = function(e) NA_real_)
  }, numeric(1))
  if (any(!is.finite(predictions))) return(c(RMSE = NA_real_, MAE = NA_real_, Rank = full_rank, Columns = full_p))
  err <- predictions - pairs$QZGap
  c(RMSE = sqrt(mean(err^2)), MAE = mean(abs(err)), Rank = full_rank, Columns = full_p)
}
cv <- do.call(rbind, lapply(model_formulas, loocv))
cv <- data.frame(Model = names(model_formulas), cv, row.names = NULL, check.names = FALSE)

cross_tab <- function(rowvar, colvar) {
  m <- table(pairs[[rowvar]], pairs[[colvar]])
  lines <- c(mdrow(rowvar, colnames(m)), mdrow(':--', rep('--:', ncol(m))))
  for (i in seq_len(nrow(m))) lines <- c(lines, mdrow(rownames(m)[i], as.integer(m[i, ])))
  lines
}
report <- c(
  '# Task-attribute incremental-value diagnostic',
  '',
  'Twenty Chinese prompts, three local model configurations, and one human-rated generation for each prompt-model combination.',
  'The unit of analysis is the prompt-level Qwen minus Z-Image gap. This is an internal study-design assessment, not a confirmatory test.',
  '',
  '## 1. Design overlap',
  '',
  '**Style by context**', '', cross_tab('Context','Style'), '',
  '**Complexity by context**', '', cross_tab('Context','Complexity'), '',
  '**Style by content**', '', cross_tab('Content','Style'), '',
  '**Complexity by content**', '', cross_tab('Content','Complexity'), '',
  'Groups are overlapping descriptions of the same 20 prompts, not independent experimental replications.',
  '',
  '## 2. Quality-gap profiles', '',
  mdrow('Attribute', 'Group', 'Prompts', 'Qwen−Z composite', 'Qwen−Z adherence', 'Qwen ahead'),
  mdrow(':--', ':--', '--:', '--:', '--:', '--:')
)
for (i in seq_len(nrow(groups))) {
  g <- groups[i, ]
  report <- c(report, mdrow(g$Attribute, g$Group, g$N, signed(g$MeanQZ),
                           signed(g$MeanQZAdherence), paste0(g$QwenAhead, '/', g$N)))
}
report <- c(report, '', '## 3. Differences between task groups', '',
            mdrow('Comparison', 'Gap of gaps', 'LOO min', 'LOO max', 'Sign unchanged'),
            mdrow(':--', '--:', '--:', '--:', ':--:'))
for (i in seq_len(nrow(contrasts))) {
  r <- contrasts[i, ]
  report <- c(report, mdrow(r$Comparison, signed(r$GapDifference), signed(r$LOOmin),
                           signed(r$LOOmax), ifelse(r$AllLOOSameSign, 'Yes', 'No')))
}
report <- c(report, '',
            'LOO removes one prompt from either comparison group. Directional stability under deletion is not independent replication.',
            '', '## 4. Incremental description beyond context and content', '',
            mdrow('Prompt-level linear description', 'LOO RMSE', 'LOO MAE', 'Design rank'),
            mdrow(':--','--:','--:','--:'))
for (i in seq_len(nrow(cv))) {
  r <- cv[i, ]
  report <- c(report, mdrow(r$Model, fmt(r$RMSE, 4L), fmt(r$MAE, 4L),
                           paste0(r$Rank, '/', r$Columns)))
}
report <- c(report, '',
            'LOOCV compares additive descriptions in the same fixed 20-prompt set. It is not external predictive validation or evidence of causal task effects. Non-full-rank fits are marked NA.',
            '', '## 5. Prompt-level data', '',
            mdrow('Prompt', 'Context', 'Content', 'Style', 'Complexity', 'Q−Z composite', 'Q−Z adherence'),
            mdrow(':--', ':--', ':--', ':--', ':--', '--:', '--:'))
for (i in seq_len(nrow(pairs))) {
  x <- pairs[i, ]
  report <- c(report, mdrow(x$PromptID, as.character(x$Context), as.character(x$Content),
                           as.character(x$Style), as.character(x$Complexity),
                           signed(x$QZGap), signed(x$QZAdherenceGap)))
}
report <- c(report, '', '## 6. Interpretation boundary', '',
            '- Task subsets are small and partially overlapping. Style and complexity annotations were not randomized.',
            '- An apparent style or complexity association may be explained by prompt context, content, or an individual prompt.',
            '- Mean differences are descriptive; no confirmatory p-values or claims about general task populations are made.',
            '- This diagnostic does not modify any manuscript figure or table.', '')

out_md <- file.path(out_dir, 'Task_attribute_incremental_diagnostic.md')
out_csv <- file.path(out_dir, 'Task_attribute_prompt_gaps.csv')
writeLines(report, out_md, useBytes = TRUE)
write.csv(pairs, out_csv, row.names = FALSE, fileEncoding = 'UTF-8')
cat('Report: ', normalizePath(out_md, winslash = '/'), '\n', sep = '')
cat('Prompt-level data: ', normalizePath(out_csv, winslash = '/'), '\n', sep = '')
cat('Validated: 20 prompts x 3 Chinese model images; Context, Content, Style and Complexity labels.\n')
