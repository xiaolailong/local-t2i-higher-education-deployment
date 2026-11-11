#!/usr/bin/env Rscript

root <- normalizePath(getwd(), winslash = '/', mustWork = TRUE)
source_file <- file.path(root, 'data', 'source', 'calibration_parameter_summary.csv')
output_file <- file.path(root, 'data', 'paper_results', 'Supplementary_Table_S3_calibration.md')

if (!file.exists(source_file)) {
  stop('Calibration data not found: ', source_file, call. = FALSE)
}

d <- read.csv(source_file, stringsAsFactors = FALSE, check.names = FALSE)
required <- c('Model', 'Resolution', 'Steps',
              'ChineseMean', 'ChineseMeanRank',
              'ChineseAdjusted', 'ChineseAdjustedRank',
              'EnglishMean', 'EnglishMeanRank',
              'EnglishAdjusted', 'EnglishAdjustedRank', 'Selected')
if (!all(required %in% names(d))) {
  stop('Missing calibration columns: ',
       paste(setdiff(required, names(d)), collapse = ', '), call. = FALSE)
}

models <- c('Qwen-Image-2512', 'SDXL-Turbo', 'Z-Image-Turbo')
if (nrow(d) != 27L || anyNA(d[, required]) ||
    !setequal(unique(d$Model), models) ||
    anyDuplicated(d[c('Model', 'Resolution', 'Steps')])) {
  stop('Incomplete or inconsistent calibration grid.', call. = FALSE)
}

score_cols <- c('ChineseMean', 'ChineseAdjusted', 'EnglishMean', 'EnglishAdjusted')
rank_cols <- c('ChineseMeanRank', 'ChineseAdjustedRank',
               'EnglishMeanRank', 'EnglishAdjustedRank')
for (model in models) {
  x <- d[d$Model == model, , drop = FALSE]
  if (nrow(x) != 9L ||
      any(!vapply(rank_cols, function(col) setequal(x[[col]], 1:9), logical(1))) ||
      any(!is.finite(as.matrix(x[, score_cols]))) ||
      any(as.matrix(x[, score_cols]) < 0 | as.matrix(x[, score_cols]) > 1) ||
      !all(x$Selected %in% c(0L, 1L)) ||
      sum(x$Selected) != 1L) {
    stop('Invalid calibration values or ranks for ', model, call. = FALSE)
  }
}

expected <- data.frame(Model = c('Qwen-Image-2512', 'SDXL-Turbo', 'Z-Image-Turbo'),
                       Resolution = c(1024L, 768L, 768L),
                       Steps = c(28L, 4L, 9L),
                       stringsAsFactors = FALSE)
for (i in seq_len(nrow(expected))) {
  x <- d[d$Model == expected$Model[i] & d$Selected == 1L, , drop = FALSE]
  if (nrow(x) != 1L || x$Resolution != expected$Resolution[i] ||
      x$Steps != expected$Steps[i]) {
    stop('Selected calibration setting is inconsistent for ',
         expected$Model[i], call. = FALSE)
  }
}

lines <- c(
  '# Supplementary Table S3. Calibration results for all candidate parameter combinations',
  '',
  '| Resolution / steps | Chinese mean score | Chinese stability-adjusted score | English mean score | English stability-adjusted score |',
  '|:--|--:|--:|--:|--:|'
)

fmt <- function(value, rank) sprintf('%.3f (%d)', value, rank)
for (model in models) {
  lines <- c(lines, sprintf('| **%s** | | | | |', model))
  x <- d[d$Model == model, , drop = FALSE]
  x <- x[order(x$ChineseMeanRank), , drop = FALSE]
  for (i in seq_len(nrow(x))) {
    setting <- sprintf('%d², %d', x$Resolution[i], x$Steps[i])
    if (x$Selected[i] == 1L) setting <- paste0('**', setting, '**')
    lines <- c(lines, paste0('| ', setting,
      ' | ', fmt(x$ChineseMean[i], x$ChineseMeanRank[i]),
      ' | ', fmt(x$ChineseAdjusted[i], x$ChineseAdjustedRank[i]),
      ' | ', fmt(x$EnglishMean[i], x$EnglishMeanRank[i]),
      ' | ', fmt(x$EnglishAdjusted[i], x$EnglishAdjustedRank[i]),
      ' |'))
  }
}
lines <- c(lines, '',
  '**Note.** Values are score (rank). Resolution is given in pixels per side (e.g., 1024² = 1024 × 1024). Stability-adjusted score = mean weighted objective score − 0.1 × standard deviation. Rankings are within each model and prompt-language condition. Bold settings were used for the main evaluation.',
  '')

dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)
writeLines(lines, output_file, useBytes = TRUE)
message('Written: ', output_file)
