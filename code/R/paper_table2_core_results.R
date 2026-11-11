#!/usr/bin/env Rscript
# Table 2: prompt-matched model comparisons and resource costs.
# Input: data/source/subjective_scores_human120_image_level.xlsx
# Unit of model comparison: PromptID (20 matched prompts).
# Output: data/paper_results/Table2_core_results.md

options(stringsAsFactors = FALSE)

REPO_ROOT <- normalizePath(getwd(), winslash = '/', mustWork = TRUE)
SUBJECTIVE_XLSX <- file.path(REPO_ROOT, 'data', 'source', 'subjective_scores_human120_image_level.xlsx')
SHEET <- 'image_level'
ADHERENCE_CUTOFF <- 3.5

result_dir <- file.path(REPO_ROOT, 'data', 'paper_results')
dir.create(result_dir, recursive = TRUE, showWarnings = FALSE)

fail <- function(...) stop(paste0(...), call. = FALSE)
if (!requireNamespace('readxl', quietly = TRUE)) {
  fail('Missing R package readxl; install.packages("readxl").')
}
normal_col <- function(x) tolower(gsub('[^a-zA-Z0-9]', '', x))
find_col <- function(d, choices) {
  nm <- normal_col(names(d))
  for (choice in normal_col(choices)) {
    hit <- which(nm == choice)
    if (length(hit)) return(names(d)[hit[1]])
  }
  NULL
}
normalize_id <- function(x) {
  z <- toupper(gsub('[[:space:]]', '', trimws(as.character(x))))
  b <- grepl('^B0*[0-9]+$', z)
  z[b] <- paste0('B', as.integer(sub('^B', '', z[b])))
  z
}
all_prompt_ids <- paste0('B', 1:20)
get_prompt_col <- function(d) {
  col <- find_col(d, c('PromptID','Prompt_ID','PromptCode','PromptNo','PromptNumber'))
  if (!is.null(col)) return(col)
  hit <- vapply(d, function(x) sum(normalize_id(x) %in% all_prompt_ids, na.rm=TRUE), integer(1))
  good <- which(hit >= 15L)
  if (length(good) == 1L) return(names(d)[good])
  NULL
}
model_name <- function(x) {
  z <- tolower(trimws(as.character(x)))
  out <- rep(NA_character_, length(z))
  out[grepl('qwen', z)] <- 'Qwen'
  out[grepl('z[-_ ]?image|zimage', z)] <- 'Z-Image'
  out[grepl('sdxl|stable.diffusion.xl', z)] <- 'SDXL'
  out
}
language_code <- function(x) {
  z <- tolower(trimws(as.character(x)))
  z <- gsub('[[:space:]_]+', '', z)
  z <- gsub('[\uFF08\uFF09()]', '', z)
  out <- rep(NA_character_, length(z))
  chinese <- c('chinese','ch','cn','zh','zh-cn','zh-hans','zhcn',
               'chineselanguage','chineseprompts','chineseprompt',
               '中文','中文提示词','汉语','汉语提示词')
  english <- c('english','en','eng','en-us','en-gb','enus','engb',
               'englishlanguage','englishprompts','englishprompt',
               '英文','英文提示词','英语','英语提示词')
  out[z %in% chinese] <- 'Chinese'
  out[z %in% english] <- 'English'
  out
}
select_chinese_rows <- function(d, prompt_col) {
  is_b <- normalize_id(d[[prompt_col]]) %in% all_prompt_ids
  if (sum(is_b) == 60L) {
    message('Data filter: exactly 60 B1-B20 rows in source.')
    return(which(is_b))
  }
  if (sum(is_b) != 120L) {
    fail('Expected 60 Chinese or 120 bilingual B1-B20 image records; found ',
         sum(is_b), '. Actual source columns: ', paste(names(d), collapse=', '))
  }
  nm <- normal_col(names(d))
  preferred <- which(grepl('language|^lang|lang$|locale', nm))
  candidates <- setdiff(c(preferred, setdiff(seq_along(d), preferred)),
                        match(prompt_col, names(d)))
  selections <- list()
  for (j in candidates) {
    z <- language_code(d[[j]])
    if (sum(z[is_b] == 'Chinese', na.rm=TRUE) == 60L &&
        sum(z[is_b] == 'English', na.rm=TRUE) == 60L &&
        all(!is.na(z[is_b]))) {
      selections[[names(d)[j]]] <- which(is_b & !is.na(z) & z == 'Chinese')
    }
  }
  if (!length(selections)) {
    fail('Could not safely identify the 60 Chinese rated images in 120 rows. ',
         'Columns: ', paste(names(d),collapse=', '),
         '\nWill not select rows by order or silently deduplicate.')
  }
  vals <- lapply(selections, sort)
  if (!all(vapply(vals, function(z) identical(z, vals[[1]]), logical(1)))) {
    fail('Conflicting language labels in original image_level sheet.')
  }
  message('Data filter: latest Chinese 60/120 rows using ', names(selections)[1])
  vals[[1]]
}

# --- 1. Read exact 60-image latest Chinese dataset ---
if (!file.exists(SUBJECTIVE_XLSX)) fail('Workbook not found: ', SUBJECTIVE_XLSX)
if (!(SHEET %in% readxl::excel_sheets(SUBJECTIVE_XLSX))) {
  fail('Missing image_level worksheet in: ', SUBJECTIVE_XLSX)
}
raw <- as.data.frame(readxl::read_excel(SUBJECTIVE_XLSX, sheet=SHEET))
col_id <- get_prompt_col(raw)
col_model <- find_col(raw, c('ModelShortName','ModelShort','ModelName','Model'))
col_composite <- find_col(raw, c('CompositeScore','MeanCompositeScore','Composite','CompositeMean'))
col_adherence <- find_col(raw, c('MeanAdherence','AdherenceMean','AvgAdherence','Adherence'))
needed <- list(PromptID=col_id, ModelShortName=col_model,
               CompositeScore=col_composite, MeanAdherence=col_adherence)
if (any(vapply(needed, is.null, logical(1)))) {
  fail('Missing required column(s): ',
       paste(names(needed)[vapply(needed, is.null, logical(1))], collapse=', '),
       '\nFound: ',paste(names(raw),collapse=', '))
}
raw <- raw[select_chinese_rows(raw,col_id), , drop=FALSE]
scored <- data.frame(
  PromptID=normalize_id(raw[[col_id]]),
  Model=model_name(raw[[col_model]]),
  CompositeScore=suppressWarnings(as.numeric(raw[[col_composite]])),
  MeanAdherence=suppressWarnings(as.numeric(raw[[col_adherence]]))
)
scored <- scored[scored$PromptID %in% all_prompt_ids, , drop=FALSE]
if (nrow(scored) != 60L || anyNA(scored) ||
    anyDuplicated(scored[c('PromptID','Model')]) ||
    !setequal(scored$PromptID,all_prompt_ids)) {
  fail('The latest Chinese data must have 60 unique B1-B20 x model image rows.')
}
for (field in c('CompositeScore','MeanAdherence')) {
  if (any(!is.finite(scored[[field]]) | scored[[field]] < 1 | scored[[field]] > 5)) {
    fail('Invalid 1–5 scores in ',field)
  }
}
model_order <- c('Qwen','Z-Image','SDXL')
for (m in model_order) {
  if (sum(scored$Model==m) != 20L) fail('Expected exactly 20 Chinese images for ',m)
}
# Anchors only check that the SAME recent Chinese scores are being used;
# all printed values below are freshly calculated, never copied from anchors.
expected_means <- c(Qwen=4.1344, `Z-Image`=4.0030, SDXL=2.9123)
expected_below <- c(Qwen=0L, `Z-Image`=1L, SDXL=18L)
for (m in model_order) {
  x <- scored[scored$Model==m, ]
  if (abs(mean(x$CompositeScore)-expected_means[m]) > 0.005 ||
      sum(x$MeanAdherence < ADHERENCE_CUTOFF) != expected_below[m]) {
    fail('Data version mismatch for ',m,'; observed Composite=',
         sprintf('%.4f',mean(x$CompositeScore)),
         ' and below-3.5=',sum(x$MeanAdherence < ADHERENCE_CUTOFF),
         '; latest Chinese research anchors are Composite~',expected_means[m],
         ' and below-3.5=',expected_below[m])
  }
}
scored <- scored[order(match(scored$PromptID,all_prompt_ids),
                       match(scored$Model,model_order)), ]

# --- 2. Measured cost is frozen from research master section 4.3 ---
resource <- data.frame(
  Model=model_order,
  Configuration=c('Qwen-Image-2512','Z-Image-Turbo','SDXL-Turbo'),
  MeanLatencySeconds=c(167.0,8.6,1.0),
  MeanVRAM_MB=c(14172.0,14274.8,8452.0),
  ResourceSource=rep('research_master_20261109.md section 4.3, verified configuration averages; not recalculated from raw logs',3),
  stringsAsFactors=FALSE
)

# --- 3. Panel A: three model descriptive summaries ---
desc <- do.call(rbind, lapply(seq_len(nrow(resource)), function(i) {
  m <- resource$Model[i]
  v <- scored[scored$Model==m, ,drop=FALSE]
  data.frame(
    Model=m, Configuration=resource$Configuration[i], PromptBlocks=nrow(v),
    CompositeMean=mean(v$CompositeScore),
    CompositeSD=stats::sd(v$CompositeScore),
    AdherenceMean=mean(v$MeanAdherence),
    AdherenceSD=stats::sd(v$MeanAdherence),
    AdherenceFloor=ADHERENCE_CUTOFF,
    AdherencePass=sum(v$MeanAdherence >= ADHERENCE_CUTOFF),
    AdherencePassDenominator=nrow(v),
    AdherencePassRate=mean(v$MeanAdherence >= ADHERENCE_CUTOFF),
    MeanLatencySeconds=resource$MeanLatencySeconds[i],
    MeanVRAM_MB=resource$MeanVRAM_MB[i],
    stringsAsFactors=FALSE
  )
}))

# --- 4. Panel B: paired, prompt-blocked inference ---
score_matrix <- sapply(model_order, function(m) {
  d <- scored[scored$Model==m, c('PromptID','CompositeScore')]
  d$CompositeScore[match(all_prompt_ids,d$PromptID)]
})
rownames(score_matrix) <- all_prompt_ids
if (!all(dim(score_matrix)==c(20L,3L)) || anyNA(score_matrix)) {
  fail('Unable to form a complete 20x3 PromptID matched score matrix.')
}
friedman <- stats::friedman.test(score_matrix)
chi2 <- unname(as.numeric(friedman$statistic))
df <- unname(as.integer(friedman$parameter))
kw <- chi2 / (nrow(score_matrix)*(ncol(score_matrix)-1))
if (abs(chi2 - 33.600) > 0.20 || abs(kw - 0.840) > 0.01) {
  fail('Friedman score ordering differs materially from verified latest Chinese ',
       'result: observed chi-square=',sprintf('%.4f',chi2),
       ', W=',sprintf('%.4f',kw),'; expected about 33.600 and 0.840.')
}
pairs <- list(c('Qwen','Z-Image'),c('Qwen','SDXL'),c('Z-Image','SDXL'))
paired <- do.call(rbind, lapply(pairs, function(pair) {
  diff <- score_matrix[,pair[1]]-score_matrix[,pair[2]]
  test <- stats::t.test(diff,mu=0,conf.level=.95)
  data.frame(
    Comparison=paste0(pair[1],' - ',pair[2]), Model1=pair[1],Model2=pair[2],
    NPrompts=length(diff), MeanPairedDifference=mean(diff),
    SDPairedDifference=stats::sd(diff),
    CI95Lower=unname(test$conf.int[1]),CI95Upper=unname(test$conf.int[2]),
    TStatistic=unname(as.numeric(test$statistic)),Df=unname(as.integer(test$parameter)),
    PRaw=as.numeric(test$p.value),
    CohenDz=mean(diff)/stats::sd(diff),
    PositivePrompts=sum(diff>0),NegativePrompts=sum(diff<0),EqualPrompts=sum(diff==0),
    stringsAsFactors=FALSE
  )
}))
paired$PBH <- stats::p.adjust(paired$PRaw,method='BH')
expected_gap <- c(.1314,1.2221,1.0906)
expected_ci <- rbind(c(.0224,.2405),c(1.0269,1.4172),c(.8774,1.3039))
if (any(abs(paired$MeanPairedDifference-expected_gap) > .005) ||
    any(abs(as.matrix(paired[c('CI95Lower','CI95Upper')])-expected_ci) > .015) ||
    paired$PositivePrompts[1] != 16L || paired$NegativePrompts[1] != 4L) {
  fail('Paired means/intervals or Qwen-Z sign counts disagree with latest Chinese audit anchors.')
}
if (abs(paired$PBH[1]-.02073) > .002) {
  fail('BH-adjusted Qwen-Z paired p-value does not match latest Chinese result (~0.02073): got ',
       sprintf('%.6f',paired$PBH[1]))
}

# --- 5. Format verified results for the single Markdown manuscript table ---
# Keep detailed computed objects in memory; do not produce redundant CSV,
# TeX or PDF/PNG artifacts. For any later LaTeX conversion, use the MD file.
format_p <- function(p) ifelse(p<.001,'<0.001',sprintf('%.4f',p))
fmt <- function(x,d=3) sprintf(paste0('%.',d,'f'),x)
comp_col <- sprintf('%.3f (%.3f)',desc$CompositeMean,desc$CompositeSD)
adh_col <- sprintf('%.3f',desc$AdherenceMean)
pass_col <- sprintf('%d/%d',desc$AdherencePass,desc$AdherencePassDenominator)
lat_col <- sprintf('%.1f',desc$MeanLatencySeconds)
vram_col <- format(round(desc$MeanVRAM_MB),big.mark=',',trim=TRUE,scientific=FALSE)
diff_col <- sprintf('%+.3f [%.3f, %.3f]',paired$MeanPairedDifference,
                    paired$CI95Lower,paired$CI95Upper)
p_col <- vapply(paired$PBH,format_p,character(1))
dz_col <- sprintf('%.3f',paired$CohenDz)
sign_col <- sprintf('%d/%d',paired$PositivePrompts,paired$NPrompts)

# --- 6. Primary manuscript output: Markdown only ---
md <- c(
  '# Table 2. Prompt-matched human ratings and measured inference cost',
  '',
  '### Panel A. Image-level quality and cost of tested configurations',
  '',
  '| Tested configuration | Composite mean (SD) | Mean adherence | Adherence >= 3.5 | Time (s) | VRAM (MB) |',
  '|:--|--:|--:|--:|--:|--:|',
  vapply(seq_len(nrow(desc)), function(i) paste0('| ',desc$Configuration[i],
    ' | ',comp_col[i],' | ',adh_col[i],' | ',pass_col[i],
    ' | ',lat_col[i],' | ',vram_col[i],' |'),character(1)),
  '',
  '### Panel B. Within-PromptID paired comparisons of CompositeScore',
  '',
  '| Contrast (first minus second) | Mean difference [95% CI] | BH-adjusted p | Paired dz | Positive prompts |',
  '|:--|--:|--:|--:|--:|',
  vapply(seq_len(nrow(paired)), function(i) paste0('| ',paired$Comparison[i],
    ' | ',diff_col[i],' | ',p_col[i],' | ',dz_col[i],' | ',sign_col[i],' |'),character(1)),
  '',
  paste0('**Overall:** Friedman chi-square(',df,') = ',fmt(chi2),
         '; p ',if (friedman$p.value < .001) '< 0.001' else paste0('= ',format_p(friedman$p.value)),
         '; Kendall W = ',fmt(kw),'.'),
  '',
  '**Note.** Scores range from 1 to 5; SD is calculated across 20 rated images per model.',
  'Adherence >= 3.5 reports the number of rated images meeting the cutoff (out of 20).',
  'Comparisons are matched by PromptID (n = 20); the 95% CIs are unadjusted and the three p-values are BH-adjusted.',
  'Paired dz is the mean within-PromptID difference divided by its standard deviation.',
  'Time and VRAM are mean measurements for the tested configurations.'
)
writeLines(md,file.path(result_dir,'Table2_core_results.md'),useBytes=TRUE)

message('\nTable 2 Markdown generated: ',
        normalizePath(file.path(result_dir, 'Table2_core_results.md'),
                      winslash='/', mustWork=TRUE))
message('  Source: latest 191 Chinese questionnaires -> 60 rated images -> 20 matched PromptID blocks.')
message('  Time/VRAM: verified configuration averages from the authoritative research MD.')
message('  No PDF, PNG, LaTeX or CSV output has been generated by this script.')
