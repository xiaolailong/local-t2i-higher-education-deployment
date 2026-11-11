#!/usr/bin/env Rscript
# Chinese-prompt subjective-rating reliability, estimated by REML.
# Model: Score ~ 1 + (1 | ImageKey) + (1 | RaterKey).
# Inputs: data/source/subjective_scores_human120*.xlsx
# Outputs: data/paper_results/Rating_reliability_variance_components.*

options(stringsAsFactors = FALSE)
root <- normalizePath(getwd(), winslash = '/', mustWork = TRUE)
src_dir <- file.path(root, 'data', 'source')
raw_file <- file.path(src_dir, 'subjective_scores_human120.xlsx')
image_file <- file.path(src_dir, 'subjective_scores_human120_image_level.xlsx')
out_dir <- file.path(root, 'data', 'paper_results')
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
out_md <- file.path(out_dir, 'Rating_reliability_variance_components.md')
out_csv <- file.path(out_dir, 'Rating_reliability_variance_components.csv')

fail <- function(...) stop(paste0(...), call. = FALSE)
for (pkg in c('readxl', 'lme4')) {
  if (!requireNamespace(pkg, quietly = TRUE))
    fail('Missing R package ', pkg, '. Install with install.packages("', pkg, '").')
}
for (f in c(raw_file, image_file)) {
  if (!file.exists(f)) fail('Project-local input missing: ', f,
                            '\nCopy latest source workbooks into data/source/ first.')
}
field <- function(d, name, sheet) {
  if (!(name %in% names(d))) fail('Missing column ', name, ' in sheet ', sheet,
                                  '; found: ', paste(names(d), collapse = ', '))
  d[[name]]
}
normalize_id <- function(x) {
  z <- toupper(trimws(as.character(x)))
  good <- grepl('^B0*[0-9]+$', z)
  z[good] <- paste0('B', as.integer(sub('^B', '', z[good])))
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
normalize_lang <- function(x) {
  z <- tolower(trimws(as.character(x)))
  out <- rep(NA_character_, length(z))
  out[z %in% c('chinese', 'cn', 'zh', 'zh-cn', '中文', '中文提示词')] <- 'Chinese'
  out[z %in% c('english', 'en', 'eng', '英文', '英文提示词')] <- 'English'
  out
}
normalize_q <- function(x) {
  z <- toupper(gsub('[[:space:]_.-]+', '', as.character(x)))
  z <- sub('^Q([123])$', '\\1', z)
  z
}
canonical_filename <- function(x) {
  z <- gsub('\\\\', '/', trimws(as.character(x)))
  z <- basename(z)
  z <- sub('^[A-Da-d]\\.[0-9]+\\.', '', z)
  tolower(z)
}
fmt <- function(x, digits = 4L) ifelse(is.finite(x),
  sprintf(paste0('%.', digits, 'f'), x), 'NA')
md_row <- function(...) paste0('| ', paste(as.character(c(...)), collapse = ' | '), ' |')

forms <- c('teacher', 'student_A', 'student_B')
if (!all(forms %in% readxl::excel_sheets(raw_file)))
  fail('Missing expected teacher/student_A/student_B worksheets in raw local data.')

parts <- lapply(forms, function(sh) {
  d <- as.data.frame(readxl::read_excel(raw_file, sheet = sh))
  d <- data.frame(
    SourceForm = sh,
    RaterID = trimws(as.character(field(d, 'respondent_id', sh))),
    PromptID = normalize_id(field(d, 'prompt_id', sh)),
    Model = normalize_model(field(d, 'model', sh)),
    Language = normalize_lang(field(d, 'language', sh)),
    Question = normalize_q(field(d, 'q_no', sh)),
    Score = suppressWarnings(as.numeric(field(d, 'score', sh))),
    ImageName = canonical_filename(field(d, 'image_name', sh)),
    stringsAsFactors = FALSE
  )
  d[d$Language == 'Chinese' & !is.na(d$Language), , drop = FALSE]
})
raw <- do.call(rbind, parts)
if (nrow(raw) != 18450L)
  fail('Expected 18,450 Chinese-prompt score rows; got ', nrow(raw),
       '. Verify data/source contains the intended 191-questionnaire snapshot.')
if (anyNA(raw) || any(!raw$Question %in% c('1','2','3')) ||
    any(!is.finite(raw$Score) | raw$Score < 1 | raw$Score > 5) ||
    any(!raw$PromptID %in% paste0('B', 1:20)) ||
    any(!raw$Model %in% c('Qwen','Z-Image','SDXL')) ||
    any(!nzchar(raw$RaterID)) || any(!nzchar(raw$ImageName)))
  fail('Invalid required image/rater/model/question/score fields in raw workbook.')

raw$RaterKey <- paste(raw$SourceForm, raw$RaterID, sep = '::')
raw$ImageKey <- paste(raw$PromptID, raw$Model, sep = '|')
raw$ResponseImageKey <- paste(raw$RaterKey, raw$ImageKey, sep = '|')
n_by_form <- vapply(forms, function(sh) length(unique(raw$RaterKey[raw$SourceForm == sh])), integer(1))
if (!identical(unname(n_by_form), c(25L, 84L, 82L)))
  fail('Source-form questionnaire counts differ from expected 25/84/82: ',
       paste(n_by_form, collapse='/'))
if (anyDuplicated(paste(raw$ResponseImageKey, raw$Question, sep='|')))
  fail('Duplicate response-image-question key found; cannot compute reliability unchanged.')
if (any(table(raw$ResponseImageKey) != 3L) ||
    length(unique(raw$ResponseImageKey)) != 6150L)
  fail('Expected 6,150 complete response-image score triplets (Q1/Q2/Q3).')
if (length(unique(raw$ImageKey)) != 60L)
  fail('Expected exactly 60 Chinese PromptID x model image keys.')
name_count <- tapply(raw$ImageName, raw$ImageKey, function(v) length(unique(v)))
if (any(name_count != 1L))
  fail('Canonicalized image filenames are not unique for some PromptID x model keys.')

# Recompute the image-level composites and verify against the local 60-image export.
qs <- split(raw, raw$Question)
key1 <- qs[['1']]$ResponseImageKey
ix2 <- match(key1, qs[['2']]$ResponseImageKey)
ix3 <- match(key1, qs[['3']]$ResponseImageKey)
if (anyNA(ix2) || anyNA(ix3) || nrow(qs[['1']]) != 6150L)
  fail('Q1/Q2/Q3 keys do not form 6,150 complete respondent-image triplets.')
w <- qs[['1']][, c('RaterKey','ImageKey','PromptID','Model'), drop=FALSE]
w$Composite <- (qs[['1']]$Score + qs[['2']]$Score[ix2] + qs[['3']]$Score[ix3]) / 3
w$Adherence <- qs[['1']]$Score
means_comp <- tapply(w$Composite, w$ImageKey, mean)
means_adh <- tapply(w$Adherence, w$ImageKey, mean)

img <- as.data.frame(readxl::read_excel(image_file, sheet = 'image_level'))
needed_cols <- c('PromptID', 'ModelShortName', 'Language', 'CompositeScore', 'MeanAdherence')
if (!all(needed_cols %in% names(img)))
  fail('Local image_level sheet missing columns: ', paste(setdiff(needed_cols,names(img)),collapse=', '))
img$PromptID <- normalize_id(img$PromptID)
img$ModelShortName <- normalize_model(img$ModelShortName)
img$Language <- normalize_lang(img$Language)
img <- img[!is.na(img$Language) & img$Language == 'Chinese', , drop=FALSE]
img$ImageKey <- paste(img$PromptID,img$ModelShortName,sep='|')
if (nrow(img) != 60L || anyDuplicated(img$ImageKey) ||
    !setequal(names(means_comp),img$ImageKey))
  fail('Image-level workbook does not contain the expected 60 unique Chinese scored images.')
ix <- match(names(means_comp), img$ImageKey)
d_comp <- max(abs(as.numeric(means_comp) - as.numeric(img$CompositeScore[ix])))
d_adh <- max(abs(as.numeric(means_adh) - as.numeric(img$MeanAdherence[ix])))
if (!is.finite(d_comp) || !is.finite(d_adh) || d_comp > 0.005 || d_adh > 0.005)
  fail('Raw and image-level means disagree: composite=', d_comp, '; adherence=', d_adh)

calc_vc <- function(d, measure, ref_k=30L) {
  image_raters <- tapply(d$RaterKey, d$ImageKey, function(x) length(unique(x)))
  k <- mean(image_raters)
  mod <- lme4::lmer(
    Score ~ 1 + (1 | ImageKey) + (1 | RaterKey), data=d, REML=TRUE,
    control=lme4::lmerControl(optimizer='bobyqa', optCtrl=list(maxfun=2e5))
  )
  vc <- as.data.frame(lme4::VarCorr(mod))
  vi <- vc$vcov[match('ImageKey', vc$grp)]
  vr <- vc$vcov[match('RaterKey', vc$grp)]
  ve <- sigma(mod)^2
  if (any(!is.finite(c(vi, vr, ve))) || any(c(vi, vr, ve) < 0))
    fail('Variance-component fit failed for ', measure)
  single <- vi/(vi+vr+ve)
  avg <- function(k0) vi/(vi+(vr+ve)/k0)
  data.frame(
    Dimension=measure, N_Images=length(image_raters),
    N_Raters=length(unique(d$RaterKey)), N_Scores=nrow(d),
    RatersMin=min(image_raters), RatersMax=max(image_raters),
    RatersMean=k, VarImage=vi, VarRater=vr, VarResidual=ve,
    ReliabilitySingle=single, ReliabilityAverageK=avg(k),
    ReliabilityAverageMinK=avg(min(image_raters)),
    ReliabilityAverageRef30=avg(ref_k),
    SingularFit=lme4::isSingular(mod, tol=1e-4),
    stringsAsFactors=FALSE
  )
}

res <- do.call(rbind, c(
  lapply(c('1','2','3'), function(q) {
    dq <- raw[raw$Question == q, c('ImageKey','RaterKey','Score'),drop=FALSE]
    calc_vc(dq, c('Adherence','Clarity','Aesthetics')[match(q,c('1','2','3'))])
  }),
  list(calc_vc(transform(w[,c('ImageKey','RaterKey','Composite')],
                         Score=Composite), 'Composite'))
))
write.csv(res,out_csv,row.names=FALSE,fileEncoding='UTF-8')

lines <- c(
 '# Chinese-prompt subjective rating reliability: variance-components method',
 '',
 'Source: project-local `data/source/` snapshots, Chinese-prompt images only.',
 'All questionnaires are in Chinese; prompt language is the generation condition.',
 'A crossed-random-intercept model is estimated with REML: `Score ~ 1 + (1|ImageKey) + (1|RaterKey)`.',
 'The Composite row extends the method to the average of a respondent\'s three Q1–Q3 scores.',
 '',
 '## Data checks',
 paste0('- Chinese-prompt questionnaires: **',sum(n_by_form),'** (teacher=',n_by_form[1],', student_A=',n_by_form[2],', student_B=',n_by_form[3],').'),
 paste0('- Scored images: **60**; Q1–Q3 rows: **',nrow(raw),'**; complete rater-image triplets: **',nrow(w),'**.'),
 paste0('- Raw-to-image-level mean discrepancies: composite **',fmt(d_comp,6),'**, adherence **',fmt(d_adh,6),'**.'),
 '',
 '## Variance-component reliability estimates',
 '',
 '| Dimension | Images | Mean raters/image | Single-rater reliability | Mean-score reliability at observed k | Mean-score reliability at min k | Fit singular? |',
 '| :-- | --: | --: | --: | --: | --: | :-- |'
)
for (i in seq_len(nrow(res))) {
  x <- res[i,]
  lines <- c(lines, md_row(x$Dimension, x$N_Images, fmt(x$RatersMean,2),
                           fmt(x$ReliabilitySingle),fmt(x$ReliabilityAverageK),
                           fmt(x$ReliabilityAverageMinK),
                           ifelse(x$SingularFit,'Yes','No')))
}
lines <- c(lines, '',
 '## Interpretation', '',
 '- The reliability of image-level mean scores is a variance-component estimate with mean rater count k, NOT evidence that individual raters agree closely.',
 '- The image variance can be enlarged by substantial differences between model configurations. Use the existing per-model split-half and teacher/student checks when interpreting pooled estimates.',
 '- Rater severity and leniency are modeled with respondent-specific random intercepts; rater IDs are scoped by questionnaire source form.',
 '- A singular fit means one or more random-effect variances are estimated at or near zero; inspect the CSV variance components before reporting.',
 '- There are only 20 rated prompts per model and one rated generation per prompt/model condition; these reliability estimates do not add independent prompt or generation replicates.',
 '- The calculation is restricted to the same Chinese-prompt analysis sample as the current Figure 2–5 and Table 2. Do not copy the 120-image reliability coefficients from a different analysis scope.',
 '',
 paste0('**Machine-readable variance components:** `data/paper_results/',basename(out_csv),'`.')
)
writeLines(lines,out_md,useBytes=TRUE)
cat('OK: ',out_md,'\n',sep='')
cat('OK: ',out_csv,'\n',sep='')
