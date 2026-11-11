#!/usr/bin/env Rscript
# Exploratory prompt-language sensitivity and decision-screen checks.
# Inputs: data/source/subjective_scores_human120*.xlsx,
# data/source/prompts.xlsx; output: data/paper_results/.
# Language groups contain different respondents.

options(stringsAsFactors = FALSE)
ROOT <- normalizePath(getwd(), winslash = '/', mustWork = TRUE)
SCORES_XLSX <- file.path(ROOT, 'data', 'source', 'subjective_scores_human120_image_level.xlsx')
PROMPTS_XLSX <- file.path(ROOT, 'data', 'source', 'prompts.xlsx')
FIG3_META <- file.path(ROOT, 'data', 'paper_results', 'Fig3_task_model_prompt_scores.csv')
OUTPUT_MD <- file.path(ROOT, 'data', 'paper_results', 'Bilingual_robustness_diagnostic.md')
RAW_RATINGS_XLSX <- file.path(ROOT, 'data', 'source', 'subjective_scores_human120.xlsx')

# Exploratory selection screens; parameters, NOT validated usage thresholds.
COMPOSITE_FLOORS <- c(3.5, 4.0)
COVERAGE_FLOOR <- 0.80
IMAGE_ADHERENCE_FLOOR <- 3.5
LATENCY_SEC <- c(Qwen = 167.0, 'Z-Image' = 8.6, SDXL = 1.0)

fail <- function(...) stop(paste0(...), call. = FALSE)
if (!requireNamespace('readxl', quietly = TRUE)) {
  fail('Please install the required package: install.packages("readxl")')
}
if (!file.exists(SCORES_XLSX)) fail('Missing latest 120-image workbook: ', SCORES_XLSX)
if (!('image_level' %in% readxl::excel_sheets(SCORES_XLSX))) {
  fail('Missing image_level worksheet in ', SCORES_XLSX)
}

norm_col <- function(x) tolower(gsub('[^A-Za-z0-9]', '', x))
find_col <- function(d, candidates) {
  nm <- norm_col(names(d))
  for (cand in norm_col(candidates)) {
    ii <- which(nm == cand)
    if (length(ii)) return(names(d)[ii[1]])
  }
  NULL
}
normalize_id <- function(x) {
  z <- toupper(gsub('[[:space:]]', '', trimws(as.character(x))))
  matches <- grepl('^B0*[0-9]+$', z)
  z[matches] <- paste0('B', as.integer(sub('^B', '', z[matches])))
  z
}
PROMPTS <- paste0('B', 1:20)
model_label <- function(x) {
  z <- tolower(trimws(as.character(x)))
  a <- grepl('qwen', z)
  b <- grepl('z[-_ ]?image|zimage', z)
  c <- grepl('sdxl|stable.diffusion.xl', z)
  n <- as.integer(a) + as.integer(b) + as.integer(c)
  out <- rep(NA_character_, length(z))
  out[a & n == 1] <- 'Qwen'
  out[b & n == 1] <- 'Z-Image'
  out[c & n == 1] <- 'SDXL'
  out
}
language_label <- function(x) {
  z <- tolower(gsub('[[:space:]_]+', '', trimws(as.character(x))))
  z <- gsub('[\uFF08\uFF09()]', '', z)
  out <- rep(NA_character_, length(z))
  cn <- c('chinese','ch','cn','zh','zh-cn','zh-hans','zhcn',
          'chineselanguage','chineseprompts','chineseprompt',
          '中文','中文提示词','汉语','汉语提示词')
  en <- c('english','en','eng','en-us','en-gb','enus','engb',
          'englishlanguage','englishprompts','englishprompt',
          '英文','英文提示词','英语','英语提示词')
  out[z %in% cn] <- 'Chinese'
  out[z %in% en] <- 'English'
  out
}
prompt_col <- function(d) {
  fixed <- find_col(d, c('PromptID','Prompt_ID','PromptCode','PromptNo','PromptNumber'))
  if (!is.null(fixed)) return(fixed)
  sums <- vapply(d, function(x) sum(normalize_id(x) %in% PROMPTS, na.rm=TRUE), integer(1))
  candidate <- which(sums >= 20L)
  if (length(candidate) == 1L) return(names(d)[candidate])
  NULL
}

# The image_level workbook is the shared latest image-level dataset for BOTH
# languages. Do not infer the language from PromptID or row order.
raw <- as.data.frame(readxl::read_excel(SCORES_XLSX, sheet='image_level'))
idc <- prompt_col(raw)
mc <- find_col(raw, c('ModelShortName','ModelShort','ModelName','Model'))
sc <- find_col(raw, c('CompositeScore','MeanCompositeScore','Composite','CompositeMean'))
ac <- find_col(raw, c('MeanAdherence','AdherenceMean','AvgAdherence','Adherence'))
if (any(vapply(list(idc,mc,sc,ac), is.null, logical(1)))) {
  fail('Missing PromptID / ModelShortName / CompositeScore / MeanAdherence. Columns: ',
       paste(names(raw), collapse=', '))
}
rows <- which(normalize_id(raw[[idc]]) %in% PROMPTS)
if (length(rows) != 120L) {
  fail('Expected 120 B1–B20 rated-image rows for two prompt languages; found ', length(rows),
       '. Never reuse the earlier 60-row Chinese-only subset for this analysis.')
}
preferred <- which(grepl('language|^lang|lang$|locale', norm_col(names(raw))))
candidates <- setdiff(c(preferred, setdiff(seq_along(raw), preferred)),
                      match(idc, names(raw)))
language_matches <- list()
for (j in candidates) {
  v <- language_label(raw[[j]])
  if (sum(v[rows] == 'Chinese', na.rm=TRUE) == 60L &&
      sum(v[rows] == 'English', na.rm=TRUE) == 60L &&
      all(!is.na(v[rows]))) language_matches[[names(raw)[j]]] <- v[rows]
}
if (!length(language_matches)) {
  fail('Cannot identify 60 Chinese-prompt + 60 English-prompt images unambiguously. Fields: ',
       paste(names(raw), collapse=', '))
}
if (!all(vapply(language_matches, function(z) identical(z, language_matches[[1]]), logical(1)))) {
  fail('Candidate prompt-language fields disagree; check workbook rather than inferring.')
}
language_source <- names(language_matches)[1]
d <- data.frame(PromptID=normalize_id(raw[[idc]][rows]),
                Model=model_label(raw[[mc]][rows]),
                Language=language_matches[[1]],
                Composite=suppressWarnings(as.numeric(raw[[sc]][rows])),
                Adherence=suppressWarnings(as.numeric(raw[[ac]][rows])),
                stringsAsFactors=FALSE)
if (anyNA(d) || nrow(d) != 120L || anyDuplicated(d[c('PromptID','Model','Language')]) ||
    !setequal(d$PromptID, PROMPTS) ||
    any(!is.finite(d$Composite) | d$Composite < 1 | d$Composite > 5) ||
    any(!is.finite(d$Adherence) | d$Adherence < 1 | d$Adherence > 5)) {
  fail('Image-level data failed 120 unique prompt×model×language / [1,5] checks.')
}
for (lang in c('Chinese','English')) for (model in c('Qwen','Z-Image','SDXL')) {
  if (sum(d$Language==lang & d$Model==model) != 20L) {
    fail('Expected 20 rated images for ', lang, '/', model)
  }
}
# Guard against silently reading the old Chinese snapshot; the English
# records are NOT version-anchored because their latest values are unknown.
chinese_anchors <- c(Qwen=4.1344, 'Z-Image'=4.0030, SDXL=2.9123)
for (model in names(chinese_anchors)) {
  obs <- mean(d$Composite[d$Language=='Chinese' & d$Model==model])
  if (abs(obs - chinese_anchors[model]) > 0.005) {
    fail('Chinese latest-version check failed for ', model, ': observed ',
         round(obs,4), '; expected approximately ', chinese_anchors[model],
         '. Do not mix old269 with new391.')
  }
}

# Recover the ORIGINAL B1–B20 taxonomy without guessing group memberships.
clean_tag <- function(x) {
  z <- tolower(trimws(as.character(x)))
  gsub('[[:space:]_./–—-]+', '', z)
}
context_tag <- function(x) {
  z <- clean_tag(x)
  out <- rep(NA_character_, length(z))
  out[grepl('educat|teaching|教育|教学',z)] <- 'Educational'
  out[grepl('general|通用|一般',z)] <- 'General'
  out[grepl('chinesecultur|culturechinese|culturalchinese|中国文化|中华文化|中国传统',z)] <- 'Chinese-cultural'
  out
}
content_tag <- function(x, allow_other=FALSE) {
  z <- clean_tag(x)
  out <- rep(NA_character_, length(z))
  is_object <- grepl('objectconcept|conceptobject|object|concept|物体|概念',z)
  out[is_object] <- 'Object–Concept'
  if (allow_other) {
    valid <- !is.na(z) & nzchar(z) & !(z %in% c('na','nan','null'))
    out[valid & !is_object] <- 'Other content'
  } else {
    out[grepl('othercontent|nonobject|其他内容|其它内容',z)] <- 'Other content'
  }
  out
}
style_tag <- function(x) {
  z <- clean_tag(x)
  out <- rep(NA_character_,length(z))
  out[grepl('realistic|photoreal|写实|真实风格',z)] <- 'Realistic'
  out[grepl('stylized|stylised|艺术化|插画|风格化',z)] <- 'Stylized'
  out
}
complexity_tag <- function(x) {
  z <- clean_tag(x)
  out <- rep(NA_character_,length(z))
  out[grepl('^simple$|简单|简易',z)] <- 'Simple'
  out[grepl('^moderate$|medium|中等|适中',z)] <- 'Moderate'
  out[grepl('^high$|complexityhigh|高复杂|复杂度高',z)] <- 'High'
  out
}
validate_meta <- function(z, require_full=FALSE) {
  if (is.null(z) || nrow(z)<20L) return(NULL)
  z <- unique(z)
  if (nrow(z)!=20L || anyDuplicated(z$PromptID) || !setequal(z$PromptID,PROMPTS)) return(NULL)
  if (anyNA(z[c('PromptID','Context','Content')])) return(NULL)
  check <- function(x, levels, counts) {
    identical(as.integer(table(factor(x, levels=levels))), as.integer(counts))
  }
  if (!check(z$Context,c('Educational','General','Chinese-cultural'),c(5,10,5)) ||
      !check(z$Content,c('Object–Concept','Other content'),c(6,14)) ||
      sum(z$Context=='Educational' & z$Content=='Object–Concept')!=3L) return(NULL)
  if (require_full || all(!is.na(z$Style))) {
    if (anyNA(z$Style) || !check(z$Style,c('Realistic','Stylized'),c(10,10))) return(NULL)
  }
  if (require_full || all(!is.na(z$Complexity))) {
    if (anyNA(z$Complexity) || !check(z$Complexity,c('Simple','Moderate','High'),c(4,12,4))) return(NULL)
  }
  z[order(as.integer(sub('B','',z$PromptID))), , drop=FALSE]
}
meta_from_prompts <- function(path) {
  if (!file.exists(path)) return(NULL)
  for (sheet in readxl::excel_sheets(path)) {
    x <- tryCatch(as.data.frame(readxl::read_excel(path, sheet=sheet)), error=function(e) NULL)
    if (is.null(x)) next
    i <- prompt_col(x)
    cx <- find_col(x,c('Context','ContextLabel','ContextCategory','ContextGroup','TaskContext','PromptContext','Scenario'))
    ct <- find_col(x,c('Content','ContentLabel','ContentCategory','ContentGroup','ContentType','PromptContent'))
    st <- find_col(x,c('Style','StyleLabel','StyleLevel','VisualStyle','PromptStyle'))
    cp <- find_col(x,c('Complexity','ComplexityLevel','PromptComplexity','SemanticComplexity'))
    if (is.null(i) || is.null(cx) || is.null(ct)) next
    ids <- normalize_id(x[[i]])
    keep <- which(ids %in% PROMPTS)
    if (length(keep)<20L) next
    m <- data.frame(PromptID=ids[keep], Context=context_tag(x[[cx]][keep]),
                    Content=content_tag(x[[ct]][keep], allow_other=TRUE),
                    Style=if (!is.null(st)) style_tag(x[[st]][keep]) else rep(NA_character_,length(keep)),
                    Complexity=if (!is.null(cp)) complexity_tag(x[[cp]][keep]) else rep(NA_character_,length(keep)))
    # A paired-prompt workbook may contain both Chinese- and English-text rows
    # for each B ID. Collapse only genuinely identical annotations.
    m <- unique(m)
    v <- validate_meta(m)
    if (!is.null(v)) {
      attr(v,'source') <- paste0('prompts.xlsx, sheet ',sheet)
      return(v)
    }
  }
  NULL
}
meta_from_fig3 <- function(path) {
  if (!file.exists(path)) return(NULL)
  x <- tryCatch(read.csv(path,check.names=FALSE,stringsAsFactors=FALSE,fileEncoding='UTF-8-BOM'),error=function(e) NULL)
  if (is.null(x)) return(NULL)
  i <- prompt_col(x)
  if (is.null(i)) return(NULL)
  x$.__ID <- normalize_id(x[[i]])
  x <- x[x$.__ID %in% PROMPTS,,drop=FALSE]
  if (nrow(x)<20L) return(NULL)
  field <- names(x)[grepl('context|content|group|task|category|class|type|style|complex',norm_col(names(x)))]
  field <- setdiff(field, c(i,'.__ID'))
  if (!length(field)) return(NULL)
  m <- data.frame(PromptID=PROMPTS,Context=NA_character_,Content=NA_character_,
                  Style=NA_character_,Complexity=NA_character_)
  for (k in seq_along(PROMPTS)) {
    vals <- unlist(x[x$.__ID==PROMPTS[k],field,drop=FALSE],use.names=FALSE)
    for (nm in c('Context','Content','Style','Complexity')) {
      found <- switch(nm,Context=context_tag(vals),Content=content_tag(vals),
                      Style=style_tag(vals),Complexity=complexity_tag(vals))
      found <- unique(stats::na.omit(found))
      if (length(found)==1L) m[[nm]][k] <- found
    }
  }
  v <- validate_meta(m)
  if (!is.null(v)) attr(v,'source') <- 'Fig3_task_model_prompt_scores.csv (labels only)'
  v
}
meta <- meta_from_prompts(PROMPTS_XLSX)
if (is.null(meta)) meta <- meta_from_fig3(FIG3_META)
if (is.null(meta)) {
  fail('Cannot validate B1–B20 Context × Content membership from prompts.xlsx or finalized Fig3 CSV. No groups were guessed.')
}
meta_source <- attr(meta,'source')
# Optional attributes are included only when COMPLETE and group sizes match.
if (anyNA(meta$Style) || !identical(as.integer(table(factor(meta$Style,levels=c('Realistic','Stylized')))),c(10L,10L))) meta$Style[] <- NA_character_
if (anyNA(meta$Complexity) || !identical(as.integer(table(factor(meta$Complexity,levels=c('Simple','Moderate','High')))),c(4L,12L,4L))) meta$Complexity[] <- NA_character_

# Prompt-level model paired gaps, then compare DIFFERENCES IN MODEL GAPS
# for the same prompt across two prompt-language conditions.
d <- merge(d, meta, by='PromptID', sort=FALSE)
d <- d[order(match(d$PromptID,PROMPTS),d$Language,d$Model),,drop=FALSE]
wide <- reshape(d[c('PromptID','Language','Model','Composite','Adherence')],
                idvar=c('PromptID','Language'),timevar='Model',direction='wide')
wide$Gap_QminusZ <- wide[['Composite.Qwen']] - wide[['Composite.Z-Image']]
wide$Gap_QminusSDXL <- wide[['Composite.Qwen']] - wide[['Composite.SDXL']]
wide$Gap_ZminusSDXL <- wide[['Composite.Z-Image']] - wide[['Composite.SDXL']]
wide <- merge(wide,meta,by='PromptID',sort=FALSE)
wc <- wide[wide$Language=='Chinese',c('PromptID','Gap_QminusZ')]
we <- wide[wide$Language=='English',c('PromptID','Gap_QminusZ')]
names(wc)[2] <- 'ChineseGap'
names(we)[2] <- 'EnglishGap'
pairs <- merge(wc,we,by='PromptID',sort=FALSE)
pairs$Change_EnglishMinusChinese <- pairs$EnglishGap - pairs$ChineseGap
pairs <- merge(pairs,meta,by='PromptID',sort=FALSE)
pairs <- pairs[order(match(pairs$PromptID,PROMPTS)),,drop=FALSE]


fmt <- function(x,n=3) ifelse(is.finite(x),sprintf(paste0('%.',n,'f'),x),'—')
sgn <- function(x,n=3) ifelse(is.finite(x),sprintf(paste0('%+.',n,'f'),x),'—')
md_row <- function(...) paste0('| ',paste(c(...),collapse=' | '),' |')

# Sensitivity table across four task attributes; all observations remain the SAME 20 prompts.
groups <- list(
  'All prompts' = PROMPTS,
  'Context: Educational' = meta$PromptID[meta$Context=='Educational'],
  'Context: General' = meta$PromptID[meta$Context=='General'],
  'Context: Chinese-cultural' = meta$PromptID[meta$Context=='Chinese-cultural'],
  'Content: Object–Concept' = meta$PromptID[meta$Content=='Object–Concept'],
  'Content: Other' = meta$PromptID[meta$Content=='Other content']
)
if (!all(is.na(meta$Style))) {
  for (level in c('Realistic','Stylized')) groups[[paste0('Style: ',level)]] <- meta$PromptID[meta$Style==level]
}
if (!all(is.na(meta$Complexity))) {
  for (level in c('Simple','Moderate','High')) groups[[paste0('Complexity: ',level)]] <- meta$PromptID[meta$Complexity==level]
}
stopifnot(length(groups[['All prompts']])==20L, all(vapply(groups,length,integer(1))>=4L))

model_stats <- function(ids,lang,model) {
  a <- d[d$PromptID %in% ids & d$Language==lang & d$Model==model,,drop=FALSE]
  if (nrow(a)!=length(ids) || anyDuplicated(a$PromptID)) fail('Model subset malformed: ',lang,'/',model)
  c(mean=mean(a$Composite), pass=sum(a$Adherence>=IMAGE_ADHERENCE_FLOOR), n=nrow(a))
}
choice <- function(ids,lang,score_floor,cover_floor) {
  ok <- character()
  for (model in c('SDXL','Z-Image','Qwen')) {
    s <- model_stats(ids,lang,model)
    if (s['mean'] >= score_floor-1e-10 && s['pass']/s['n'] >= cover_floor-1e-10) ok <- c(ok,model)
  }
  if (!length(ok)) return('None')
  ok[which.min(LATENCY_SEC[ok])]
}

lines <- c(
  '# Bilingual prompt-condition robustness diagnostic',
  '',
  '**Purpose:** internal research-story decision; not a manuscript figure, table or caption.',
  '',
  'All questionnaires were in Chinese. "Chinese" / "English" refers to the **generation prompt**. Different respondents rated the two prompt-language conditions.',
  '',
  paste0('Source: latest 120-image workbook; language field `',language_source,'`, task labels `',meta_source,'`. Historical early-response snapshot not used.'),
  '',
  '## 1. Overall model changes and rating-screen margins',
  '',
  md_row('Model','Chinese composite','English composite','English−Chinese','CN adherence ≥3.5','EN adherence ≥3.5'),
  md_row(':--','--:','--:','--:','--:','--:')
)
for (model in c('Qwen','Z-Image','SDXL')) {
  a <- model_stats(PROMPTS,'Chinese',model)
  b <- model_stats(PROMPTS,'English',model)
  lines <- c(lines,md_row(model,fmt(a['mean']),fmt(b['mean']),sgn(b['mean']-a['mean']),
                         paste0(a['pass'],'/',a['n']),paste0(b['pass'],'/',b['n'])))
}
lines <- c(lines,'',
  'At a coverage gate of 80%, a pass count of 16/20 is EXACTLY on the boundary. This alone is not evidence of robust deployment eligibility.',
  '', '## 2. Task-group Qwen−Z-Image gap-change stability', '',
  'The change is (English Qwen−Z gap) − (Chinese Qwen−Z gap). Delete-one ranges are descriptive across the selected prompts.',
  '',md_row('Task group','n','Mean change','Leave-one-out min','Leave-one-out max','Largest absolute contributor'),
  md_row(':--','--:','--:','--:','--:','--:'))
for (nm in names(groups)) {
  z <- pairs[pairs$PromptID %in% groups[[nm]],,drop=FALSE]
  delta <- z$Change_EnglishMinusChinese
  loo <- vapply(seq_along(delta),function(i) mean(delta[-i]),numeric(1))
  leverage <- which.max(abs(delta - mean(delta)))
  lines <- c(lines,md_row(nm,nrow(z),sgn(mean(delta)),sgn(min(loo)),sgn(max(loo)),
                          paste0(z$PromptID[leverage],' (',sgn(delta[leverage]),')')))
}
lines <- c(lines,'',
  '## 3. Fastest-eligible choice under score and adherence screens', '',
  'Each model must pass both a composite-score floor and a minimum fraction of images with subjective adherence ≥3.5. Reported generation times are the same measured **configuration-level** means for both prompt-language groups; there are no language-specific timing estimates.',
  '',md_row('Task group','Min. mean','CN choice','EN choice','CN LOO retention','EN LOO retention'),
  md_row(':--','--:',':--',':--','--:','--:'))
for (nm in names(groups)) for (floor in c(3.5,4.0)) {
  ids <- groups[[nm]]
  cn <- choice(ids,'Chinese',floor,0.80)
  en <- choice(ids,'English',floor,0.80)
  cn_ret <- sum(vapply(ids, function(id) choice(setdiff(ids,id),'Chinese',floor,0.80)==cn, logical(1)))
  en_ret <- sum(vapply(ids, function(id) choice(setdiff(ids,id),'English',floor,0.80)==en, logical(1)))
  lines <- c(lines,md_row(nm,fmt(floor,1),cn,en,paste0(cn_ret,'/',length(ids)),paste0(en_ret,'/',length(ids))))
}
lines <- c(lines,'',
  'Leave-one-out retention counts how often the full-group selected candidate remains the same after removing one prompt. The subsets overlap and are NOT independent validation experiments.',
  '', '## 4. Sensitivity to the adherence coverage requirement', '',
  'For each of two illustrative mean-score floors, the table below describes how **the All-20 choice** changes when the minimum adherence coverage requirement changes.',
  '',md_row('Mean floor','Coverage requirement','Chinese choice','English choice','Different?'),md_row('--:','--:',':--',':--',':--'))
for (floor in c(3.5,4.0)) for (cover in c(0.70,0.75,0.80,0.85,0.90,0.95,1.0)) {
  cn <- choice(PROMPTS,'Chinese',floor,cover)
  en <- choice(PROMPTS,'English',floor,cover)
  lines <- c(lines,md_row(fmt(floor,1),paste0(round(cover*100),'%'),cn,en,if(cn==en)'No' else 'Yes'))
}
lines <- c(lines,'',
  '## 5. Original raw-ratings workbook structural inventory', '',
  'This inventory helps plan a later respondent–image linking audit. It does not itself verify image display provenance or independence of rater groups.', '')
if (!file.exists(RAW_RATINGS_XLSX)) {
  lines <- c(lines,'- Raw ratings workbook not found at the expected path. The image-level analyses above are unaffected.')
} else {
  sheets <- tryCatch(readxl::excel_sheets(RAW_RATINGS_XLSX),error=function(e) character())
  if (!length(sheets)) {
    lines <- c(lines,'- Raw ratings workbook exists but sheet list could not be read.')
  } else {
    lines <- c(lines,md_row('Worksheet','First row field names (up to 30)','Potential grouping columns'),
               md_row(':--',':--',':--'))
    for (sheet in head(sheets,12L)) {
      x <- tryCatch(readxl::read_excel(RAW_RATINGS_XLSX,sheet=sheet,n_max=0),error=function(e) NULL)
      nm <- if (is.null(x)) character() else names(x)
      nm <- gsub('[|\r\n]',' ',nm)
      maybe <- nm[grepl('language|model|prompt|image|group|form|respond|评分|问卷|图像|提示|分组|来源|样本',nm,ignore.case=TRUE)]
      lines <- c(lines,md_row(gsub('[|\r\n]',' ',sheet),paste(head(nm,30L),collapse=', '),paste(head(maybe,16L),collapse=', ')))
    }
    if (length(sheets)>12L) lines <- c(lines,paste0('',length(sheets)-12L,' additional sheets not listed.'))
  }
}
lines <- c(lines,'',
  '## 6. Interpretation boundary and next decision', '',
  '- One rated image per prompt × model × generation language means the sample-level changes include image-to-image and seed variability.',
  '- The two prompt-language conditions were rated by different people, so differences are not isolated causal language effects.',
  '- The original rated-image identities, questionnaire display mapping, and valid respondent composition require direct raw-record audit before elevating language-dependent deployment choices to main manuscript claims.',
  '- Threshold-derived fastest-choice maps are exploratory and depend on the score and coverage requirements.',
  ''
)
dir.create(dirname(OUTPUT_MD),recursive=TRUE,showWarnings=FALSE)
writeLines(enc2utf8(lines),OUTPUT_MD,useBytes=TRUE)
message('Wrote one internal Markdown diagnostic: ',OUTPUT_MD)
message('120 unique rated images and all B1–B20 prompt × model × language keys checked.')
