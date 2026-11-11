#!/usr/bin/env Rscript
# Exploratory comparison of task-conditioned model gaps by prompt language.
# Inputs: data/source/subjective_scores_human120_image_level.xlsx
# and data/source/prompts.xlsx; output: data/paper_results/.
# Rating questionnaires are Chinese; prompt languages differ across groups.

options(stringsAsFactors = FALSE)
ROOT <- normalizePath(getwd(), winslash = '/', mustWork = TRUE)
SCORES_XLSX <- file.path(ROOT, 'data', 'source', 'subjective_scores_human120_image_level.xlsx')
PROMPTS_XLSX <- file.path(ROOT, 'data', 'source', 'prompts.xlsx')
FIG3_META <- file.path(ROOT, 'data', 'paper_results', 'Fig3_task_model_prompt_scores.csv')
OUTPUT_MD <- file.path(ROOT, 'data', 'paper_results', 'Bilingual_task_value_diagnostic.md')

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
line <- c(
  '# Exploratory diagnostic: does prompt language change task-conditioned model choice?',
  '',
  '**Purpose.** Research design decision only; not manuscript tables or captions.',
  '',
  paste0('**Source.** Latest image_level worksheet (120 scored images); prompt-language field `',language_source,
         '`; task taxonomy ',meta_source,'.'),
  'All evaluation questionnaires are in Chinese. The two generation-prompt language conditions have different rater groups.',
  '',
  '## 1. Data checks',
  '',
  '- Source contains 120 unique prompt × model × prompt-language rated-image records (20 × 3 × 2).',
  '- Each prompt-language × model has 20 distinct PromptIDs; each cell has one rated image.',
  '- Latest Chinese prompt group means match the 191-questionnaire authoritative results.',
  '- The image-level file verifies the mapping of score rows to PromptID/model/language **as recorded**; it cannot by itself prove the original questionnaire image display mapping or respondent-level group composition.',
  '- Historical 269-questionnaire data were not used.',
  '',
  '## 2. Model-level results by generation-prompt language',
  '',
  md_row('Prompt language','Model','Composite mean','Adherence mean','Images ≥3.5 adherence'),
  md_row(':--',':--','--:','--:','--:')
)
for (lang in c('Chinese','English')) for (model in c('Qwen','Z-Image','SDXL')) {
  z <- d[d$Language==lang & d$Model==model,,drop=FALSE]
  line <- c(line,md_row(lang,model,fmt(mean(z$Composite)),fmt(mean(z$Adherence)),
                        paste0(sum(z$Adherence >= IMAGE_ADHERENCE_FLOOR),'/20')))
}
line <- c(line,'','## 3. Qwen–Z-Image model gap by task group','','Positive gaps favor Qwen. The change column is (English gap − Chinese gap); it is **not** a causal language effect.','',
          md_row('Task subset','Prompt n','Chinese Q−Z','English Q−Z','Change E−C','Qwen ahead (CN / EN)'),
          md_row(':--','--:','--:','--:','--:','--:'))
groups <- list(
  'All prompts'=rep(TRUE,nrow(pairs)),
  'Context: Educational'=pairs$Context=='Educational',
  'Context: General'=pairs$Context=='General',
  'Context: Chinese-cultural'=pairs$Context=='Chinese-cultural',
  'Content: Object–Concept'=pairs$Content=='Object–Concept',
  'Content: Other'=pairs$Content=='Other content'
)
if (!all(is.na(meta$Style))) {
  groups[['Style: Realistic']] <- pairs$Style=='Realistic'
  groups[['Style: Stylized']] <- pairs$Style=='Stylized'
}
if (!all(is.na(meta$Complexity))) {
  for (level in c('Simple','Moderate','High')) {
    groups[[paste0('Complexity: ',level)]] <- pairs$Complexity==level
  }
}
for (nm in names(groups)) {
  z <- pairs[groups[[nm]],,drop=FALSE]
  line <- c(line,md_row(nm, nrow(z),sgn(mean(z$ChineseGap)),sgn(mean(z$EnglishGap)),
                        sgn(mean(z$Change_EnglishMinusChinese)),
                        paste0(sum(z$ChineseGap>0),'/',nrow(z),' / ',sum(z$EnglishGap>0),'/',nrow(z))))
}
# Numeric paired-prompt contrast CI is descriptive because raters differ by
# language and one rated seed is sampled per prompt × model × language.
x <- pairs$Change_EnglishMinusChinese
ci <- tryCatch(stats::t.test(x,mu=0,conf.level=.95)$conf.int,error=function(e)c(NA_real_,NA_real_))
loo <- vapply(seq_along(x), function(i) mean(x[-i]), numeric(1))
line <- c(line,'','## 4. Prompt-level comparison', '',
          md_row('PromptID','Context','Content','Chinese Q−Z','English Q−Z','Change E−C'),
          md_row(':--',':--',':--','--:','--:','--:'))
for (i in seq_len(nrow(pairs))) {
  z <- pairs[i,,drop=FALSE]
  line <- c(line,md_row(z$PromptID,z$Context,z$Content,sgn(z$ChineseGap),sgn(z$EnglishGap),sgn(z$Change_EnglishMinusChinese)))
}
line <- c(line,'',paste0('Across 20 paired prompt IDs, mean change in the Qwen−Z-Image gap (English−Chinese) = **',
    sgn(mean(x)),'**; nominal 95% paired-prompt t interval [',fmt(ci[1]),', ',fmt(ci[2]),
    ']. Leave-one-prompt-out mean-change range [',fmt(min(loo)),', ',fmt(max(loo)),']. These are descriptive comparisons of prompt-matched images rated by different groups, not estimates of a pure language effect.'),
    '', '## 5. Implications for an illustrative two-screen deployment choice',
    '',
    paste0('For each task subset and language, models first must have composite mean ≥ 3.5 or 4.0 and at least ',
           round(100*COVERAGE_FLOOR),'% of rated images with adherence ≥ ',IMAGE_ADHERENCE_FLOOR,
           '. The fastest eligible model is selected using the SAME previously measured configuration mean latencies (Qwen 167 s, Z-Image 8.6 s, SDXL 1 s), not language-specific timing measurements.'),
    '',md_row('Task subset','Mean score floor','Chinese fastest eligible','English fastest eligible','Same choice?'),
    md_row(':--','--:',':--',':--',':--'))
choose <- function(task_name,lang,floor_value) {
  s <- groups[[task_name]]
  ids <- pairs$PromptID[s]
  slice <- d[d$PromptID %in% ids & d$Language == lang,,drop=FALSE]
  valid <- character()
  for (m in c('Qwen','Z-Image','SDXL')) {
    a <- slice[slice$Model==m,,drop=FALSE]
    if (!nrow(a) || nrow(a)!=length(ids)) fail('Unexpected group/model size in decision screen for ',task_name)
    if (mean(a$Composite) >= floor_value - 1e-10 && mean(a$Adherence >= IMAGE_ADHERENCE_FLOOR) >= COVERAGE_FLOOR-1e-10) valid <- c(valid,m)
  }
  if (!length(valid)) return('None')
  valid[which.min(LATENCY_SEC[valid])]
}
changes <- list()
for (nm in names(groups)) for (floor_value in COMPOSITE_FLOORS) {
  cn <- choose(nm,'Chinese',floor_value)
  en <- choose(nm,'English',floor_value)
  changes[[length(changes)+1L]] <- (cn!=en)
  line <- c(line,md_row(nm,sprintf('%.1f',floor_value),cn,en,if (cn==en) 'Yes' else '**No**'))
}
line <- c(line,'',paste0('Illustrative selection switches across language conditions: **',
  sum(unlist(changes)),' / ',length(changes),'** task×floor scenarios. This count is not an independent-observation significance test.'),
  '', '## 6. Evidence assessment before adding a language result to the manuscript',
  '',
  '- Inspect whether any language-related change in the Qwen−Z-Image gap is concentrated in Educational / Object–Concept rather than just a uniform model shift.',
  '- Verify that observed differences survive individual PromptID removal and do not only reflect a single image.',
  '- Confirm the latest 200 English-prompt-image rating questionnaires and original scored-image mapping at respondent/image level. The aggregated workbook cannot independently establish those links.',
  '- If the differences do not alter task-conditioned choices or offer a distinct explanation beyond the old PeerJ bilingual comparison, keep the manuscript Chinese-centered.',
  '- If changes are substantial and well-supported, reconsider the paper story, research questions and *later* figure layout. Do not automatically append a second language panel to all finalized figures.',
  '',
  '## 7. Metadata availability', '',
  paste0('- Context and Content: validated from ',meta_source,'.'),
  paste0('- Style: ',if (all(is.na(meta$Style))) 'not reliably recovered; no Style comparison made.' else 'complete and validated for all 20 prompts.'),
  paste0('- Complexity: ',if (all(is.na(meta$Complexity))) 'not reliably recovered; no Complexity comparison made.' else 'complete and validated for all 20 prompts.'),
  '- Actual rater counts, group composition, and inter-rater reliability must be derived separately from questionnaire-level data.',
  ''
)
dir.create(dirname(OUTPUT_MD),recursive=TRUE,showWarnings=FALSE)
writeLines(enc2utf8(line), OUTPUT_MD, useBytes=TRUE)
message('Wrote ONE exploratory Markdown report: ',OUTPUT_MD)
message('Images checked: 120; language field: ',language_source,'; taxonomy: ',meta_source)
message('Model-gap change (English−Chinese): ',sgn(mean(x)))
