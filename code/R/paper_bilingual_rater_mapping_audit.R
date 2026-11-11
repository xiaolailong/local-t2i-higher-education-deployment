#!/usr/bin/env Rscript
# Verify questionnaire ratings and image names against image-level means.
# Inputs: data/source/subjective_scores_human120*.xlsx
# Output: data/paper_results/Bilingual_rater_mapping_audit.md
# Student image filenames may include a form/item-number prefix.

options(stringsAsFactors=FALSE)
ROOT <- normalizePath(getwd(),winslash='/',mustWork=TRUE)
RAW_XLSX <- file.path(ROOT, 'data', 'source', 'subjective_scores_human120.xlsx')
IMAGE_XLSX <- file.path(ROOT, 'data', 'source', 'subjective_scores_human120_image_level.xlsx')
OUTPUT_MD <- file.path(ROOT,'data','paper_results','Bilingual_rater_mapping_audit.md')

fail <- function(...) stop(paste0(...),call.=FALSE)
if (!requireNamespace('readxl',quietly=TRUE)) fail('Install readxl: install.packages("readxl")')
for (f in c(RAW_XLSX,IMAGE_XLSX)) if (!file.exists(f)) fail('Required source not found: ',f)

norm_name <- function(x) tolower(gsub('[^a-z0-9]','',tolower(x)))
col_of <- function(d,options) {
  ix <- match(norm_name(options),norm_name(names(d)))
  ix <- ix[!is.na(ix)]
  if (length(ix)) names(d)[ix[1]] else NULL
}
field <- function(d,options,where) {
  v <- col_of(d,options)
  if (is.null(v)) fail('Required column absent (',where,'): ',paste(options,collapse=' / '),
                       '. Available: ',paste(names(d),collapse=', '))
  d[[v]]
}
norm_prompt <- function(v) {
  x <- toupper(gsub('[[:space:]]+','',trimws(as.character(v))))
  ok <- grepl('^B0*[0-9]+$',x)
  x[ok] <- paste0('B',as.integer(sub('^B','',x[ok])))
  x
}
norm_model <- function(v) {
  x <- tolower(trimws(as.character(v)))
  is_q <- grepl('qwen',x); is_z <- grepl('z[-_ ]?image|zimage',x)
  is_s <- grepl('sdxl|stable.diffusion.xl',x)
  cnt <- as.integer(is_q)+as.integer(is_z)+as.integer(is_s)
  out <- rep(NA_character_,length(x))
  out[is_q & cnt==1] <- 'Qwen'; out[is_z & cnt==1] <- 'Z-Image'
  out[is_s & cnt==1] <- 'SDXL'; out
}
norm_language <- function(v) {
  x <- tolower(gsub('[[:space:]_]+','',trimws(as.character(v))))
  out <- rep(NA_character_,length(x))
  out[x %in% c('chinese','ch','cn','zh','zh-cn','zh-hans','中文','中文提示词','汉语','汉语提示词')] <- 'Chinese'
  out[x %in% c('english','en','eng','en-us','en-gb','英文','英文提示词','英语','英语提示词')] <- 'English'
  out
}
norm_question <- function(v) {
  x <- toupper(gsub('[[:space:]_.-]+','',as.character(v)))
  n <- sub('^Q([123])$','\\1',x)
  out <- rep(NA_character_,length(n))
  out[n %in% as.character(1:3)] <- paste0('Q',n[n %in% as.character(1:3)])
  out
}
short_image <- function(v) {
  x <- gsub('\\\\','/',trimws(as.character(v)))
  x <- sub('[?#].*$','',x)
  tolower(basename(x))
}
# Student questionnaire exports prepend a form/item index, e.g.
# A.7.c_qwen_B1_chinese_...png -> c_qwen_b1_chinese_...png.
# Remove ONLY this known anchored form-item prefix. Never remove the
# original 'c_' generated-image filename prefix or the seed suffix.
canonical_image <- function(v) {
  x <- short_image(v)
  sub('^[a-d]\\.[0-9]+\\.(?=c_)','',x,perl=TRUE)
}
md_row <- function(...) paste0('| ',paste(gsub('[|\r\n]',' ',as.character(c(...))),collapse=' | '),' |')
fmt <- function(x,k=3) ifelse(is.finite(x),sprintf(paste0('%.',k,'f'),x),'—')
sign_fmt <- function(x,k=3) ifelse(is.finite(x),sprintf(paste0('%+.',k,'f'),x),'—')

# 1. Read the cleaned/latest individual question ratings, not an old snapshot.
wanted <- c('teacher','student_A','student_B','student_C','student_D')
found <- readxl::excel_sheets(RAW_XLSX)
if (!all(wanted %in% found)) fail('Missing questionnaire sheets: ',paste(setdiff(wanted,found),collapse=', '))
raw_parts <- lapply(wanted,function(sh) {
  z <- as.data.frame(readxl::read_excel(RAW_XLSX,sheet=sh))
  data.frame(Form=sh,
    Group=if(sh=='teacher') 'Teachers' else 'Students',
    Respondent=trimws(as.character(field(z,c('respondent_id','RespondentID','respondent'),sh))),
    ImageName=trimws(as.character(field(z,c('image_name','ImageName','ImageFile','ImagePath'),sh))),
    PromptID=norm_prompt(field(z,c('prompt_id','PromptID','Prompt_ID'),sh)),
    Model=norm_model(field(z,c('model','Model','ModelShortName'),sh)),
    Language=norm_language(field(z,c('language','Language','PromptLanguage'),sh)),
    RawContext=trimws(as.character(field(z,c('Context','TaskContext','ContextLabel','PromptContext'),sh))),
    Question=norm_question(field(z,c('q_no','QuestionNo','QNo','Question'),sh)),
    Score=suppressWarnings(as.numeric(field(z,c('score','Score','Rating'),sh))),
    stringsAsFactors=FALSE)
})
r <- do.call(rbind,raw_parts)
ids <- paste0('B',1:20)
valid <- !is.na(r$Respondent) & nzchar(r$Respondent) &
  !is.na(r$ImageName) & nzchar(r$ImageName) &
  r$PromptID %in% ids & !is.na(r$Model) & !is.na(r$Language) &
  !is.na(r$Question) & is.finite(r$Score) & r$Score>=1 & r$Score<=5
bad_rows <- sum(!valid)
if (bad_rows) fail('Latest long-format ratings contain ',bad_rows,
                   ' invalid/empty key or score rows; no partial analysis performed.')
r$RawImageBase <- short_image(r$ImageName)
r$RatedImageBase <- canonical_image(r$ImageName)
# Validate that a stripped item prefix belongs to its questionnaire form.
r$HasSurveyPrefix <- grepl('^[a-d]\\.[0-9]+\\.',r$RawImageBase)
r$PrefixLetter <- ifelse(r$HasSurveyPrefix,sub('^([a-d])\\..*$','\\1',r$RawImageBase),NA_character_)
r$ExpectedLetter <- ifelse(r$Form=='teacher','',tolower(sub('^student_','',r$Form)))
wrong_prefix <- r$HasSurveyPrefix & (r$Form=='teacher' | r$PrefixLetter != r$ExpectedLetter)
unstripped_prefix <- r$HasSurveyPrefix & (r$RawImageBase==r$RatedImageBase)
if(any(wrong_prefix | unstripped_prefix)) {
  examples <- unique(r$ImageName[wrong_prefix | unstripped_prefix])
  fail('Unrecognized/incorrect survey image prefix (n=',sum(wrong_prefix | unstripped_prefix),
       '), examples: ',paste(head(examples,5),collapse='; '))
}
r$RaterKey <- paste(r$Form,r$Language,r$Respondent,sep='|')
r$ImageKey <- paste(r$PromptID,r$Model,r$Language,sep='|')
r$RaterImageKey <- paste(r$RaterKey,r$ImageKey,sep='|')
item_key <- paste(r$RaterImageKey,r$Question,sep='|')
item_dups <- sum(duplicated(item_key))

# Analyze respondent-image completeness and sample coverage.
q_by_unit <- split(r$Question,r$RaterImageKey)
n_complete <- sum(vapply(q_by_unit,function(x) length(x)==3L &&
                        setequal(x,c('Q1','Q2','Q3')),logical(1)))
n_units <- length(q_by_unit)
unit_complete <- n_complete==n_units && item_dups==0L
image_ref_count <- tapply(r$RatedImageBase,r$ImageKey,function(x) length(unique(x)))
ambiguous_image_keys <- names(image_ref_count)[image_ref_count>1]
raw_ref_count <- tapply(r$RawImageBase,r$ImageKey,function(x) length(unique(x)))
raw_alias_keys <- names(raw_ref_count)[raw_ref_count>1]
# Additional source-reference inventory: distinguish different source-form aliases
# from different images *within the same form*. Preserve raw names for inspection.
ref_inventory <- unique(r[c('PromptID','Model','Language','Form','ImageName','RawImageBase','RatedImageBase','ImageKey')])
within_form_ref_count <- aggregate(RatedImageBase ~ ImageKey+Form,
    data=ref_inventory, FUN=function(x) length(unique(x)))
within_form_multi <- within_form_ref_count[within_form_ref_count$RatedImageBase>1,,drop=FALSE]
source_ref_info <- aggregate(RatedImageBase ~ ImageKey, data=ref_inventory,
    FUN=function(x) length(unique(x)))
source_ref_info$CrossFormCount <- source_ref_info$RatedImageBase
source_ref_info$RatedImageBase <- NULL
# At the same prompt/model/language key, display the actual values for each
# source form, not an NA produced by collapsing conflicting strings.
ref_summary <- function(v, max_items=6L) {
  v <- unique(as.character(v))
  v <- v[!is.na(v) & nzchar(v)]
  if (!length(v)) return('(missing)')
  paste(c(head(v,max_items), if(length(v)>max_items) paste0('… +',length(v)-max_items)),collapse=' ; ')
}

# 2. Compare long-form ratings with 120-image aggregates.
agg <- as.data.frame(readxl::read_excel(IMAGE_XLSX,sheet='image_level'))
agg_ids <- norm_prompt(field(agg,c('PromptID','prompt_id'), 'image_level'))
a <- data.frame(PromptID=agg_ids,
  Model=norm_model(field(agg,c('ModelShortName','Model','ModelName'),'image_level')),
  Language=norm_language(field(agg,c('Language','PromptLanguage'),'image_level')),
  Composite=suppressWarnings(as.numeric(field(agg,c('CompositeScore','MeanCompositeScore','Composite'),'image_level'))),
  Adherence=suppressWarnings(as.numeric(field(agg,c('MeanAdherence','AdherenceMean'),'image_level'))),
  stringsAsFactors=FALSE)
a <- a[a$PromptID %in% ids,,drop=FALSE]
if(nrow(a)!=120L || anyNA(a) || anyDuplicated(a[c('PromptID','Model','Language')])) {
  fail('The latest image_level sheet is not 120 unique complete PromptID × Model × Language rows.')
}
agg_img_col <- col_of(agg,c('ImageName','image_name','ImageFileName','ImageFile','ImagePath','ImageFilename'))
if(!is.null(agg_img_col)) a$ImageBase <- short_image(agg[[agg_img_col]][agg_ids %in% ids])

by_dim <- aggregate(Score~PromptID+Model+Language+Question,data=r,FUN=mean)
mean_q <- reshape(by_dim,idvar=c('PromptID','Model','Language'),
                  timevar='Question',direction='wide')
required_q <- c('Score.Q1','Score.Q2','Score.Q3')
if (!all(required_q %in% names(mean_q))) fail('Cannot aggregate all Q1/Q2/Q3 ratings.')
mean_q$RecalcComposite <- rowMeans(mean_q[required_q])
mean_q$RecalcAdherence <- mean_q[['Score.Q1']]
check <- merge(a,mean_q,by=c('PromptID','Model','Language'),all=TRUE,sort=FALSE)
if(nrow(check)!=120L || anyNA(check[c('Composite','Adherence','RecalcComposite','RecalcAdherence')])) {
  fail('Raw/imaged-level keys do not form a complete 120-row match.')
}
check$CompositeDiff <- check$RecalcComposite-check$Composite
check$AdherenceDiff <- check$RecalcAdherence-check$Adherence
max_c <- max(abs(check$CompositeDiff)); max_a <- max(abs(check$AdherenceDiff))
mismatches <- check[abs(check$CompositeDiff)>0.005 | abs(check$AdherenceDiff)>0.005,,drop=FALSE]
if(nrow(mismatches)) mismatches <- mismatches[order(-pmax(abs(mismatches$CompositeDiff),abs(mismatches$AdherenceDiff))),,drop=FALSE]
if(!is.null(agg_img_col)) {
  map <- unique(r[c('PromptID','Model','Language','Form','RatedImageBase')])
  x <- merge(map,a[c('PromptID','Model','Language','ImageBase')],
             by=c('PromptID','Model','Language'),all=TRUE)
  x$Matched <- !is.na(x$RatedImageBase) & !is.na(x$ImageBase) &
               x$RatedImageBase==x$ImageBase
  match_source_count <- sum(x$Matched)
  total_source_count <- nrow(x)
  match_key_count <- sum(vapply(split(x$Matched,paste(x$PromptID,x$Model,x$Language,sep='|')),
      function(v) length(v)>0L && all(v),logical(1)))
  mismatched_img <- x[!x$Matched,,drop=FALSE]
  match_img_count <- match_key_count
} else { match_img_count <- NA_integer_; match_source_count <- NA_integer_; total_source_count <- NA_integer_; mismatched_img <- NULL }

# 3. Rater group inventory and per-image coverage.
keys <- unique(r[c('Form','Group','Language','RaterKey')])
full_keys <- unique(r[c('PromptID','Model','Language','ImageKey')])
forms <- expand.grid(Form=wanted,Language=c('Chinese','English'),stringsAsFactors=FALSE)
form_info <- lapply(seq_len(nrow(forms)),function(i) {
  sh<-forms$Form[i]; lang<-forms$Language[i]
  z<-r[r$Form==sh & r$Language==lang,,drop=FALSE]
  data.frame(Form=sh,Language=lang,
    Questionnaires=length(unique(z$RaterKey)),
    RatedImages=length(unique(z$ImageKey)),
    QuestionScores=nrow(z),
    RaterImageUnits=length(unique(z$RaterImageKey)))
})
form_info<-do.call(rbind,form_info)
count_cn <- sum(form_info$Questionnaires[form_info$Language=='Chinese'])
count_en <- sum(form_info$Questionnaires[form_info$Language=='English'])
per_image <- aggregate(RaterKey~Group+Language+PromptID+Model,data=r,
                       FUN=function(x) length(unique(x)))
names(per_image)[5]<-'RaterCount'
coverage_lines <- character()
for(g in c('Teachers','Students')) for(l in c('Chinese','English')) {
  v<-per_image$RaterCount[per_image$Group==g & per_image$Language==l]
  coverage_lines<-c(coverage_lines,md_row(g,l,length(v),if(length(v))min(v) else '—',
                                          if(length(v))max(v) else '—',
                                          if(length(v))fmt(mean(v),1) else '—'))
}

# 4. Compare the signs of task-conditioned Qwen-Z shifts separately in
# teachers and students; this is a CONSISTENCY check, not rater matching.
# Task labels already exist alongside each raw score in all five questionnaire
# worksheets. They provide a direct PromptID -> Context mapping, without assuming
# a particular physical column layout in the separate prompts workbook.
context_label <- function(x) {
  v <- tolower(gsub('[[:space:]_./–—-]+','',trimws(as.character(x))))
  out <- rep(NA_character_,length(v))
  out[grepl('educat|teaching|教学|教育',v)] <- 'Educational'
  out[grepl('general|通用|一般',v)] <- 'General'
  out[grepl('chinesecultur|culturechinese|culturalchinese|中华文化|中国文化|中国传统',v)] <- 'Chinese-cultural'
  out
}
cm <- unique(data.frame(PromptID=r$PromptID, Context=context_label(r$RawContext)))
if(nrow(cm)!=20L || anyNA(cm) || anyDuplicated(cm$PromptID) ||
   !setequal(cm$PromptID,ids) ||
   !identical(as.integer(table(factor(cm$Context,
     levels=c('Educational','General','Chinese-cultural')))),c(5L,10L,5L))) {
  ex <- unique(r[c('PromptID','RawContext')]); ex <- ex[order(ex$PromptID),,drop=FALSE]
  fail('Context labels in the raw questionnaire sheets do not provide one validated ',
       'label per B1–B20 PromptID (expected Educational=5, General=10, ',
       'Chinese-cultural=5). Example observed pairs: ',
       paste(head(paste(ex$PromptID,ex$RawContext,sep='='),25L),collapse='; '))
}
cm <- cm[match(ids,cm$PromptID),,drop=FALSE]

# For each group, calculate one image mean per task/prompt/model/language.
subgroup_scores<-aggregate(Score~Group+PromptID+Model+Language+Question,data=r,FUN=mean)
sg_wide<-reshape(subgroup_scores,idvar=c('Group','PromptID','Model','Language'),
                 timevar='Question',direction='wide')
sg_wide$Composite<-rowMeans(sg_wide[required_q],na.rm=FALSE)
sg_wide<-sg_wide[is.finite(sg_wide$Composite),,drop=FALSE]
sg_pair<-reshape(sg_wide[c('Group','PromptID','Language','Model','Composite')],
                 idvar=c('Group','PromptID','Language'),timevar='Model',direction='wide')
if(!all(c('Composite.Qwen','Composite.Z-Image') %in% names(sg_pair))) fail('Rater groups missing key models.')
sg_pair$QminusZ<-sg_pair[['Composite.Qwen']]-sg_pair[['Composite.Z-Image']]
sg_lang<-reshape(sg_pair[c('Group','PromptID','Language','QminusZ')],
                 idvar=c('Group','PromptID'),timevar='Language',direction='wide')
sg_lang$Shift<-sg_lang[['QminusZ.English']]-sg_lang[['QminusZ.Chinese']]
subgroups<-list('All prompts'=ids,'Educational'=cm$PromptID[cm$Context=='Educational'],
                'General'=cm$PromptID[cm$Context=='General'],
                'Chinese-cultural'=cm$PromptID[cm$Context=='Chinese-cultural'])

out<-c('# Raw-ratings and rater-stratified prompt-language audit','',
  '**Purpose:** verify internal consistency of the latest rating data and assess whether prompt-language-condition differences appear in both teacher and student evaluations. This is an internal diagnostic, not a manuscript table.','',
  '**Design:** all rating questionnaires were in Chinese; Chinese/English denotes the **generation-prompt language**. Respondents differ across those conditions.','',
  '**Task labels:** recovered directly from the Context column of the latest raw questionnaire scoring sheets; checked for one consistent label per B1–B20 PromptID and the expected 5/10/5 task composition.','',
  '## 1. Record integrity','',
  paste0('- Latest long-format question-score rows: **',nrow(r),'**.'),
  paste0('- Valid respondent–image units: **',n_units,'**, complete Q1/Q2/Q3 units: **',n_complete,'**.'),
  paste0('- Duplicate respondent–image–question keys: **',item_dups,'**.'),
  paste0('- Rated prompt × model × language image keys: **',length(unique(r$ImageKey)),'** (expected 120).'),
  paste0('- Keys with multiple RAW names before removal of survey item prefixes: **',length(raw_alias_keys),' / 120**.'),
  paste0('- Keys with multiple CANONICAL image names after normalization: **',length(ambiguous_image_keys),' / 120**.'),
  paste0('- Respondent records by form and prompt language: **Chinese ',count_cn,'; English ',count_en,'** (expected 191/200 from the current research record).'),'',
  '### Respondent counts by questionnaire source','',
  md_row('Source form','Prompt language','Questionnaires','Distinct images','Q1–Q3 scores','Rater–image units'),
  md_row(':--',':--','--:','--:','--:','--:'))
for(i in seq_len(nrow(form_info))) {
  z<-form_info[i,]
  if(z$Questionnaires==0L && z$QuestionScores==0L) next
  out<-c(out,md_row(z$Form,z$Language,z$Questionnaires,z$RatedImages,z$QuestionScores,z$RaterImageUnits))
}
out<-c(out,'','### Coverage by respondent group','',
       md_row('Group','Prompt language','Images with ratings','Min raters/image','Max raters/image','Mean raters/image'),
       md_row(':--',':--','--:','--:','--:','--:'))
out<-c(out,coverage_lines,'',
  '## 2. Agreement between long-format ratings and image-level score workbook','',
  paste0('- Keys reconciled: **',nrow(check),' / 120**.'),
  paste0('- Largest absolute composite-score discrepancy: **',fmt(max_c,5),'**; adherence discrepancy: **',fmt(max_a,5),'**.'),
  paste0('- Keys with discrepancy >0.005 in either score: **',nrow(mismatches),'**.'),
  if(!is.na(match_img_count)) paste0('- Canonical image identity matching at prompt × model × language level: **',match_img_count,' / 120**; individual form–image references: **',match_source_count,' / ',total_source_count,'**.')
  else '- No explicit image filename column identified in image_level; raw source filename consistency only.',
  '')
if(nrow(mismatches)) {
  out<-c(out,md_row('Prompt','Model','Prompt language','Composite delta','Adherence delta'),
         md_row(':--',':--',':--','--:','--:'))
  for(i in head(seq_len(nrow(mismatches)),10L)) {
    z<-mismatches[i,];out<-c(out,md_row(z$PromptID,z$Model,z$Language,sign_fmt(z$CompositeDiff,5),sign_fmt(z$AdherenceDiff,5)))
  }
  out<-c(out,'')
}
# by aggregate() whenever a key had >1 distinct reference name. It did not
# mean the raw workbook contains an NA image_name value.
out <- c(out,
  '### Resolution of the previous `NA` / 0-of-120 false alarm','',
  'The student survey exports prepend a **form letter + questionnaire item number** (e.g. `A.7.` or `C.10.`) to the original image filename; the teacher form and image-level workbook use the unprefixed generated-image filename. The original script aggregated different raw strings into `NA`, rather than comparing their canonical image identities.','',
  paste0('- Keys with multiple raw filename strings across forms: **',length(raw_alias_keys),' / 120**.'),
  paste0('- Keys with multiple canonical filenames across forms: **',length(ambiguous_image_keys),' / 120**.'),
  paste0('- Source form × prompt/model/language cells with more than one canonical image name: **',nrow(within_form_multi),'**.'),
  paste0('- Records with incorrect or unrecognized form prefix: **',sum(wrong_prefix | unstripped_prefix),'**.'),
  '- Filename mapping is internally consistent after normalization. Original survey-platform display contents are still a separate provenance question.','',
  '### Actual filename references by questionnaire form (first 18 keys)','',
  md_row('Prompt','Model','Prompt language','Source form','Raw image_name basename(s)','Image-level filename'),
  md_row(':--',':--',':--',':--',':--',':--'))
ref_keys <- unique(ref_inventory[c('PromptID','Model','Language','ImageKey')])
ref_keys <- ref_keys[order(as.integer(sub('^B','',ref_keys$PromptID)),ref_keys$Model,ref_keys$Language),,drop=FALSE]
for (k in head(seq_len(nrow(ref_keys)),18L)) {
  z <- ref_keys[k,]
  rows <- ref_inventory[ref_inventory$ImageKey == z$ImageKey,,drop=FALSE]
  matchrow <- a[a$PromptID==z$PromptID & a$Model==z$Model & a$Language==z$Language,,drop=FALSE]
  target <- if('ImageBase' %in% names(matchrow)) as.character(matchrow$ImageBase[1]) else '(no image-level filename field)'
  for (form in unique(rows$Form)) {
    vals <- rows$RawImageBase[rows$Form==form]
    out <- c(out,md_row(z$PromptID,z$Model,z$Language,form,ref_summary(vals),target))
  }
}
out <- c(out,'',
  '### Full 120-key filename crosswalk (compact)','',
  'Each row reports the **raw survey aliases**; the match requires that **every canonicalized source-form image filename** equals the image-level filename. Matching names alone do not prove image display by the questionnaire platform.','',
  md_row('Prompt','Model','Prompt language','Raw basenames by source form','Image-level filename','All canonical names match?'),
  md_row(':--',':--',':--',':--',':--',':--'))
for (k in seq_len(nrow(ref_keys))) {
  z <- ref_keys[k,]
  rows <- ref_inventory[ref_inventory$ImageKey == z$ImageKey,,drop=FALSE]
  form_part <- vapply(unique(rows$Form),function(f) {
    paste0(f,': ',ref_summary(rows$RawImageBase[rows$Form==f],max_items=3L))
  },character(1))
  matchrow <- a[a$PromptID==z$PromptID & a$Model==z$Model & a$Language==z$Language,,drop=FALSE]
  target <- if('ImageBase' %in% names(matchrow)) as.character(matchrow$ImageBase[1]) else '(no image-level filename field)'
  found_match <- if('ImageBase' %in% names(matchrow)) all(rows$RatedImageBase==target) else NA
  out <- c(out,md_row(z$PromptID,z$Model,z$Language,paste(form_part,collapse=' ; '),
                       target,if(is.na(found_match)) 'Unknown' else if(found_match) 'Yes' else 'No'))
}
out <- c(out,'',
  '### Examples of ORIGINAL image_name values (before basename normalization)','',
  md_row('Prompt','Model','Prompt language','Form','Original image_name'),
  md_row(':--',':--',':--',':--',':--'))
for (k in head(seq_len(nrow(ref_keys)),6L)) {
  z <- ref_keys[k,];rows <- ref_inventory[ref_inventory$ImageKey==z$ImageKey,,drop=FALSE]
  for(i in seq_len(nrow(rows))) out <- c(out,md_row(z$PromptID,z$Model,z$Language,rows$Form[i],rows$ImageName[i]))
}
out<-c(out,'')
out<-c(out,'## 3. Task-conditioned Qwen−Z-Image gap shifts by rater group','',
  'For each respondent group, scores are averaged within image; reported shifts compare prompt-matched English- and Chinese-generated images. Raters are NOT paired across languages.','',
  md_row('Task subset','Group','Complete prompts','CN Q−Z','EN Q−Z','EN−CN change'),
  md_row(':--',':--','--:','--:','--:','--:'))
for(nm in names(subgroups)) for(g in c('Teachers','Students')) {
  z<-sg_lang[sg_lang$Group==g & sg_lang$PromptID %in% subgroups[[nm]],,drop=FALSE]
  complete<-z[is.finite(z$Shift) & is.finite(z[['QminusZ.Chinese']]) & is.finite(z[['QminusZ.English']]),,drop=FALSE]
  if(nrow(complete)) {
    out<-c(out,md_row(nm,g,paste0(nrow(complete),'/',length(subgroups[[nm]])),
      sign_fmt(mean(complete[['QminusZ.Chinese']])),sign_fmt(mean(complete[['QminusZ.English']])),
      sign_fmt(mean(complete$Shift))))
  } else out<-c(out,md_row(nm,g,paste0('0/',length(subgroups[[nm]])),'—','—','—'))
}
out<-c(out,'','## 4. Research interpretation checks','',
  '- A consistent direction in teacher and student strata would strengthen the **descriptive robustness** of the prompt-condition pattern; it cannot remove confounding from different raters, seeds or images.',
  '- Raw and image-level SCORE aggregates agree. After removing questionnaire-item filename prefixes, all source-form image references are compared to the image-level workbook; any canonical mismatches are explicitly reported above.',
  '- An exact filename match across Excel tables does not itself verify original questionnaire display. Survey export or original image-to-question mapping remains the definitive evidence.',
  '- Distinct respondent IDs are unique only **within source form and prompt-language condition** unless respondent identity linkage is independently established.',
  '- The current workbook is a cleaned rating source; this audit does not independently prove the original response-exclusion rules.',
  '')
if(!unit_complete || length(ambiguous_image_keys) || nrow(within_form_multi) ||
    nrow(mismatches) || nrow(mismatched_img) ||
    (!is.na(match_img_count) && match_img_count!=120L) ||
    count_cn!=191L || count_en!=200L) {
  out<-c(out,'**Audit status:** one or more checks require investigation before elevating cross-language comparisons to primary evidence.','')
} else out<-c(out,'**Audit status:** source spreadsheets pass these internal consistency checks; original questionnaire display provenance remains a separate step.','')
dir.create(dirname(OUTPUT_MD),recursive=TRUE,showWarnings=FALSE)
writeLines(enc2utf8(out),OUTPUT_MD,useBytes=TRUE)
message('Report: ',OUTPUT_MD)
message('Questionnaires CN/EN: ',count_cn,' / ',count_en,
        '; score mismatches: ',nrow(mismatches),
        '; raw cross-form alias keys: ',length(raw_alias_keys),
        '; canonical conflict keys: ',length(ambiguous_image_keys),
        '; within-form multi-reference cells: ',nrow(within_form_multi))
