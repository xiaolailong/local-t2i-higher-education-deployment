#!/usr/bin/env Rscript
# Internal diagnostic: score dispersion, repeated split-half stability,
# and teacher/student comparisons for the 60 Chinese-prompt images.
# Inputs: data/source/subjective_scores_human120*.xlsx
# Outputs: data/paper_results/Rating_reliability_diagnostic.md and image CSV.

options(stringsAsFactors = FALSE)
root <- normalizePath(getwd(), winslash='/', mustWork=TRUE)
raw_file <- file.path(root, 'data', 'source', 'subjective_scores_human120.xlsx')
image_file <- file.path(root, 'data', 'source', 'subjective_scores_human120_image_level.xlsx')
out_dir <- file.path(root,'data','paper_results')
dir.create(out_dir, recursive=TRUE, showWarnings=FALSE)
out_md <- file.path(out_dir,'Rating_reliability_diagnostic.md')
out_csv <- file.path(out_dir,'Rating_reliability_image_summary.csv')

fail <- function(...) stop(paste0(...),call.=FALSE)
if(!requireNamespace('readxl',quietly=TRUE)) fail('Install required package: install.packages("readxl")')
for(f in c(raw_file,image_file)) if(!file.exists(f)) fail('Input file not found: ',f)
canon_col <- function(z) tolower(gsub('[^a-z0-9]','',z))
colname <- function(d, options, where) {
  i <- match(canon_col(options),canon_col(names(d)))
  i <- i[!is.na(i)]
  if(!length(i)) fail('Missing field in ',where,': ',paste(options,collapse=' / '),
                      '; actual: ',paste(names(d),collapse=', '))
  names(d)[i[1]]
}
pick <- function(d,opts,where) d[[colname(d,opts,where)]]
normalize_id <- function(x) {
  z<-toupper(gsub('[[:space:]]+','',trimws(as.character(x))))
  valid<-grepl('^B0*[0-9]+$',z)
  z[valid]<-paste0('B',as.integer(sub('^B','',z[valid])))
  z
}
normalize_model <- function(x) {
  z<-tolower(trimws(as.character(x))); a<-rep(NA_character_,length(z))
  a[grepl('qwen',z)]<-'Qwen'
  a[grepl('z[-_ ]?image|zimage',z)]<-'Z-Image'
  a[grepl('sdxl|stable.diffusion.xl',z)]<-'SDXL'
  a
}
normalize_lang <- function(x) {
  z<-tolower(gsub('[[:space:]_]+','',trimws(as.character(x))))
  a<-rep(NA_character_,length(z))
  a[z %in% c('chinese','ch','cn','zh','zh-cn','zh-hans','中文','中文提示词','汉语','汉语提示词')]<-'Chinese'
  a[z %in% c('english','en','eng','en-us','en-gb','英文','英文提示词','英语','英语提示词')]<-'English'
  a
}
normalize_question <- function(x) {
  z<-toupper(gsub('[[:space:]_.-]+','',as.character(x)))
  z<-sub('^Q([123])$','\\1',z)
  ans<-rep(NA_integer_,length(z))
  good<-z %in% c('1','2','3')
  ans[good]<-as.integer(z[good]); ans
}
fmt <- function(v,dec=3) ifelse(is.finite(v),sprintf(paste0('%.',dec,'f'),v),'NA')
mdrow <- function(...) paste0('| ',paste(as.character(c(...)),collapse=' | '),' |')

# 1. Read respondent scores; Chinese images only, teachers + A/B students.
forms <- c('teacher','student_A','student_B')
if(!all(forms %in% readxl::excel_sheets(raw_file)))
  fail('Expected sheets teacher, student_A, student_B in latest raw workbook.')
parts <- lapply(forms,function(form) {
  d<-as.data.frame(readxl::read_excel(raw_file,sheet=form))
  x<-data.frame(Form=form,
    Rater=trimws(as.character(pick(d,c('respondent_id','RespondentID'),form))),
    PromptID=normalize_id(pick(d,c('prompt_id','PromptID'),form)),
    Model=normalize_model(pick(d,c('model','Model','ModelShortName'),form)),
    Language=normalize_lang(pick(d,c('language','Language'),form)),
    Question=normalize_question(pick(d,c('q_no','QuestionNo','QNo'),form)),
    Score=suppressWarnings(as.numeric(pick(d,c('score','Score','Rating'),form))),
    stringsAsFactors=FALSE)
  x <- x[x$Language=='Chinese' & !is.na(x$Language),,drop=FALSE]
  x
})
raw<-do.call(rbind,parts)
required_prompts<-paste0('B',1:20)
if(anyNA(raw) || any(!raw$PromptID %in% required_prompts) ||
   any(!raw$Model %in% c('Qwen','Z-Image','SDXL')) ||
   any(!raw$Question %in% c(1L,2L,3L)) ||
   any(!is.finite(raw$Score) | raw$Score<1 | raw$Score>5) ||
   any(!nzchar(raw$Rater))) fail('Invalid score, ID, prompt, model, question, or language in latest Chinese source.')
if(nrow(raw)!=18450L) fail('Expected 18,450 Chinese Q1-Q3 rows, got ',nrow(raw),
    '. Verify latest 191 questionnaire source.')
raw$RaterKey<-paste(raw$Form,raw$Rater,sep='|')
raw$ImageKey<-paste(raw$PromptID,raw$Model,sep='|')
raw$ResponseImageKey<-paste(raw$RaterKey,raw$ImageKey,sep='|')
item_key<-paste(raw$ResponseImageKey,raw$Question,sep='|')
if(anyDuplicated(item_key)) fail('Duplicate (rater,image,question) scores exist; stopping instead of deduplicating.')
counts<-table(raw$ResponseImageKey)
if(any(counts!=3L) || length(counts)*3L!=nrow(raw))
  fail('Some respondent-image units do not contain exactly Q1/Q2/Q3.')
# Explicit Q1/Q2/Q3 pivot.  IMPORTANT: a default reshape() call would
# also pivot Form/PromptID/Model/ImageKey into .1/.2/.3 columns.  Subsequent
# code expects their unsuffixed invariant names; that was the source of the
# Keep image and respondent metadata from Q1, then key-match scores Q2/Q3.
meta_cols <- c('Form','RaterKey','PromptID','Model','ImageKey','ResponseImageKey')
q1 <- raw[raw$Question==1L, c(meta_cols,'Score'),drop=FALSE]
q2 <- raw[raw$Question==2L, c('ResponseImageKey','Score'),drop=FALSE]
q3 <- raw[raw$Question==3L, c('ResponseImageKey','Score'),drop=FALSE]
if(nrow(q1)!=length(unique(raw$ResponseImageKey)) ||
   !setequal(q1$ResponseImageKey,q2$ResponseImageKey) ||
   !setequal(q1$ResponseImageKey,q3$ResponseImageKey))
  fail('Q1/Q2/Q3 respondent-image keys differ unexpectedly.')
wide <- q1[,meta_cols,drop=FALSE]
wide[['Score.1']] <- q1$Score
wide[['Score.2']] <- q2$Score[match(wide$ResponseImageKey,q2$ResponseImageKey)]
wide[['Score.3']] <- q3$Score[match(wide$ResponseImageKey,q3$ResponseImageKey)]
needed<-c('Score.1','Score.2','Score.3')
if(!all(needed %in% names(wide)) || anyNA(wide[,needed]))
  fail('Could not pivot complete Q1/Q2/Q3 scores to respondent-image rows.')
wide$Composite<-rowMeans(wide[,needed]); wide$Adherence<-wide[['Score.1']]
wide$Clarity<-wide[['Score.2']]; wide$Aesthetics<-wide[['Score.3']]
measures<-c('Composite','Adherence','Clarity','Aesthetics')
forms_n<-vapply(forms,function(form)
  length(unique(raw$RaterKey[raw$Form==form])),integer(1))
if(!identical(unname(forms_n),c(25L,84L,82L)))
  fail('Unexpected latest Chinese questionnaire counts: ',
       paste(names(forms_n),forms_n,sep='=',collapse=', '),
       ' (expected teacher=25, A=84, B=82). Check data version.')
if(nrow(wide)!=sum(c(1170L,2520L,2460L)))
  fail('Unexpected latest Chinese respondent-image units: ',nrow(wide),
       '; expected 6150. Check workbook version.')

# 2. All 60 image aggregates, and verify against current authoritative workbook.
images <- expand.grid(PromptID=required_prompts,
                      Model=c('Qwen','Z-Image','SDXL'),stringsAsFactors=FALSE)
images$ImageKey<-paste(images$PromptID,images$Model,sep='|')
observed_image_keys <- unique(wide$ImageKey)
missing_image_keys <- setdiff(images$ImageKey,observed_image_keys)
extra_image_keys <- setdiff(observed_image_keys,images$ImageKey)
if(length(missing_image_keys) || length(extra_image_keys) ||
   length(observed_image_keys)!=60L)
  fail('Expected exactly 60 B1–B20 × model Chinese images. Observed=',
       length(observed_image_keys),
       '; missing=',paste(head(missing_image_keys,10),collapse=','),
       '; extra=',paste(head(extra_image_keys,10),collapse=','))
wide$ImageIndex<-match(wide$ImageKey,images$ImageKey)
by_img<-lapply(seq_len(nrow(images)),function(i) {
  d<-wide[wide$ImageIndex==i,,drop=FALSE]
  o<-images[i,,drop=FALSE]
  o$N_Raters<-nrow(d)
  o$N_Teachers<-sum(d$Form=='teacher')
  o$N_Students<-sum(d$Form!='teacher')
  for(m in measures) {
    o[[paste0(m,'Mean')]]<-mean(d[[m]])
    o[[paste0(m,'SD')]]<-sd(d[[m]])
  }
  o
})
summary<-do.call(rbind,by_img)
expected<-as.data.frame(readxl::read_excel(image_file,sheet='image_level'))
e<-data.frame(
 PromptID=normalize_id(pick(expected,c('PromptID','prompt_id'),'image_level')),
 Model=normalize_model(pick(expected,c('ModelShortName','Model','ModelName'),'image_level')),
 Language=normalize_lang(pick(expected,c('Language','PromptLanguage'),'image_level')),
 Composite=suppressWarnings(as.numeric(pick(expected,c('CompositeScore','MeanCompositeScore'),'image_level'))),
 Adherence=suppressWarnings(as.numeric(pick(expected,c('MeanAdherence','AdherenceMean'),'image_level'))))
e<-e[e$Language=='Chinese' & !is.na(e$Language),,drop=FALSE]
e$ImageKey<-paste(e$PromptID,e$Model,sep='|')
if(nrow(e)!=60L || anyNA(e) || anyDuplicated(e$ImageKey) ||
   !setequal(e$ImageKey,summary$ImageKey)) fail('Image-level workbook not exactly latest 60 Chinese images.')
e<-e[match(summary$ImageKey,e$ImageKey),,drop=FALSE]
dif_comp<-max(abs(summary$CompositeMean-e$Composite))
dif_adh<-max(abs(summary$AdherenceMean-e$Adherence))
if(dif_comp>0.005 || dif_adh>0.005)
  fail('Raw aggregate does not match image_level (Composite diff=',fmt(dif_comp,6),
       ', Adherence diff=',fmt(dif_adh,6),').')
# Save a traceable source table; no in-place edits to original data.
write.csv(summary,out_csv,row.names=FALSE,fileEncoding='UTF-8')

# 3. Teacher–student image-level agreement: same images, independently averaged.
by_group<-lapply(c('teacher','students'),function(group) {
  d<-wide[if(group=='teacher') wide$Form=='teacher' else wide$Form!='teacher',,drop=FALSE]
  z<-data.frame(ImageKey=images$ImageKey)
  for(m in measures) {
    z[[m]]<-vapply(images$ImageKey,function(key) mean(d[[m]][d$ImageKey==key]),numeric(1))
  }
  z
})
teacher<-by_group[[1]]; student<-by_group[[2]]
sets<-c('All60','Qwen','Z-Image','SDXL')
set_idx<-function(x) if(x=='All60') seq_len(60L) else which(images$Model==x)
compare_rows<-do.call(rbind,lapply(sets,function(s) {
  ids<-set_idx(s)
  do.call(rbind,lapply(measures,function(m) {
    x<-teacher[[m]][ids]; y<-student[[m]][ids]
    data.frame(Group=s,Measure=m,N_Images=length(ids),
      R=if(sd(x)>0 && sd(y)>0) cor(x,y) else NA_real_,
      StudentMinusTeacher=mean(y-x),MeanAbsoluteDifference=mean(abs(y-x)))
  }))
}))

# 4. Repeated half-panel image aggregate stability, with form-stratified
#    GLOBAL respondent splits. One respondent always stays in the same half
#    across all of their scored images in a repetition (no pseudo-independence).
#    The half-panel statistic describes aggregate-score consistency, NOT
#    single-rater inter-rater agreement or a formal ICC estimate.
N_REPETITIONS<-400L
set.seed(20261109L)
unique_rater_rows<-unique(wide[,c('Form','RaterKey')])
for_split<-split(unique_rater_rows$RaterKey,unique_rater_rows$Form)
# Each source-form respondent is assigned a half every iteration.
row_rater<-wide$RaterKey
all_rater_keys<-unique(wide$RaterKey)
values<-as.matrix(wide[,measures]); storage.mode(values)<-'double'
reps_list<-vector('list',N_REPETITIONS * length(sets) * length(measures))
rep_row<-0L
for(rep_i in seq_len(N_REPETITIONS)) {
  names_to_halves<-setNames(rep(NA_integer_,length(all_rater_keys)),all_rater_keys)
  for(form in names(for_split)) {
    ids<-for_split[[form]]
    names_to_halves[ids]<-sample(rep(1:2,length.out=length(ids)),length(ids),replace=FALSE)
  }
  h<-as.integer(names_to_halves[row_rater])
  if(anyNA(h)) fail('Internal half assignment failed for rater IDs.')
  k<-(wide$ImageIndex-1L)*2L+h
  ss<-rowsum(values,group=k,reorder=TRUE)
  ns<-tabulate(k,nbins=120L)
  if(any(ns==0)) fail('An image was missing all scores in one random half; see source form structure.')
  mm<-matrix(NA_real_,nrow=120L,ncol=length(measures))
  rownames(mm)<-as.character(seq_len(120L))
  mm[rownames(ss),]<-ss / ns[as.integer(rownames(ss))]
  a<-mm[seq(1L,120L,by=2L),,drop=FALSE]
  b<-mm[seq(2L,120L,by=2L),,drop=FALSE]
  for(s in sets) for(j in seq_along(measures)) {
    idx<-set_idx(s)
    cr<-if(sd(a[idx,j])>0 && sd(b[idx,j])>0)
      cor(a[idx,j],b[idx,j]) else NA_real_
    sb<-if(is.finite(cr) && cr > -0.9999) (2*cr)/(1+cr) else NA_real_
    rep_row<-rep_row+1L
    reps_list[[rep_row]]<-data.frame(Iteration=rep_i,Group=s,
                        Measure=measures[j],HalfCorrelation=cr,
                        SpearmanBrown=sb)
  }
}
reps<-do.call(rbind,reps_list)
ss_rows<-do.call(rbind,lapply(sets,function(s) {
  idx<-set_idx(s)
  do.call(rbind,lapply(measures,function(m) {
    x<-reps$SpearmanBrown[reps$Group==s & reps$Measure==m]
    x<-x[is.finite(x)]
    data.frame(Group=s,Measure=m,Images=length(idx),
      MeanWithinImageSD=mean(summary[[paste0(m,'SD')]][idx]),
      SplitHalfSB_Median=if(length(x)) median(x) else NA_real_,
      SplitHalfSB_P05=if(length(x)) unname(quantile(x,0.05)) else NA_real_,
      SplitHalfSB_P95=if(length(x)) unname(quantile(x,0.95)) else NA_real_,
      ValidIterations=length(x))
  }))
}))

# 5. Write compact analysis report. Outputs are not manuscript-ready claims.
lines<-c(
'# Latest Chinese-prompt human-rating reliability diagnostic',
'',
'**Purpose:** decide whether the revised paper needs a separate rater-quality table. This is a diagnostic, not Table 3 and not an ICC study.',
'',
'**Scope:** latest 191 Chinese-prompt questionnaires only (teacher=25, student_A=84, student_B=82). All questionnaires are in Chinese. Historical 269-version data and English-prompt images are excluded.',
'',
'## 1. Data integrity and coverage',
'',
 paste0('- Raw Q1–Q3 rows: **',nrow(raw),'**; complete respondent–image units: **',nrow(wide),'**.'),
 paste0('- Unique prompts: **',length(unique(wide$PromptID)),'**; models: **',length(unique(wide$Model)),
        '**; rated images: **',nrow(summary),'**.'),
 paste0('- Raw aggregate vs latest image_level maximum discrepancy: composite **',fmt(dif_comp,6),
        '**, adherence **',fmt(dif_adh,6),'**.'),
 paste0('- Raters/image: **',min(summary$N_Raters),'–',max(summary$N_Raters),'**, mean **',fmt(mean(summary$N_Raters),2),'**.'),
 paste0('- Teacher ratings/image: **',min(summary$N_Teachers),'–',max(summary$N_Teachers),'**; students/image: **',
        min(summary$N_Students),'–',max(summary$N_Students),'**.'),
 '',
 '## 2. Image-level score dispersion and repeated half-panel stability',
 '',
 'The SD is the average across images of the within-image SD of **individual raters**. Split-half panels are formed by dividing respondents within each source form and keeping each respondent in the same half for all images; each half then independently computes an image mean. The median Spearman–Brown corrected Pearson correlation estimates how reproducible the image-score *ordering* is under two half-panels. It is **not** individual-rater agreement or a standard ICC. The 5th–95th percentiles describe variation across random splits, **not 95% statistical confidence intervals**.',
 '',
 '| Image set | Dimension | Images | Mean within-image SD | Median corrected split-half r | Random-split P05–P95 | Valid splits |',
 '| :-- | :-- | --: | --: | --: | :-- | --: |'
)
for(i in seq_len(nrow(ss_rows))) {
  z<-ss_rows[i,]
  lines<-c(lines,mdrow(z$Group,z$Measure,z$Images,fmt(z$MeanWithinImageSD),
    fmt(z$SplitHalfSB_Median),paste0('[',fmt(z$SplitHalfSB_P05),', ',fmt(z$SplitHalfSB_P95),']'),
    z$ValidIterations))
}
lines<-c(lines,'',
'## 3. Teacher vs student image-level consistency',
'',
'Both groups independently rated the same 60 canonical Chinese images. The correlations reflect between-image ordering across rated images, not individual agreement. A pooled 60-image correlation may be inflated by between-model differences; therefore within-model comparisons are included.',
'',
'| Image set | Dimension | Images | Teacher–student Pearson r | Student−teacher mean | Mean absolute difference |',
'| :-- | :-- | --: | --: | --: | --: |')
for(i in seq_len(nrow(compare_rows))) {
  z<-compare_rows[i,]
  lines<-c(lines,mdrow(z$Group,z$Measure,z$N_Images,fmt(z$R),
      fmt(z$StudentMinusTeacher),fmt(z$MeanAbsoluteDifference)))
}
lines<-c(lines,'',
'## 4. Interpretation rules',
'',
'- The analysis uses 60 image-level means (20 prompt-matched images per model), not 6,150 independent task instances.',
'- Split-half image-mean consistency measures stability of aggregate ratings; it cannot establish that single raters agree closely.',
'- Mean within-image SD gives a separate measure of absolute rater dispersion.',
'- Compare within-model with pooled results; the latter can look artificially high if SDXL is substantially worse than both other models.',
'- Teacher/student disagreements may reflect rubric interpretation and respondent composition; they do not by themselves invalidate all scores.',
'- No experiment design changes, added questionnaires, generated images, or edits to existing Figure 2–5/Table 1–2 are made.',
'- A concise Methods paragraph plus a supplementary table may be preferable to a new main-text Table 3; decide using these results.',
'',
 '**Traceable image summary:** `data/paper_results/Rating_reliability_image_summary.csv`.')
writeLines(lines,out_md,useBytes=TRUE)
cat('OK: ',out_md,'\n',sep='')
cat('OK: ',out_csv,'\n',sep='')
