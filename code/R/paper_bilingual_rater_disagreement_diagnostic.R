#!/usr/bin/env Rscript
# Compare teacher/student prompt-level Qwen-Z-Image gaps across prompt languages.
# Input: data/source/subjective_scores_human120.xlsx
# Output: data/paper_results/Bilingual_rater_disagreement_diagnostic.md
# The two language conditions were rated by separate respondent groups.

options(stringsAsFactors=FALSE)
ROOT <- normalizePath(getwd(),winslash='/',mustWork=TRUE)
RAW_XLSX <- file.path(ROOT, 'data', 'source', 'subjective_scores_human120.xlsx')
OUT_MD <- file.path(ROOT,'data','paper_results','Bilingual_rater_disagreement_diagnostic.md')
fail <- function(...) stop(paste0(...),call.=FALSE)
if (!requireNamespace('readxl',quietly=TRUE)) fail('Install readxl first: install.packages("readxl")')
if (!file.exists(RAW_XLSX)) fail('Input workbook not found: ',RAW_XLSX)

norm_col <- function(x) gsub('[^a-z0-9]','',tolower(x))
required_field <- function(z, aliases, sh) {
  k <- match(norm_col(aliases),norm_col(names(z)))
  k <- k[!is.na(k)]
  if (!length(k)) fail('Column not found in ',sh,': ',paste(aliases,collapse='/'),
                       '; available: ',paste(names(z),collapse=', '))
  z[[k[1]]]
}
norm_prompt <- function(v) {
  x <- toupper(gsub('[[:space:]]+','',trimws(as.character(v))))
  ok <- grepl('^B0*[0-9]+$',x)
  x[ok] <- paste0('B',as.integer(sub('^B','',x[ok])))
  x
}
norm_model <- function(v) {
  x <- tolower(trimws(as.character(v)))
  is_q <- grepl('qwen',x)
  is_z <- grepl('z[-_ ]?image|zimage',x)
  is_s <- grepl('sdxl|stable.diffusion.xl',x)
  n <- as.integer(is_q)+as.integer(is_z)+as.integer(is_s)
  out <- rep(NA_character_,length(x))
  out[is_q & n==1] <- 'Qwen'
  out[is_z & n==1] <- 'Z-Image'
  out[is_s & n==1] <- 'SDXL'
  out
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
  ok <- n %in% as.character(1:3)
  out[ok] <- paste0('Q',n[ok])
  out
}
norm_context <- function(v) {
  x <- tolower(gsub('[[:space:]_./–—-]+','',trimws(as.character(v))))
  out <- rep(NA_character_,length(x))
  out[grepl('educat|teaching|教学|教育',x)] <- 'Educational'
  out[grepl('general|通用|一般',x)] <- 'General'
  out[grepl('chinesecultur|culturechinese|culturalchinese|中华文化|中国文化|中国传统',x)] <- 'Chinese-cultural'
  out
}
md <- function(...) paste0('| ',paste(gsub('[|\r\n]',' ',as.character(c(...))),collapse=' | '),' |')
fs <- function(x,n=3) ifelse(is.finite(x),sprintf(paste0('%+.',n,'f'),x),'—')
fn <- function(x,n=3) ifelse(is.finite(x),sprintf(paste0('%.',n,'f'),x),'—')

sheets <- c('teacher','student_A','student_B','student_C','student_D')
if (!all(sheets %in% readxl::excel_sheets(RAW_XLSX)))
  fail('Missing worksheets: ',paste(setdiff(sheets,readxl::excel_sheets(RAW_XLSX)),collapse=', '))
raws <- lapply(sheets,function(sh) {
  z <- as.data.frame(readxl::read_excel(RAW_XLSX,sheet=sh))
  data.frame(
    Form=sh,
    Group=if(sh=='teacher') 'Teachers' else 'Students',
    Respondent=trimws(as.character(required_field(z,c('respondent_id','RespondentID','respondent'),sh))),
    PromptID=norm_prompt(required_field(z,c('prompt_id','PromptID','Prompt_ID'),sh)),
    Model=norm_model(required_field(z,c('model','Model','ModelShortName'),sh)),
    Language=norm_language(required_field(z,c('language','Language','PromptLanguage'),sh)),
    Context=norm_context(required_field(z,c('Context','TaskContext','ContextLabel','PromptContext'),sh)),
    Question=norm_question(required_field(z,c('q_no','QuestionNo','QNo','Question'),sh)),
    Score=suppressWarnings(as.numeric(required_field(z,c('score','Score','Rating'),sh))))
})
r <- do.call(rbind,raws)
ids <- paste0('B',1:20)
ok <- !is.na(r$Respondent) & nzchar(r$Respondent) & r$PromptID %in% ids &
      !is.na(r$Model) & !is.na(r$Language) & !is.na(r$Context) &
      !is.na(r$Question) & is.finite(r$Score) & r$Score>=1 & r$Score<=5
if (any(!ok)) fail('Invalid keys/scores/task labels: ',sum(!ok),' rows; do not continue with incomplete data.')
if(nrow(r)!=37440L) fail('Unexpected latest-rating total; expected 37440, got ',nrow(r),'.')
ctx <- unique(r[c('PromptID','Context')])
if(nrow(ctx)!=20L || anyDuplicated(ctx$PromptID) ||
   !identical(as.integer(table(factor(ctx$Context,levels=c('Educational','General','Chinese-cultural')))),c(5L,10L,5L))) {
  fail('PromptID -> Context mapping not unique or not 5/10/5.')
}
# Explicitly scope respondent identities within source form AND language.
r$RaterKey <- paste(r$Form,r$Language,r$Respondent,sep='|')
r$ImageKey <- paste(r$PromptID,r$Model,r$Language,sep='|')
r$UnitKey <- paste(r$RaterKey,r$ImageKey,sep='|')
if(anyDuplicated(paste(r$UnitKey,r$Question,sep='|')))
  fail('Duplicate source-form/respondent/image/question keys.')
qcount <- table(r$UnitKey)
if(any(qcount!=3L) || length(qcount)!=12480L)
  fail('Expected 12480 complete respondent-image Q1/2/3 units.')
counts <- unique(r[c('Form','Language','RaterKey')])
by_form <- aggregate(RaterKey~Form+Language,data=counts,FUN=function(x) length(unique(x)))
get_n <- function(sh,lg) {
  x <- by_form$RaterKey[by_form$Form==sh & by_form$Language==lg]
  if(!length(x)) 0L else x[1]
}
expect <- c(teacher_cn=25,teacher_en=29,A_cn=84,B_cn=82,C_en=88,D_en=83)
actual <- c(teacher_cn=get_n('teacher','Chinese'), teacher_en=get_n('teacher','English'),
            A_cn=get_n('student_A','Chinese'),B_cn=get_n('student_B','Chinese'),
            C_en=get_n('student_C','English'),D_en=get_n('student_D','English'))
if(!identical(as.numeric(actual),as.numeric(expect))) fail('Form respondent counts changed: ',paste(names(actual),actual,collapse=', '))

# Mean each Q1-Q3 inside prompt/image/model/group/language; every available
# respondent contributes once per rated image. The final mean uses equal
# weights for each prompt, NOT a pseudo-sample of all individual question rows.
byq <- aggregate(Score~Group+PromptID+Model+Language+Question,data=r,FUN=mean)
wide <- reshape(byq,idvar=c('Group','PromptID','Model','Language'),timevar='Question',direction='wide')
qcols <- c('Score.Q1','Score.Q2','Score.Q3')
if(!all(qcols %in% names(wide)) || anyNA(wide[qcols])) fail('Incomplete Q1-Q3 mean cells.')
wide$Composite <- rowMeans(wide[qcols])
wide <- wide[wide$Model %in% c('Qwen','Z-Image'),c('Group','PromptID','Model','Language','Composite')]
model_pairs <- reshape(wide,idvar=c('Group','PromptID','Language'),timevar='Model',direction='wide')
if(!all(c('Composite.Qwen','Composite.Z-Image') %in% names(model_pairs)) ||
   nrow(model_pairs)!=80L || anyNA(model_pairs)) fail('Missing Qwen-Z matched model cells: expected 80.')
model_pairs$Gap <- model_pairs[['Composite.Qwen']]-model_pairs[['Composite.Z-Image']]
lang_pairs <- reshape(model_pairs[c('Group','PromptID','Language','Gap')],
                      idvar=c('Group','PromptID'),timevar='Language',direction='wide')
if(nrow(lang_pairs)!=40L || anyNA(lang_pairs)) fail('Missing paired Chinese/English prompt cells: expected 40.')
lang_pairs$Shift <- lang_pairs[['Gap.English']]-lang_pairs[['Gap.Chinese']]
lang_pairs <- merge(lang_pairs,ctx,by='PromptID',all.x=TRUE,sort=FALSE)
if(anyNA(lang_pairs)) fail('Context assignment failed.')

# Same-prompt Teacher-vs-Student shift contrast (NOT paired raters).
teachers <- lang_pairs[lang_pairs$Group=='Teachers',c('PromptID','Context','Gap.Chinese','Gap.English','Shift')]
students <- lang_pairs[lang_pairs$Group=='Students',c('PromptID','Context','Gap.Chinese','Gap.English','Shift')]
names(teachers)[3:5] <- c('T_CN','T_EN','T_Shift')
names(students)[3:5] <- c('S_CN','S_EN','S_Shift')
comp <- merge(teachers,students,by=c('PromptID','Context'),all=TRUE,sort=FALSE)
if(nrow(comp)!=20L || anyNA(comp)) fail('Teacher/student comparisons incomplete.')
comp$StudentMinusTeacher <- comp$S_Shift-comp$T_Shift
comp <- comp[order(as.integer(sub('^B','',comp$PromptID))),]

task_sets <- list('All prompts'=ids,
                  'Educational'=ctx$PromptID[ctx$Context=='Educational'],
                  'General'=ctx$PromptID[ctx$Context=='General'],
                  'Chinese-cultural'=ctx$PromptID[ctx$Context=='Chinese-cultural'])
# Descriptive t interval is for the selected prompt IDs, and should not be
# interpreted as causal or population-representative task uncertainty.
stat <- function(v) {
  v <- as.numeric(v)
  n <- length(v)
  if(n<2 || any(!is.finite(v))) fail('Cannot summarize missing/insufficient prompt shifts.')
  mu <- mean(v); s <- sd(v)
  hw <- qt(0.975,df=n-1)*s/sqrt(n)
  tt <- if(s==0) NA_real_ else 2*pt(-abs(mu/(s/sqrt(n))),df=n-1)
  lo <- vapply(seq_len(n),function(i) mean(v[-i]),numeric(1))
  list(n=n,mu=mu,lwr=mu-hw,upr=mu+hw,p=tt,
       loo_low=min(lo),loo_high=max(lo),positive=sum(v>0),negative=sum(v<0),zero=sum(v==0))
}
headings <- c(
 '# Teacher–student disagreement in prompt-language-condition model gaps','',
 '**Purpose:** One-time internal decision diagnostic, not a paper result table.','',
 '**Design:** All questionnaires are Chinese-language. Chinese/English refers to generation-prompt language. The language conditions were rated by different respondents; teacher/student strata are also not matched across languages. One scored image per PromptID × Model × language.','',
 '**Interpretation:** Every row below uses PromptID as the equal-weight comparison unit. The t intervals and nominal p-values are exploratory descriptions for a small, designed prompt set; no confirmatory claims and no multiple-testing correction. Leave-one-prompt-out (LOO) ranges are internal sensitivity checks, not external validation.','',
 '## 1. Data checks','',
 paste0('- Latest Q1–Q3 score records: **',nrow(r),'**.'),
 paste0('- Complete respondent–image units: **',length(qcount),'**; duplicate respondent–image–question keys: **0**.'),
 paste0('- Teachers: Chinese **',get_n('teacher','Chinese'),'**, English **',get_n('teacher','English'),'** source-form questionnaires.'),
 paste0('- Students: Chinese **',get_n('student_A','Chinese')+get_n('student_B','Chinese'),
        '**, English **',get_n('student_C','English')+get_n('student_D','English'),'** source-form questionnaires.'),
 '- Task mapping: 20 B1–B20 prompts, Educational=5, General=10, Chinese-cultural=5; complete 2 rater groups × 20 prompts × 2 prompt-language comparisons.','',
 '## 2. Group-specific Qwen − Z-Image model gaps','',
 'Shift = (English Qwen−Z gap) − (Chinese Qwen−Z gap). Positive values indicate a larger Qwen lead *in the English-rated image condition*; they are NOT causal language effects.','',
 md('Task','Group','Prompts','CN Q−Z','EN Q−Z','Shift EN−CN'),
 md(':--',':--','--:','--:','--:','--:')
)
for(task in names(task_sets)) for(group in c('Teachers','Students')) {
  d <- lang_pairs[lang_pairs$Group==group & lang_pairs$PromptID %in% task_sets[[task]],]
  if(nrow(d)!=length(task_sets[[task]])) fail('Incomplete task/group cells: ',task,' / ',group)
  headings <- c(headings,md(task,group,nrow(d),fs(mean(d[['Gap.Chinese']])),
                            fs(mean(d[['Gap.English']])),fs(mean(d$Shift))))
}
headings <- c(headings,'', '## 3. Prompt-level uncertainty and internal stability','',
 'Nominal t intervals/p-values use the prompt-level shift vector (n=5/10/20); a small interval does not make a language-effect claim valid.','',
 md('Task','Quantity','n','Mean','Nominal 95% t interval','Nominal p','LOO mean range','Positive / Negative'),
 md(':--',':--','--:','--:','--:','--:','--:','--:'))
for(task in names(task_sets)) {
  idx <- comp$PromptID %in% task_sets[[task]]
  for(z in list(list(label='Teacher shift',value=comp$T_Shift[idx]),
                list(label='Student shift',value=comp$S_Shift[idx]),
                list(label='Student − Teacher',value=comp$StudentMinusTeacher[idx]))) {
    s <- stat(z$value)
    p_str <- if(is.na(s$p)) '—' else sprintf('%.4f',s$p)
    headings <- c(headings,md(task,z$label,s$n,fs(s$mu),
                              paste0('[',fs(s$lwr),', ',fs(s$upr),']'),p_str,
                              paste0('[',fs(s$loo_low),', ',fs(s$loo_high),']'),
                              paste(s$positive,s$negative,sep=' / ')))
  }
}
headings <- c(headings,'', '## 4. Educational: every PromptID (n=5)','',
 'Teacher and student columns are changes in Qwen−Z gap (English minus Chinese).','',
 md('Prompt','Teacher CN gap','Teacher EN gap','Teacher shift','Student CN gap','Student EN gap','Student shift','Student − Teacher shift'),
 md(':--','--:','--:','--:','--:','--:','--:','--:'))
for(i in which(comp$Context=='Educational')) {
  z <- comp[i,]
  headings <- c(headings,md(z$PromptID,fs(z$T_CN),fs(z$T_EN),fs(z$T_Shift),
                             fs(z$S_CN),fs(z$S_EN),fs(z$S_Shift),fs(z$StudentMinusTeacher)))
}
headings <- c(headings,'', '## 5. General: every PromptID (n=10)','',
 md('Prompt','Teacher shift','Student shift','Student − Teacher shift'),
 md(':--','--:','--:','--:'))
for(i in which(comp$Context=='General')) {
  z <- comp[i,]
  headings <- c(headings,md(z$PromptID,fs(z$T_Shift),fs(z$S_Shift),fs(z$StudentMinusTeacher)))
}
headings <- c(headings,'','## 6. What the report can and cannot establish','',
 '- It describes within-rater-type consistency and prompt sensitivity, not the cause of teacher/student disagreement. Rater identity, rating severity, different rendered seeds and differing image content remain potential explanations.',
 '- The model-gap comparison within each group uses the same PromptID for Qwen and Z-Image; it does NOT mean a given respondent necessarily rated both models.',
 '- An Educational teacher/student discrepancy with a wide prompt-level interval should remain a limitation or exploratory note, not a new RQ.',
 '- Any cross-language deployment choice should remain a threshold-dependent illustration and should not be termed an empirically validated optimal policy.',
 '- Do not treat 20 prompts or 391 questionnaires as independent replicates of the task × language × model interaction.',
 '- Earlier 269-questionnaire historical snapshot is not read and is not a comparison cohort.','')
dir.create(dirname(OUT_MD),showWarnings=FALSE,recursive=TRUE)
writeLines(enc2utf8(headings),OUT_MD,useBytes=TRUE)
message('Diagnostic written: ',OUT_MD)
message('Data checks: ',nrow(r),' Q-score rows, ',length(qcount),' complete rater-image units, 20 prompts.')
message('Educational teacher/student shift means: ',fs(mean(comp$T_Shift[comp$Context=='Educational'])),
        ' / ',fs(mean(comp$S_Shift[comp$Context=='Educational'])))
