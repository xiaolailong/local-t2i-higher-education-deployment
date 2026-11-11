#!/usr/bin/env Rscript
# Table 1: tested model configurations and evaluation design.
# Input: data/source/subjective_scores_human120_image_level.xlsx
# Output: data/paper_results/Table1_study_design.md

options(stringsAsFactors = FALSE)

REPO_ROOT <- normalizePath(getwd(), winslash = '/', mustWork = TRUE)
SCORES_XLSX <- file.path(REPO_ROOT, 'data', 'source', 'subjective_scores_human120_image_level.xlsx')
RESULT_MD <- file.path(REPO_ROOT, 'data', 'paper_results', 'Table1_study_design.md')

fail <- function(...) stop(paste0(...), call. = FALSE)
if (!requireNamespace('readxl', quietly = TRUE)) {
  fail('Package readxl is required. Install: install.packages("readxl")')
}
normalized <- function(x) tolower(gsub('[^A-Za-z0-9]', '', x))
find_col <- function(d, names) {
  field <- normalized(colnames(d))
  for (n in normalized(names)) {
    hit <- which(field == n)
    if (length(hit)) return(colnames(d)[hit[1]])
  }
  NULL
}
model_short <- function(x) {
  z <- tolower(trimws(as.character(x)))
  a <- grepl('qwen',z)
  b <- grepl('z[-_ ]?image|zimage',z)
  c <- grepl('sdxl|stable.diffusion.xl',z)
  n <- as.integer(a)+as.integer(b)+as.integer(c)
  y <- rep(NA_character_,length(z))
  y[a & n==1L] <- 'Qwen'
  y[b & n==1L] <- 'Z-Image'
  y[c & n==1L] <- 'SDXL'
  y
}
language_code <- function(x) {
  z <- tolower(gsub('[[:space:]_]+','', trimws(as.character(x))))
  o <- rep(NA_character_, length(z))
  o[z %in% c('chinese','ch','cn','zh','zh-cn','zh-hans','zhcn',
             '中文','中文提示词','汉语','chineselanguage','chineseprompt')] <- 'Chinese'
  o[z %in% c('english','en','eng','en-us','en-gb','enus',
             '英文','英文提示词','英语','englishlanguage','englishprompt')] <- 'English'
  o
}
normalize_id <- function(x) {
  z <- toupper(gsub('[[:space:]]','',trimws(as.character(x))))
  b <- grepl('^B0*[0-9]+$',z)
  z[b] <- paste0('B',as.integer(sub('^B','',z[b])))
  z
}
ids <- paste0('B',1:20)
get_id_col <- function(d) find_col(d,c('PromptID','Prompt_ID','PromptCode','PromptNumber','PromptNo'))
find_chinese <- function(d, idcol) {
  isB <- normalize_id(d[[idcol]]) %in% ids
  if (sum(isB)==60L) return(which(isB))
  if (sum(isB)!=120L) fail('Rated workbook: expected 60 Chinese or 120 bilingual B image rows; observed ',sum(isB))
  j <- which(grepl('language|^lang|lang$|locale',normalized(names(d))))
  j <- c(j,setdiff(seq_along(d),j))
  found <- list()
  for (k in setdiff(j,match(idcol,names(d)))) {
    x <- language_code(d[[k]])
    if (sum(x[isB]=='Chinese',na.rm=TRUE)==60 &&
        sum(x[isB]=='English',na.rm=TRUE)==60 && all(!is.na(x[isB]))) {
      found[[names(d)[k]]] <- which(isB & !is.na(x) & x=='Chinese')
    }
  }
  if (!length(found)) fail('Cannot identify 60 Chinese images from 120 rows. Source fields: ', paste(names(d),collapse=', '))
  if (!all(vapply(found,function(x) identical(x,found[[1]]),logical(1)))) fail('Language columns conflict in human ratings.')
  found[[1]]
}

if (!file.exists(SCORES_XLSX)) fail('Latest Chinese rating workbook missing: ',SCORES_XLSX)
raw <- as.data.frame(readxl::read_excel(SCORES_XLSX,sheet='image_level'))
idcol <- get_id_col(raw)
modcol <- find_col(raw,c('ModelShortName','ModelShort','ModelName','Model'))
scorecol <- find_col(raw,c('CompositeScore','MeanCompositeScore','Composite','CompositeMean'))
if (is.null(idcol) || is.null(modcol) || is.null(scorecol)) {
  fail('Cannot find PromptID, model, CompositeScore in latest rating workbook. Columns: ',paste(names(raw),collapse=', '))
}
raw <- raw[find_chinese(raw,idcol),,drop=FALSE]
validated <- data.frame(PromptID=normalize_id(raw[[idcol]]),
                        Model=model_short(raw[[modcol]]),
                        CompositeScore=suppressWarnings(as.numeric(raw[[scorecol]])))
if (nrow(validated)!=60 || anyNA(validated) || anyDuplicated(validated[c('PromptID','Model')]) ||
    !setequal(validated$PromptID,ids)) fail('Expected exactly 60 Chinese images: 20 B prompts x 3 models.')
model_order <- c('Qwen','Z-Image','SDXL')
for (m in model_order) {
  if (sum(validated$Model==m)!=20L) fail('Wrong Chinese image count for ',m)
}
expected <- c(Qwen=4.1344,`Z-Image`=4.0030,SDXL=2.9123)
for (m in model_order) {
  if (abs(mean(validated$CompositeScore[validated$Model==m])-expected[m])>0.005)
    fail('Latest Chinese rating version check failed for ',m)
}

tested <- data.frame(
  Configuration = c('Qwen-Image-2512', 'Z-Image-Turbo', 'SDXL-Turbo'),
  Loading = c('GGUF diffusion model, GGUF text encoder, and VAE',
              'Separate diffusion model, text encoder, and VAE',
              'Single-checkpoint loading'),
  Quantized = c('Yes', 'No', 'No'),
  Resolution = c('1024 × 1024', '768 × 768', '768 × 768'),
  Steps = c(28L, 9L, 4L),
  stringsAsFactors = FALSE
)
stopifnot(nrow(tested)==3L, !anyNA(tested), setequal(model_order,c('Qwen','Z-Image','SDXL')))

markdown <- c(
  '# Table 1. Tested generation configurations and Chinese evaluation design',
  '',
  '### Panel A. Image-generation configurations',
  '',
  '| Configuration | Model loading | Quantized | Resolution (px) | Sampling steps |',
  '|:--|:--|:--:|--:|--:|'
)
for (i in seq_len(nrow(tested))) {
  markdown <- c(markdown, sprintf('| %s | %s | %s | %s | %d |',
    tested$Configuration[i], tested$Loading[i], tested$Quantized[i],
    tested$Resolution[i], tested$Steps[i]))
}
markdown <- c(markdown,
  '',
  '### Panel B. Chinese human-evaluation design',
  '',
  '| Study component | Setting |',
  '|:--|:--|',
  '| GPU | NVIDIA GeForce RTX 5060 Ti (16 GB VRAM) |',
  '| Software environment | Ubuntu 24.04.3 LTS; ComfyUI 0.14.1 |',
  '| Prompt set | 20 Chinese prompts (B1–B20) |',
  '| Generated images | 180 (three seeds for each prompt–model combination) |',
  '| Human-rated images | 60 (one image randomly selected per prompt–model combination) |',
  '| Rating dimensions | Prompt adherence, image clarity, and aesthetics (1–5 scale) |',
  '| Task context | Educational (5); General (10); Chinese-cultural (5) |',
  '| Content classification | Object–Concept (6); other content (14) |'
)
dir.create(dirname(RESULT_MD), recursive=TRUE, showWarnings=FALSE)
writeLines(markdown, RESULT_MD, useBytes=TRUE)
message('Table 1 Markdown written: ',normalizePath(RESULT_MD,winslash='/'))
message('Validated: latest Chinese ratings = 20 PromptID x 3 models = 60 image means.')
message('Reported generation settings: Qwen 1024/28; Z-Image 768/9; SDXL 768/4.')
message('No supplementary files created; source metadata kept in script comments.')
