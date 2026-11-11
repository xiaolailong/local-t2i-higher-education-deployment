#!/usr/bin/env Rscript
# Figure 4: contrastive examples selected from the rated images.
# Inputs: project data/source/ workbooks and original subjective Chinese benchmark images.
# Outputs: data/paper_results/ and outputs/paper_figures/.

options(stringsAsFactors = FALSE)

REPO_ROOT <- normalizePath(getwd(), winslash = '/', mustWork = TRUE)
SUBJECTIVE_XLSX <- file.path(REPO_ROOT, 'data', 'source', 'subjective_scores_human120_image_level.xlsx')
PROMPTS_XLSX <- file.path(REPO_ROOT, 'data', 'source', 'prompts.xlsx')
FIG3_METADATA_CSV <- file.path(REPO_ROOT, 'data', 'paper_results', 'Fig3_task_model_prompt_scores.csv')
VERIFIED_IMAGE_MANIFEST <- file.path(REPO_ROOT, 'data', 'paper_results', 'Fig4_verified_image_manifest.csv')
ALLOW_UNVERIFIED_FILENAME_MATCH <- FALSE
FIG_WIDTH_MM <- 178
FIG_HEIGHT_MM <- 165
PNG_DPI <- 600

result_dir <- file.path(REPO_ROOT, 'data', 'paper_results')
figure_dir <- file.path(REPO_ROOT, 'outputs', 'paper_figures')
dir.create(result_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
fail <- function(...) stop(paste0(...), call. = FALSE)
if (!requireNamespace('readxl', quietly = TRUE)) {
  fail('Missing package readxl. Install: install.packages("readxl")')
}

normal_col <- function(x) tolower(gsub('[^a-zA-Z0-9]', '', x))
find_col <- function(d, names_expected) {
  n <- normal_col(names(d))
  for (candidate in normal_col(names_expected)) {
    ii <- which(n == candidate)
    if (length(ii)) return(names(d)[ii[1]])
  }
  NULL
}
normalize_id <- function(x) {
  z <- toupper(gsub('[[:space:]]', '', trimws(as.character(x))))
  b <- grepl('^B0*[0-9]+$', z)
  z[b] <- paste0('B', as.integer(sub('^B', '', z[b])))
  z
}
prompt_ids <- paste0('B', 1:20)
prompt_col <- function(d) {
  p <- find_col(d, c('PromptID', 'Prompt_ID', 'PromptCode', 'PromptNo', 'PromptNumber'))
  if (!is.null(p)) return(p)
  counts <- vapply(d, function(x) sum(normalize_id(x) %in% prompt_ids, na.rm = TRUE), integer(1))
  good <- which(counts >= 15L)
  if (length(good) == 1L) return(names(d)[good])
  NULL
}
model_label <- function(x) {
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
  res <- rep(NA_character_, length(z))
  ch <- c('chinese','ch','cn','zh','zh-cn','zh-hans','zhcn',
          'chineselanguage','chineseprompts','chineseprompt',
          '中文','中文提示词','汉语','汉语提示词')
  en <- c('english','en','eng','en-us','en-gb','enus','engb',
          'englishlanguage','englishprompts','englishprompt',
          '英文','英文提示词','英语','英语提示词')
  res[z %in% ch] <- 'Chinese'
  res[z %in% en] <- 'English'
  res
}
select_chinese_rows <- function(d, pc) {
  is_b <- normalize_id(d[[pc]]) %in% prompt_ids
  n_b <- sum(is_b)
  if (n_b == 60L) return(which(is_b))
  if (n_b != 120L) fail('Expected 60 or 120 B1-B20 scored-image rows; got ', n_b,
                        '. Columns: ', paste(names(d), collapse=', '))
  norm <- normal_col(names(d))
  preferred <- which(grepl('language|^lang|lang$|locale', norm))
  candidates <- setdiff(c(preferred, setdiff(seq_along(d), preferred)), match(pc, names(d)))
  possible <- list()
  for (j in candidates) {
    z <- language_code(d[[j]])
    if (sum(z[is_b] == 'Chinese', na.rm=TRUE) == 60L &&
        sum(z[is_b] == 'English', na.rm=TRUE) == 60L &&
        all(!is.na(z[is_b]))) possible[[names(d)[j]]] <- which(is_b & !is.na(z) & z == 'Chinese')
  }
  if (!length(possible)) fail('Could not safely select Chinese scored images from 120 rows. Fields: ',
                             paste(names(d), collapse=', '))
  vals <- lapply(possible, sort)
  if (!all(vapply(vals, function(x) identical(x, vals[[1]]), logical(1)))) {
    fail('Language columns disagree; no source rows selected.')
  }
  message('Chinese scored records: 60 rows via ', names(possible)[1])
  vals[[1]]
}

if (!file.exists(SUBJECTIVE_XLSX)) fail('Scored workbook missing: ', SUBJECTIVE_XLSX)
if (!'image_level' %in% readxl::excel_sheets(SUBJECTIVE_XLSX)) {
  fail('Required image_level sheet missing in: ', SUBJECTIVE_XLSX)
}
raw_all <- as.data.frame(readxl::read_excel(SUBJECTIVE_XLSX, sheet='image_level'))
pc <- prompt_col(raw_all)
mc <- find_col(raw_all, c('ModelShortName', 'ModelShort', 'ModelName', 'Model'))
cc <- find_col(raw_all, c('CompositeScore', 'MeanCompositeScore', 'Composite', 'CompositeMean'))
ac <- find_col(raw_all, c('MeanAdherence', 'AdherenceMean', 'AvgAdherence', 'Adherence'))
if (any(vapply(list(pc,mc,cc,ac), is.null, logical(1)))) {
  fail('Missing image-level PromptID / ModelShortName / CompositeScore / MeanAdherence. Columns: ',
       paste(names(raw_all), collapse=', '))
}
raw <- raw_all[select_chinese_rows(raw_all, pc), , drop=FALSE]
ratings <- data.frame(PromptID=normalize_id(raw[[pc]]), Model=model_label(raw[[mc]]),
                      CompositeScore=suppressWarnings(as.numeric(raw[[cc]])),
                      MeanAdherence=suppressWarnings(as.numeric(raw[[ac]])),
                      SourceRow=seq_len(nrow(raw)))
ratings <- ratings[ratings$PromptID %in% prompt_ids, ]
if (nrow(ratings) != 60L || anyNA(ratings) ||
    anyDuplicated(ratings[c('PromptID','Model')]) ||
    !setequal(unique(ratings$PromptID), prompt_ids) ||
    any(!(ratings$CompositeScore >= 1 & ratings$CompositeScore <= 5)) ||
    any(!(ratings$MeanAdherence >= 1 & ratings$MeanAdherence <= 5))) {
  fail('Latest Chinese data failed 60-row unique Prompt x Model / 1–5 score validation.')
}
for (m in c('Qwen','Z-Image','SDXL')) {
  if (sum(ratings$Model == m) != 20L) fail('Expected 20 scored images for ', m)
}
expected <- c(Qwen=4.1344, `Z-Image`=4.0030, SDXL=2.9123)
for (m in names(expected)) {
  v <- mean(ratings$CompositeScore[ratings$Model == m])
  if (abs(v - expected[m]) > 0.025) fail('Version check failed for ', m,
                                          ': got ', round(v,4), ', expected near ', expected[m])
}
q <- ratings[ratings$Model == 'Qwen', c('PromptID','CompositeScore')]
z <- ratings[ratings$Model == 'Z-Image', c('PromptID','CompositeScore')]
names(q)[2] <- 'QwenScore'; names(z)[2] <- 'ZImageScore'
gaps <- merge(q, z, by='PromptID', sort=FALSE)
gaps$GapQwenMinusZ <- gaps$QwenScore - gaps$ZImageScore
gaps$PromptNumber <- as.integer(sub('^B','',gaps$PromptID))
gaps <- gaps[order(gaps$PromptNumber), ]
if (sum(gaps$GapQwenMinusZ > 0) != 16L || sum(gaps$GapQwenMinusZ < 0) != 4L) {
  fail('Expected latest Chinese sign pattern 16 positive / 4 negative, observed ',
       sum(gaps$GapQwenMinusZ > 0), ' / ', sum(gaps$GapQwenMinusZ < 0))
}
write.csv(gaps, file.path(result_dir,'Fig4_all_prompt_pair_gaps.csv'), row.names=FALSE, na='')
lead <- gaps[order(-gaps$GapQwenMinusZ, gaps$PromptNumber), ][1, ]
near <- gaps[order(abs(gaps$GapQwenMinusZ), gaps$PromptNumber), ][1, ]
reverse <- gaps[order(gaps$GapQwenMinusZ, gaps$PromptNumber), ][1, ]
if (reverse$GapQwenMinusZ >= 0 || length(unique(c(lead$PromptID,near$PromptID,reverse$PromptID))) != 3L) {
  fail('Contrast cases cannot be selected as three distinct positive/near-zero/negative prompts.')
}
selected <- rbind(lead, near, reverse)
selected$Case <- c('Largest Qwen lead','Closest quality scores','Largest Z-Image lead')
selected$SelectionRule <- c('max signed Qwen-Z composite gap',
                            'min absolute Qwen-Z composite gap',
                            'min signed Qwen-Z composite gap')
selected$Rank <- 1:3
write.csv(selected, file.path(result_dir,'Fig4_case_selection.csv'), row.names=FALSE, na='')
message('Deterministic illustrative selection (latest Chinese composite scores):')
for (i in seq_len(nrow(selected))) {
  message(sprintf('  %s: %s (Qwen-Z = %+.3f)', selected$Case[i],
                  selected$PromptID[i], selected$GapQwenMinusZ[i]))
}

clean_tag <- function(x) {
  z <- tolower(trimws(as.character(x)))
  z <- gsub('[[:space:]_./–—-]+', '', z)
  z
}
context_tag <- function(x) {
  v <- clean_tag(x)
  o <- rep(NA_character_, length(v))
  o[grepl('educat|teaching|教育|教学', v)] <- 'Educational'
  o[grepl('general|通用|一般', v)] <- 'General'
  o[grepl('chinesecultur|culturechinese|culturalchinese|中国文化|中华文化|中国传统', v)] <- 'Chinese-cultural'
  o
}
content_tag <- function(x, allow_non_object = TRUE) {
  v <- clean_tag(x)
  o <- rep(NA_character_, length(v))
  object <- grepl('objectconcept|conceptobject|object|concept|物体|概念', v)
  o[object] <- 'Object–Concept'
  nonblank <- !is.na(v) & nzchar(v) & !(v %in% c('na', 'nan', 'null'))
  if (allow_non_object) o[nonblank & !object] <- 'Other content'
  else o[grepl('othercontent|nonobject|其他内容|其它内容', v)] <- 'Other content'
  o
}
validate_metadata <- function(meta) {
  if (!all(c('PromptID', 'Context', 'Content') %in% names(meta))) return(NULL)
  meta$PromptID <- normalize_id(meta$PromptID)
  meta <- meta[meta$PromptID %in% prompt_ids, c('PromptID', 'Context', 'Content'), drop = FALSE]
  if (nrow(meta) < 20) return(NULL)
  meta <- unique(meta)
  if (anyDuplicated(meta$PromptID) || anyNA(meta)) return(NULL)
  if (!setequal(meta$PromptID, prompt_ids)) return(NULL)
  if (!identical(as.integer(table(factor(meta$Context, levels=c('Educational','General','Chinese-cultural')))), c(5L,10L,5L))) return(NULL)
  if (!identical(as.integer(table(factor(meta$Content, levels=c('Object–Concept','Other content')))), c(6L,14L))) return(NULL)
  overlap <- sum(meta$Context == 'Educational' & meta$Content == 'Object–Concept')
  if (overlap != 3L) return(NULL)
  meta[order(as.integer(sub('^B','', meta$PromptID))), , drop = FALSE]
}
metadata_from_prompts_xlsx <- function(path) {
  if (!file.exists(path)) return(NULL)
  for (sheet in readxl::excel_sheets(path)) {
    d <- tryCatch(as.data.frame(readxl::read_excel(path, sheet = sheet)), error = function(e) NULL)
    if (is.null(d)) next
    id <- prompt_col(d)
    context <- find_col(d, c('Context', 'PromptContext', 'ContextLabel', 'ContextCategory', 'ContextGroup', 'TaskContext', 'Scenario', 'ScenarioCategory'))
    content <- find_col(d, c('Content', 'ContentLabel', 'ContentCategory', 'ContentClass', 'ContentGroup', 'PromptContent', 'ContentType', 'SubjectType'))
    if (is.null(id) || is.null(context) || is.null(content)) next
    tmp <- data.frame(PromptID = normalize_id(d[[id]]),
                      Context = context_tag(d[[context]]),
                      Content = content_tag(d[[content]], allow_non_object = TRUE),
                      stringsAsFactors = FALSE)
    checked <- validate_metadata(tmp)
    if (!is.null(checked)) return(checked)
  }
  NULL
}
metadata_from_fig3_csv <- function(path) {
  if (!file.exists(path)) return(NULL)
  d <- tryCatch(read.csv(path, check.names = FALSE, stringsAsFactors = FALSE, fileEncoding='UTF-8-BOM'), error = function(e) NULL)
  if (is.null(d)) return(NULL)
  id <- prompt_col(d)
  if (is.null(id)) return(NULL)
  d$.__Prompt <- normalize_id(d[[id]])
  d <- d[d$.__Prompt %in% prompt_ids, , drop = FALSE]
  if (nrow(d) < 20) return(NULL)
  candidate_cols <- setdiff(names(d)[grepl('context|content|group|task|category|class|type', normal_col(names(d)))], c(id, '.__Prompt'))
  if (!length(candidate_cols)) {
    possibilities <- setdiff(names(d), c(id, '.__Prompt'))
    candidate_cols <- possibilities[vapply(d[possibilities], function(x) is.character(x) && length(unique(stats::na.omit(x))) <= 8L, logical(1))]
  }
  source_context <- rep(NA_character_, 20)
  source_content <- rep(NA_character_, 20)
  for (i in seq_along(prompt_ids)) {
    sub <- d[d$.__Prompt == prompt_ids[i], candidate_cols, drop = FALSE]
    values <- unlist(sub, use.names = FALSE)
    ctx <- unique(na.omit(context_tag(values)))
    ctt <- unique(na.omit(content_tag(values, allow_non_object = FALSE)))
    if (length(ctx) == 1L) source_context[i] <- ctx
    if (length(ctt) == 1L) source_content[i] <- ctt
  }
  validate_metadata(data.frame(PromptID = prompt_ids, Context = source_context, Content = source_content, stringsAsFactors = FALSE))
}
meta <- metadata_from_prompts_xlsx(PROMPTS_XLSX)
if (is.null(meta)) meta <- metadata_from_fig3_csv(FIG3_METADATA_CSV)
selected$TaskLabel <- ''
if (!is.null(meta)) {
  tag_map <- setNames(paste(meta$Context, meta$Content, sep=' · '), meta$PromptID)
  selected$TaskLabel <- unname(tag_map[selected$PromptID])
}

BENCHMARK_IMAGE_ROOT <- 'D:/xujc/科研/文生图模型/data/benchmark/subjective/chinese'
if (!dir.exists(BENCHMARK_IMAGE_ROOT)) {
  fail('Rated-image directory missing: ', BENCHMARK_IMAGE_ROOT,
       '\nCheck BENCHMARK_IMAGE_ROOT; no other image directory will be substituted.')
}
BENCHMARK_IMAGE_ROOT <- normalizePath(BENCHMARK_IMAGE_ROOT, winslash='/', mustWork=TRUE)
message('Searching rated images under: ', BENCHMARK_IMAGE_ROOT)
image_extensions <- '\\.(png|jpg|jpeg|tif|tiff|webp|bmp)$'
all_files <- list.files(BENCHMARK_IMAGE_ROOT, pattern=image_extensions,
                        recursive=TRUE, full.names=TRUE, ignore.case=TRUE)
all_files <- sort(unique(normalizePath(all_files, winslash='/', mustWork=TRUE)))
all_files <- all_files[!grepl('/(thumbnails|thumbs|\\.git)/', tolower(all_files))]
if (!length(all_files)) fail('No image files found under: ', BENCHMARK_IMAGE_ROOT)
message('  Image files indexed: ', length(all_files))

image_fields <- names(raw)[normal_col(names(raw)) %in% normal_col(c(
  'ImagePath','ImageFile','ImageFileName','ImageFilename','ImageName',
  'FilePath','FileName','SourceImage','SourceImagePath','OriginalImagePath',
  'GeneratedImagePath','GenerationPath','ImageLocation','OutputImagePath',
  'ImageID','ImageIdentifier','PictureID','ImageKey'))]

normalize_path_text <- function(x) {
  z <- trimws(as.character(x))
  if (is.na(z) || !nzchar(z) || z %in% c('NA','NULL','NaN')) return('')
  z <- gsub('\\\\', '/', z)
  z <- sub('^"', '', z)
  z <- sub('"$', '', z)
  trimws(z)
}
within_benchmark <- function(path) {
  x <- tolower(gsub('\\\\', '/', path))
  root <- tolower(BENCHMARK_IMAGE_ROOT)
  startsWith(x, paste0(root, '/'))
}

empty_candidates <- function() {
  data.frame(ImagePath=character(), EvidenceField=character(),
             EvidenceValue=character(), MatchMethod=character(),
             Priority=integer(), stringsAsFactors=FALSE)
}
add_candidates <- function(d, paths, field, value, method, priority) {
  if (!length(paths)) return(d)
  paths <- paths[file.exists(paths)]
  if (!length(paths)) return(d)
  paths <- normalizePath(paths, winslash='/', mustWork=TRUE)
  paths <- paths[vapply(paths, within_benchmark, logical(1))]
  if (!length(paths)) return(d)
  rbind(d, data.frame(ImagePath=paths, EvidenceField=field,
                      EvidenceValue=value, MatchMethod=method,
                      Priority=as.integer(priority), stringsAsFactors=FALSE))
}

workbook_candidates <- function(value, field) {
  x <- normalize_path_text(value)
  out <- empty_candidates()
  if (!nzchar(x)) return(out)
  has_ext <- grepl(image_extensions, x, ignore.case=TRUE)
  if (has_ext) {
    direct <- if (file.exists(x)) x else character()
    out <- add_candidates(out, direct, field, x, 'explicit_absolute_path', 5L)

    is_absolute <- grepl('^[[:alpha:]]:/|^/', x)
    if (!is_absolute) {
      rel_candidates <- c(file.path(BENCHMARK_IMAGE_ROOT, x),
                          file.path(REPO_ROOT, x),
                          file.path(REPO_ROOT, 'data', x))
      out <- add_candidates(out, rel_candidates, field, x,
                            'exact_relative_path', 4L)
    }
    xlow <- tolower(x)
    rel <- ''
    if (grepl('/benchmark/', xlow, fixed=TRUE)) {
      rel <- substring(x, regexpr('/benchmark/', xlow, fixed=TRUE)[1] + nchar('/benchmark/'))
    } else if (startsWith(xlow, 'benchmark/')) {
      rel <- substring(x, nchar('benchmark/') + 1L)
    } else if (startsWith(xlow, 'data/benchmark/')) {
      rel <- substring(x, nchar('data/benchmark/') + 1L)
    } else if (!is_absolute && grepl('/', x, fixed=TRUE)) {
      rel <- x
    }
    if (nzchar(rel)) {
      relative_hits <- all_files[endsWith(tolower(all_files), paste0('/', tolower(rel)))]
      out <- add_candidates(out, relative_hits, field, x, 'benchmark_relative_suffix', 3L)
    }
    basename_hits <- all_files[tolower(basename(all_files)) == tolower(basename(x))]
    out <- add_candidates(out, basename_hits, field, x, 'exact_basename', 2L)
  } else if (grepl('^[A-Za-z0-9._-]{2,160}$', x)) {
    stem_hits <- all_files[tolower(tools::file_path_sans_ext(basename(all_files))) == tolower(x)]
    out <- add_candidates(out, stem_hits, field, x, 'exact_image_id', 2L)
  }
  out
}

match_token <- function(p, id) {
  k <- sub('^B','',id)
  grepl(paste0('(^|[^[:alnum:]])b0*', k, '([^[:digit:]]|$)'),
        tolower(p), perl=TRUE)
}
path_model <- function(path) {
  z <- tolower(path)
  out <- character()
  if (grepl('qwen', z)) out <- c(out,'Qwen')
  if (grepl('z[-_ ]?image|zimage', z)) out <- c(out,'Z-Image')
  if (grepl('sdxl|stable.diffusion.xl', z)) out <- c(out,'SDXL')
  unique(out)
}
file_candidate_matches <- function(id, model) {
  hits <- all_files[vapply(all_files, match_token, logical(1), id=id)]
  hits[vapply(hits, function(x) identical(path_model(x), model), logical(1))]
}

manifest <- NULL
if (file.exists(VERIFIED_IMAGE_MANIFEST)) {
  manifest <- tryCatch(read.csv(VERIFIED_IMAGE_MANIFEST, check.names=FALSE,
                                stringsAsFactors=FALSE, fileEncoding='UTF-8-BOM'),
                       error=function(e) fail('Cannot read verified image manifest: ',conditionMessage(e)))
  keys <- c('PromptID','Model','ImagePath')
  if (!all(keys %in% names(manifest))) fail('Verified manifest needs: ',paste(keys,collapse=', '))
  manifest$PromptID <- normalize_id(manifest$PromptID)
  manifest$Model <- model_label(manifest$Model)
  if (anyNA(manifest[keys]) || anyDuplicated(manifest[c('PromptID','Model')])) {
    fail('Verified manifest has missing fields or duplicate PromptID x Model keys.')
  }
  message('Using researcher-confirmed manifest: ', VERIFIED_IMAGE_MANIFEST)
}

all_candidate_details <- list()
source_list <- list()
selected_i <- 0L
for (case_i in seq_len(nrow(selected))) {
  for (m in c('Qwen','Z-Image')) {
    selected_i <- selected_i + 1L
    id <- selected$PromptID[case_i]
    r <- ratings[ratings$PromptID == id & ratings$Model == m, ]
    if (nrow(r) != 1L) fail('Missing selected rated image: ', id, ' / ', m)
    candidates <- empty_candidates()
    workbook_values <- character()
    for (field in image_fields) {
      value <- normalize_path_text(raw[[field]][r$SourceRow])
      if (!nzchar(value)) next
      workbook_values <- c(workbook_values, paste0(field, '=', value))
      candidates <- rbind(candidates, workbook_candidates(value, field))
    }
    has_workbook_hit <- nrow(candidates) > 0L
    if (!has_workbook_hit) {
      matches <- file_candidate_matches(id, m)
      candidates <- add_candidates(candidates, matches, '', '',
                                   'prompt_model_filename_only', 1L)
    }
    manifest_used <- FALSE
    if (!is.null(manifest)) {
      mm <- manifest[manifest$PromptID == id & manifest$Model == m, ]
      if (nrow(mm) == 1L) {
        value <- normalize_path_text(mm$ImagePath)
        approved <- workbook_candidates(value, 'VERIFIED_MANIFEST')
        if (!nrow(approved)) {
          fail('Verified manifest file missing from benchmark or not locatable: ',
               id, ' / ', m, ' -> ', value)
        }
        candidates <- approved
        manifest_used <- TRUE
      }
    }
    if (nrow(candidates)) {
      candidates <- candidates[order(-candidates$Priority, candidates$ImagePath,
                                     candidates$EvidenceField), , drop=FALSE]
      candidates <- candidates[!duplicated(candidates$ImagePath), , drop=FALSE]
      candidates$MD5 <- unname(as.character(tools::md5sum(candidates$ImagePath)))
      candidates$Bytes <- as.numeric(file.info(candidates$ImagePath)$size)
    } else {
      candidates$MD5 <- character()
      candidates$Bytes <- numeric()
    }
    candidates$PromptID <- rep(id, nrow(candidates))
    candidates$Model <- rep(m, nrow(candidates))
    candidates$CaseRank <- rep(case_i, nrow(candidates))
    all_candidate_details[[selected_i]] <- candidates

    best <- if (nrow(candidates)) candidates[candidates$Priority == max(candidates$Priority), , drop=FALSE] else candidates
    chosen <- ''
    status <- 'REVIEW_REQUIRED'
    method <- 'unresolved'
    if (nrow(best)) {
      distinct_content <- length(unique(best$MD5))
      if (distinct_content == 1L && (manifest_used || has_workbook_hit || ALLOW_UNVERIFIED_FILENAME_MATCH)) {
        chosen <- best$ImagePath[1]
        status <- 'RESOLVED'
        method <- if (manifest_used) 'verified_manifest' else if (nrow(best) > 1L) {
          'workbook_match_identical_copies'
        } else if (has_workbook_hit) {
          paste0('workbook_', best$MatchMethod[1])
        } else 'filename_only_user_confirmed'
      } else {
        method <- if (distinct_content > 1L) 'conflicting_distinct_images' else 'unverified_filename_only'
      }
    }
    source_list[[selected_i]] <- data.frame(
      CaseRank=case_i, Case=selected$Case[case_i], PromptID=id,
      Model=m, CompositeScore=r$CompositeScore,
      GapQwenMinusZ=selected$GapQwenMinusZ[case_i],
      SourceMethod=method, CandidateCount=nrow(candidates),
      DistinctMD5=length(unique(candidates$MD5)), Status=status,
      ResolvedImagePath=chosen,
      CandidatePaths=paste(candidates$ImagePath, collapse=' ; '),
      WorkbookImageValues=paste(workbook_values, collapse=' ; '),
      BenchmarkRoot=BENCHMARK_IMAGE_ROOT, stringsAsFactors=FALSE)
  }
}
source_rows <- do.call(rbind, source_list)
candidate_details <- do.call(rbind, all_candidate_details)
write.csv(source_rows, file.path(result_dir,'Fig4_image_match_audit.csv'),
          row.names=FALSE, na='')
write.csv(candidate_details, file.path(result_dir,'Fig4_image_candidate_details.csv'),
          row.names=FALSE, na='')
if (any(source_rows$Status != 'RESOLVED')) {
  pending <- source_rows[source_rows$Status != 'RESOLVED', ]
  message('\nOriginal-image mapping still needs verification:')
  for (i in seq_len(nrow(pending))) {
    message('  ', pending$PromptID[i], ' / ',pending$Model[i], ': ',
            pending$CandidateCount[i], ' benchmark candidate(s); ',
            pending$DistinctMD5[i], ' different file content hash(es); ',
            pending$SourceMethod[i])
  }
  fail('Figure 4 not rendered: distinct image candidates cannot be assumed identical.\n',
       'See data/paper_results/Fig4_image_match_audit.csv and ',
       'Fig4_image_candidate_details.csv.\n',
       'The workbook reference must identify the exact rated image; never choose ',
       'a different seed or crop just because the filename resembles the PromptID.')
}
if (any(!file.exists(source_rows$ResolvedImagePath))) fail('Approved image source not found.')
if (anyDuplicated(source_rows$ResolvedImagePath)) fail('Two rated records map to the same image file.')
source_rows$MD5 <- unname(as.character(tools::md5sum(source_rows$ResolvedImagePath)))
source_rows$Bytes <- as.numeric(file.info(source_rows$ResolvedImagePath)$size)
write.csv(source_rows, file.path(result_dir,'Fig4_verified_image_sources.csv'),
          row.names=FALSE, na='')

read_original_image <- function(path) {
  ext <- tolower(tools::file_ext(path))
  if (requireNamespace('magick', quietly=TRUE)) {
    im <- magick::image_read(path)
    return(as.raster(im))
  }
  if (ext == 'png') {
    if (!requireNamespace('png',quietly=TRUE)) fail('To read PNG images: install.packages("png")')
    return(as.raster(png::readPNG(path)))
  }
  if (ext %in% c('jpg','jpeg')) {
    if (!requireNamespace('jpeg',quietly=TRUE)) fail('To read JPEG images: install.packages("jpeg")')
    return(as.raster(jpeg::readJPEG(path)))
  }
  if (ext %in% c('tif','tiff') && requireNamespace('tiff',quietly=TRUE)) {
    return(as.raster(tiff::readTIFF(path)))
  }
  fail('Image extension ',ext,' requires magick (install.packages("magick")) or a supported raster reader.')
}
images <- lapply(source_rows$ResolvedImagePath,read_original_image)

W <- FIG_WIDTH_MM; H <- FIG_HEIGHT_MM
palette <- c('Qwen'='#526E90','Z-Image'='#4C968C')
ink <- '#283D4B'; muted <- '#637783'; rule <- '#D6DFE3'
case_accents <- c('#526E90','#89979E','#4C968C')
cols_x <- c(8,64,120); panel_w <- 50
mm <- function(v) grid::unit(v,'mm')
pt <- function(v) grid::unit(v,'pt')
xy <- function(x, top) list(x=mm(x),y=mm(H-top))
label <- function(txt,x,y,size=8.5,col=ink,bold=FALSE,just='left') {
  z <- xy(x,y)
  grid::grid.text(txt,x=z$x,y=z$y,just=c(just,'centre'),
                  gp=grid::gpar(col=col,fontsize=size,fontface=if(bold)'bold' else 'plain',fontfamily='sans'))
}
line <- function(x0,top0,x1,top1,col=rule,width=.5) {
  grid::grid.lines(x=mm(c(x0,x1)),y=mm(H-c(top0,top1)),
                   gp=grid::gpar(col=col,lwd=width))
}
rect <- function(x, top, width,height, fill='#FAFBFB',stroke=rule,lwd=.5) {
  grid::grid.rect(x=mm(x+width/2),y=mm(H-top-height/2),
                  width=mm(width),height=mm(height),
                  gp=grid::gpar(fill=fill,col=stroke,lwd=lwd))
}
image_panel <- function(im,x,top,w=panel_w,h=47.5) {
  rect(x,top,w,h,fill='#F7F9F9')
  aspect <- ncol(im)/nrow(im)
  if (!is.finite(aspect) || aspect <= 0) fail('Invalid image dimensions.')
  draw_w <- min(w-1.2,(h-1.2)*aspect)
  draw_h <- draw_w/aspect
  grid::grid.raster(im,x=mm(x+w/2),y=mm(H-top-h/2),
                    width=mm(draw_w),height=mm(draw_h),interpolate=TRUE)
}

render_fig <- function() {
  grid::grid.newpage()
  grid::pushViewport(grid::viewport(width=mm(W),height=mm(H)))
  label('THREE AUDITABLE PAIRED EXAMPLES',8,7.2,size=8.2,col=ink,bold=TRUE)
  label('Latest Chinese human ratings',170,7.2,size=7.9,col=muted,just='right')
  line(8,12,170,12,width=.65)
  titles <- c('LARGEST QWEN LEAD','CLOSEST TO A TIE','LARGEST Z-IMAGE LEAD')
  for(i in seq_len(3)) {
    x <- cols_x[i]
    rect(x,17.6,1.6,3.8,fill=case_accents[i],stroke=NA,lwd=0)
    label(titles[i],x+3.7,19.3,size=7.65,col=case_accents[i],bold=TRUE)
    sign_gap <- sprintf('%+.3f',selected$GapQwenMinusZ[i])
    label(paste0(selected$PromptID[i],'   Qwen - Z-Image: ',sign_gap),x,27.0,
          size=8.2,col=ink,bold=TRUE)
    if (nzchar(selected$TaskLabel[i])) {
      label(selected$TaskLabel[i], x, 32.2, size=7.35, col=muted)
    }
    q_image <- images[[(i-1L)*2L+1L]]
    z_image <- images[[(i-1L)*2L+2L]]
    image_panel(q_image,x,36.0)
    label('Qwen',x,87.7,size=8.2,col=palette['Qwen'],bold=TRUE)
    label(sprintf('%.3f / 5',selected$QwenScore[i]),x+panel_w,87.7,
          size=8.25,col=ink,just='right')
    image_panel(z_image,x,96.5)
    label('Z-Image',x,148.2,size=8.2,col=palette['Z-Image'],bold=TRUE)
    label(sprintf('%.3f / 5',selected$ZImageScore[i]),x+panel_w,148.2,
          size=8.25,col=ink,just='right')
  }
  line(8,156.3,170,156.3,width=.65)
  label('Mean CompositeScore (1-5)  |  Paired by PromptID',8,161.0,
        size=8.0,col=muted)
  grid::popViewport()
}
fig_pdf <- file.path(figure_dir,'Fig4_auditable_rated_image_cases.pdf')
fig_png <- file.path(figure_dir,'Fig4_auditable_rated_image_cases.png')
if (capabilities('cairo')) {
  grDevices::cairo_pdf(fig_pdf,width=W/25.4,height=H/25.4,onefile=FALSE)
} else {
  grDevices::pdf(fig_pdf,width=W/25.4,height=H/25.4,onefile=FALSE,useDingbats=FALSE)
}
render_fig(); grDevices::dev.off()
png_args <- list(filename=fig_png,width=W/25.4,height=H/25.4,units='in',res=PNG_DPI)
if(capabilities('cairo')) png_args$type <- 'cairo'
do.call(grDevices::png,png_args)
render_fig(); grDevices::dev.off()
message('\nFigure 4 created:')
message('  ',fig_pdf)
message('  ',fig_png,' (600 dpi)')
message('  Fig4_case_selection.csv, Fig4_verified_image_sources.csv, Fig4_all_prompt_pair_gaps.csv')
message('  Source audit: Fig4_image_match_audit.csv and Fig4_image_candidate_details.csv')
message('  Samples are preselected contrast cases, not representative observations.')
