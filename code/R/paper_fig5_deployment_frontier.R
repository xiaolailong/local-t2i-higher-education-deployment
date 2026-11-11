#!/usr/bin/env Rscript
# Figure 5: task-conditioned quality screens and generation-time tradeoffs.
# Inputs: data/source/ and the tested configuration's measured resource means.
# Outputs: data/paper_results/ and outputs/paper_figures/.

options(stringsAsFactors = FALSE)

REPO_ROOT <- getwd()  # invoke from the repository root
SUBJECTIVE_XLSX <- file.path(REPO_ROOT, "data", "source", "subjective_scores_human120_image_level.xlsx")
PROMPTS_XLSX <- file.path(REPO_ROOT, "data", "source", "prompts.xlsx")
FIG3_METADATA_CSV <- file.path(REPO_ROOT, "data", "paper_results", "Fig3_task_model_prompt_scores.csv")

ADHERENCE_ITEM_FLOOR <- 3.5
COMPOSITE_MEAN_FLOORS <- c(3.5, 4.0)
POLICY_COVERAGE_LEVELS <- c(0, 0.50, 0.80, 0.90, 1.00)
FIGURE_WIDTH_MM <- 178
FIGURE_HEIGHT_MM <- 158
PNG_DPI <- 600

resource <- data.frame(
  Model = c("Qwen", "Z-Image", "SDXL"),
  Configuration = c("Qwen-Image-2512", "Z-Image-Turbo", "SDXL-Turbo"),
  MeanLatencySeconds = c(167.0, 8.6, 1.0),
  MeanVRAM_MB = c(14172.0, 14274.8, 8452.0),
  Provenance = rep("research_master_20261109.md, section 4.3: verified AM4 mean of tested configuration; not recomputed here", 3)
)

for (pkg in c("readxl", "ggplot2")) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop("Missing R package '", pkg, "'. Install once with install.packages('", pkg, "').", call. = FALSE)
  }
}
suppressPackageStartupMessages(library(ggplot2))

path_result <- file.path(REPO_ROOT, "data", "paper_results")
path_figure <- file.path(REPO_ROOT, "outputs", "paper_figures")
dir.create(path_result, recursive = TRUE, showWarnings = FALSE)
dir.create(path_figure, recursive = TRUE, showWarnings = FALSE)

abort <- function(...) stop(paste0(...), call. = FALSE)
normal_col <- function(x) tolower(gsub("[^a-zA-Z0-9]", "", x))
find_named_col <- function(d, candidates) {
  nm <- normal_col(names(d)); ca <- normal_col(candidates)
  for (name in ca) {
    ii <- which(nm == name)
    if (length(ii)) return(names(d)[ii[1]])
  }
  NULL
}
normalize_id <- function(x) {
  z <- toupper(gsub("[[:space:]]", "", trimws(as.character(x))))
  b <- grepl("^B0*[0-9]+$", z)
  z[b] <- paste0("B", as.integer(sub("^B", "", z[b])))
  z
}
all_prompt_ids <- paste0("B", 1:20)

get_prompt_col <- function(d) {
  candidate <- find_named_col(d, c("PromptID", "Prompt_ID", "PromptId", "PromptCode", "PromptNo", "PromptNumber"))
  if (!is.null(candidate)) return(candidate)
  hit <- vapply(d, function(v) sum(normalize_id(v) %in% all_prompt_ids, na.rm = TRUE), integer(1))
  good <- which(hit >= 15)
  if (length(good) == 1) return(names(d)[good])
  NULL
}

model_name <- function(x) {
  z <- tolower(trimws(as.character(x)))
  out <- rep(NA_character_, length(z))
  out[grepl("qwen", z)] <- "Qwen"
  out[grepl("z[-_ ]?image|zimage", z)] <- "Z-Image"
  out[grepl("sdxl|stable.diffusion.xl", z)] <- "SDXL"
  out
}

if (!file.exists(SUBJECTIVE_XLSX)) {
  abort("Latest Chinese image-level workbook not found: ", SUBJECTIVE_XLSX,
        "\nEdit SUBJECTIVE_XLSX at the top only if the data directory was moved.")
}
if (!("image_level" %in% readxl::excel_sheets(SUBJECTIVE_XLSX))) {
  abort("Workbook has no 'image_level' worksheet: ", SUBJECTIVE_XLSX)
}
raw <- as.data.frame(readxl::read_excel(SUBJECTIVE_XLSX, sheet = "image_level"))
col_id <- get_prompt_col(raw)
col_model <- find_named_col(raw, c("ModelShortName", "ModelShort", "ModelName", "Model"))
col_composite <- find_named_col(raw, c("CompositeScore", "MeanCompositeScore", "Composite", "CompositeMean"))
col_adherence <- find_named_col(raw, c("MeanAdherence", "AdherenceMean", "AvgAdherence", "Adherence"))
needed <- list(PromptID = col_id, ModelShortName = col_model,
               CompositeScore = col_composite, MeanAdherence = col_adherence)
if (any(vapply(needed, is.null, logical(1)))) {
  abort("Missing required image_level fields: ",
        paste(names(needed)[vapply(needed, is.null, logical(1))], collapse = ", "),
        "\nActual Excel column names: ", paste(names(raw), collapse = ", "))
}

language_code <- function(x) {
  z <- tolower(trimws(as.character(x)))
  z <- gsub("[[:space:]_]+", "", z)
  z <- gsub("[\uFF08\uFF09()]", "", z)
  ans <- rep(NA_character_, length(z))
  zh <- c("chinese", "ch", "cn", "zh", "zh-cn", "zh-hans", "zhcn",
          "chineselanguage", "chineseprompts", "chineseprompt",
          "中文", "中文提示词", "汉语", "汉语提示词")
  en <- c("english", "en", "eng", "en-us", "en-gb", "enus", "engb",
          "englishlanguage", "englishprompts", "englishprompt",
          "英文", "英文提示词", "英语", "英语提示词")
  ans[z %in% zh] <- "Chinese"
  ans[z %in% en] <- "English"
  ans
}
select_chinese_image_rows <- function(d, prompt_col) {
  is_b <- normalize_id(d[[prompt_col]]) %in% all_prompt_ids
  n_b <- sum(is_b)
  if (n_b == 60L) {
    message("Chinese selection: exactly 60 B1-B20 rows in source; no extra language split needed.")
    return(which(is_b))
  }
  if (n_b != 120L) {
    abort("Expected 60 Chinese B1-B20 image rows (possibly among 120 bilingual rows); found ",
          n_b, " B1-B20 rows out of ", nrow(d), " total. Source columns: ",
          paste(names(d), collapse = ", "))
  }

  cn <- normal_col(names(d))
  preferred <- which(grepl("language|^lang|lang$|locale", cn))
  candidates <- c(preferred, setdiff(seq_along(d), preferred))
  candidates <- setdiff(candidates, match(prompt_col, names(d)))
  matches <- list()
  for (j in candidates) {
    labels <- language_code(d[[j]])
    if (sum(labels[is_b] == "Chinese", na.rm = TRUE) == 60L &&
        sum(labels[is_b] == "English", na.rm = TRUE) == 60L &&
        all(!is.na(labels[is_b]))) {
      matches[[names(d)[j]]] <- which(is_b & !is.na(labels) & labels == "Chinese")
    }
  }
  if (!length(matches)) {
    preview <- if (length(preferred)) paste(vapply(preferred, function(j) {
      paste0(names(d)[j], "={",
             paste(utils::head(unique(as.character(d[[j]][is_b])), 7), collapse = " | "),
             "}")
    }, character(1)), collapse = "; ") else "no language/locale-named field"
    abort("The 120 B1-B20 rows include more than the 60 Chinese rated images, ",
          "but the Chinese/English language column could not be recognized safely.\n",
          "Detected fields: ", paste(names(d), collapse = ", "), "\n",
          "Language-field values: ", preview, "\n",
          "No rows were selected by spreadsheet order or deduplication. ",
          "Please provide the actual language field/value so the formal script can match it exactly.")
  }
  selections <- lapply(matches, sort)
  if (length(selections) > 1L &&
      !all(vapply(selections, function(z) identical(z, selections[[1]]), logical(1)))) {
    abort("Multiple candidate language fields disagree on Chinese rows: ",
          paste(names(matches), collapse = ", "), ". Selection stopped to prevent mixing languages.")
  }
  message("Chinese selection: 60/120 bilingual B1-B20 rows using source field '",
          names(matches)[1], "'.")
  selections[[1]]
}
raw <- raw[select_chinese_image_rows(raw, col_id), , drop = FALSE]
image_scores <- data.frame(
  PromptID = normalize_id(raw[[col_id]]),
  Model = model_name(raw[[col_model]]),
  CompositeScore = suppressWarnings(as.numeric(raw[[col_composite]])),
  MeanAdherence = suppressWarnings(as.numeric(raw[[col_adherence]]))
)
image_scores <- image_scores[image_scores$PromptID %in% all_prompt_ids, , drop = FALSE]
if (nrow(image_scores) != 60L) abort("Expected exactly 60 Chinese rated images after language filtering; found ", nrow(image_scores), ".")
if (anyNA(image_scores)) abort("Missing/unrecognized model, prompt, or numeric score among 60 Chinese images.")
if (anyDuplicated(image_scores[c("PromptID", "Model")])) {
  abort("Duplicate Chinese PromptID x Model. Figure 5 requires one rated image per pair.")
}
if (!setequal(unique(image_scores$PromptID), all_prompt_ids)) abort("Chinese PromptID set is not B1-B20.")
if (!setequal(unique(image_scores$Model), resource$Model)) abort("Model names do not map to Qwen / Z-Image / SDXL.")
for (m in resource$Model) {
  if (sum(image_scores$Model == m) != 20L) abort("Expected 20 images for model ", m)
}
for (col in c("CompositeScore", "MeanAdherence")) {
  if (any(!is.finite(image_scores[[col]]) | image_scores[[col]] < 1 | image_scores[[col]] > 5)) {
    abort("Invalid 1–5 values in ", col, ".")
  }
}
expected_scores <- c(Qwen = 4.1344, `Z-Image` = 4.0030, SDXL = 2.9123)
expected_below <- c(Qwen = 0L, `Z-Image` = 1L, SDXL = 18L)
for (m in names(expected_scores)) {
  actual <- mean(image_scores$CompositeScore[image_scores$Model == m])
  actual_below <- sum(image_scores$MeanAdherence[image_scores$Model == m] < ADHERENCE_ITEM_FLOOR)
  if (abs(actual - expected_scores[m]) > 0.025 || actual_below != expected_below[m]) {
    abort("Current image-level workbook does not match verified latest Chinese scoring for ", m,
          ": actual mean = ", round(actual, 4), ", below-3.5 = ", actual_below,
          "; expected ~", expected_scores[m], " and ", expected_below[m], ".",
          "\nCheck that the NEW 391-questionnaire image-level workbook (191 Chinese questionnaires) is used.")
  }
}

clean_tag <- function(x) {
  z <- tolower(trimws(as.character(x)))
  z <- gsub("[[:space:]_./–—-]+", "", z)
  z
}
context_tag <- function(x) {
  v <- clean_tag(x)
  o <- rep(NA_character_, length(v))
  o[grepl("educat|teaching|教育|教学", v)] <- "Educational"
  o[grepl("general|通用|一般", v)] <- "General"
  o[grepl("chinesecultur|culturechinese|culturalchinese|中国文化|中华文化|中国传统", v)] <- "Chinese-cultural"
  o
}
content_tag <- function(x, allow_non_object = TRUE) {
  v <- clean_tag(x)
  o <- rep(NA_character_, length(v))
  object <- grepl("objectconcept|conceptobject|object|concept|物体|概念", v)
  o[object] <- "Object-Concept"
  nonblank <- !is.na(v) & nzchar(v) & !(v %in% c("na", "nan", "null"))
  if (allow_non_object) o[nonblank & !object] <- "Other content"
  else o[grepl("othercontent|nonobject|其他内容|其它内容", v)] <- "Other content"
  o
}

validate_metadata <- function(meta) {
  if (!all(c("PromptID", "Context", "Content") %in% names(meta))) {
    abort("Metadata must have PromptID, Context, Content.")
  }
  meta$PromptID <- normalize_id(meta$PromptID)
  meta <- meta[meta$PromptID %in% all_prompt_ids, c("PromptID", "Context", "Content"), drop = FALSE]
  if (nrow(meta) < 20) abort("Task metadata includes fewer than 20 Chinese prompt records.")
  meta <- unique(meta)
  if (anyDuplicated(meta$PromptID)) abort("Conflicting/multiple task labels for a PromptID.")
  if (!setequal(meta$PromptID, all_prompt_ids) || anyNA(meta)) abort("Incomplete B1-B20 Context/Content mapping.")
  if (!identical(as.integer(table(factor(meta$Context,
             levels = c("Educational", "General", "Chinese-cultural")))), c(5L, 10L, 5L))) {
    abort("Wrong Context group sizes; expected Educational=5, General=10, Chinese-cultural=5.")
  }
  if (!identical(as.integer(table(factor(meta$Content,
             levels = c("Object-Concept", "Other content")))), c(6L, 14L))) {
    abort("Wrong Content group sizes; expected Object-Concept=6, Other=14.")
  }
  overlap <- sum(meta$Context == "Educational" & meta$Content == "Object-Concept")
  if (overlap != 3L) abort("Context/Content mapping contradicts the authoritative overlap of 3 prompts; got ", overlap)
  meta[order(as.integer(sub("B", "", meta$PromptID))), , drop = FALSE]
}

metadata_from_prompts_xlsx <- function(path) {
  if (!file.exists(path)) return(NULL)
  for (sheet in readxl::excel_sheets(path)) {
    d <- tryCatch(as.data.frame(readxl::read_excel(path, sheet = sheet)), error = function(e) NULL)
    if (is.null(d)) next
    id <- get_prompt_col(d)
    context <- find_named_col(d, c("Context", "PromptContext", "ContextLabel", "ContextCategory", "ContextGroup", "TaskContext", "Scenario", "ScenarioCategory"))
    content <- find_named_col(d, c("Content", "ContentLabel", "ContentCategory", "ContentClass", "ContentGroup", "PromptContent", "ContentType", "SubjectType"))
    if (is.null(id) || is.null(context) || is.null(content)) next
    data.frame(PromptID = normalize_id(d[[id]]),
               Context = context_tag(d[[context]]),
               Content = content_tag(d[[content]], allow_non_object = TRUE)) -> tmp
    checked <- tryCatch(validate_metadata(tmp), error = function(e) {
      message("  Prompts sheet '", sheet, "' not usable: ", conditionMessage(e)); NULL
    })
    if (!is.null(checked)) {
      attr(checked, "mapping_source") <- paste0(path, " [sheet ", sheet, "]")
      return(checked)
    }
  }
  NULL
}

metadata_from_fig3_csv <- function(path) {
  if (!file.exists(path)) return(NULL)
  d <- tryCatch(read.csv(path, check.names = FALSE, stringsAsFactors = FALSE,
                         fileEncoding = "UTF-8-BOM"), error = function(e) NULL)
  if (is.null(d)) return(NULL)
  id <- get_prompt_col(d)
  if (is.null(id)) return(NULL)
  d$.__Prompt <- normalize_id(d[[id]])
  d <- d[d$.__Prompt %in% all_prompt_ids, , drop = FALSE]
  if (nrow(d) < 20) return(NULL)
  candidate_cols <- setdiff(names(d)[grepl(
    "context|content|group|task|category|class|type", normal_col(names(d)))],
    c(id, ".__Prompt"))
  if (!length(candidate_cols)) {
    possibilities <- setdiff(names(d), c(id, ".__Prompt"))
    candidate_cols <- possibilities[vapply(d[possibilities], function(x) {
      is.character(x) && length(unique(stats::na.omit(x))) <= 8L
    }, logical(1))]
  }
  source_context <- rep(NA_character_, 20)
  source_content <- rep(NA_character_, 20)
  for (i in seq_along(all_prompt_ids)) {
    sub <- d[d$.__Prompt == all_prompt_ids[i], candidate_cols, drop = FALSE]
    if (!nrow(sub)) next
    values <- unlist(sub, use.names = FALSE)
    ctx <- unique(na.omit(context_tag(values)))
    ctt <- unique(na.omit(content_tag(values, allow_non_object = FALSE)))
    if (length(ctx) == 1) source_context[i] <- ctx
    if (length(ctt) == 1) source_content[i] <- ctt
  }
  tmp <- data.frame(PromptID = all_prompt_ids, Context = source_context,
                    Content = source_content)
  checked <- tryCatch(validate_metadata(tmp), error = function(e) {
    message("  Finalized Fig3 metadata CSV is incomplete: ", conditionMessage(e)); NULL
  })
  if (!is.null(checked)) attr(checked, "mapping_source") <- path
  checked
}

message("Reading authentic B1-B20 task taxonomy...")
meta <- metadata_from_prompts_xlsx(PROMPTS_XLSX)
if (is.null(meta)) {
  message("  Could not validate task labels from prompts.xlsx; trying finalized Fig3 CSV.")
  meta <- metadata_from_fig3_csv(FIG3_METADATA_CSV)
}
if (is.null(meta)) {
  abort("Could not recover an AUDITABLE B1-B20 Context x Content mapping. No task membership was guessed.\n",
        "Provide task labels as real columns Context and Content in: ", PROMPTS_XLSX, "\n",
        "or retain the finalized Figure 3 prompt-label result: ", FIG3_METADATA_CSV, "\n",
        "If Figure 3's CSV lacks task labels, copy its existing, validated B1-B20 mapping ",
        "into prompts.xlsx. Do not invent a mapping from group counts alone.")
}
message("  Taxonomy source: ", attr(meta, "mapping_source"))
meta$MappingSource <- attr(meta, "mapping_source")

wide <- reshape(image_scores[c("PromptID", "Model", "CompositeScore")],
                idvar = "PromptID", timevar = "Model", direction = "wide")
wide$GapQwenMinusZ <- wide$CompositeScore.Qwen - wide$`CompositeScore.Z-Image`
wide <- merge(wide, meta[c("PromptID", "Context", "Content")], by = "PromptID")
gap_checks <- c(
  Educational = mean(wide$GapQwenMinusZ[wide$Context == "Educational"]),
  General = mean(wide$GapQwenMinusZ[wide$Context == "General"]),
  `Chinese-cultural` = mean(wide$GapQwenMinusZ[wide$Context == "Chinese-cultural"]),
  `Object-Concept` = mean(wide$GapQwenMinusZ[wide$Content == "Object-Concept"]),
  `Other content` = mean(wide$GapQwenMinusZ[wide$Content == "Other content"])
)
expected_gaps <- c(Educational = 0.407, General = 0.032, `Chinese-cultural` = 0.054,
                   `Object-Concept` = 0.342, `Other content` = 0.041)
if (any(abs(gap_checks - expected_gaps) > 0.025)) {
  abort("Taxonomy/score provenance mismatch! New-Chinese subgroup gaps are ",
        paste(sprintf("%s=%.3f", names(gap_checks), gap_checks), collapse = ", "),
        "; expected approx ",
        paste(sprintf("%s=%.3f", names(expected_gaps), expected_gaps), collapse = ", "),
        ". Check input files and category mapping, not the R code's thresholds.")
}

group_defs <- data.frame(
  Group = c("All20", "Educational", "General", "Chinese-cultural", "Object-Concept", "Other content"),
  Display = c("ALL PROMPTS", "EDUCATIONAL", "GENERAL", "CHINESE-CULTURAL", "OBJECT–CONCEPT", "OTHER CONTENT"),
  Classification = c("All", "Context", "Context", "Context", "Content", "Content"),
  N = c(20L, 5L, 10L, 5L, 6L, 14L),
  Y = c(15.4, 12.8, 10.7, 8.6, 5.8, 3.7)
)
get_group_ids <- function(group) {
  if (group == "All20") return(all_prompt_ids)
  if (group %in% c("Educational", "General", "Chinese-cultural")) {
    return(meta$PromptID[meta$Context == group])
  }
  meta$PromptID[meta$Content == group]
}

metrics <- do.call(rbind, lapply(seq_len(nrow(group_defs)), function(g) {
  gd <- group_defs[g, ]
  ids <- get_group_ids(gd$Group)
  if (length(ids) != gd$N) abort("Wrong task count: ", gd$Group)
  do.call(rbind, lapply(resource$Model, function(m) {
    rows <- image_scores[image_scores$Model == m & image_scores$PromptID %in% ids, ]
    rr <- resource[resource$Model == m, ]
    if (nrow(rows) != gd$N) abort("Incomplete task-model scores: ", gd$Group, " / ", m)
    num_pass <- sum(rows$MeanAdherence >= ADHERENCE_ITEM_FLOOR)
    data.frame(Group = gd$Group, Classification = gd$Classification, N = nrow(rows),
               Model = m, MeanCompositeScore = mean(rows$CompositeScore),
               MeanAdherence = mean(rows$MeanAdherence),
               AdherenceFloor = ADHERENCE_ITEM_FLOOR,
               PassingImages = num_pass, Coverage = num_pass / nrow(rows),
               MeanLatencySeconds = rr$MeanLatencySeconds,
               MeanVRAM_MB = rr$MeanVRAM_MB)
  }))
}))

choose_fastest <- function(group, score_floor, required_coverage) {
  sub <- metrics[metrics$Group == group &
                   metrics$MeanCompositeScore >= score_floor - 1e-10 &
                   metrics$Coverage >= required_coverage - 1e-10, , drop = FALSE]
  if (!nrow(sub)) return("None")
  sub$Model[order(sub$MeanLatencySeconds, sub$Model)][1]
}

intervals <- do.call(rbind, lapply(seq_len(nrow(group_defs)), function(i) {
  g <- group_defs$Group[i]
  do.call(rbind, lapply(COMPOSITE_MEAN_FLOORS, function(qfloor) {
    breaks <- sort(unique(c(0, 1, metrics$Coverage[metrics$Group == g])))
    seg <- data.frame(
      Group = g, QualityFloor = qfloor,
      LeftCoverage = head(breaks, -1), RightCoverage = tail(breaks, -1)
    )
    seg <- seg[seg$RightCoverage > seg$LeftCoverage, , drop = FALSE]
    seg$SelectedModel <- vapply(seq_len(nrow(seg)), function(j) {
      choose_fastest(g, qfloor, (seg$LeftCoverage[j] + seg$RightCoverage[j]) / 2)
    }, character(1))
    if (nrow(seg) > 1) {
      run_id <- cumsum(c(TRUE, seg$SelectedModel[-1] != head(seg$SelectedModel, -1)))
      seg <- do.call(rbind, lapply(split(seg, run_id), function(x) {
        data.frame(Group = g, QualityFloor = qfloor,
                   LeftCoverage = min(x$LeftCoverage),
                   RightCoverage = max(x$RightCoverage),
                   SelectedModel = x$SelectedModel[1])
      }))
      rownames(seg) <- NULL
    }
    seg
  }))
}))
intervals$FromPct <- 100 * intervals$LeftCoverage
intervals$ToPct <- 100 * intervals$RightCoverage
intervals$Condition <- paste0("group mean CompositeScore >= ", intervals$QualityFloor,
                              "; image-level MeanAdherence >= ", ADHERENCE_ITEM_FLOOR)

policy_grid <- do.call(rbind, lapply(group_defs$Group, function(g) {
  do.call(rbind, lapply(COMPOSITE_MEAN_FLOORS, function(floor) {
    data.frame(Group = g, CompositeFloor = floor,
               RequiredCoverage = POLICY_COVERAGE_LEVELS,
               SelectedFastest = vapply(POLICY_COVERAGE_LEVELS,
                                        function(x) choose_fastest(g, floor, x), character(1)),
               Rule = "Fastest tested config satisfying mean composite floor and >= required share of images with MeanAdherence >= 3.5")
  }))
}))
policy_grid$SelectedMeanLatencySeconds <- resource$MeanLatencySeconds[
  match(policy_grid$SelectedFastest, resource$Model)]
policy_grid$SelectedMeanVRAM_MB <- resource$MeanVRAM_MB[
  match(policy_grid$SelectedFastest, resource$Model)]

palette <- c("Qwen" = "#526E90", "Z-Image" = "#4C968C",
             "SDXL" = "#BD9C70", "None" = "#E5E9EB")
ink <- "#263A4A"
secondary <- "#617480"
line_col <- "#BAC6CB"

rails <- do.call(rbind, lapply(seq_len(nrow(group_defs)), function(i) {
  y <- group_defs$Y[i]
  data.frame(Group = group_defs$Group[i],
             QualityFloor = COMPOSITE_MEAN_FLOORS,
             Y = c(y + 0.27, y - 0.27))
}))
interval_plot <- merge(intervals, rails, by = c("Group", "QualityFloor"), sort = FALSE)
interval_plot$YMin <- interval_plot$Y - 0.19
interval_plot$YMax <- interval_plot$Y + 0.19
interval_plot$LabelX <- (interval_plot$FromPct + interval_plot$ToPct) / 2
interval_plot$ShortLabel <- c("Qwen"="Q", "Z-Image"="Z", "SDXL"="S", "None"="–")[interval_plot$SelectedModel]
interval_plot$IsLong <- (interval_plot$ToPct - interval_plot$FromPct) >= 12

switch_candidates <- lapply(
  split(interval_plot, interaction(interval_plot$Group, interval_plot$QualityFloor,
                                   drop = TRUE)),
  function(d) {
    d <- d[order(d$FromPct), , drop = FALSE]
    if (nrow(d) < 2L) return(NULL)
    k <- which(head(d$SelectedModel, -1L) != tail(d$SelectedModel, -1L))
    if (!length(k)) return(NULL)
    data.frame(Group = d$Group[k + 1L], QualityFloor = d$QualityFloor[k + 1L],
               FromModel = d$SelectedModel[k], ToModel = d$SelectedModel[k + 1L],
               SwitchAtPct = d$FromPct[k + 1L], Y = d$Y[k + 1L],
               YMin = d$YMin[k + 1L], YMax = d$YMax[k + 1L],
               stringsAsFactors = FALSE)
  }
)
switch_candidates <- Filter(Negate(is.null), switch_candidates)
if (length(switch_candidates)) {
  switches <- do.call(rbind, switch_candidates)
  rownames(switches) <- NULL
} else {
  switches <- data.frame(Group = character(), QualityFloor = numeric(),
                         FromModel = character(), ToModel = character(),
                         SwitchAtPct = numeric(), Y = numeric(),
                         YMin = numeric(), YMax = numeric())
}

if (nrow(switches)) {
  switch_callouts <- unique(switches[c("Group", "SwitchAtPct")])
  switch_callouts$Y <- vapply(seq_len(nrow(switch_callouts)), function(i) {
    sub <- switches[switches$Group == switch_callouts$Group[i] &
                      abs(switches$SwitchAtPct - switch_callouts$SwitchAtPct[i]) < 1e-7, ]
    max(sub$YMax) + 0.31
  }, numeric(1))
  switch_callouts$Label <- vapply(switch_callouts$SwitchAtPct, function(x) {
    if (abs(x - round(x)) < 0.03) sprintf("%.0f%%", x)
    else sprintf("%.1f%%", x)
  }, character(1))
} else {
  switch_callouts <- data.frame(Group = character(), SwitchAtPct = numeric(),
                                Y = numeric(), Label = character())
}

region_bg <- data.frame(
  x0 = -52, x1 = 102,
  ymin = c(7.65, 2.80), ymax = c(14.10, 7.25),
  fill = c("#F3F6F8", "#F0F6F4")
)

p_main <- ggplot() +
  geom_rect(data = region_bg,
            aes(xmin = x0, xmax = x1, ymin = ymin, ymax = ymax),
            fill = region_bg$fill, color = NA) +
  geom_rect(data = interval_plot,
            aes(xmin = FromPct, xmax = ToPct, ymin = YMin, ymax = YMax,
                fill = SelectedModel), color = "white", linewidth = 0.20) +
  geom_rect(data = rails,
            aes(xmin = 0, xmax = 100, ymin = Y - 0.19, ymax = Y + 0.19),
            fill = NA, color = "#CBD3D6", linewidth = 0.28) +
  geom_segment(data = switches,
               aes(x = SwitchAtPct, xend = SwitchAtPct,
                   y = YMin - 0.025, yend = YMax + 0.075),
               color = "#334D60", linewidth = 0.38) +
  geom_segment(data = switch_callouts,
               aes(x = SwitchAtPct, xend = SwitchAtPct,
                   y = Y - 0.29, yend = Y - 0.23),
               color = "#334D60", linewidth = 0.32) +
  geom_text(data = switch_callouts,
            aes(x = SwitchAtPct, y = Y, label = Label),
            color = "#334D60", fontface = "bold", size = 2.65, vjust = 0.45) +
  geom_text(data = subset(interval_plot, IsLong & SelectedModel != "None"),
            aes(x = LabelX, y = Y, label = ShortLabel),
            color = "white", fontface = "bold", size = 2.8) +
  geom_text(data = subset(interval_plot, IsLong & SelectedModel == "None"),
            aes(x = LabelX, y = Y, label = ShortLabel),
            color = secondary, size = 2.8) +
  geom_text(data = group_defs,
            aes(x = -51, y = Y, label = paste0(Display, "  ·  n=", N)),
            hjust = 0, vjust = 0.5, color = ink, fontface = "bold", size = 3.02) +
  geom_text(data = rails, aes(x = -6.2, y = Y, label = sprintf("%.1f", QualityFloor)),
            color = secondary, hjust = 0.5, size = 2.85) +
  annotate("text", x = -51, y = 16.70, label = "TASK / PROMPT SUBSET",
           hjust = 0, fontface = "bold", size = 2.87, color = secondary) +
  annotate("text", x = -6.2, y = 16.70, label = "MIN. MEAN SCORE",
           hjust = 0.5, fontface = "bold", size = 2.68, color = secondary) +
  annotate("text", x = 50, y = 16.70, label = "MINIMUM SHARE OF IMAGES WITH ADHERENCE >= 3.5",
           hjust = 0.5, fontface = "bold", size = 2.87, color = secondary) +
  annotate("text", x = -49.3, y = 13.76, label = "TASK CONTEXT",
           hjust = 0, vjust = 0.5, fontface = "bold", size = 2.65, color = "#6D8796") +
  annotate("text", x = -49.3, y = 6.92, label = "CONTENT CLASSIFICATION",
           hjust = 0, vjust = 0.5, fontface = "bold", size = 2.65, color = "#69978D") +
  annotate("segment", x = 0, xend = 100, y = 2.65, yend = 2.65,
           linewidth = 0.32, color = line_col) +
  annotate("segment", x = c(0, 20, 40, 60, 80, 100),
           xend = c(0, 20, 40, 60, 80, 100),
           y = 2.65, yend = 2.79, color = line_col, linewidth = 0.3) +
  annotate("text", x = c(0, 20, 40, 60, 80, 100), y = 2.35,
           label = c("0%", "20", "40", "60", "80", "100%"),
           color = secondary, size = 2.85) +
  scale_fill_manual(
    values = palette,
    breaks = c("SDXL", "Z-Image", "Qwen", "None"),
    labels = c("SDXL-Turbo", "Z-Image-Turbo", "Qwen-Image-2512", "No eligible model"),
    drop = FALSE
  ) +
  guides(fill = guide_legend(title = NULL, nrow = 1, byrow = TRUE,
                             override.aes = list(color = NA))) +
  coord_cartesian(xlim = c(-52, 103), ylim = c(2.0, 17.15), clip = "off", expand = FALSE) +
  theme_void(base_size = 9.5, base_family = "sans") +
  theme(
    legend.position = "top", legend.justification = "right",
    legend.margin = margin(b = 3, t = 1), legend.spacing.x = grid::unit(5, "pt"),
    legend.key.width = grid::unit(10, "pt"),
    legend.text = element_text(size = 8.5, color = ink),
    plot.margin = margin(t = 4, r = 8, b = 1, l = 12)
  )

cost <- resource
cost$Y <- c(3.15, 2.25, 1.35)
cost$LogX <- 18 + log10(cost$MeanLatencySeconds) / log10(200) * 91
cost$SecText <- sprintf("%.1f s", cost$MeanLatencySeconds)
cost$MemText <- format(round(cost$MeanVRAM_MB, 0), big.mark = ",", scientific = FALSE, trim = TRUE)

p_cost <- ggplot() +
  annotate("segment", x = -51, xend = 177, y = 4.28, yend = 4.28,
           color = line_col, linewidth = 0.4) +
  annotate("text", x = -51, y = 4.66, label = "MEASURED COST OF THE THREE TESTED CONFIGURATIONS",
           hjust = 0, fontface = "bold", size = 2.9, color = ink) +
  annotate("text", x = 62, y = 4.00, label = "Mean generation time · log axis",
           hjust = 0.5, size = 2.8, color = secondary) +
  annotate("text", x = 129, y = 4.00, label = "Seconds", hjust = 0.5,
           size = 2.8, color = secondary) +
  annotate("text", x = 161, y = 4.00, label = "Mean VRAM (MB)", hjust = 0.5,
           size = 2.8, color = secondary) +
  geom_segment(data = cost, aes(x = 18, xend = LogX, y = Y, yend = Y),
               linewidth = 0.95, color = "#D4DBDE") +
  geom_point(data = cost, aes(x = LogX, y = Y, fill = Model),
             color = "white", shape = 21, size = 3.65, stroke = 0.55) +
  geom_text(data = cost, aes(x = -51, y = Y, label = Configuration, color = Model),
            hjust = 0, fontface = "bold", size = 3.10, show.legend = FALSE) +
  geom_text(data = cost, aes(x = 129, y = Y, label = SecText),
            color = ink, hjust = 0.5, size = 3.0) +
  geom_text(data = cost, aes(x = 161, y = Y, label = MemText),
            color = ink, hjust = 0.5, size = 3.0) +
  annotate("segment", x = 18, xend = 109, y = 0.82, yend = 0.82,
           color = line_col, linewidth = 0.32) +
  annotate("segment", x = 18 + log10(c(1, 10, 100))/log10(200)*91,
           xend = 18 + log10(c(1, 10, 100))/log10(200)*91,
           y = 0.82, yend = 0.95, color = line_col, linewidth = 0.3) +
  annotate("text", x = 18 + log10(c(1, 10, 100))/log10(200)*91,
           y = 0.54, label = c("1", "10", "100"), color = secondary, size = 2.65) +
  scale_fill_manual(values = palette, guide = "none") +
  scale_color_manual(values = palette, guide = "none") +
  coord_cartesian(xlim = c(-52, 179), ylim = c(0.35, 4.85),
                  expand = FALSE, clip = "off") +
  theme_void(base_size = 9.5, base_family = "sans") +
  theme(plot.margin = margin(t = 0, r = 8, b = 4, l = 8))

write.csv(image_scores[order(image_scores$PromptID, image_scores$Model), ],
          file.path(path_result, "Fig5_latest_chinese_prompt_model_scores.csv"), row.names = FALSE, na = "")
write.csv(meta, file.path(path_result, "Fig5_task_taxonomy.csv"), row.names = FALSE, na = "")
write.csv(metrics, file.path(path_result, "Fig5_task_model_inputs.csv"), row.names = FALSE, na = "")
write.csv(intervals, file.path(path_result, "Fig5_decision_intervals.csv"), row.names = FALSE, na = "")
write.csv(switches[c("Group", "QualityFloor", "FromModel", "ToModel", "SwitchAtPct")],
          file.path(path_result, "Fig5_choice_switch_boundaries.csv"),
          row.names = FALSE, na = "")
write.csv(policy_grid, file.path(path_result, "Fig5_exact_policy_scenarios.csv"), row.names = FALSE, na = "")
write.csv(resource, file.path(path_result, "Fig5_verified_configuration_costs.csv"), row.names = FALSE, na = "")

draw_figure <- function() {
  grid::grid.newpage()
  v <- grid::viewport(layout = grid::grid.layout(
    nrow = 2, ncol = 1, heights = grid::unit(c(0.768, 0.232), "null")))
  grid::pushViewport(v)
  print(p_main, vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 1), newpage = FALSE)
  print(p_cost, vp = grid::viewport(layout.pos.row = 2, layout.pos.col = 1), newpage = FALSE)
  grid::popViewport()
}
fig_pdf <- file.path(path_figure, "Fig5_conditional_deployment_frontier.pdf")
fig_png <- file.path(path_figure, "Fig5_conditional_deployment_frontier.png")

if (capabilities("cairo")) {
  grDevices::cairo_pdf(fig_pdf, width = FIGURE_WIDTH_MM/25.4,
                      height = FIGURE_HEIGHT_MM/25.4, onefile = FALSE)
} else {
  grDevices::pdf(fig_pdf, width = FIGURE_WIDTH_MM/25.4,
                 height = FIGURE_HEIGHT_MM/25.4, onefile = FALSE,
                 useDingbats = FALSE)
}
draw_figure()
grDevices::dev.off()

png_args <- list(filename = fig_png, width = FIGURE_WIDTH_MM/25.4,
                 height = FIGURE_HEIGHT_MM/25.4, units = "in", res = PNG_DPI)
if (capabilities("cairo")) png_args$type <- "cairo"
do.call(grDevices::png, png_args)
draw_figure()
grDevices::dev.off()

message("\nFigure 5 created (", FIGURE_WIDTH_MM, " x ", FIGURE_HEIGHT_MM,
        " mm, PNG ", PNG_DPI, " dpi).")
message("  PDF: ", normalizePath(fig_pdf, winslash = "/"))
message("  PNG: ", normalizePath(fig_png, winslash = "/"))
message("  CSV outputs: ", normalizePath(path_result, winslash = "/"))
message("  Data provenance: latest Chinese image_level + verified AM4 summary means.")
message("  Resource means are fixed from 2026-11-09 master MD, NOT reprocessed from raw logs.")
message("  Figure legend: Q=Qwen, Z=Z-Image, S=SDXL, gray=no eligible configuration.")
message("  Switch labels mark coverage cutoffs; the model on the left remains",
        " eligible at the exact cutoff, and the right-side choice applies above it.")
message("  Coverage is the fraction of rated images meeting MeanAdherence >= 3.5,")
message("  and is NOT an educational correctness, clinical or safety approval rate.")
