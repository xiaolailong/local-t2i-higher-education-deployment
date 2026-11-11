#!/usr/bin/env Rscript
# Figure 3: model quality distributions and adherence coverage by task.
# Input: data/source/subjective_scores_human120_image_level.xlsx
# Outputs: data/paper_results/ and outputs/paper_figures/.

options(stringsAsFactors = FALSE)

script_path <- function() {
  a <- grep('^--file=', commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(a) != 1L) stop('Run using Rscript.')
  normalizePath(sub('^--file=', '', a[[1L]]), winslash = '/', mustWork = TRUE)
}
config <- list(
  repo = normalizePath(file.path(dirname(script_path()), '..', '..'),
                       winslash = '/', mustWork = FALSE),
  threshold = 3.5
)
args <- commandArgs(trailingOnly = TRUE)
if (length(args) %% 2L != 0L) stop('Options: --repo DIR --threshold NUMBER')
if (length(args) > 0L) for (i in seq.int(1L, length(args), 2L)) {
  k <- sub('^--', '', args[[i]])
  if (!startsWith(args[[i]], '--') || !k %in% names(config)) stop('Unknown option: ', args[[i]])
  config[[k]] <- args[[i + 1L]]
}
config$input <- file.path(config$repo, "data", "source", "subjective_scores_human120_image_level.xlsx")
config$threshold <- suppressWarnings(as.numeric(config$threshold))
if (!is.finite(config$threshold) || config$threshold < 1 || config$threshold > 5) {
  stop('The illustrative threshold must lie between 1 and 5.')
}
for (p in c('readxl','ggplot2')) {
  if (!requireNamespace(p, quietly = TRUE)) stop('Missing R package: ', p)
}
if (!file.exists(config$input)) stop('Missing input: ', config$input)
if (!dir.exists(config$repo)) stop('Missing repository root: ', config$repo)
figure_dir <- file.path(config$repo, 'outputs', 'paper_figures')
result_dir <- file.path(config$repo, 'data', 'paper_results')
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(result_dir, recursive = TRUE, showWarnings = FALSE)

design <- data.frame(
  PromptID = paste0('B', 1:20),
  Context = c('General','General','Chinese-cultural','Chinese-cultural','General',
              'Chinese-cultural','Educational','Educational','Educational','General',
              'General','General','General','General','Educational','General',
              'Chinese-cultural','General','Chinese-cultural','Educational'),
  ObjectConcept = paste0('B',1:20) %in% c('B7','B8','B9','B16','B17','B18'),
  stringsAsFactors = FALSE
)
if (sum(design$Context == 'Educational') != 5L ||
    sum(design$Context == 'General') != 10L ||
    sum(design$Context == 'Chinese-cultural') != 5L ||
    sum(design$ObjectConcept) != 6L ||
    sum(design$ObjectConcept & design$Context == 'Educational') != 3L) {
  stop('Prompt design unexpectedly changed.')
}

raw <- as.data.frame(readxl::read_excel(config$input, sheet = 'image_level'))
needed <- c('PromptID','ImageName','Language','ModelShortName',
            'CompositeScore','MeanAdherence','ValidRaterCount')
miss <- setdiff(needed, colnames(raw))
if (length(miss)) stop('Missing Excel columns: ', paste(miss, collapse=', '))
raw$PromptID <- toupper(trimws(as.character(raw$PromptID)))
raw$ImageName <- trimws(as.character(raw$ImageName))
raw$Language <- tolower(trimws(as.character(raw$Language)))
raw$ModelShortName <- tolower(trimws(as.character(raw$ModelShortName)))
for (key in c('CompositeScore','MeanAdherence','ValidRaterCount')) {
  raw[[key]] <- suppressWarnings(as.numeric(raw[[key]]))
}
d <- raw[raw$Language == 'chinese', , drop = FALSE]
image_key <- paste(d$PromptID, d$ModelShortName, sep = '::')
if (nrow(d) != 60L || anyDuplicated(image_key) || anyDuplicated(d$ImageName) ||
    !setequal(unique(d$PromptID), design$PromptID) ||
    !setequal(unique(d$ModelShortName), c('qwen','zimage','sdxl')) ||
    any(table(d$PromptID) != 3L) ||
    any(!is.finite(d$CompositeScore)) || any(d$CompositeScore < 1 | d$CompositeScore > 5) ||
    any(!is.finite(d$MeanAdherence)) || any(d$MeanAdherence < 1 | d$MeanAdherence > 5) ||
    any(!is.finite(d$ValidRaterCount)) || any(d$ValidRaterCount < 1)) {
  stop('Expected exactly 20 Chinese prompts x 3 model-specific scored images.')
}
if (anyNA(d$PromptID) || anyNA(d$ImageName)) stop('Missing prompt or image key.')

model_order <- c('qwen','zimage','sdxl')
model_names <- c(qwen = 'Qwen', zimage = 'Z-Image', sdxl = 'SDXL')
model_colors <- c(qwen = '#335E7B', zimage = '#C46C50', sdxl = '#39817D')
tasks <- list(
  list(id = 'educational', name = 'EDUCATIONAL', dimension = 'Context',
       prompt_ids = design$PromptID[design$Context == 'Educational'],
       header_y = 29.8, rows = c(28.75,27.75,26.75)),
  list(id = 'general', name = 'GENERAL', dimension = 'Context',
       prompt_ids = design$PromptID[design$Context == 'General'],
       header_y = 24.6, rows = c(23.55,22.55,21.55)),
  list(id = 'chinese_cultural', name = 'CHINESE-CULTURAL', dimension = 'Context',
       prompt_ids = design$PromptID[design$Context == 'Chinese-cultural'],
       header_y = 19.4, rows = c(18.35,17.35,16.35)),
  list(id = 'object_concept', name = 'OBJECT–CONCEPT', dimension = 'Content',
       prompt_ids = design$PromptID[design$ObjectConcept],
       header_y = 12.4, rows = c(11.35,10.35,9.35)),
  list(id = 'other_content', name = 'OTHER CONTENT', dimension = 'Content',
       prompt_ids = design$PromptID[!design$ObjectConcept],
       header_y = 7.2, rows = c(6.15,5.15,4.15))
)
expected <- c(5L,10L,5L,6L,14L)
if (!identical(as.integer(vapply(tasks, function(x) length(x$prompt_ids), integer(1))), expected)) {
  stop('Task subset membership unexpectedly changed.')
}

points_list <- list(); summary_list <- list(); coverage_list <- list(); header_list <- list()
for (j in seq_along(tasks)) {
  task <- tasks[[j]]
  header_list[[j]] <- data.frame(TaskID = task$id, TaskName = task$name,
    Dimension = task$dimension, N = length(task$prompt_ids), Y = task$header_y,
    stringsAsFactors = FALSE)
  for (i in seq_along(model_order)) {
    model <- model_order[[i]]
    a <- d[d$ModelShortName == model & d$PromptID %in% task$prompt_ids, , drop = FALSE]
    a <- a[order(as.integer(sub('^B', '', a$PromptID))), , drop = FALSE]
    if (nrow(a) != length(task$prompt_ids)) stop('Missing score for ', task$id, '/', model)
    yy <- task$rows[[i]]
    prompt_i <- as.integer(sub('^B','',a$PromptID))
    a$Y <- yy
    a$Y_point <- yy - 0.15 + (((prompt_i * 13L + i * 3L) %% 7L) - 3L) * 0.047
    a$TaskID <- task$id
    a$TaskName <- task$name
    a$TaskDimension <- task$dimension
    a$ModelLabel <- unname(model_names[[model]])
    a$AtThreshold <- a$MeanAdherence >= config$threshold
    points_list[[length(points_list) + 1L]] <- a[,c('TaskID','TaskName','TaskDimension','PromptID',
      'ImageName','ModelShortName','ModelLabel','CompositeScore','MeanAdherence',
      'ValidRaterCount','AtThreshold','Y','Y_point')]
    summary_list[[length(summary_list) + 1L]] <- data.frame(
      TaskID = task$id, TaskName = task$name, Dimension = task$dimension,
      ModelShortName = model, ModelLabel = unname(model_names[[model]]),
      N_Prompts = nrow(a), MeanComposite = mean(a$CompositeScore),
      MedianComposite = stats::median(a$CompositeScore),
      MinComposite = min(a$CompositeScore), MaxComposite = max(a$CompositeScore),
      MeanAdherence = mean(a$MeanAdherence),
      N_AboveThreshold = sum(a$AtThreshold), Threshold = config$threshold,
      Y = yy, stringsAsFactors = FALSE)
    coverage_list[[length(coverage_list) + 1L]] <- data.frame(
      TaskID = task$id, ModelShortName = model, Slot = seq_len(nrow(a)),
      Passed = c(rep(TRUE, sum(a$AtThreshold)),rep(FALSE, sum(!a$AtThreshold))),
      Y = yy, stringsAsFactors = FALSE)
  }
}
point_data <- do.call(rbind, points_list)
summary_data <- do.call(rbind, summary_list)
coverage <- do.call(rbind, coverage_list)
headers <- do.call(rbind, header_list)
rownames(point_data) <- rownames(summary_data) <- rownames(coverage) <- NULL

pair_q <- d[d$ModelShortName == 'qwen',c('PromptID','CompositeScore'),drop=FALSE]
pair_z <- d[d$ModelShortName == 'zimage',c('PromptID','CompositeScore'),drop=FALSE]
pairs <- merge(pair_q,pair_z,by='PromptID',suffixes=c('_Qwen','_ZImage'),sort=FALSE)
pairs$Gap <- pairs$CompositeScore_Qwen - pairs$CompositeScore_ZImage
pairs <- merge(pairs, design, by='PromptID',sort=FALSE)
if (nrow(pairs) != 20L || abs(mean(pairs$Gap) - 0.1314365) > 0.012 ||
    sum(pairs$Gap > 0) != 16L || sum(pairs$Gap < 0) != 4L) {
  stop('Latest Chinese model gap differs from the audited version; check source.')
}
contrast_specs <- list(
  list(name='Educational vs General',
       focus = pairs$Context == 'Educational', reference = pairs$Context == 'General'),
  list(name='Object–Concept vs other content',
       focus = pairs$ObjectConcept, reference = !pairs$ObjectConcept)
)
contrast_out <- list()
for (z in contrast_specs) {
  a <- pairs$Gap[z$focus]; b <- pairs$Gap[z$reference]
  if (length(a) < 2L || length(b) < 2L || anyNA(c(a,b))) stop('Bad contrast groups')
  obs <- mean(a) - mean(b)
  ci <- stats::t.test(a,b, var.equal=FALSE, conf.level=.95)$conf.int
  loo <- c(vapply(seq_along(a), function(k) mean(a[-k]) - mean(b), numeric(1)),
           vapply(seq_along(b), function(k) mean(a) - mean(b[-k]), numeric(1)))
  contrast_out[[length(contrast_out)+1L]] <- data.frame(
    TaskComparison=z$name, N_Focus=length(a), N_Reference=length(b),
    FocusGap=mean(a), ReferenceGap=mean(b), ExtraGap=obs,
    ExploratoryWelch95Low=ci[[1L]], ExploratoryWelch95High=ci[[2L]],
    LOOMin=min(loo), LOOMax=max(loo), LOOSameSign=all(loo > 0),
    stringsAsFactors=FALSE)
}
contrasts <- do.call(rbind,contrast_out)

write.csv(point_data, file.path(result_dir,'Fig3_task_model_prompt_scores.csv'),
          row.names=FALSE,fileEncoding='UTF-8')
write.csv(summary_data, file.path(result_dir,'Fig3_task_model_summary.csv'),
          row.names=FALSE,fileEncoding='UTF-8')
write.csv(contrasts, file.path(result_dir,'Fig3_exploratory_task_contrasts.csv'),
          row.names=FALSE,fileEncoding='UTF-8')

ink        <- '#243B50'
muted      <- '#65798C'
pale_edge  <- '#DDE6ED'
model_ink  <- unname(model_colors)

score_x <- function(score) 22.2 + (score - 1) * (34.6 / 4)
coverage_x <- function(slot) 68.3 + (slot - 1) * 1.64

point_data$X_score <- score_x(point_data$CompositeScore)
point_data$ModelColor <- unname(model_colors[point_data$ModelShortName])
summary_data$X_mean <- score_x(summary_data$MeanComposite)
summary_data$ModelColor <- unname(model_colors[summary_data$ModelShortName])
coverage$X_dot <- coverage_x(coverage$Slot)
coverage$DotFill <- ifelse(coverage$Passed,
                            unname(model_colors[coverage$ModelShortName]),
                            '#FFFFFF')
coverage$DotOutline <- ifelse(coverage$Passed,
                               unname(model_colors[coverage$ModelShortName]),
                               '#B7C6D2')

family_blocks <- data.frame(
  Family = c('Task context', 'Content classification'),
  xmin = c(0.8, 0.8), xmax = c(99.2, 99.2),
  ymin = c(15.15, 3.45), ymax = c(31.65, 14.65),
  Fill = c('#F2F6F9', '#F1F7F4'),
  stringsAsFactors = FALSE
)
section_headings <- data.frame(
  x = c(2.0, 2.0), y = c(31.02, 14.06),
  text = c('TASK CONTEXT', 'CONTENT CLASSIFICATION'),
  stringsAsFactors = FALSE
)
headers$X <- 2.0
headers$Display <- sprintf('%s  ·  n=%d', headers$TaskName, headers$N)
summary_data$Coverage <- sprintf('%d/%d', summary_data$N_AboveThreshold,
                                   summary_data$N_Prompts)

score_axis <- data.frame(Value = 1:5)
score_axis$X <- score_x(score_axis$Value)

fig <- ggplot2::ggplot() +
  ggplot2::geom_rect(
    data = family_blocks,
    ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
                 fill = Fill),
    inherit.aes = FALSE, colour = '#E1E8ED', linewidth = 0.25
  ) +
  ggplot2::annotate('text', x = 2.0, y = 32.80,
                    label = 'Model quality by task group',
                    hjust = 0, vjust = .5, size = 4.0,
                    fontface = 'bold', colour = ink) +
  ggplot2::annotate('text', x = 64.0, y = 32.80,
                    label = sprintf('Adherence coverage  (>= %.1f/5)', config$threshold),
                    hjust = 0, vjust = .5, size = 4.0,
                    fontface = 'bold', colour = ink) +
  ggplot2::geom_text(
    data = section_headings,
    ggplot2::aes(x = x, y = y, label = text),
    hjust = 0, vjust = .5, fontface = 'bold', size = 3.35,
    colour = muted
  ) +
  ggplot2::geom_text(
    data = headers,
    ggplot2::aes(x = X, y = Y, label = Display),
    hjust = 0, vjust = .5, fontface = 'bold', size = 3.43,
    colour = ink
  ) +
  ggplot2::geom_text(
    data = summary_data,
    ggplot2::aes(x = 3.0, y = Y, label = ModelLabel, colour = ModelColor),
    hjust = 0, vjust = .5, size = 3.33
  ) +
  ggplot2::geom_point(
    data = point_data,
    ggplot2::aes(x = X_score, y = Y_point, colour = ModelColor),
    size = 1.75, alpha = .74
  ) +
  ggplot2::geom_point(
    data = summary_data,
    ggplot2::aes(x = X_mean, y = Y + .14, fill = ModelColor),
    shape = 23, colour = ink, stroke = .52, size = 3.20
  ) +
  ggplot2::geom_point(
    data = coverage,
    ggplot2::aes(x = X_dot, y = Y, fill = DotFill,
                 colour = DotOutline),
    shape = 21, size = 2.35, stroke = .52
  ) +
  ggplot2::geom_text(
    data = summary_data,
    ggplot2::aes(x = 96.6, y = Y, label = Coverage),
    hjust = 1, vjust = .5, size = 3.40, colour = ink
  ) +
  ggplot2::geom_segment(
    data = score_axis,
    ggplot2::aes(x = X, xend = X, y = 2.69, yend = 2.84),
    linewidth = .37, colour = '#B5C2CD'
  ) +
  ggplot2::geom_text(
    data = score_axis,
    ggplot2::aes(x = X, y = 2.20, label = Value),
    size = 3.24, colour = muted
  ) +
  ggplot2::annotate('text', x = 39.5, y = 1.06,
                    label = 'Composite score (1–5)',
                    size = 3.62, colour = ink) +
  ggplot2::scale_colour_identity() +
  ggplot2::scale_fill_identity() +
  ggplot2::scale_x_continuous(limits = c(0,100), expand = c(0,0)) +
  ggplot2::scale_y_continuous(limits = c(0.38,33.8), expand = c(0,0)) +
  ggplot2::coord_cartesian(clip = 'off') +
  ggplot2::theme_void(base_size = 9.5, base_family = 'sans') +
  ggplot2::theme(
    panel.grid = ggplot2::element_blank(),
    plot.background = ggplot2::element_rect(fill = 'white', colour = NA),
    panel.background = ggplot2::element_rect(fill = 'white', colour = NA),
    plot.margin = ggplot2::margin(t = 3, r = 3, b = 3, l = 3)
  )

pdf_file <- file.path(figure_dir, 'Fig3_task_model_profiles.pdf')
png_file <- file.path(figure_dir, 'Fig3_task_model_profiles.png')
width_in <- 178 / 25.4
height_in <- 138 / 25.4
if (capabilities('cairo')) {
  ggplot2::ggsave(pdf_file, fig, width = width_in, height = height_in,
                  device = grDevices::cairo_pdf, bg = 'white')
} else {
  ggplot2::ggsave(pdf_file, fig, width = width_in, height = height_in,
                  device = 'pdf', bg = 'white', useDingbats = FALSE)
}
ggplot2::ggsave(png_file, fig, width = width_in, height = height_in,
                device = 'png', dpi = 600, bg = 'white')

cat('Figure 3: unified task rows, latest Chinese scores only.\n')
cat('PDF: ', pdf_file, '\nPNG: ', png_file, '\n', sep = '')
cat('Source CSVs: ', result_dir, '\n', sep = '')
cat('Two classification-family backgrounds span both measures on one aligned coordinate system.\n')
cat('Context and Content are overlapping classifications of the same prompts.\n')
print(summary_data[,c('TaskName','ModelLabel','N_Prompts','MeanComposite',
                       'N_AboveThreshold','Threshold')], row.names = FALSE)
