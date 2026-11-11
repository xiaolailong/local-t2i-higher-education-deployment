#!/usr/bin/env Rscript
# Figure 2: prompt-level paired quality differences.
# Input: data/source/subjective_scores_human120_image_level.xlsx
# Outputs: data/paper_results/ and outputs/paper_figures/.

options(stringsAsFactors = FALSE)

get_script_path <- function() {
  x <- grep('^--file=', commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(x) != 1L) stop('This script must be executed with Rscript.')
  normalizePath(sub('^--file=', '', x[[1L]]), winslash = '/', mustWork = TRUE)
}

parse_args <- function(x) {
  out <- list(
    repo = normalizePath(file.path(dirname(get_script_path()), '..', '..'),
                         winslash = '/', mustWork = FALSE)
  )
  if (length(x) %% 2L != 0L) stop('Options: --repo REPOSITORY_ROOT')
  if (length(x) != 0L) for (i in seq.int(1L, length(x), by = 2L)) {
    flag <- x[[i]]
    key <- substring(flag, 3L)
    if (!startsWith(flag, '--') || !(key %in% names(out))) stop(paste('Unknown flag:', flag))
    out[[key]] <- x[[i + 1L]]
  }
  out
}

cfg <- parse_args(commandArgs(trailingOnly = TRUE))
cfg$input <- file.path(cfg$repo, "data", "source", "subjective_scores_human120_image_level.xlsx")
for (pkg in c('readxl', 'ggplot2')) {
  if (!requireNamespace(pkg, quietly = TRUE)) stop('Install required R package: ', pkg)
}
if (!file.exists(cfg$input)) stop('Missing latest image-level Excel: ', cfg$input)
if (!dir.exists(cfg$repo)) stop('Repository root does not exist: ', cfg$repo)

fig_dir <- file.path(cfg$repo, 'outputs', 'paper_figures')
data_dir <- file.path(cfg$repo, 'data', 'paper_results')
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
if (!dir.exists(fig_dir) || !dir.exists(data_dir)) stop('Cannot create output folders.')

prompt_design <- data.frame(
  PromptID = paste0('B',1:20),
  Context = c('General','General','Chinese-cultural','Chinese-cultural','General',
              'Chinese-cultural','Educational','Educational','Educational','General',
              'General','General','General','General','Educational','General',
              'Chinese-cultural','General','Chinese-cultural','Educational'),
  ObjectConcept = paste0('B',1:20) %in% c('B7','B8','B9','B16','B17','B18'),
  stringsAsFactors = FALSE
)
if (sum(prompt_design$Context == 'Educational') != 5L ||
    sum(prompt_design$Context == 'General') != 10L ||
    sum(prompt_design$Context == 'Chinese-cultural') != 5L ||
    sum(prompt_design$ObjectConcept & prompt_design$Context=='Educational') != 3L) {
  stop('Unexpected experimental task labels')
}

raw <- as.data.frame(readxl::read_excel(cfg$input, sheet='image_level'))
required <- c('ImageName','PromptID','ModelShortName','Language',
              'CompositeScore','ValidRaterCount')
missing <- setdiff(required,names(raw))
if (length(missing)) stop('Missing columns: ',paste(missing,collapse=', '))
raw$Language <- tolower(trimws(as.character(raw$Language)))
raw$PromptID <- toupper(trimws(as.character(raw$PromptID)))
raw$ModelShortName <- tolower(trimws(as.character(raw$ModelShortName)))
raw$ImageName <- trimws(as.character(raw$ImageName))
raw$CompositeScore <- suppressWarnings(as.numeric(raw$CompositeScore))
raw$ValidRaterCount <- suppressWarnings(as.numeric(raw$ValidRaterCount))
d <- raw[raw$Language == 'chinese',,drop=FALSE]
keys <- paste(d$PromptID,d$ModelShortName,sep='::')
if (nrow(d) != 60L || anyDuplicated(keys) || anyDuplicated(d$ImageName) ||
    !setequal(unique(d$PromptID),prompt_design$PromptID) ||
    !setequal(unique(d$ModelShortName),c('qwen','zimage','sdxl')) ||
    any(table(d$PromptID) != 3L) ||
    any(!is.finite(d$CompositeScore)) ||
    any(d$CompositeScore < 1 | d$CompositeScore > 5) ||
    any(!is.finite(d$ValidRaterCount)) || any(d$ValidRaterCount < 1)) {
  stop('Expected 20 Chinese prompts x 3 unique scored model images.')
}

ids <- prompt_design$PromptID
rows <- lapply(ids,function(id) {
  a <- d[d$PromptID==id,,drop=FALSE]
  q <- a[a$ModelShortName=='qwen',,drop=FALSE]
  z <- a[a$ModelShortName=='zimage',,drop=FALSE]
  m <- prompt_design[prompt_design$PromptID==id,,drop=FALSE]
  data.frame(PromptID=id, Context=m$Context,
    Content=ifelse(m$ObjectConcept,'Object–Concept','Other content'),
    ObjectConcept=ifelse(m$ObjectConcept,'Object–Concept','Other content'),
    QwenComposite=q$CompositeScore, ZimageComposite=z$CompositeScore,
    PairedGap=q$CompositeScore-z$CompositeScore,
    QwenImage=q$ImageName,ZimageImage=z$ImageName,
    stringsAsFactors=FALSE)
})
figdata <- do.call(rbind,rows)
rownames(figdata) <- NULL

avg <- mean(figdata$PairedGap)
wins <- sum(figdata$PairedGap > 1e-10)
losses <- sum(figdata$PairedGap < -1e-10)
if (wins != 16L || losses != 4L || abs(avg - 0.131436507936508) > 0.003) {
  stop('This data differs from the verified newer Chinese snapshot; check input before drawing')
}

contexts <- c('Educational', 'General', 'Chinese-cultural')
position_set <- list(
  'Educational' = 25:21,
  'General' = 18:9,
  'Chinese-cultural' = 6:2
)
headers_y <- c('Educational' = 26.15, 'General' = 19.15,
               'Chinese-cultural' = 7.15)
expected_n <- c('Educational' = 5L, 'General' = 10L,
                'Chinese-cultural' = 5L)
figdata$Y <- NA_real_
for (ctx in contexts) {
  indices <- which(figdata$Context == ctx)
  if (length(indices) != expected_n[[ctx]]) {
    stop('Unexpected prompt count in ', ctx, ': ', length(indices))
  }
  sorted <- indices[order(-figdata$PairedGap[indices], figdata$PromptID[indices])]
  figdata$Y[sorted] <- position_set[[ctx]]
}
if (anyNA(figdata$Y) || anyDuplicated(figdata$Y)) stop('Invalid plot row positions')
figdata <- figdata[order(-figdata$Y), , drop = FALSE]
rownames(figdata) <- NULL

group_stats <- do.call(rbind, lapply(contexts, function(ctx) {
  a <- figdata[figdata$Context == ctx, , drop = FALSE]
  data.frame(Context = ctx, N = nrow(a), MeanGap = mean(a$PairedGap),
             HeaderY = unname(headers_y[[ctx]]),
             Ymin = min(a$Y), Ymax = max(a$Y),
             stringsAsFactors = FALSE)
}))
if (any(abs(group_stats$MeanGap - c(0.40733333, 0.03234921, 0.05371429)) > 0.003)) {
  stop('Context means differ from verified newer Chinese snapshot')
}

csv_path <- file.path(data_dir, 'Fig2_prompt_level_quality_gaps.csv')
write.csv(figdata, csv_path, row.names = FALSE, fileEncoding = 'UTF-8')

figdata$Context <- factor(figdata$Context, levels = contexts)
figdata$ObjectConcept <- factor(figdata$ObjectConcept,
                                levels = c('Other content', 'Object–Concept'))
pal <- c('Educational' = '#BB6049',
         'General' = '#5C788D',
         'Chinese-cultural' = '#32847B')

G <- ggplot2::ggplot() +
  ggplot2::geom_segment(
    data = group_stats,
    ggplot2::aes(x = 0, xend = 0, y = Ymin - 0.53, yend = Ymax + 0.53),
    colour = '#6D7C8C', linewidth = 0.48
  ) +
  ggplot2::geom_segment(
    data = group_stats,
    ggplot2::aes(x = avg, xend = avg, y = Ymin - 0.53, yend = Ymax + 0.53),
    colour = '#A2B0BF', linewidth = 0.42, linetype = '33'
  ) +
  ggplot2::geom_segment(
    data = figdata,
    ggplot2::aes(x = 0, xend = PairedGap, y = Y, yend = Y),
    colour = '#B8C3CD', linewidth = 0.67, lineend = 'round'
  ) +
  ggplot2::geom_point(
    data = figdata,
    ggplot2::aes(x = PairedGap, y = Y, fill = Context, shape = ObjectConcept),
    colour = '#243749', size = 3.12, stroke = 0.56
  ) +
  ggplot2::scale_fill_manual(values = pal, guide = 'none') +
  ggplot2::scale_shape_manual(
    name = NULL,
    values = c('Other content' = 21, 'Object–Concept' = 23),
    breaks = c('Other content', 'Object–Concept')
  ) +
  ggplot2::scale_x_continuous(
    limits = c(-0.36, 1.23),
    breaks = c(-0.2, 0, 0.2, 0.4, 0.6, 0.8, 1.0),
    expand = ggplot2::expansion(mult = c(0, 0))
  ) +
  ggplot2::scale_y_continuous(
    limits = c(1, 27.8),
    breaks = figdata$Y,
    labels = figdata$PromptID,
    expand = ggplot2::expansion(mult = c(0, 0))
  ) +
  ggplot2::labs(
    title = NULL,
    subtitle = NULL,
    x = 'Qwen − Z-Image (composite-score points)',
    y = NULL
  ) +
  ggplot2::guides(
    shape = ggplot2::guide_legend(
      order = 1, nrow = 1,
      override.aes = list(fill = '#667B8C', size = 2.8)
    )
  ) +
  ggplot2::theme_classic(base_size = 9.3, base_family = 'sans') +
  ggplot2::theme(
    axis.line = ggplot2::element_blank(),
    axis.ticks = ggplot2::element_blank(),
    panel.grid = ggplot2::element_blank(),
    axis.text.y = ggplot2::element_text(colour = '#283B4E', size = 9.6,
                                       margin = ggplot2::margin(r = 7)),
    axis.text.x = ggplot2::element_text(colour = '#576C7F', size = 9.4,
                                       margin = ggplot2::margin(t = 7)),
    axis.title.x = ggplot2::element_text(colour = '#334A5F', size = 9.6,
                                        margin = ggplot2::margin(t = 13)),
    plot.title = ggplot2::element_text(colour = '#22374B', face = 'bold',
                                       size = 11.3, margin = ggplot2::margin(b = 7)),
    plot.subtitle = ggplot2::element_text(colour = '#596D7E', size = 8.7,
                                          margin = ggplot2::margin(b = 9)),
    plot.title.position = 'plot',
    legend.position = 'top',
    legend.justification = 'right',
    legend.box.just = 'right',
    legend.direction = 'horizontal',
    legend.text = ggplot2::element_text(colour = '#34495C', size = 9.3),
    legend.key.width = grid::unit(11, 'pt'),
    legend.key.height = grid::unit(9, 'pt'),
    legend.margin = ggplot2::margin(0, 0, 4, 0),
    plot.margin = ggplot2::margin(7, 8, 9, 8)
  )

for (i in seq_len(nrow(group_stats))) {
  a <- group_stats[i, , drop = FALSE]
  ctx <- a$Context[[1L]]
  G <- G +
    ggplot2::annotate('text', x = -0.335, y = a$HeaderY,
                      label = sprintf('%s   ·   n = %d', toupper(ctx), a$N),
                      hjust = 0, size = 3.12, fontface = 'bold',
                      colour = unname(pal[[ctx]])) +
    ggplot2::annotate('text', x = 1.16, y = a$HeaderY,
                      label = sprintf('Mean Δ %+.3f', a$MeanGap),
                      hjust = 1, size = 3.00, colour = '#697A8A')
}
G <- G +
  ggplot2::annotate('text', x = -0.32, y = 27.2, label = 'Z-Image higher',
                    hjust = 0, colour = '#8795A2', size = 3.0) +
  ggplot2::annotate('text', x = 0.85, y = 27.2, label = 'Qwen higher',
                    hjust = 1, colour = '#8795A2', size = 3.0)

for (pid in c('B9', 'B5', 'B2')) {
  a <- figdata[figdata$PromptID == pid, , drop = FALSE]
  if (nrow(a) != 1L) stop('Missing anchor prompt ', pid)
  delta <- a$PairedGap[[1L]]
  xx <- delta + ifelse(delta < 0, -0.034, 0.038)
  G <- G + ggplot2::annotate('text', x = xx, y = a$Y[[1L]],
                             label = sprintf('%+.3f', delta),
                             hjust = ifelse(delta < 0, 1, 0),
                             size = 3.0, colour = '#344B60')
}

pdf_path <- file.path(fig_dir, 'Fig2_prompt_level_quality_gaps.pdf')
png_path <- file.path(fig_dir, 'Fig2_prompt_level_quality_gaps.png')
if (capabilities('cairo')) {
  ggplot2::ggsave(pdf_path, plot = G, device = grDevices::cairo_pdf,
                  width = 170/25.4, height = 152/25.4, units = 'in', bg = 'white')
} else {
  ggplot2::ggsave(pdf_path, plot = G, device = 'pdf',
                  width = 170/25.4, height = 152/25.4, units = 'in', bg = 'white',
                  useDingbats = FALSE)
}
ggplot2::ggsave(png_path, plot = G, device = 'png',
                width = 170/25.4, height = 152/25.4, units = 'in', dpi = 600,
                bg = 'white')

cat('FORMAL FIGURE 2: current Chinese snapshot, 170 x 152 mm manuscript-width\n')
cat('Input: ', normalizePath(cfg$input, winslash = '/'), '\n', sep = '')
cat('Output data: ', normalizePath(csv_path, winslash = '/'), '\n', sep = '')
cat('Output PDF: ', normalizePath(pdf_path, winslash = '/'), '\n', sep = '')
cat('Output PNG: ', normalizePath(png_path, winslash = '/'), '\n', sep = '')
cat(sprintf('Check: %d prompts; %d positive; %d negative; mean gap %.6f\n',
            nrow(figdata), wins, losses, avg))
for (i in seq_len(nrow(group_stats))) {
  a <- group_stats[i, , drop = FALSE]
  cat(sprintf('  %s: n=%d, mean=%+.6f\n', a$Context, a$N, a$MeanGap))
}
cat('NOTE: matching is at the prompt-image level, NOT the individual-rater level.\n')
