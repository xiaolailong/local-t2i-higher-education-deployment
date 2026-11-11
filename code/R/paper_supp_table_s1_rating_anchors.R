#!/usr/bin/env Rscript
# Subjective rating anchors used in the evaluation questionnaire.

root <- normalizePath(getwd(), winslash = '/', mustWork = TRUE)
out <- file.path(root, 'data', 'paper_results', 'Supplementary_Table_S1_rating_anchors.md')

anchors <- data.frame(
  Score = 1:5,
  Adherence = c(
    'Most key content is missing or incorrect.',
    'Only a small part matches, with obvious omissions or errors.',
    'Basic match, but some content is missing or inaccurate.',
    'Mostly matches, with only minor detail inconsistencies.',
    'Fully matches the prompt, with key content accurately presented.'
  ),
  Clarity = c(
    'Severe blur or distortion, affecting recognition.',
    'Clear blur or considerable noise.',
    'Basically clear, with slight blur or defects.',
    'Clear, with only minor defects.',
    'Very clear, with no obvious defects.'
  ),
  Aesthetics = c(
    'Disordered or visually unnatural, with poor overall effect.',
    'Poor overall impression, with obvious visual problems.',
    'Acceptable but not outstanding.',
    'Aesthetically pleasing, with good overall impression.',
    'Highly aesthetically pleasing, with excellent visual effect.'
  ),
  stringsAsFactors = FALSE
)

lines <- c(
  '# Supplementary Table S1. Subjective rating dimensions and score anchors',
  '',
  '| Score | Q1. Prompt adherence | Q2. Image clarity | Q3. Overall aesthetics |',
  '|:--:|:--|:--|:--|'
)
for (i in seq_len(nrow(anchors))) {
  lines <- c(lines, sprintf('| %d | %s | %s | %s |',
    anchors$Score[i], anchors$Adherence[i], anchors$Clarity[i], anchors$Aesthetics[i]))
}
lines <- c(lines, '',
  '**Note.** Composite score is the equally weighted mean of Q1–Q3. Each dimension uses a 1–5 rating scale.',
  '')
dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
writeLines(lines, con = out, useBytes = TRUE)
message('Written: ', out)
