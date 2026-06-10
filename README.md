# Local T2I Deployment Evaluation Scripts

## Project title

Local text-to-image model evaluation under consumer-GPU and Chinese-prompt constraints.

## Description

This repository contains the code used for a deployment-oriented evaluation of local text-to-image (T2I) models under consumer-grade graphics processing unit (GPU) and Chinese-prompt constraints. The scripts cover image generation, metadata extraction, objective scoring, subjective-rating transformation, image-level aggregation, reliability analysis, mixed-effects modeling, figure production, and complementary Z-Image-Turbo analyses.

The code supports the manuscript **"An Empirical Evaluation of Local Text-to-Image Models under Consumer-GPU and Chinese-Prompt Constraints: A Higher-Education Deployment Study"**.

The project is organized around four experimental modules:

1. **Calibration module**: evaluates candidate parameter settings and selects fixed settings for each model.
2. **Benchmark module**: analyzes subjective image ratings, objective metrics, bilingual score shifts, and deployment trade-offs.
3. **Extended objective-metric module**: examines objective-metric behavior across additional prompts.
4. **Z-Image-Turbo complementary module**: examines prompt micro-revision stability, incremental-information responsiveness, and complex-prompt boundary cases.

## Citation and archive

- **Code archive DOI:** `10.5281/zenodo.20356433`
- **Data archive:** Zenodo record `20356669`, provided through the restricted review link in the PeerJ submission system.

The code archive contains Python scripts, R scripts, ComfyUI workflow templates, configuration files, and utility scripts. The data archive contains the prompt workbook, original score workbooks, subjective image-level score summaries, statistical analysis outputs, ComfyUI-related supplementary materials, and generated-image archives.

## Repository/database links for review

For PeerJ review, the raw data and supplementary materials are provided through the associated Zenodo data record. If the data record is still restricted during peer review, use the restricted review link supplied in the PeerJ submission system. The data record will be made public upon acceptance if required by the journal's open-data policy.

## Directory structure

The exact file locations can be changed, but the paths in the Python and R scripts must then be updated accordingly. In the archived code, the key Python scripts are expected at the repository root unless they have been moved into a local `scripts/` directory.

A typical local structure for reproducing the analyses is:

```text
project_root/
├── data/
│   ├── prompts.xlsx
│   ├── calibration_scores_ori.xlsx
│   ├── benchmark_objective_scores_ori.xlsx
│   ├── benchmark_subjective_scores_ori_image_level.xlsx
│   ├── benchmark_subjective_reliability_results.xlsx
│   ├── extended_scores_ori.xlsx
│   ├── zimage_complement_scores_ori.xlsx
│   ├── benchmarkImages/
│   ├── calibrationImages/
│   ├── calibrationBestSetImages/
│   ├── extendedImages/
│   └── zimage_complementImages/
├── outputs/
│   ├── calibration/
│   ├── benchmark/
│   ├── extended/
│   └── zimage_complement/
├── workflows/
│   └── ComfyUI workflow JSON files
├── R/
│   ├── benchmark_reliability_analysis.R
│   ├── benchmark_am1_model_deploy_analysis.R
│   ├── benchmark_am2_plot_scores.R
│   ├── benchmark_am2_profile_plot.R
│   ├── benchmark_am3_bilingual_transfer.R
│   ├── benchmark_am3_plot_composite_cn_en.R
│   ├── benchmark_am4_deployment_analysis.R
│   ├── extended_analysis.R
│   └── figure8_extended_LMM_bilingual_emmeans.R
├── calibration_t2i.py
├── benchmark_extended_t2i.py
├── benchmark_subjective_sample.py
├── extract_and_score_to_excel.py
├── calibration_analyze.py
├── benchmark_objective_normalize.py
├── extended_analyze.py
├── zimage_complement_t2i.py
├── zimage_complement_m1_stability_analysis.py
├── zimage_complement_m2_checklist_summary.py
├── figure4_benchmark_3x3_merge_image.py
├── figure9_zimage_complement_m3_merge_image.py
├── config.py
├── requirements.txt
└── README.md
```

If your local copy stores Python scripts under `scripts/` or another subfolder, keep that structure and update the commands or path variables accordingly.

## Main dependencies

### Python

Python scripts require Python 3.10+ and commonly use:

- `pandas`
- `numpy`
- `openpyxl`
- `Pillow`
- `requests`
- `psutil`
- `matplotlib`
- `open_clip_torch`
- `pyiqa`

Install Python dependencies with:

```bash
pip install -r requirements.txt
```

The exact package versions should follow the environment used for the manuscript if full reproduction is required. Objective scores may change if scoring libraries, model weights, or image-preprocessing behavior are changed.

### R

R scripts require packages such as:

- `tidyverse`
- `readxl`
- `writexl`
- `openxlsx`
- `ggplot2`
- `lme4`
- `lmerTest`
- `emmeans`
- `effectsize`
- `car`

Install missing R packages before running the analysis scripts. The exact package requirements may vary by script.

### ComfyUI and model weights

Image-generation scripts require:

- a running local ComfyUI environment;
- a reachable local ComfyUI API endpoint;
- locally available model weights;
- model paths and workflow JSON files configured in `config.py` and the workflow directory.

Model weight files are not included in this repository. Please obtain them from the corresponding official model repositories or model cards and configure the local paths before running the generation scripts.

## Dataset information

The main data files are stored in the associated Zenodo data archive, not in this GitHub/code repository, because the generated-image archives are large.

Key data files include:

- `prompts.xlsx`: prompt workbook and prompt metadata.
- `calibration_scores_ori.xlsx`: original objective scores for the Calibration module.
- `benchmark_objective_scores_ori.xlsx`: original objective scores for the Benchmark module.
- `benchmark_subjective_scores_ori_image_level.xlsx`: curated image-level subjective-score summaries for the Benchmark module.
- `benchmark_subjective_reliability_results.xlsx`: reliability analysis outputs.
- `extended_scores_ori.xlsx`: original objective scores for the Extended objective-metric module.
- `zimage_complement_scores_ori.xlsx`: original objective scores for the Z-Image-Turbo complementary module.
- `benchmark_am*.xlsx`: Benchmark analysis outputs.
- `calibration_analysis.zip`, `extended_analysis.zip`, and `zimage_complement_analysis.zip`: processed analysis outputs.
- `*Images.zip`: generated-image archives for the experimental modules.

The workbooks with `ori` in their names are the original score workbooks or curated raw-data tables used as the starting point for the analyses.

## Data assumptions

The prompt workbook is expected to contain a prompt sheet with columns such as:

- `ID`
- `Set`
- `Chinese Prompt`
- `English Prompt`
- `Content`
- `Style`
- `Complexity`
- `Context`

Generated image names and sidecar metadata files are expected to follow the naming conventions used in the generation scripts. Excel sheet names and generated headers use English labels. Multi-word generated headers and sheet names use CamelCase without spaces, for example `ModelShortName`, `PromptID`, `MeanAdherence`, `ValidRaterCount`, `MetricDistributionStats`, and `NormalizationStrategy`.

Some downstream scripts depend on exact workbook names, sheet names, and column names. If files are renamed or reorganized, the path variables and sheet-name references in the scripts should be updated.

## Python script overview

### Image generation and sampling

- `calibration_t2i.py`  
  Generates Calibration images for candidate resolution and step settings using the prompt workbook, ComfyUI API configuration, local model weights, and workflow templates.

- `benchmark_extended_t2i.py`  
  Generates Benchmark and Extended images using the fixed parameter settings selected from Calibration.

- `benchmark_subjective_sample.py`  
  Randomly selects one image from each model-prompt-language group in the Benchmark module for questionnaire-based subjective evaluation.

- `zimage_complement_t2i.py`  
  Generates Z-Image-Turbo complementary-analysis images for prompt micro-revision stability, incremental-information responsiveness, and complex-prompt boundary cases.

### Metadata extraction, objective scoring, and normalization

- `extract_and_score_to_excel.py`  
  Extracts generation metadata and computes objective image-quality and text-image alignment scores. It is used to produce the original objective-score workbooks for the Calibration, Benchmark, Extended, and Z-Image-Turbo complementary modules.

- `calibration_analyze.py`  
  Normalizes Calibration objective metrics, computes weighted composite objective scores, summarizes candidate parameter settings, and ranks candidate configurations.

- `benchmark_objective_normalize.py`  
  Normalizes Benchmark objective metrics and constructs objective-score summaries where required.

- `extended_analyze.py`  
  Normalizes Extended objective metrics and generates Extended objective-score summaries and supporting outputs.

### Z-Image-Turbo complementary analysis

- `zimage_complement_m1_stability_analysis.py`  
  Analyzes prompt micro-revision stability using objective metrics and manually checked columns where applicable.

- `zimage_complement_m2_checklist_summary.py`  
  Summarizes structured checklist results for incremental-information responsiveness.

### Figure production

- `figure4_benchmark_3x3_merge_image.py`  
  Merges selected Benchmark images into a publication-ready composite figure.

- `figure9_zimage_complement_m3_merge_image.py`  
  Merges selected Z-Image-Turbo complementary boundary-case images into a publication-ready composite figure.

Additional Python utility scripts may be present in the archive. Their inputs and outputs should follow the same workbook, image-folder, and output-folder conventions described here.

## Quick start

1. Download or clone this code repository.
2. Download the associated Zenodo data archive.
3. Extract generated-image ZIP archives if image inspection or composite-figure reproduction is required.
4. Place the downloaded data files and extracted image folders under a local `data/` directory, or update the paths in the Python and R scripts.
5. Install Python dependencies with `pip install -r requirements.txt`.
6. Install the required R packages.
7. Configure the local ComfyUI endpoint, model paths, workflow JSON paths, and output directories in `config.py` if image generation or objective scoring will be rerun.
8. Run the Python scripts and R scripts in the recommended execution order below.
9. Check `outputs/` or script-specific output folders for generated workbooks, tables, and figures.

If the goal is only to verify the statistical analyses reported in the manuscript, begin with the original score workbooks and the curated subjective image-level workbook from the Zenodo data archive. It is not necessary to regenerate all images unless the image-generation process itself needs to be reproduced.

## Recommended execution order

### 1. Calibration module

1. `calibration_t2i.py`  
   Generates Calibration images for candidate parameter settings.  
   Input: `prompts.xlsx`, ComfyUI workflow configuration, local model weights, and model parameter settings.  
   Output: Calibration images and sidecar metadata files.  
   Data archive counterpart: `calibrationImages.zip` and `calibrationBestSetImages.zip`.

2. `extract_and_score_to_excel.py`  
   Extracts generation metadata and computes objective metrics.  
   Input: generated Calibration images and sidecar metadata files.  
   Output: objective-score Excel workbook.  
   Data archive counterpart: `calibration_scores_ori.xlsx`.

3. `calibration_analyze.py`  
   Normalizes metrics, computes weighted objective scores, and ranks candidate parameter settings.  
   Input: Calibration objective-score workbook.  
   Output: `summary_statistics.xlsx`, `normalized_scores.xlsx`, and `parameter_group_ranking.xlsx`.  
   Data archive counterpart: `calibration_analysis.zip`.

### 2. Benchmark module and subjective analysis

4. `benchmark_extended_t2i.py` with the Benchmark prompt set  
   Generates Benchmark images using the fixed parameter settings from Calibration.  
   Input: `prompts.xlsx`, fixed model settings, ComfyUI workflows, and local model weights.  
   Output: Benchmark images and sidecar metadata files.  
   Data archive counterpart: `benchmarkImages.zip`.

5. `extract_and_score_to_excel.py`  
   Extracts Benchmark generation metadata and computes objective metrics.  
   Input: Benchmark images and sidecar metadata files.  
   Output: Benchmark objective-score workbook.  
   Data archive counterpart: `benchmark_objective_scores_ori.xlsx`.

6. `benchmark_subjective_sample.py`  
   Randomly selects one image from each model-prompt-language group for questionnaire construction.  
   Input: Benchmark image directory.  
   Output: sampled Chinese and English image folders for questionnaire construction.

7. `R/benchmark_reliability_analysis.R`  
   Estimates rating coverage, variance components, and reliability.  
   Input: subjective-rating workbooks and/or image-level subjective-rating files.  
   Output: reliability tables.  
   Data archive counterpart: `benchmark_subjective_reliability_results.xlsx`.

8. `R/benchmark_am1_model_deploy_analysis.R`  
   Analyzes Chinese-prompt subjective deployment suitability.  
   Input: `benchmark_subjective_scores_ori_image_level.xlsx`.  
   Output: AM1 tables and statistical results.

9. `R/benchmark_am2_plot_scores.R`, `R/benchmark_am2_profile_plot.R`, and related figure scripts  
   Analyze task-attribute patterns and generate figures.  
   Input: image-level subjective-analysis workbook.  
   Output: task-attribute tables and figures.

10. `R/benchmark_am3_bilingual_transfer.R` and `R/benchmark_am3_plot_composite_cn_en.R`  
    Analyze bilingual transfer and cross-language score shifts.  
    Input: image-level subjective-analysis workbook.  
    Output: bilingual analysis tables and figures.

11. `R/benchmark_am4_deployment_analysis.R` and related plots  
    Analyze quality, inference time, and video-memory trade-offs.  
    Input: image-level subjective-analysis workbook with resource metadata.  
    Output: deployment-scenario tables and resource-effect figures.

### 3. Extended objective-metric module

12. `benchmark_extended_t2i.py` with the Extended prompt set  
    Generates Extended images using fixed parameter settings.  
    Input: `prompts.xlsx`, fixed model settings, ComfyUI workflows, and local model weights.  
    Output: Extended images and sidecar metadata files.  
    Data archive counterpart: `extendedImages.zip`.

13. `extract_and_score_to_excel.py`  
    Computes objective metrics for Extended images.  
    Input: Extended images and sidecar metadata files.  
    Output: Extended objective-score workbook.  
    Data archive counterpart: `extended_scores_ori.xlsx`.

14. `benchmark_objective_normalize.py` or `extended_analyze.py`  
    Normalizes objective metrics and constructs weighted objective scores.  
    Input: Extended objective-score workbook.  
    Output: normalized objective-score workbooks and summary reports.  
    Data archive counterpart: `extended_analysis.zip`.

15. `R/extended_analysis.R` and `R/figure8_extended_LMM_bilingual_emmeans.R`  
    Run Extended objective-metric analyses and generate the bilingual estimated-marginal-means figure.  
    Input: normalized Extended objective-score workbook.  
    Output: linear mixed-model tables, robustness checks, and figures.

### 4. Z-Image-Turbo complementary module

16. `zimage_complement_t2i.py`  
    Generates images for Z-Image-Turbo complementary analyses.  
    Input: complementary prompts and fixed Z-Image-Turbo parameters.  
    Output: complementary images and sidecar metadata files.  
    Data archive counterpart: `zimage_complementImages.zip`.

17. `extract_and_score_to_excel.py`  
    Computes objective metrics for complementary images where needed.  
    Input: complementary images and sidecar metadata files.  
    Output: complementary objective-score workbook.  
    Data archive counterpart: `zimage_complement_scores_ori.xlsx`.

18. `zimage_complement_m1_stability_analysis.py`  
    Analyzes prompt-revision stability using objective metrics and manual-check columns.  
    Input: complementary objective-score workbook.  
    Output: Module 1 stability tables.

19. `zimage_complement_m2_checklist_summary.py`  
    Summarizes structured checklist results for incremental-information responsiveness.  
    Input: manually completed checklist workbook.  
    Output: Module 2 checklist summary tables.

20. `figure4_benchmark_3x3_merge_image.py` and `figure9_zimage_complement_m3_merge_image.py`  
    Merge selected generated images into publication-ready composite figures.  
    Input: selected image files.  
    Output: merged figure images.

## Methodology overview

The scripts implement the following workflow:

1. Generate candidate images with fixed local model and ComfyUI settings.
2. Record generation metadata and resource-use information.
3. Compute objective metrics for generated images.
4. Normalize objective metrics and compute weighted objective scores.
5. Transform and aggregate subjective ratings to image-level summaries for the Benchmark module.
6. Estimate rater coverage and reliability.
7. Analyze model, language, task-attribute, and deployment-scenario differences.
8. Generate manuscript-ready tables and figures.
9. Run complementary Z-Image-Turbo analyses for prompt revision, incremental information, and complex-prompt boundary cases.

## Notes on reproducibility

- Keep random seeds, model file names, ComfyUI workflow files, software versions, hardware information, and scoring model versions unchanged when reproducing the reported results.
- The generation scripts assume that ComfyUI is accessible through the configured local API endpoint.
- Objective scores should be regenerated if image files, scoring models, or normalization rules are changed.
- Model weight files are not included in this repository and must be obtained from the corresponding official model repositories or model cards.
- Some downstream scripts depend on the exact sheet names and column names in the provided Excel workbooks.
- The provided original score workbooks and processed output ZIP files can be used to verify analyses without rerunning the full image-generation pipeline.

## Privacy and ethics note

The subjective-evaluation data used by the analysis scripts are provided at the image-summary level in the associated data archive. The data files do not include names, student identification numbers, contact information, or other direct personal identifiers.

## License

The code is released under the MIT License. Data and prompt materials in the separate Zenodo data archive are released under the license specified in that archive. Generated images are shared for research reproducibility and remain subject to the licenses or use terms of the corresponding source models.

## Citation

If you use this code or the associated data, please cite the manuscript and the code archive:

- Chen, Lianglong, and Guo, Yijing. *An Empirical Evaluation of Local Text-to-Image Models under Consumer-GPU and Chinese-Prompt Constraints: A Higher-Education Deployment Study*.
- Code archive DOI: `10.5281/zenodo.20356433`.
