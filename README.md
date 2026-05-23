# Local T2I Deployment Evaluation Scripts

This repository contains the scripts used for a deployment-oriented evaluation of local text-to-image (T2I) models under consumer-grade GPU and Chinese-prompt constraints. The scripts cover image generation, objective scoring, subjective-rating transformation, image-level aggregation, statistical analysis, figure production, and complementary Z-Image-Turbo analyses.

## Citation and archive

**Code archive:**

**Data archive:**

## Main dependencies

Python scripts require Python 3.10+ and commonly use `pandas`, `numpy`, `openpyxl`, `Pillow`, `requests`, `psutil`, `matplotlib`, `open_clip_torch`, and `pyiqa`. The Python dependencies can be installed with:

```bash
pip install -r requirements.txt
```

Image-generation scripts require a running ComfyUI environment and locally available model weights configured in `config.py` and workflow JSON files.

R scripts require packages such as `tidyverse`, `readxl`, `writexl`, `openxlsx`, `ggplot2`, `lme4`, `lmerTest`, `emmeans`, `effectsize`, and `car`, depending on the specific script.

## Data availability

This repository contains the analysis code, R scripts, and ComfyUI workflow templates. The prompt workbook, objective-score workbooks, image-level subjective-score summaries, analysis outputs, and generated image archives are not stored in this GitHub repository because of file size. They are available from the Zenodo data archive listed above.

## Data assumptions

The prompt workbook is expected to contain a prompt sheet with columns such as `ID`, `Set`, `Chinese Prompt`, `English Prompt`, `Content`, `Style`, `Complexity`, and `Context`. Generated image names and sidecar metadata files are expected to follow the naming conventions used in the generation scripts.

All revised generated Excel columns and sheet names use English labels. Multi-word generated headers and sheet names use CamelCase without spaces, for example `ModelShortName`, `PromptID`, `MeanAdherence`, `ValidRaterCount`, `MetricDistributionStats`, and `NormalizationStrategy`.

## Recommended execution order

### 1. Calibration module

1. `calibration_t2i.py`  
   Generates calibration images for candidate parameter settings.  
   Input: prompt workbook and ComfyUI workflow configuration.  
   Output: generated images and sidecar metadata files.

2. `extract_and_score_to_excel.py`  
   Extracts generation metadata and computes objective metrics.  
   Input: generated images and metadata files.  
   Output: objective-score Excel workbook with English headers.

3. `calibration_analyze.py`  
   Normalizes metrics, computes weighted objective scores, and ranks candidate parameter settings.  
   Input: calibration objective-score workbook.  
   Output: `summary_statistics.xlsx`, `normalized_scores.xlsx`, and `parameter_group_ranking.xlsx`.

### 2. Benchmark module and subjective analysis

4. `benchmark_extended_t2i.py` with the Benchmark prompt set  
   Generates Benchmark images using the fixed parameter settings from Calibration.  
   Output: Benchmark images and sidecar metadata files.

5. `benchmark_subjective_sample.py`  
   Randomly selects one image from each model-prompt-language group for subjective rating.  
   Input: Benchmark image directory.  
   Output: sampled Chinese and English folders for questionnaire construction.

6. `R/benchmark_reliability_analysis.R`  
   Estimates rating coverage, variance components, and reliability.  
   Input: long-format and/or image-level subjective-rating files.  
   Output: reliability tables.

7. `R/benchmark_am1_model_deploy_analysis.R`  
   Analyzes Chinese-prompt subjective deployment suitability.  
   Input: image-level subjective-analysis workbook.  
   Output: AM1 tables and statistical results.

8. `R/benchmark_am2_plot_scores.R`, `R/benchmark_am2_profile_plot.R`, and related figure scripts  
   Analyze task-attribute patterns and generate figures.  
   Input: image-level subjective-analysis workbook.  
   Output: task-attribute tables and figures.

9. `R/benchmark_am3_bilingual_transfer.R` and `R/benchmark_am3_plot_composite_cn_en.R`  
   Analyze bilingual transfer and cross-language score shifts.  
   Input: image-level subjective-analysis workbook.  
   Output: bilingual analysis tables and figures.

10. `R/benchmark_am4_deployment_analysis.R` and related plots  
    Analyze quality, inference time, and VRAM tradeoffs.  
    Input: image-level subjective-analysis workbook with resource metadata.  
    Output: deployment-scenario tables and resource-effect figures.

### 3. Extended objective-metric module

11. `benchmark_extended_t2i.py` with the Extended prompt set  
    Generates Extended images using fixed parameter settings.  
    Output: Extended images and sidecar metadata files.

12. `extract_and_score_to_excel.py`  
    Computes objective metrics for Extended images.  
    Output: Extended objective-score workbook.

13. `benchmark_objective_normalize.py` or `extended_analyze.py`  
    Normalizes objective metrics and constructs weighted objective scores.  
    Output: normalized objective-score workbooks and summary reports.

14. `R/extended_analysis.R` and `R/figure8_extended_LMM_bilingual_emmeans.R`  
    Run Extended objective-metric analyses and generate the bilingual EMM figure.  
    Input: normalized Extended objective-score workbook.  
    Output: LMM tables, robustness checks, and figures.

### 4. Z-Image-Turbo complementary module

15. `zimage_complement_t2i.py`  
    Generates images for Z-Image-Turbo complementary analyses.  
    Input: complementary prompts and fixed Z-Image-Turbo parameters.  
    Output: complementary images and sidecar metadata files.

16. `extract_and_score_to_excel.py`  
    Computes objective metrics for complementary images where needed.  
    Output: complementary objective-score workbook.

17. `zimage_complement_m1_stability_analysis.py`  
    Analyzes prompt-revision stability using objective metrics and manual-check columns.  
    Input: complementary objective-score workbook.  
    Output: Module 1 stability tables.

18. `zimage_complement_m2_checklist_summary.py`  
    Summarizes structured checklist results for incremental-information responsiveness.  
    Input: manually completed checklist workbook.  
    Output: Module 2 checklist summary tables.

19. `figure4_benchmark_3x3_merge_image.py` and `figure9_zimage_complement_m3_merge_image.py`  
    Merge selected generated images into publication-ready composite figures.  
    Input: selected image files.  
    Output: merged figure images.

## Notes on reproducibility

- Keep random seeds, model file names, ComfyUI workflow files, software versions, and hardware information unchanged when reproducing the reported results.
- The generation scripts assume that ComfyUI is accessible through the configured local API endpoint.
- Objective scores should be regenerated if image files, scoring models, or normalization rules are changed.
- Model weight files are not included in this repository. Please obtain them from the corresponding official model repositories or model cards and configure the local paths before running the generation scripts.

## License

The code is released under the MIT License. Data and prompt materials in the separate Zenodo data archive are released under the license specified in that archive. Generated images are shared for research reproducibility and remain subject to the licenses of the corresponding source models.
