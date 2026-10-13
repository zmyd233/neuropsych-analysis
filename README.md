# Analysis Code

Manuscript: **Glymphatic dysfunction is linked to brain aging in prodromal Alzheimer's disease** (Ms. Ref. No. DADM-D-26-00613)

This folder contains the R code used for the statistical analyses and figure generation reported in the manuscript. The study examined whether glymphatic function (indexed by the diffusion-tensor-image analysis along the perivascular space [DTI-ALPS] index) is associated with accelerated brain aging (indexed by the corrected brain age gap [cBAG]) and cognitive performance in prodromal Alzheimer's disease (pAD), and tested whether cBAG mediates the association between glymphatic function and cognition.

## Software requirements

- R (>= 4.1; analyses were run with R 4.5.1)
- Key packages (with the versions used in the manuscript, where relevant):

  - Data: `readxl`, `openxlsx`, `foreign`, `haven`
  - Wrangling: `dplyr`, `tidyr`, `tidyverse`, `reshape2`
  - Statistics: `ppcor` (partial correlation), `lavaan` (v0.6-21, SEM), `mice` (v3.18.0, multiple imputation), `pROC`, `glmnet`, `compareGroups`, `table1`, `gtsummary`, `flextable`, `broom`, `outliers`, `Hmisc`
  - Visualization: `ggplot2`, `patchwork`, `gridExtra`, `cowplot`, `pheatmap`, `ggsignif`, `ggtext`, `ggseg`, `glue`, `grid`

Install any missing package with `install.packages("package_name")`.

## Data

- `alps_clinic_age.xlsx` — de-identified analysis dataset (n = 196: 101 normal controls [NC] and 95 pAD). This file is located in the **parent folder** of this code folder (one level up). It is **not** shared publicly owing to patient privacy but is available from the corresponding author upon reasonable request.
- Raw clinical source files (`库1.xlsx`–`库7.xlsx`, `CDR-BS.xlsx`) used by `01_preprocess.R` are not shared; that script is provided for transparency of the preprocessing pipeline only.

## How to run

1. Run the scripts in numerical order (01 → 10) from within this code folder.
2. Each script sets its working directory to the parent folder (`setwd("..")`), which contains `alps_clinic_age.xlsx`.
3. Scripts 02–10 require `alps_clinic_age.xlsx` (either produced by `01_preprocess.R` or placed in the parent folder manually).

## Scripts

| Script | Purpose | Manuscript output |
|---|---|---|
| `01_preprocess.R` | Data cleaning and variable construction (raw → `alps_clinic_age.xlsx`) | `alps_clinic_age.xlsx` |
| `02_figure1.R` | Between-group comparison of five key markers (box plots) | Figure 1 |
| `03_figure2.R` | Correlations among ALPS indices, cBAG, and AD biomarkers (heatmaps) | Figure 2 |
| `04_figure3.R` | Correlations between ALPS/cBAG and cognitive domains | Figure 3 |
| `05_mediation.R` | Structural equation model: cBAG mediates ALPS → cognition | Figure 4; Tables S5–S11 |
| `06_table1.R` | Demographic and clinical characteristics (NC vs pAD) | Table 1 |
| `07_figureS2_brain_age.R` | Brain-age bias correction (Cole leave-one-out cross-validation) | Figure S2 |
| `08_figureS3_PSMD.R` | PSMD vs ALPS/cBAG partial correlations | Figure S3; Table S3 |
| `09_figureS4_cortical.R` | Cortical thickness vs ALPS/cBAG partial correlations | Figure S4; Table S4 |
| `10_sex_stratified.R` | Sex-stratified correlations, SEM, and sex × ALPS interactions | Section 3.7 (sex-stratified results) |

## Analysis conventions

- False-discovery-rate-corrected p-values are reported as `q`.
- Bootstrap resampling: 5,000 bias-corrected resamples for indirect effects.
- Primary covariate adjustment: age, sex, education, APOE ε4 status, and PSMD (a marker of vascular-related white matter injury).
- Cognitive domain composites: Memory = AVLT (N1–N3 + N4 + N5 + N6); Executive function = 1/TMT-B + Stroop; Attention = SDMT + Digit Span; Language = BNT + Verbal Fluency; Visuospatial = Clock Drawing + ROCF copy score.

## Notes

- `01_preprocess.R` requires the raw clinical source files and is provided for transparency only; it cannot be run without those files.
- Output filenames inside some scripts use the authors' internal numbering (e.g., script `08_figureS3_PSMD.R` writes files named `Figure_S4_*`); the table above gives the correspondence to the final manuscript figure/table numbers.
