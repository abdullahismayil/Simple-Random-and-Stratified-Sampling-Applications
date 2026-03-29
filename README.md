# Simple Random and Stratified Sampling Applications

This project applies **Stratified Random Sampling (TRÖ)** and **Simple Random Sampling (BRÖ)** methods on a student lifestyle dataset using R, and compares the estimation results.

## Dataset

- **Source:** `student_lifestyle_dataset.csv`
- **Population size (N):** 2000
- **Variable of interest:** GPA
- **Stratification variable:** Stress Level (High, Low, Moderate)

## Contents

- `Örnekleme Ödev.R` — R script with full sampling analysis
- `Örnekleme Ödev 1.docx` — Project report with results and interpretations

## Methods Applied

### Stratified Random Sampling (TRÖ)
- Stratum sizes (Nₕ), standard deviations (Sₕ) and weights (Wₕ) computed per stress level
- Optimal allocation using Neyman allocation (kₕ)
- Sample size determination
- Estimation of: mean, total, proportion, and number of units with a given characteristic
- 95% confidence intervals for all estimators

### Simple Random Sampling (BRÖ)
- Applied on the "Low" stress level stratum (stratum 2)
- Estimation of: mean, total, proportion, and number of units
- 95% confidence intervals for all estimators

## Strata Summary

| Stress Level | Nₕ   | Wₕ    | Sₕ    | kₕ    |
|-------------|------|-------|-------|-------|
| High        | 1029 | 0.514 | 0.275 | 0.571 |
| Low         | 297  | 0.148 | 0.215 | 0.129 |
| Moderate    | 674  | 0.337 | 0.221 | 0.300 |

## Course

Sampling Methods (Örnekleme Yöntemleri)
