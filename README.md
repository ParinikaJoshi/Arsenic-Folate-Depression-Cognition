# Folate-Arsenic Effect Modification on Depression & Cognition

**Domain:** Causal Inference · Environmental Epidemiology  
**Tools:** R · G-formula · DAG · NHANES · Sensitivity Analysis

---

## Overview

Investigated whether red blood cell (RBC) folate modifies the causal effect of low-level arsenic exposure on depression (PHQ-9 score) and cognitive function in a nationally representative US adult sample (n = 3,004, NHANES 2011–2014). Applied formal causal inference methods rather than standard regression adjustment.

---

## What I Built

- Constructed a formal **causal DAG** (directed acyclic graph) using domain knowledge to identify confounders, mediators, and effect modifiers — determining the correct adjustment set under the backdoor criterion
- Implemented the **parametric G-formula** to estimate the average treatment effect (ATE) of arsenic under hypothetical interventions, properly handling effect modification by folate status
- Conducted **7 sensitivity analyses** to assess robustness:
  1. E-value calculation (unmeasured confounding threshold)
  2. Quantitative bias analysis (QBA) for selection bias
  3. Arsenic speciation sensitivity (total vs. inorganic)
  4. Alternative folate cutpoints (tertiles vs. clinical threshold)
  5. Complete-case vs. multiple imputation comparison
  6. Alternate depression outcome (binary PHQ-9 ≥ 10)
  7. Cognitive subsample restriction (CERAD word recall subset)

---

## Key Results

| Analysis | Finding |
|---|---|
| Primary (G-formula, depression) | Arsenic effect modified by folate status — significant among low-folate stratum |
| E-value | Unmeasured confounder would need RR > [X] to explain away the association |
| QBA | Results robust to moderate selection bias assumptions |
| Cognitive outcome | Direction consistent; attenuated in high-folate group |

---

## Causal Framework

```
Causal Question:
Does RBC folate modify the effect of arsenic on PHQ-9 / cognition?

DAG nodes:
  Exposure (A):     Urinary total arsenic (log-transformed)
  Outcome (Y):      PHQ-9 depression score; CERAD word recall
  Effect modifier:  RBC folate (low/adequate)
  Confounders (L):  Age, sex, race/ethnicity, BMI, smoking, alcohol,
                    creatinine, poverty-income ratio, education
  Blocked paths:    Dietary pattern (mediator — not adjusted)

Identification:     Backdoor criterion satisfied given adjustment set L
Estimator:          Parametric G-formula (standardization)
```

---

## Technical Stack

| Component | Method/Package |
|---|---|
| DAG construction | dagitty, ggdag (R) |
| G-formula implementation | Custom R code + gfoRmula package |
| Sensitivity analyses | EValue (R), custom QBA functions |
| Survey weighting | survey package (R) |
| Visualization | ggplot2, forestplot |

---

## Why Causal Inference, Not Regression?

Standard multivariable regression answers "is arsenic associated with depression after adjusting for covariates?" — but it cannot answer "what would happen to depression rates if we intervened to lower arsenic exposure?" The G-formula answers the interventional question directly, and properly handles effect modification without collider bias introduced by naive stratification.

---

## Clinical/Policy Relevance

Low-level arsenic is pervasive in US drinking water and rice-based diets. If folate modifies neurotoxic effects, folate supplementation may represent a low-cost, scalable intervention — particularly relevant for populations with both high arsenic exposure and nutritional vulnerability.

---

## Data Source

NHANES 2011–2014 (Laboratory, Mental Health, Cognitive, Dietary modules)  
[https://www.cdc.gov/nchs/nhanes/](https://www.cdc.gov/nchs/nhanes/)
