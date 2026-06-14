

# BEV–ICEV Conjoint Experiment Design & Analysis Pipeline

This repository contains the full pipeline for a stated-choice conjoint experiment studying consumer preferences between Battery Electric Vehicles (BEVs) and Internal Combustion Engine Vehicles (ICEVs).

The project integrates experimental design, survey construction, and econometric analysis in a unified workflow.

---

## What this project does

This project consists of two main components:

### Experimental Design Module
- Constructs full-factorial vehicle attribute profiles
- Applies realism constraints to remove implausible combinations
- Uses D-optimal design (Federov algorithm) to select efficient profiles
- Builds BEV vs ICEV choice tasks
- Removes dominance cases to ensure meaningful trade-offs
- Generates blocked choice sets for survey implementation
- Exports Qualtrics-ready survey files (QSF)

### Econometric Analysis Module
- Processes raw survey data into long-format choice datasets
- Merges experimental design attributes with respondent choices
- Prepares data for conditional logit (clogit) modeling
- Estimates:
  - Baseline models
  - Powertrain interaction models
  - Robustness checks (FE / alternative specifications)
  - Heterogeneity subgroup models

---

##  Key Outputs

- Efficient D-optimal experimental design (BEV vs ICEV)
- Clean choice-task datasets (long & wide format)
- Conditional logit estimation results
- Robustness and interaction effect tables
- Subgroup heterogeneity analysis datasets

---

##  Methods Used

- Stated Choice Experiment (Conjoint Analysis)
- D-optimal Experimental Design
- Conditional Logit Model (clogit)
- Fixed Effects Logit Models
- Subgroup Heterogeneity Analysis
- Dominance Filtering & Trade-off Design

## 📌 Survey Instrument

The original survey instrument used in this study was developed in Chinese and administered to respondents using a Qualtrics-based stated choice experiment platform.

The original Chinese questionnaire can be accessed via the following link:
https://erasmusuniversity.eu.qualtrics.com/jfe/form/SV_0oW71teOfAU5q2q

English-translated version of the full questionnaire is provided in the repository:

- File name: `Questionnaire_Translate.pdf`

This translated version includes all screening questions, attribute descriptions, choice tasks, and demographic questions used in the experiment.

