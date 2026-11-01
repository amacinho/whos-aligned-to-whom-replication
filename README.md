# Replication package

This package reproduces the results given in the paper **Who's Aligned to Whom? How AI Agents Shape Experts' Research Judgment Calls**
Amaç Herdağdelen, Hailey Joren, Alex Tamkin

Preregistration: [osf.io/t3q59](https://osf.io/t3q59)

## Running it

```r
install.packages(c("tidyverse", "clubSandwich", "glmnet", "ragg"))
```

From this folder:

```sh
Rscript R/run_all.R
```

The run should take a few minutes and writes everything to `output/`:

- `output/tables/*.csv`: one result table per analysis, named after it (for example `h1_flip_main.csv`); files ending `_main_idle50` repeat the analysis without the disengaged sessions;
- `output/numbers.tex`: the LaTeX macros for the numbers/results the paper's text uses;
- `output/tables/*.tex`: tables;
- `output/figures/`: the data figures;
- `output/derived/`: intermediate participant and trial files.

## Files

```
README.md
R/
  run_all.R                         runs everything below in order
  _setup.R                          packages, paths, readers, scale construction, shared model code
  01_clean.R                        exclusions, usable d1-d4 trials
  02_build_vars.R                   pre-task scales and covariates
  03_h1_flip.R                      H1 pooled flip, per-decision flips
  04_h2_h3.R                        H2 policy shift, H3 departure, H3 falsification checks
  05_covariate_adjustment.R         covariate-adjusted H1 and H2 (lasso selection, held-out gate)
  06_h4.R                           H4 switch-rate gap, randomization test, falsification checks, coder agreement
  07_moderators.R                   flip-rate moderators
  08_posttask_recall.R              recall of choices and estimate, survey vs LLM judge
  09_perceived_purpose.R            perceived-purpose labels and awareness ladder
  10_participant_descriptives.R     participant table, expertise shares, exclusions
  11_ownership.R                    ownership of the recommendation
  12_lean_predicts_recommendation.R leans predicting the recommendation
  13_cite_rates.R                   talking-point citation and AI mention in justifications
  14_decision_paths.R               hand-coded decision paths
  15_decision_flow.R                machine-coded decision flow
  16_code_audit.R                   code audit
  17_confidence.R                   post-task confidence
  18_completion_time.R              task duration
  19_exhibits.R                     reproducible exhibit facts
  20_direct_configs.R               direct simulations: configuration figure, Opus 4.8 baseline
  21_speakup_prevalence.R           speak-up simulations
  22_fig_headline_forest.R          main-results figure
  23_fig_ownership_dots.R           ownership figure
  24_fig_speakup_barbell.R          speak-up figure
  25_fig_sample_flow.R              decision-flow figure (SVG)
  numbers.R                         output/numbers.tex
  tables.R                          output/tables/*.tex
data/
  DICTIONARY.md    every file and column
  participants.csv one row per participant
  decisions.csv    one row per participant and decision
  survey.csv       survey answers
  coded/           transcript and free-text codes, code-audit verdicts
  simulations/     direct-simulation and speak-up runs
  materials/       study materials
  task/            briefing and the synthetic farm data participants analyzed
```

## Tables and figures

| Paper | Script | Output |
|---|---|---|
| Main-results figure (H1-H4 forest) | 22 (from 04, 05, 06) | `figures/headline_forest.pdf` |
| Participant characteristics (Methods) | `tables.R` (from 10) | `tables/participant_descriptives_short.tex` |
| Confirmatory estimates (Results) | `tables.R` (from 04, 05, 06) | `tables/main_estimates.tex` |
| How decisions unfolded (Results, inline table) | 15 | `Flow*` macros |
| Ownership figure | 23 (from 11) | `figures/ownership_dots.pdf` |
| Participant descriptives (appendix) | `tables.R` (from 10) | `tables/participant_descriptives.tex` |
| Estimates without high-idle sessions (appendix) | `tables.R` (from 04, 05, 06 on `main_idle50`) | `tables/main_estimates_idle.tex` |
| Decision-flow figure (appendix) | 25 (from 15) | `figures/sample_flow.svg` |
| Perceived-purpose ladder (appendix) | `tables.R` (from 09) | `tables/purpose_ladder.tex` |
| Direct-simulation configurations (appendix) | 20 | `figures/direct_configs.pdf` |
| Speak-up figure (appendix) | 24 (from 21) | `figures/speakup_barbell.pdf` |

## License

The code in `R/` is released under the MIT License (`LICENSE`). The data and documentation in `data/` are released under CC BY 4.0 (`LICENSE-DATA`).
