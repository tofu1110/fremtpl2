# Pure Premium Modelling with Frequency–Severity GLMs (freMTPL2)

Predicting the pure premium for French motor third-party liability
insurance with a Poisson GLM for claim frequency and a Gamma GLM for
claim severity, evaluated against intercept-only baselines on a
held-out test set.

## Key results

- The frequency GLM reduced test deviance by 4.4% over the baseline,
  mainly by capturing the high claim rates of young drivers.
- Severity GLMs overfit to a few large claims, so a constant mean severity
  was the better choice.
- Frequency GLM × constant severity ranked risks best
  (Gini 0.31, top-to-bottom decile lift 7.3).

![Lift chart](analysis/03_pure_premium_glm_files/figure-commonmark/lift-chart-1.png)

**Full report:** [analysis/03_pure_premium_glm.md](analysis/03_pure_premium_glm.md)

## Repository layout

```
fremtpl2/
├── R/
│   ├── preprocess.R      # prepare_data()
│   ├── split.R           # split_data()
│   └── metrics.R         # freq_metrics(), sev_metrics(), pp_metrics()
├── scripts/
│   └── 01_download.R
├── analysis/
│   ├── 02_data_check.qmd
│   └── 03_pure_premium_glm.qmd
├── data/raw/             # not committed; SOURCE.txt only
└── renv.lock
```

## How to run

```r
renv::restore()
source("scripts/01_download.R")
```

```bash
quarto render analysis/02_data_check.qmd
quarto render analysis/03_pure_premium_glm.qmd
```
   
Package versions are recorded in `renv.lock` (R version 4.6.1).

## Data

We use freMTPL2freq and freMTPL2sev from the
[CASdatasets](https://dutangc.github.io/CASdatasets/reference/freMTPL.html)
R package. Policies are randomly split 80/20 into train and test sets,
and all claims of a policy go to the same set.

## References

Noll, A., Salzmann, R. and Wüthrich, M. V. (2018).
[Case Study: French Motor Third-Party Liability Claims](https://papers.ssrn.com/sol3/papers.cfm?abstract_id=3164764).
SSRN.

## Use of AI tools

Claude (Anthropic) was used to help with writing code and editing the text.
I ran and reviewed all code and results, and I am responsible for the analysis and its conclusions.