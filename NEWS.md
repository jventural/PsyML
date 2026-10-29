# PsyML (development version)

* Stratified splits now keep rare classes stratified: `rsample` pools strata
  that hold less than 10% of the data, so with a rare outcome the train/test
  split and the cross-validation folds were not stratified and the number of
  positive cases varied between partitions. Classification splits now use
  `pool = 0`.
* `balance_method = "down"` failed in every model ("All models failed")
  because `themis::step_downsample()` was called with `over_ratio` instead of
  `under_ratio`.

# PsyML 2.4.2

First release under the name PsyML, prepared for CRAN.

* The package was renamed from easyML (version 2.4.2) to PsyML.
* The package is organized around three approaches, each run by one
  function: supervised learning (`supervised_ml()`), unsupervised learning
  (`unsupervised_ml()`) and causal inference (`causal_ml()`). The package
  help page (`?PsyML`), the new vignette "Three Approaches" and the
  "Other supervised learning", "Other unsupervised learning" and "Other
  causal inference" links in each help page show which functions belong to
  each approach.
* The reinforcement learning module (`reinforcement_ml()` and its methods,
  bandits, MDP and tabular agents, which depended on the `contextual` package)
  was removed.
* Variable importance no longer uses the archived `vip` package:
  `get_importance()` now uses model-specific measures (impurity for ranger and
  rpart, gain for xgboost, absolute coefficients for glmnet, absolute z/t
  statistics for glm/lm) and falls back to permutation importance implemented
  in the package.
* SHAP values are computed with `kernelshap` (Suggests) instead of the
  archived `fastshap` package; `shapviz` (Suggests) is still used for plots
  when installed.
* All documentation, the vignette, the README and the condition messages
  (errors, warnings, messages) are now in English. Console progress output
  of the pipelines is still in Spanish.
* `seed` arguments now default to `NULL`: the random number generator is
  only seeded when a seed is supplied. `supervised_ml()` uses 2 cores by
  default (`n_cores = 2`).
* Nothing is written to disk by default: `export_verbose_txt()`,
  `export_verbose_json()`, `generate_report_from_json()`,
  `supervised_ml_export()`, `save_all_plots()`, `save_unsupervised_plots()`
  and `generate_report_with_ai()` require an explicit path.
* `explain_with_ai()` and `generate_report_with_ai()` no longer include a
  shared API key; they read the key from `api_key` or from the environment
  variable `PSYML_API_KEY`. `explain_with_ai()` gains a `verbose` argument.
* `predict.unsupervisedml()` now takes `object` as its first argument, and
  Gaussian mixture clustering no longer attaches `mclust`.
* `cluster_profile()` gains a `verbose` argument.
* Imports were reduced to the packages actually used (the `tidyverse` and
  `tidymodels` meta-packages are no longer imported).

# easyML 2.4.2 (2026-09-23)

* `fit_models_cv()`: in binary classification the cross-validated PR-AUC was
  computed for the negative class, because `tune::fit_resamples()` passes the
  first probability column to the metrics and `pr_auc` did not follow the
  `event_level`. With imbalanced classes this gave implausible values (e.g.
  .999 with 4% positives versus a true .786). It is now recomputed per fold
  with the probability of the positive class. ROC-AUC was not affected.

# easyML 2.4.1 (2026-09-22)

Fixes found while running `supervised_ml()` on data with strongly imbalanced
classes (4% positives) and highly collinear predictors.

* `compare_models()`: `supervised_ml()` with a single model failed when
  computing the average ranking.
* `calculate_shap()`: predictors removed by preprocessing (high correlation,
  VIF, zero variance) appeared in the SHAP importance with value 0; they are
  now excluded and reported in `$removed_vars`.
* `calculate_shap()`: new `$collinear_vars` with the predictors used that have
  VIF > 5, and a note that the split of SHAP values among nearly redundant
  predictors should not be interpreted variable by variable.
* `calculate_shap()`: in classification the SHAP sample is stratified with the
  same number of cases per class (`$sample_design`).
* Optimal threshold: it is now chosen with out-of-fold predictions on the
  training set and evaluated on the test set
  (`$threshold_optimization$source`, `$threshold_optimization$test_metrics`).
  Previously it was chosen and evaluated on the same test predictions.
