## Submission

This is a new submission of PsyML 2.4.2.

## Test environments

* Local: Windows 11 x64, R 4.4.1, `R CMD check --as-cran` (with the PDF
  manual, examples including `\donttest{}`, tests and vignette)
* win-builder: R-devel (2026-10-26 r90766 ucrt), Status: 1 NOTE

## R CMD check results

0 errors | 0 warnings | 1 note

* New submission.
* Possibly misspelled words in DESCRIPTION: DBSCAN, SNE, UMAP (names of
  clustering and dimension-reduction methods) and Shapley (surname, as in
  Shapley values). They are spelled correctly.

The local note "unable to verify current time" comes from the offline check
environment.

## Notes for the reviewers

* Functions do not write to disk unless the user supplies a path
  (`export_verbose_txt()`, `export_verbose_json()`, `save_all_plots()`,
  `save_unsupervised_plots()`, `supervised_ml_export()`,
  `generate_report_from_json()`, `generate_report_with_ai()`); the examples
  use `tempdir()` and remove the files.
* Functions do not set the random seed unless a `seed` argument is supplied
  (all `seed` arguments default to `NULL`), and do not change `par()`,
  `options()` or the working directory.
* Console output is produced with `cat()` only inside `print()`/`summary()`
  methods or when `verbose = TRUE`; status information otherwise uses
  `message()`.
* Examples and tests use at most 2 cores (`n_cores = 1` in the examples).
* `\dontrun{}` is used only in `explain_with_ai()` and
  `generate_report_with_ai()`, which need a personal API key and an internet
  connection. The Shiny launchers are wrapped in `if (interactive())`.
* Optional modelling back-ends (e.g. kernelshap, glmnet, kernlab, mclust,
  MatchIt, grf, DoubleML, tmle) are in Suggests and are checked with
  `requireNamespace()` before use.

## Downstream dependencies

There are currently no downstream dependencies for this package.
