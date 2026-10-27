# PsyML

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

## Overview

**PsyML** provides single-function machine learning pipelines for tabular
data, as commonly found in psychological, behavioral, and health research.
It is organized around three approaches:

| Approach | Research question | Main function |
|---|---|---|
| Supervised learning | How well can we **predict** an outcome we already observe? | `supervised_ml()` |
| Unsupervised learning | Are there **groups** or unusual cases when there is no outcome? | `unsupervised_ml()` |
| Causal inference | What is the **effect** of a treatment or exposure on an outcome? | `causal_ml()` |

In more detail:

- **`supervised_ml()`**: classification and regression. Exploratory checks,
  preprocessing, training of several models with cross-validation,
  hyperparameter tuning, test-set evaluation, threshold optimization,
  calibration, fairness and decision curve analysis, and interpretation
  (model-specific or permutation importance, Kernel SHAP values).
- **`unsupervised_ml()`**: clustering (k-means, hierarchical, DBSCAN, Gaussian
  mixtures, latent class analysis), dimensionality reduction (PCA, t-SNE,
  UMAP), and anomaly detection (isolation forest, LOF, Mahalanobis distance).
- **`causal_ml()`**: treatment effect estimation with propensity scores
  (matching, IPW, doubly robust), meta-learners, double machine learning,
  causal forests, and TMLE.

It also includes sample size estimation for machine learning based on
learning curves (`ml_sample_size()`), block and diversity-based train/test
splitting, and export of the results to text and JSON files.

PsyML is a renamed and revised version of the easyML package by the same
author.

## Installation

```r
# Development version from GitHub
# install.packages("remotes")
remotes::install_github("jventural/PsyML")
```

## Quick start

### Supervised learning

```r
library(PsyML)

set.seed(1)
n <- 300
df <- data.frame(x1 = rnorm(n), x2 = rnorm(n), x3 = rnorm(n))
df$y <- factor(ifelse(df$x1 + 0.5 * df$x2 + rnorm(n) > 0, "yes", "no"))

result <- supervised_ml(
  data = df,
  target = "y",
  models = c("glm", "rf"),
  cv_folds = 5,
  seed = 2026
)

print(result)
summary(result)
plot(result)
predict(result, df[1:5, ])
```

### Unsupervised learning

```r
result <- unsupervised_ml(
  data = iris,
  exclude_cols = "Species",
  methods = c("clustering", "reduction", "anomaly"),
  clustering_algorithms = c("kmeans", "hierarchical"),
  reduction_methods = "pca",
  anomaly_methods = "mahalanobis",
  seed = 2026
)

print(result)
cluster_profile(result, original_data = iris[, 1:4])
```

### Causal inference

```r
set.seed(1)
n <- 500
d <- data.frame(x1 = rnorm(n), x2 = rnorm(n))
d$treat <- rbinom(n, 1, plogis(0.5 * d$x1))
d$y <- 1 + 0.8 * d$treat + d$x1 + 0.5 * d$x2 + rnorm(n)

result <- causal_ml(d, treatment = "treat", outcome = "y",
                    methods = c("propensity", "meta_learners"), seed = 2026)
print(result)
```

### Sample size estimation

```r
size <- ml_sample_size(
  task = "classification",
  metric = "auc",
  target = 0.80,
  reps = 100,
  seed = 2026
)
size$recommend_n
plot_learning_curve(size)
```

## Reproducibility

Functions that use random numbers have a `seed` argument whose default is
`NULL`: the random number generator is not touched unless you supply a seed
(or call `set.seed()` yourself).

## Files and reports

Nothing is written to disk unless you give a path:

```r
out <- tempdir()
export_verbose_txt(result, file.path(out, "analysis.txt"))
export_verbose_json(result, file.path(out, "analysis.json"))
save_all_plots(result, path = out)
```

`explain_with_ai()` and `generate_report_with_ai()` send a summary of the
results to a large language model provider (Groq or OpenAI). They need your
own API key, given in `api_key` or in the environment variable
`PSYML_API_KEY`.

## Author

Jose Ventura-Leon (ORCID
[0000-0003-2996-4244](https://orcid.org/0000-0003-2996-4244)).

## License

MIT
