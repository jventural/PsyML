#' PsyML: Machine Learning Workflows for Supervised, Unsupervised, and Causal
#' Analysis
#'
#' PsyML is organized around three approaches to machine learning with
#' tabular data, each run by a single function that returns an object with
#' `print()`, `summary()`, `plot()` and `predict()` methods.
#'
#' @section Supervised learning (predicting a known outcome):
#' [supervised_ml()] fits and compares classification or regression models
#' for an outcome that is already observed. It covers exploratory checks,
#' preprocessing, cross-validated training of several models, hyperparameter
#' tuning, test-set evaluation, threshold optimization, calibration, fairness
#' and decision curve analysis, and interpretation with variable importance
#' and Shapley values. Each step is also available as a separate function:
#' [preprocess_data()], [train_models()], [compare_models()],
#' [tune_best_model()], [evaluate_model()], [optimize_threshold()],
#' [analyze_fairness()], [analyze_dca()] and [interpret_model()]. See the
#' "Other supervised learning" links in each help page.
#'
#' @section Unsupervised learning (finding structure without an outcome):
#' [unsupervised_ml()] looks for groups and unusual cases when there is no
#' outcome variable. It compares k-means, hierarchical, DBSCAN, Gaussian
#' mixture and latent class clustering, chooses the number of clusters,
#' reduces dimensionality (PCA, t-SNE, UMAP) and detects anomalies (isolation
#' forest, local outlier factor, Mahalanobis distance). [cluster_profile()]
#' describes the clusters found.
#'
#' @section Causal inference (estimating the effect of a treatment):
#' [causal_ml()] estimates the effect of a treatment or exposure on an
#' outcome from observational data. It checks overlap and balance, and
#' estimates average and heterogeneous effects with propensity scores
#' (matching, inverse probability weighting, doubly robust), meta-learners
#' (S, T and X), double machine learning, causal forests and targeted maximum
#' likelihood estimation.
#'
#' @section Tools shared by the three approaches:
#' * Study design: [ml_sample_size()] estimates the sample size needed for a
#'   target performance from learning curves; [block_split()] and
#'   [diversity_split()] build train and test sets.
#' * Exploratory data analysis: [eda_summary()] and the other `eda_*()`
#'   functions.
#' * Reporting: [export_verbose_txt()], [export_verbose_json()] and
#'   [generate_report_from_json()].
#'
#' The vignette `vignette("three-approaches", package = "PsyML")` runs a short
#' example of each approach.
#'
#' @keywords internal
"_PACKAGE"

#' @importFrom stats sd uniroot plogis rbinom rnorm nls coef predict
#'   as.formula model.frame model.response model.matrix reorder shapiro.test
#'   cor lm quantile median aggregate glm binomial var dist hclust cutree
#'   kmeans prcomp mahalanobis cov qchisq as.dendrogram weighted.mean qnorm
#'   pnorm na.omit anova approx complete.cases cor.test density fitted isoreg
#'   ks.test mad residuals setNames
#' @importFrom utils head capture.output combn
#' @importFrom graphics abline boxplot legend lines polygon text
#' @importFrom grDevices rgb
#' @importFrom pROC roc auc multiclass.roc
#' @importFrom rlang .data :=
NULL
