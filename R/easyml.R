
# =============================================================================
# PsyML: Funcion Principal para Machine Learning Supervisado
# =============================================================================

#' @title Supervised Machine Learning for Classification and Regression
#'
#' @description
#' Main function of the package. It runs a complete supervised machine
#' learning pipeline: exploratory data analysis, preprocessing, training of
#' several models with cross-validation, hyperparameter tuning of the best
#' model, evaluation on a held-out test set, interpretability (permutation
#' importance and SHAP values) and optional advanced diagnostics (data
#' leakage, threshold optimization, interactions, calibration, nested
#' cross-validation, fairness and decision curve analysis).
#'
#' @details
#' The data are split into a training and a test set (`test_split`). All
#' models listed in `models` are fitted with `cv_folds`-fold cross-validation
#' on the training set and compared with `select_metric`. The best model is
#' optionally tuned (`tune_best`), refitted on the whole training set and
#' evaluated on the test set. For binary classification the decision
#' threshold is chosen with out-of-fold training predictions and evaluated on
#' the test set. When `verbose = TRUE` the console output (in Spanish) is
#' shown in real time and also stored in the returned object
#' (`verbose_text`, `verbose_lines`).
#'
#' @param data A data frame with the predictors and the outcome.
#' @param target Character string with the name of the outcome variable.
#' @param task Type of task: `"auto"` (default; detected from the outcome),
#'   `"classification"` or `"regression"`.
#' @param models Character vector with the models to train (default:
#'   `c("rf", "xgboost", "svm", "nnet", "glm", "tree")`). Available: `"rf"` (random forest), `"xgboost"`, `"svm"` (RBF kernel),
#'   `"nnet"` (neural network), `"glm"` (logistic or linear regression),
#'   `"glmnet"` (regularized regression), `"tree"` (decision tree) and
#'   `"nb"` (naive Bayes). Some models need suggested packages (e.g.
#'   \pkg{kernlab}, \pkg{glmnet}, \pkg{rpart}).
#' @param test_split Proportion of the data held out as test set
#'   (default: 0.20).
#' @param cv_folds Number of cross-validation folds (default: 10).
#' @param select_metric Metric used to select the best model. If NULL
#'   (default) it is chosen automatically: ROC-AUC for classification and
#'   R-squared for regression. Classification: `"roc_auc"`, `"mcc"`,
#'   `"f_meas"` (F1), `"f2_meas"` (F2), `"accuracy"`, `"sensitivity"`,
#'   `"specificity"`, `"bal_accuracy"`, `"pr_auc"`, `"kap"`, `"rank_mean"`.
#'   Regression: `"rsq"`, `"rmse"`, `"mae"`, `"rank_mean"`.
#' @param tune_best Logical. Tune the hyperparameters of the best model
#'   (default: TRUE).
#' @param tune_method Hyperparameter search method: `"random"` (random
#'   search, default), `"grid"` (grid search), `"bayes"` (Bayesian
#'   optimization) or `"racing"` (racing with ANOVA).
#' @param tune_grid Number of hyperparameter combinations evaluated during
#'   tuning (default: 20).
#' @param tune_iter Number of iterations for Bayesian optimization
#'   (default: 30).
#' @param feature_engineering Logical. Apply automatic feature engineering:
#'   detection of suspicious zeros, log-transformation of skewed variables,
#'   missing-value indicators and interactions between the top variables
#'   (default: FALSE).
#' @param custom_features Optional named list to create domain variables.
#'   Each element name is the new variable and its value is an R expression
#'   given as a character string, e.g.
#'   `list(family = "SibSp + Parch", fare_pp = "Fare / (SibSp + Parch + 1)")`
#'   (default: NULL).
#' @param feature_selection Logical. Perform variable selection with
#'   cross-validation (default: FALSE).
#' @param balance_classes Logical. For classification, balance the classes
#'   of the training set (default: FALSE).
#' @param balance_method Balancing method: `"smote"` (default), `"adasyn"`,
#'   `"rose"`, `"up"` or `"down"`.
#' @param impute Logical. Impute missing values (default: TRUE).
#' @param impute_method Imputation method: `"knn"` (default), `"median"` or
#'   `"mean"`.
#' @param normalize Logical. Normalize numeric predictors (default: TRUE).
#' @param normalize_method Normalization method: `"zscore"` (default,
#'   standardization) or `"minmax"` (rescaling between 0 and 1).
#' @param use_pca Logical. Apply principal component analysis after
#'   normalization (default: FALSE).
#' @param pca_threshold Proportion of variance retained by PCA
#'   (default: 0.95).
#' @param treat_outliers Logical. Treat outliers by winsorization
#'   (default: TRUE).
#' @param outlier_percentile Percentile used for winsorization
#'   (default: 0.05).
#' @param remove_high_cor Logical. Remove highly correlated predictors
#'   (default: TRUE).
#' @param cor_threshold Correlation threshold for removal (default: 0.90).
#' @param remove_high_vif Logical. Remove predictors with a high variance
#'   inflation factor (default: TRUE).
#' @param vif_threshold VIF threshold for removal (default: 5).
#' @param run_eda Logical. Run the exploratory data analysis (default: TRUE).
#' @param run_shap Logical. Compute variable importance and SHAP values
#'   (default: TRUE).
#' @param n_shap Number of observations used to compute SHAP values
#'   (default: 100).
#' @param optimize_threshold Logical. Optimize the decision threshold in
#'   binary classification (default: TRUE).
#' @param threshold_method Threshold optimization criterion: `"youden"`
#'   (default), `"f1"` or `"balanced"`.
#' @param calibrate_probs Logical. Calibrate predicted probabilities in
#'   binary classification (default: FALSE).
#' @param calibration_method Calibration method: `"platt"` (default) or
#'   `"isotonic"`.
#' @param check_leakage Logical. Check for possible data leakage after
#'   fitting (default: TRUE).
#' @param nested_cv Logical. Run nested cross-validation for a less biased
#'   performance estimate; only used when the best model is `"rf"` or
#'   `"xgboost"` (default: FALSE).
#' @param analyze_interactions Logical. Analyze interactions and non-linear
#'   effects with splines (default: TRUE).
#' @param protected_var Optional name of a protected variable used in the
#'   fairness analysis, e.g. `"sex"`, `"race"` or `"age_group"`
#'   (default: NULL).
#' @param run_fairness Logical. Run the fairness analysis (default: FALSE).
#'   Requires `protected_var`. Computes demographic parity, equalized odds,
#'   predictive parity and the disparate impact ratio (four-fifths rule).
#'   Classification only.
#' @param fairness_threshold Threshold of the four-fifths rule for the
#'   disparate impact ratio (default: 0.8).
#' @param fairness_correction Bias mitigation strategy for classification:
#'   `"none"` (default, no correction), `"postprocesamiento"`
#'   (post-processing: group-specific decision thresholds, stored in
#'   `group_thresholds` and used by [predict.supervisedml()]) or
#'   `"preprocesamiento"` (pre-processing: the training set is resampled by
#'   protected group and outcome before fitting). Any value other than
#'   `"none"` requires `protected_var` and switches `run_fairness` on; it is
#'   ignored for regression.
#' @param run_dca Logical. Run decision curve analysis of clinical utility,
#'   classification only: net benefit compared with treating all or none
#'   (default: FALSE).
#' @param dca_thresholds Numeric vector of probability thresholds for the
#'   decision curve analysis (default: `seq(0.01, 0.99, by = 0.01)`).
#' @param n_cores Number of cores for parallel processing (default: 2). Use
#'   1 for sequential processing. If NULL, all available cores minus one are
#'   used. Parallel processing requires the packages doParallel and foreach;
#'   if they are not installed, the analysis runs sequentially.
#' @param seed Optional integer seed. If NULL (default) the random number
#'   generator state is not modified.
#' @param verbose Logical. Print the progress and the interpretation of
#'   each step in the console (default: TRUE).
#'
#' @return An object of class `supervisedml` (a list). Main elements:
#' \describe{
#'   \item{task}{Type of task: `"classification"` or `"regression"`.}
#'   \item{target}{Name of the outcome variable.}
#'   \item{best_model}{Name of the selected model.}
#'   \item{best_params}{Tuned hyperparameters of the best model (NULL if not
#'     tuned).}
#'   \item{final_fit}{Final fitted model (a tidymodels workflow).}
#'   \item{test_metrics}{Tibble with the metrics on the test set
#'     (`.metric`, `.estimator`, `.estimate`).}
#'   \item{predictions}{Tibble with the predictions on the test set.}
#'   \item{cv_summary}{Cross-validation summary of all trained models.}
#'   \item{importance}{Data frame with variable importance (`Variable`,
#'     `Importance`).}
#'   \item{train_data, test_data}{Preprocessed training and test sets.}
#'   \item{eda, preprocessing, modeling, tuning, evaluation}{Detailed
#'     results of each step.}
#'   \item{interpretation}{Importance and SHAP results (if
#'     `run_shap = TRUE`).}
#'   \item{threshold_optimization, optimal_threshold}{Threshold optimization
#'     (binary classification).}
#'   \item{leakage_check, interactions_analysis, calibration,
#'     residual_analysis, nested_cv}{Advanced analyses, when run.}
#'   \item{fairness_analysis, fairness_correction, group_thresholds}{Fairness
#'     results (if requested).}
#'   \item{dca}{Decision curve analysis (if `run_dca = TRUE`).}
#'   \item{plots, figures}{Lists of ggplot2 figures.}
#'   \item{figures_catalog}{Catalog of figures with metadata for reports.}
#'   \item{verbose_text, verbose_lines}{Captured console output (only when
#'     `verbose = TRUE`).}
#'   \item{metadata}{List with analysis information (sample sizes, models,
#'     folds, metric, seed, elapsed time, date).}
#' }
#'
#' @seealso [predict.supervisedml()], [plot.supervisedml()],
#'   [save_all_plots()]
#'
#' @examples
#' \donttest{
#' set.seed(1)
#' n <- 200
#' df <- data.frame(x1 = rnorm(n), x2 = rnorm(n), x3 = rnorm(n))
#' df$y <- factor(ifelse(df$x1 + 0.5 * df$x2 + rnorm(n) > 0, "yes", "no"))
#' res <- supervised_ml(df, target = "y", models = "glm", cv_folds = 3,
#'                      tune_best = FALSE, run_shap = FALSE, n_cores = 1,
#'                      verbose = FALSE)
#' print(res)
#' summary(res)
#'
#' # Predictions for new observations
#' new_df <- data.frame(x1 = rnorm(5), x2 = rnorm(5), x3 = rnorm(5))
#' predict(res, new_df)
#' predict(res, new_df, type = "prob")
#'
#' plot(res, type = "roc")
#'
#' # Two models, tuning and SHAP values (slower)
#' res2 <- supervised_ml(df, target = "y", models = c("glm", "rf"),
#'                       cv_folds = 3, tune_grid = 3, n_shap = 20,
#'                       n_cores = 1, seed = 123, verbose = FALSE)
#' res2$best_model
#' plot(res2, type = "shap_bar")
#' if (requireNamespace("patchwork", quietly = TRUE)) plot(res2)
#' }
#'
#' @family supervised learning
#' @export
supervised_ml <- function(data,
                    target,
                    task = c("auto", "classification", "regression"),
                    models = c("rf", "xgboost", "svm", "nnet", "glm", "tree"),
                    test_split = 0.20,
                    cv_folds = 10,
                    select_metric = NULL,
                    tune_best = TRUE,
                    tune_method = c("random", "grid", "bayes", "racing"),
                    tune_grid = 20,
                    tune_iter = 30,
                    feature_engineering = FALSE,
                    custom_features = NULL,
                    feature_selection = FALSE,
                    balance_classes = FALSE,
                    balance_method = c("smote", "adasyn", "rose", "up", "down"),
                    impute = TRUE,
                    impute_method = c("knn", "median", "mean"),
                    normalize = TRUE,
                    normalize_method = c("zscore", "minmax"),
                    use_pca = FALSE,
                    pca_threshold = 0.95,
                    treat_outliers = TRUE,
                    outlier_percentile = 0.05,
                    remove_high_cor = TRUE,
                    cor_threshold = 0.90,
                    remove_high_vif = TRUE,
                    vif_threshold = 5,
                    run_eda = TRUE,
                    run_shap = TRUE,
                    n_shap = 100,
                    optimize_threshold = TRUE,
                    threshold_method = "youden",
                    calibrate_probs = FALSE,
                    calibration_method = "platt",
                    check_leakage = TRUE,
                    nested_cv = FALSE,
                    analyze_interactions = TRUE,
                    protected_var = NULL,
                    run_fairness = FALSE,
                    fairness_threshold = 0.8,
                    fairness_correction = c("none", "postprocesamiento", "preprocesamiento"),
                    run_dca = FALSE,
                    dca_thresholds = seq(0.01, 0.99, by = 0.01),
                    n_cores = 2,
                    seed = NULL,
                    verbose = TRUE) {

  # =========================================================================
  # CAPTURA INTERNA DEL VERBOSE
  # =========================================================================
  # Ejecutar el analisis mostrando output en tiempo real Y capturando
  # para exportacion posterior. Usa sink() con split=TRUE para lograr ambos.

  if (verbose) {
    # Crear archivo temporal para capturar el output
    temp_file <- tempfile(fileext = ".txt")

    # sink con split=TRUE: muestra en consola Y guarda en archivo
    sink(temp_file, split = TRUE)

    tryCatch({
      resultado <- .supervised_ml_internal(
        data = data, target = target, task = task, models = models,
        test_split = test_split, cv_folds = cv_folds, select_metric = select_metric,
        tune_best = tune_best, tune_method = tune_method, tune_grid = tune_grid,
        tune_iter = tune_iter, feature_engineering = feature_engineering,
        custom_features = custom_features, feature_selection = feature_selection,
        balance_classes = balance_classes, balance_method = balance_method,
        impute = impute, impute_method = impute_method,
        normalize = normalize, normalize_method = normalize_method,
        use_pca = use_pca, pca_threshold = pca_threshold,
        treat_outliers = treat_outliers, outlier_percentile = outlier_percentile,
        remove_high_cor = remove_high_cor, cor_threshold = cor_threshold,
        remove_high_vif = remove_high_vif, vif_threshold = vif_threshold,
        run_eda = run_eda, run_shap = run_shap, n_shap = n_shap,
        optimize_threshold = optimize_threshold, threshold_method = threshold_method,
        calibrate_probs = calibrate_probs, calibration_method = calibration_method,
        check_leakage = check_leakage, nested_cv = nested_cv,
        analyze_interactions = analyze_interactions,
        protected_var = protected_var, run_fairness = run_fairness,
        fairness_threshold = fairness_threshold,
        fairness_correction = fairness_correction,
        run_dca = run_dca, dca_thresholds = dca_thresholds,
        n_cores = n_cores,
        seed = seed, verbose = TRUE
      )
    }, finally = {
      # Siempre cerrar el sink
      sink()
    })

    # Leer el output capturado del archivo temporal
    if (file.exists(temp_file)) {
      verbose_output <- readLines(temp_file, warn = FALSE)
      unlink(temp_file)  # Eliminar archivo temporal
    } else {
      verbose_output <- character(0)
    }

    # Almacenar el verbose en el resultado para exportacion posterior
    resultado$verbose_text <- paste(verbose_output, collapse = "\n")
    resultado$verbose_lines <- verbose_output

  } else {
    # Sin verbose, ejecutar directamente
    resultado <- .supervised_ml_internal(
      data = data, target = target, task = task, models = models,
      test_split = test_split, cv_folds = cv_folds, select_metric = select_metric,
      tune_best = tune_best, tune_method = tune_method, tune_grid = tune_grid,
      tune_iter = tune_iter, feature_engineering = feature_engineering,
      custom_features = custom_features, feature_selection = feature_selection,
      balance_classes = balance_classes, balance_method = balance_method,
      impute = impute, impute_method = impute_method,
      normalize = normalize, normalize_method = normalize_method,
      use_pca = use_pca, pca_threshold = pca_threshold,
      treat_outliers = treat_outliers, outlier_percentile = outlier_percentile,
      remove_high_cor = remove_high_cor, cor_threshold = cor_threshold,
      remove_high_vif = remove_high_vif, vif_threshold = vif_threshold,
      run_eda = run_eda, run_shap = run_shap, n_shap = n_shap,
      optimize_threshold = optimize_threshold, threshold_method = threshold_method,
      calibrate_probs = calibrate_probs, calibration_method = calibration_method,
      check_leakage = check_leakage, nested_cv = nested_cv,
      analyze_interactions = analyze_interactions,
      protected_var = protected_var, run_fairness = run_fairness,
      fairness_threshold = fairness_threshold,
      fairness_correction = fairness_correction,
      run_dca = run_dca, dca_thresholds = dca_thresholds,
      n_cores = n_cores,
      seed = seed, verbose = FALSE
    )
  }

  return(resultado)
}


# Runs the internal pipeline with package-version and convergence warnings
# from the modelling back-ends suppressed locally (instead of changing
# options(warn)).
#' @noRd
.supervised_ml_internal <- function(...) {
  suppressWarnings(.supervised_ml_internal_impl(...))
}

#' @noRd
#' @title Internal Implementation of supervised_ml
#' @noRd
.supervised_ml_internal_impl <- function(data,
                              target,
                              task = c("auto", "classification", "regression"),
                              models = c("rf", "xgboost", "svm", "nnet", "glm", "tree"),
                              test_split = 0.20,
                              cv_folds = 10,
                              select_metric = NULL,
                              tune_best = TRUE,
                              tune_method = c("random", "grid", "bayes", "racing"),
                              tune_grid = 20,
                              tune_iter = 30,
                              feature_engineering = FALSE,
                              custom_features = NULL,
                              feature_selection = FALSE,
                              balance_classes = FALSE,
                              balance_method = c("smote", "adasyn", "rose", "up", "down"),
                              impute = TRUE,
                              impute_method = c("knn", "median", "mean"),
                              normalize = TRUE,
                              normalize_method = c("zscore", "minmax"),
                              use_pca = FALSE,
                              pca_threshold = 0.95,
                              treat_outliers = TRUE,
                              outlier_percentile = 0.05,
                              remove_high_cor = TRUE,
                              cor_threshold = 0.90,
                              remove_high_vif = TRUE,
                              vif_threshold = 5,
                              run_eda = TRUE,
                              run_shap = TRUE,
                              n_shap = 100,
                              optimize_threshold = TRUE,
                              threshold_method = "youden",
                              calibrate_probs = FALSE,
                              calibration_method = "platt",
                              check_leakage = TRUE,
                              nested_cv = FALSE,
                              analyze_interactions = TRUE,
                              protected_var = NULL,
                              run_fairness = FALSE,
                              fairness_threshold = 0.8,
                              fairness_correction = c("none", "postprocesamiento", "preprocesamiento"),
                              run_dca = FALSE,
                              dca_thresholds = seq(0.01, 0.99, by = 0.01),
                              n_cores = 2,
                              seed = NULL,
                              verbose = TRUE) {

  # Suprimir warnings de versiones de paquetes

  # Configurar procesamiento paralelo
  parallel_active <- FALSE
  if (is.null(n_cores)) {
    n_cores <- max(1, parallel::detectCores() - 1)
  }
  if (n_cores > 1 && requireNamespace("doParallel", quietly = TRUE) &&
      requireNamespace("foreach", quietly = TRUE)) {
    tryCatch({
      cl <- parallel::makeCluster(n_cores)
      doParallel::registerDoParallel(cl)
      parallel_active <- TRUE
      on.exit({
        parallel::stopCluster(cl)
        foreach::registerDoSEQ()
      })
    }, error = function(e) {
      # Si falla la paralelizacion, continuar secuencial
      n_cores <- 1L
    })
  }

  if (!is.null(seed)) set.seed(seed)
  task <- match.arg(task)
  tune_method <- match.arg(tune_method)
  fairness_correction <- match.arg(fairness_correction)
  if (fairness_correction != "none") {
    if (is.null(protected_var)) stop("fairness_correction requires protected_var")
    if (task == "regression") {
      message("fairness_correction only applies to classification; it is ignored.")
      fairness_correction <- "none"
    }
    if (!run_fairness) {
      run_fairness <- TRUE
      if (verbose) cat("    NOTA: run_fairness activado por fairness_correction\n")
    }
  }
  start_time <- Sys.time()

  .validate_inputs(data, target, models)

  if (task == "auto") {
    task <- .detect_task(data[[target]])
  }

 # Validar y establecer select_metric
  n_levels_target <- length(levels(factor(data[[target]])))
  metric_auto_selected <- is.null(select_metric)

  if (is.null(select_metric)) {
    if (task == "classification") {
      select_metric <- "roc_auc"
    } else {
      select_metric <- "rsq"  # Chicco et al. (2021)
    }
  } else {
    valid_class_metrics <- c("roc_auc", "f_meas", "f2_meas", "accuracy", "sensitivity",
                              "specificity", "bal_accuracy", "pr_auc", "mcc", "kap",
                              "rank_mean")
    valid_reg_metrics <- c("rmse", "rsq", "mae", "rank_mean")

    if (task == "classification" && !select_metric %in% valid_class_metrics) {
      stop("Invalid metric for classification: ", select_metric,
           "\nOptions: ", paste(valid_class_metrics, collapse = ", "))
    }
    if (task == "regression" && !select_metric %in% valid_reg_metrics) {
      stop("Invalid metric for regression: ", select_metric,
           "\nOptions: ", paste(valid_reg_metrics, collapse = ", "))
    }
    # pr_auc no disponible en multiclass
    if (task == "classification" && select_metric == "pr_auc" && n_levels_target >= 3) {
      if (verbose) cat("    [!] pr_auc no disponible en multiclase. Usando roc_auc.\n")
      select_metric <- "roc_auc"
    }
  }

  if (verbose) {
    .msg_header("supervised_ml - Machine Learning Supervisado")
    cat("Observaciones:", nrow(data), "\n")
    cat("Variables:", ncol(data) - 1, "\n")
    cat("Variable objetivo:", target, "\n")
    cat("Tarea:", task, "\n")
    cat("Modelos:", paste(models, collapse = ", "), "\n")
    cat("Procesamiento paralelo:", if (parallel_active) paste0("SI (", n_cores, " cores)") else "NO (secuencial)", "\n")
    cat("Metrica de seleccion:", select_metric,
        if (metric_auto_selected) "(auto)" else "(usuario)", "\n")
    if (metric_auto_selected) {
      if (task == "classification") {
        cat("  Referencia: Chicco & Jurman (2023). The MCC should replace the\n")
        cat("  ROC AUC as the standard metric. Patterns, 4(12), 100804.\n")
      } else {
        cat("  Referencia: Chicco et al. (2021). R-squared is more informative\n")
        cat("  than SMAPE, MAE, MSE and RMSE. PeerJ Comput Sci, 7, e623.\n")
      }
    }
  }

  resultado <- list()

  # 3. EDA
  if (run_eda) {
    eda_result <- eda_summary(data = data, target = target, task = task, verbose = verbose)
    resultado$eda <- eda_result
  }

  # 3.5 FEATURE ENGINEERING AUTOMATICO
  if (feature_engineering) {
    fe_result <- auto_feature_engineering(data = data, target = target, verbose = verbose)
    data <- fe_result$data
    resultado$feature_engineering <- fe_result
  }

  # 3.6 FEATURE ENGINEERING DE DOMINIO (custom_features)
  if (!is.null(custom_features) && length(custom_features) > 0) {
    cf_result <- apply_custom_features(data = data, custom_features = custom_features,
                                       verbose = verbose)
    data <- cf_result$data
    resultado$custom_features <- cf_result
  }

  # 4. PREPROCESAMIENTO
  preprocess_result <- preprocess_data(
    data = data, target = target, task = task, test_split = test_split,
    feature_selection = feature_selection,
    balance_classes = balance_classes, balance_method = balance_method,
    impute = impute, impute_method = impute_method,
    normalize = normalize, normalize_method = normalize_method,
    use_pca = use_pca, pca_threshold = pca_threshold,
    treat_outliers = treat_outliers, outlier_percentile = outlier_percentile,
    remove_high_cor = remove_high_cor, cor_threshold = cor_threshold,
    remove_high_vif = remove_high_vif, vif_threshold = vif_threshold,
    seed = seed, verbose = verbose
  )
  resultado$preprocessing <- preprocess_result

  # Fairness resample (pre-processing)
  if (fairness_correction == "preprocesamiento" && !is.null(protected_var)) {
    if (protected_var %in% names(preprocess_result$train_data)) {
      if (verbose) .print_subsection(2, 4, "Resampleo por Equidad")
      preprocess_result$train_data <- resample_fairness(
        train_data = preprocess_result$train_data,
        target = target,
        protected_var = protected_var,
        seed = seed,
        verbose = verbose
      )
      # Disable SMOTE if active to avoid double balancing
      if (balance_classes && verbose) {
        cat("    NOTA: fairness_correction='preprocesamiento' incluye balanceo.\n")
        cat("    Se omite SMOTE adicional.\n")
      }
    }
  }

  # 5. MODELADO
  modeling_result <- train_models(
    preprocess_result = preprocess_result, models = models,
    cv_folds = cv_folds, select_metric = select_metric,
    seed = seed, verbose = verbose
  )
  resultado$modeling <- modeling_result

  # 6. TUNING
  if (tune_best) {
    tuning_result <- tune_best_model(
      modeling_result = modeling_result,
      method = tune_method,
      grid_size = tune_grid,
      iter = tune_iter,
      select_metric = select_metric,
      seed = seed,
      verbose = verbose
    )
  } else {
    tuning_result <- list(
      tuned = FALSE, original_model = modeling_result$best_model,
      best_params = NULL,
      final_workflow = .create_final_workflow(modeling_result$best_model, task, preprocess_result$recipe)
    )
    class(tuning_result) <- c("supervisedml_tuning", "list")
    if (verbose) {
      .print_section(6, "Tuning de Hiperparametros")
      cat("\n    Tuning omitido por configuracion\n")
    }
  }
  resultado$tuning <- tuning_result

  # 7. EVALUACION
  evaluation_result <- evaluate_model(
    tuning_result = tuning_result,
    train_data = preprocess_result$train_data,
    test_data = preprocess_result$test_data,
    target = target, task = task,
    modeling_result = modeling_result,
    best_model = modeling_result$best_model,
    verbose = verbose
  )
  resultado$evaluation <- evaluation_result

  # 6. INTERPRETABILIDAD
  if (run_shap) {
    interpret_result <- interpret_model(
      evaluation_result = evaluation_result,
      train_data = preprocess_result$train_data,
      test_data = preprocess_result$test_data,
      target = target, n_shap = n_shap, verbose = verbose
    )
    resultado$interpretation <- interpret_result
  } else {
    if (verbose) {
      .print_section(6, "Interpretabilidad del Modelo")
      cat("\n    [!] Seccion omitida: run_shap = FALSE\n")
      cat("    Para calcular importancia de variables y valores SHAP, ejecute\n")
      cat("    supervised_ml(..., run_shap = TRUE)\n")
    }
  }

  # 7. FUNCIONALIDADES AVANZADAS
  if (verbose) {
    .print_section(7, "Analisis Avanzado")
    cat("    Esta seccion incluye analisis opcionales que van mas alla de la\n")
    cat("    evaluacion basica. Se llama 'avanzado' porque estos diagnosticos\n")
    cat("    ayudan a detectar problemas sutiles y optimizar el modelo.\n")
  }

  # Contador dinamico para subsecciones de la seccion 7
  adv_sub <- 0

  # Deteccion de Data Leakage (Post-Modelo)
  if (check_leakage) {
    adv_sub <- adv_sub + 1
    if (verbose) {
      .print_subsection(7, adv_sub, "Deteccion de Data Leakage (Post-Modelo)")
      cat("    Las verificaciones pre-modelo (correlacion > 0.95 con target,\n")
      cat("    posibles IDs) se realizaron en EDA seccion 1.2.8.\n\n")
      cat("    Aqui se verifican indicadores que solo se pueden evaluar\n")
      cat("    despues de entrenar el modelo.\n\n")
    }
    importance_for_leakage <- resultado$importance
    leakage_result <- detect_data_leakage(
      model_results = list(test_metrics = evaluation_result$metrics),
      importance = importance_for_leakage,
      verbose = verbose
    )
    resultado$leakage_check <- leakage_result
    if (verbose) .print_reference("leakage")
  }

  # Detectar numero de niveles del target
  n_levels <- length(levels(factor(preprocess_result$train_data[[target]])))

  # Optimizacion de Threshold (solo clasificacion binaria)
  if (task == "classification" && optimize_threshold && n_levels == 2) {
    adv_sub <- adv_sub + 1
    if (verbose) {
      .print_subsection(7, adv_sub, "Optimizacion de Threshold")
      cat("    Por defecto, el modelo clasifica como positivo si la probabilidad\n")
      cat("    es >= 0.5. Pero este umbral no siempre es optimo. La optimizacion\n")
      cat("    busca el mejor punto de corte segun el criterio seleccionado.\n\n")
    }
    # El umbral se ELIGE con predicciones fuera de pliegue del conjunto de
    # entrenamiento (el workflow final, ya afinado, reajustado con los mismos
    # pliegues de la validacion cruzada) y se EVALUA en el conjunto de prueba.
    # Hasta la v2.4.0 se elegia y evaluaba sobre las mismas predicciones de
    # prueba: la sensibilidad y especificidad reportadas eran optimistas.
    oof <- .oof_predictions(tuning_result$final_workflow, modeling_result$cv_folds,
                            target, seed)
    threshold_source <- if (is.null(oof)) "test" else "cv_oof"
    if (verbose && threshold_source == "cv_oof") {
      cat("    El umbral se elige con predicciones fuera de pliegue (validacion\n")
      cat("    cruzada del entrenamiento) y se evalua en el conjunto de prueba.\n\n")
    } else if (verbose) {
      cat("    [!] No se pudieron obtener predicciones fuera de pliegue: el umbral\n")
      cat("        se elige sobre el conjunto de prueba (estimacion optimista).\n\n")
    }
    threshold_result <- optimize_threshold(
      predictions = if (is.null(oof)) evaluation_result$predictions else oof,
      target = target,
      method = threshold_method,
      verbose = verbose
    )
    threshold_result$source <- threshold_source
    threshold_result$test_metrics <- .threshold_metrics_on(
      evaluation_result$predictions, target, threshold_result$optimal_threshold)
    if (verbose && !is.null(threshold_result$test_metrics)) {
      tm <- threshold_result$test_metrics
      cat("    En el conjunto de prueba, con ese umbral:\n")
      cat("      - Sensibilidad:", round(tm$sensitivity * 100, 1), "%\n")
      cat("      - Especificidad:", round(tm$specificity * 100, 1), "%\n\n")
    }
    resultado$threshold_optimization <- threshold_result
    resultado$optimal_threshold <- threshold_result$optimal_threshold
    if (verbose) .print_reference("threshold")
  }

  if (task == "classification" && optimize_threshold && n_levels >= 3) {
    adv_sub <- adv_sub + 1
    if (verbose) {
      .print_subsection(7, adv_sub, "Optimizacion de Threshold")
      cat("    Optimizacion de threshold disponible solo para clasificacion binaria.\n")
      cat("    (", n_levels, " clases detectadas)\n\n", sep = "")
    }
  }

  # Analisis de Interacciones y Efectos No Lineales
  if (analyze_interactions) {
    adv_sub <- adv_sub + 1
    if (verbose) {
      .print_subsection(7, adv_sub, "Analisis de Interacciones y Splines")
      cat("    Este analisis detecta posibles interacciones entre variables y\n")
      cat("    efectos no lineales que podrian mejorar el modelo, especialmente\n")
      cat("    para modelos lineales (GLM). Los modelos de arboles (RF, XGBoost)\n")
      cat("    capturan estos efectos automaticamente.\n\n")
    }
    importance_for_interactions <- if (run_shap && !is.null(interpret_result)) {
      interpret_result$importance
    } else {
      .get_variable_importance(evaluation_result$final_fit)
    }

    interactions_result <- analyze_interactions_splines(
      data = data,
      target = target,
      task = task,
      importance = importance_for_interactions,
      top_n = 5,
      verbose = verbose
    )
    resultado$interactions_analysis <- interactions_result
    if (verbose) {
      .print_reference("interactions")
      .print_reference("splines")
    }
  }

  # Calibracion de Probabilidades (solo clasificacion binaria)
  if (task == "classification" && calibrate_probs && n_levels == 2) {
    adv_sub <- adv_sub + 1
    if (verbose) .print_subsection(7, adv_sub, "Calibracion de Probabilidades")
    calibration_result <- calibrate_probabilities(
      predictions = evaluation_result$predictions,
      target = target,
      method = calibration_method,
      verbose = verbose
    )
    resultado$calibration <- calibration_result
    if (verbose) .print_reference("calibration")
  }

  if (task == "classification" && calibrate_probs && n_levels >= 3) {
    adv_sub <- adv_sub + 1
    if (verbose) {
      .print_subsection(7, adv_sub, "Calibracion de Probabilidades")
      cat("    Calibracion de probabilidades disponible solo para clasificacion binaria.\n")
      cat("    (", n_levels, " clases detectadas)\n\n", sep = "")
    }
  }

  # Analisis de Residuos Avanzado (solo regresion)
  if (task == "regression") {
    adv_sub <- adv_sub + 1
    if (verbose) .print_subsection(7, adv_sub, "Analisis de Residuos Avanzado")
    residual_result <- advanced_residual_analysis(
      predictions = evaluation_result$predictions,
      target = target,
      verbose = verbose
    )
    resultado$residual_analysis <- residual_result
    if (verbose) .print_reference("residuals")
  } else if (verbose) {
    adv_sub <- adv_sub + 1
    .print_subsection(7, adv_sub, "Analisis de Residuos Avanzado")
    cat("    (Solo disponible para tareas de regresion. Tarea actual: clasificacion)\n")
  }

  # Nested CV (opcional)
  if (nested_cv && modeling_result$best_model %in% c("rf", "xgboost")) {
    adv_sub <- adv_sub + 1
    if (verbose) .print_subsection(7, adv_sub, "Nested Cross-Validation")
    nested_result <- run_nested_cv(
      data = preprocess_result$train_data,
      target = target,
      task = task,
      recipe = preprocess_result$recipe,
      model_name = modeling_result$best_model,
      outer_folds = 5,
      inner_folds = 5,
      grid_size = 10,
      seed = seed,
      verbose = verbose
    )
    resultado$nested_cv <- nested_result
    if (verbose) .print_reference("nested_cv")
  }

  # Analisis de Fairness (solo clasificacion)
  if (task == "classification" && run_fairness && !is.null(protected_var)) {
    adv_sub <- adv_sub + 1
    if (verbose) .print_subsection(7, adv_sub, "Analisis de Equidad (Fairness)")

    if (!protected_var %in% names(preprocess_result$test_data)) {
      if (verbose) cat("    Variable protegida '", protected_var, "' no encontrada en test_data\n", sep = "")
    } else {
      preds_fair <- evaluation_result$predictions
      if (!protected_var %in% names(preds_fair)) {
        preds_fair[[protected_var]] <- preprocess_result$test_data[[protected_var]]
      }

      fairness_result <- analyze_fairness(
        predictions = preds_fair,
        target = target,
        protected_var = protected_var,
        fairness_threshold = fairness_threshold,
        verbose = verbose
      )
      resultado$fairness_analysis <- fairness_result
      if (verbose) .print_reference("fairness")

      # Fairness threshold correction (post-processing)
      if (fairness_correction == "postprocesamiento") {
        adv_sub <- adv_sub + 1
        if (verbose) .print_subsection(7, adv_sub, "Correccion de Fairness (Post-procesamiento)")
        correction_result <- correct_fairness_threshold(
          predictions = preds_fair,
          target = target,
          protected_var = protected_var,
          verbose = verbose
        )
        resultado$fairness_correction <- correction_result
        resultado$group_thresholds <- correction_result$group_thresholds
      }
    }
  }

  # Decision Curve Analysis (solo clasificacion)
  if (task == "classification" && run_dca) {
    adv_sub <- adv_sub + 1
    if (verbose) .print_subsection(7, adv_sub, "Utilidad Clinica (Decision Curve Analysis)")

    dca_result <- analyze_dca(
      predictions = evaluation_result$predictions,
      target = target,
      thresholds = dca_thresholds,
      verbose = verbose
    )
    resultado$dca <- dca_result
    if (verbose) .print_reference("dca")
  }

  # 8. GENERACION DE GRAFICOS
  if (verbose) .print_section(8, "Generacion de Graficos")

  # Preparar objeto temporal para generar graficos
  resultado$task <- task
  resultado$target <- target
  resultado$best_model <- modeling_result$best_model
  resultado$test_metrics <- evaluation_result$metrics
  resultado$predictions <- evaluation_result$predictions
  resultado$importance <- if (run_shap) interpret_result$importance else .get_variable_importance(evaluation_result$final_fit)
  resultado$cv_summary <- modeling_result$comparison$summary

  # Generar todos los graficos con catalogo de figuras
  plots_result <- generate_all_plots(resultado, verbose = verbose)
  resultado$plots <- plots_result

  # Crear acceso directo a figuras: resultado$figures$importance, etc.
  resultado$figures <- plots_result$plots
  resultado$figures_catalog <- plots_result$figures_catalog

  # RESULTADO FINAL
  end_time <- Sys.time()
  elapsed <- round(difftime(end_time, start_time, units = "secs"), 1)

  resultado$task <- task
  resultado$target <- target
  resultado$best_model <- modeling_result$best_model
  resultado$best_params <- tuning_result$best_params
  resultado$final_fit <- evaluation_result$final_fit
  resultado$test_metrics <- evaluation_result$metrics
  resultado$predictions <- evaluation_result$predictions
  resultado$importance <- if (run_shap) interpret_result$importance else .get_variable_importance(evaluation_result$final_fit)
  resultado$train_data <- preprocess_result$train_data
  resultado$test_data <- preprocess_result$test_data
  resultado$cv_summary <- modeling_result$comparison$summary

  resultado$metadata <- list(
    n_obs = nrow(data), n_train = nrow(preprocess_result$train_data),
    n_test = nrow(preprocess_result$test_data), n_features = ncol(data) - 1,
    models_trained = models, cv_folds = cv_folds,
    select_metric = select_metric, tuned = tuning_result$tuned,
    tune_method = if (tune_best) tune_method else NA,
    seed = seed, elapsed_time = elapsed, date = Sys.time()
  )

  class(resultado) <- c("supervisedml", "list")

  if (verbose) {
    # Labels legibles para metricas
    metric_labels <- c(
      roc_auc = "ROC-AUC", f_meas = "F1-Score", f2_meas = "F2-Score",
      accuracy = "Accuracy", sensitivity = "Sensitivity", specificity = "Specificity",
      bal_accuracy = "Balanced Accuracy", pr_auc = "PR-AUC", mcc = "MCC",
      kap = "Cohen's Kappa", rmse = "RMSE", rsq = "R-squared", mae = "MAE"
    )

    .msg_header("Analisis Completado!")
    cat("Tiempo total:", elapsed, "segundos\n")
    cat("Mejor modelo:", .get_model_label(resultado$best_model), "\n")

    # Mostrar metrica seleccionada (dinamico)
    metric_val <- resultado$test_metrics$.estimate[
      resultado$test_metrics$.metric == select_metric
    ]
    metric_label <- metric_labels[select_metric]
    if (length(metric_val) > 0 && !is.na(metric_val)) {
      cat(metric_label, "en test:", round(metric_val, 4), "\n")
      interpretation <- .interpret_metric_value(select_metric, metric_val)
      cat("Interpretacion:", interpretation, "\n")
    }

    cat("\n")
    cat(.line("-"), "\n")
    cat("CONCLUSION\n")
    cat(.line("-"), "\n")
    cat("Se entreno un modelo de", .get_model_label(resultado$best_model), "para predecir\n")
    cat("'", target, "'. El modelo fue evaluado en ", nrow(preprocess_result$test_data),
        " observaciones nuevas\n", sep = "")
    if (length(metric_val) > 0 && !is.na(metric_val)) {
      cat("(que no uso durante el entrenamiento) y obtuvo un ",
          metric_label, " de ", round(metric_val, 4), ".\n", sep = "")
      interpretation <- .interpret_metric_value(select_metric, metric_val)
      cat("Interpretacion: ", interpretation, "\n", sep = "")
    }

    cat("\nPara usar el modelo:\n")
    cat("  - Ver resultados: print(resultado)\n")
    cat("  - Ver graficos: plot(resultado)\n")
    cat("  - Hacer predicciones: predict(resultado, nuevos_datos)\n")
    cat("  - Guardar graficos: save_all_plots(resultado, path = 'carpeta')\n")
    cat(.line("="), "\n")
  }

  # Suprimir warnings de versiones de paquetes al retornar
  suppressWarnings(return(resultado))
}


#' @title Print a supervisedml Object
#'
#' @description
#' Prints a console report of a [supervised_ml()] result: configuration,
#' best model and tuned parameters, test metrics, top five variables and,
#' when available, fairness and decision curve results (console text is in
#' Spanish).
#'
#' @param x An object of class `supervisedml`.
#' @param ... Additional arguments (not used).
#'
#' @return Invisibly returns `x`. Called for its side effect of printing.
#'
#' @examples
#' \donttest{
#' set.seed(1)
#' n <- 200
#' df <- data.frame(x1 = rnorm(n), x2 = rnorm(n), x3 = rnorm(n))
#' df$y <- factor(ifelse(df$x1 + 0.5 * df$x2 + rnorm(n) > 0, "yes", "no"))
#' res <- supervised_ml(df, target = "y", models = "glm", cv_folds = 3,
#'                      tune_best = FALSE, run_shap = FALSE, n_cores = 1,
#'                      verbose = FALSE)
#' print(res)
#' }
#'
#' @family supervised learning
#' @export
print.supervisedml <- function(x, ...) {
  cat("\n")
  cat(.line("="), "\n")
  cat("  supervised_ml - Resultados del Analisis\n")
  cat(.line("="), "\n\n")

  cat("CONFIGURACION:\n")
  cat("  Tarea:", x$task, "\n")
  cat("  Variable objetivo:", x$target, "\n")
  cat("  Observaciones (train/test):", x$metadata$n_train, "/", x$metadata$n_test, "\n")
  cat("  Variables:", x$metadata$n_features, "\n")
  cat("  Modelos evaluados:", paste(x$metadata$models_trained, collapse = ", "), "\n")

  cat("\nMEJOR MODELO:", .get_model_label(x$best_model), "\n")

  if (!is.null(x$best_params) && length(x$best_params) > 0) {
    method_labels <- c(
      random = "Random Search",
      grid = "Grid Search",
      bayes = "Bayesian Optimization",
      racing = "Racing ANOVA"
    )
    tune_method <- x$metadata$tune_method
    if (!is.na(tune_method)) {
      cat("  Metodo de tuning:", method_labels[tune_method], "\n")
    }
    cat("  Parametros tuneados:\n")
    for (param in names(x$best_params)) {
      cat("    -", param, ":", x$best_params[[param]], "\n")
    }
  }

  cat("\nMETRICAS EN TEST:\n")
  for (i in 1:nrow(x$test_metrics)) {
    cat("  ", x$test_metrics$.metric[i], ":", round(x$test_metrics$.estimate[i], 4), "\n")
  }

  if (!is.null(x$importance) && nrow(x$importance) > 0) {
    cat("\nTOP 5 VARIABLES IMPORTANTES:\n")
    top_vars <- utils::head(x$importance, 5)
    for (i in 1:nrow(top_vars)) {
      cat("  ", i, ". ", top_vars$Variable[i], " (", round(top_vars$Importance[i], 3), ")\n", sep = "")
    }
  }

  if (!is.null(x$fairness_analysis)) {
    cat("\nFAIRNESS (variable protegida: ", x$fairness_analysis$protected_var, "):\n", sep = "")
    cat("  Disparate Impact Ratio: ", round(x$fairness_analysis$disparate_impact_ratio, 3), "\n", sep = "")
    cat("  Regla 4/5: ", ifelse(x$fairness_analysis$passes_four_fifths, "CUMPLE", "NO CUMPLE"), "\n", sep = "")
    cat("  Dem. Parity Diff: ", round(x$fairness_analysis$demographic_parity_diff, 3), "\n", sep = "")
    cat("  Eq. Odds (TPR Diff): ", round(x$fairness_analysis$equalized_odds_tpr_diff, 3), "\n", sep = "")
  }

  if (!is.null(x$dca)) {
    cat("\nUTILIDAD CLINICA (DCA):\n")
    if (!is.na(x$dca$useful_range[1])) {
      cat("  Rango util: [", round(x$dca$useful_range[1], 2), ", ",
          round(x$dca$useful_range[2], 2), "]\n", sep = "")
    }
    cat("  Net Benefit maximo: ", round(x$dca$max_net_benefit, 4), "\n", sep = "")
  }

  if (!is.null(x$plots) && length(x$plots) > 0) {
    cat("\nGRAFICOS GENERADOS:", length(x$plots), "\n")
    cat("  Disponibles:", paste(names(x$plots), collapse = ", "), "\n")
    cat("  Usar: plot(resultado) para panel combinado\n")
    cat("        plot(resultado, type = 'nombre') para grafico individual\n")
    cat("        save_all_plots(resultado, path = '.') para guardar todos\n")
  }

  cat("\n", .line("-"), "\n")
  cat("Tiempo de ejecucion:", x$metadata$elapsed_time, "segundos\n")
  cat(.line("="), "\n")

  invisible(x)
}


#' @title Predict with a supervisedml Model
#'
#' @description
#' Generates predictions for new data with the final model of a
#' [supervised_ml()] result. When the model was fitted with
#' `fairness_correction = "postprocesamiento"` and `new_data` contains the
#' protected variable, class predictions use the group-specific thresholds
#' (observations of unknown groups use 0.5).
#'
#' @param object An object of class `supervisedml`.
#' @param new_data A data frame with the same predictors used for training.
#' @param type Type of prediction: `"class"` (default; predicted class for
#'   classification, numeric prediction for regression) or `"prob"` (class
#'   probabilities, classification only).
#' @param ... Additional arguments (not used).
#'
#' @return A tibble with one row per row of `new_data`: column `.pred_class`
#'   (factor) for class predictions, `.pred` (numeric) for regression, or one
#'   `.pred_<level>` column per class with the probabilities when
#'   `type = "prob"`.
#'
#' @examples
#' \donttest{
#' set.seed(1)
#' n <- 200
#' df <- data.frame(x1 = rnorm(n), x2 = rnorm(n), x3 = rnorm(n))
#' df$y <- factor(ifelse(df$x1 + 0.5 * df$x2 + rnorm(n) > 0, "yes", "no"))
#' res <- supervised_ml(df, target = "y", models = "glm", cv_folds = 3,
#'                      tune_best = FALSE, run_shap = FALSE, n_cores = 1,
#'                      verbose = FALSE)
#' new_df <- data.frame(x1 = rnorm(5), x2 = rnorm(5), x3 = rnorm(5))
#' predict(res, new_df)
#' predict(res, new_df, type = "prob")
#' }
#'
#' @family supervised learning
#' @export
predict.supervisedml <- function(object, new_data, type = "class", ...) {
  if (type == "prob" && object$task == "classification") {
    stats::predict(object$final_fit, new_data = new_data, type = "prob")
  } else if (!is.null(object$group_thresholds) && object$task == "classification" && type != "prob") {
    # Fairness-corrected prediction using group-specific thresholds
    pvar <- object$fairness_analysis$protected_var
    if (!is.null(pvar) && pvar %in% names(new_data)) {
      probs <- stats::predict(object$final_fit, new_data = new_data, type = "prob")
      event_info <- .detect_event_level(object$predictions[[object$target]])
      prob_col <- event_info$prob_col
      gt <- object$group_thresholds

      pred_class <- character(nrow(new_data))
      for (i in seq_len(nrow(gt))) {
        idx <- new_data[[pvar]] == gt$group[i]
        if (any(idx)) {
          pred_class[idx] <- ifelse(
            probs[[prob_col]][idx] >= gt$threshold[i],
            event_info$positive_class, event_info$negative_class
          )
        }
      }
      missing <- pred_class == ""
      if (any(missing)) {
        pred_class[missing] <- ifelse(
          probs[[prob_col]][missing] >= 0.5,
          event_info$positive_class, event_info$negative_class
        )
      }
      tibble::tibble(.pred_class = factor(pred_class, levels = event_info$levels))
    } else {
      stats::predict(object$final_fit, new_data = new_data)
    }
  } else {
    stats::predict(object$final_fit, new_data = new_data)
  }
}


#' @title Summarize a supervisedml Object
#'
#' @description
#' Prints the cross-validation comparison of all trained models, the test
#' metrics and the tuned hyperparameters of a [supervised_ml()] result
#' (console text is in Spanish).
#'
#' @param object An object of class `supervisedml`.
#' @param ... Additional arguments (not used).
#'
#' @return Invisibly returns `object`. Called for its side effect of
#'   printing.
#'
#' @examples
#' \donttest{
#' set.seed(1)
#' n <- 200
#' df <- data.frame(x1 = rnorm(n), x2 = rnorm(n), x3 = rnorm(n))
#' df$y <- factor(ifelse(df$x1 + 0.5 * df$x2 + rnorm(n) > 0, "yes", "no"))
#' res <- supervised_ml(df, target = "y", models = "glm", cv_folds = 3,
#'                      tune_best = FALSE, run_shap = FALSE, n_cores = 1,
#'                      verbose = FALSE)
#' summary(res)
#' }
#'
#' @family supervised learning
#' @export
summary.supervisedml <- function(object, ...) {
  cat("\n=== Resumen supervised_ml ===\n\n")
  cat("Tarea:", object$task, "\n")
  cat("Target:", object$target, "\n")
  cat("Mejor modelo:", .get_model_label(object$best_model), "\n\n")
  cat("--- Comparacion de Modelos (CV) ---\n")
  print(object$cv_summary)
  cat("\n--- Metricas en Test ---\n")
  print(object$test_metrics)
  if (!is.null(object$best_params)) {
    cat("\n--- Hiperparametros Tuneados ---\n")
    print(as.data.frame(object$best_params))
  }
  invisible(object)
}
