# =============================================================================
# Helper functions for PsyML
# =============================================================================

# --- Funciones de formato para consola ---

#' @noRd
.line <- function(char = "=", n = 60) {
  paste(rep(char, n), collapse = "")
}

#' @noRd
.color_green <- function(text) {
  paste0("\033[32m", text, "\033[0m")
}

#' @noRd
.color_blue <- function(text) {

  paste0("\033[34m", text, "\033[0m")
}

#' @noRd
.color_bold <- function(text) {
  paste0("\033[1m", text, "\033[0m")
}

#' @noRd
.msg_header <- function(text) {
  cat("\n", .line("="), "\n", sep = "")
  cat(" ", text, "\n", sep = "")
  cat(.line("="), "\n\n", sep = "")
}

#' @noRd
.msg_step <- function(step, total, text) {
  cat("\n[", step, "/", total, "] ", text, "\n", sep = "")
  cat(.line("-"), "\n", sep = "")
}

#' @noRd
.msg_info <- function(text) {
  cat(text, "\n")
}

# --- Funciones de verbose con numeracion jerarquica ---

#' @title Print a Main Section Header
#' @noRd
.print_section <- function(number, title) {
  cat("\n")
  cat(.line("="), "\n")
  cat(number, ". ", title, "\n", sep = "")
  cat(.line("="), "\n")
}

#' @title Print a Subsection Header
#' @noRd
.print_subsection <- function(section, subsection, title) {
  cat("\n")
  cat(.line("-", 50), "\n")
  cat(section, ".", subsection, ". ", title, "\n", sep = "")
  cat(.line("-", 50), "\n")
}

#' @title Print a Sub-subsection Header
#' @noRd
.print_subsubsection <- function(section, subsection, subsubsection, title) {
  cat("\n  ", section, ".", subsection, ".", subsubsection, ". ", title, "\n", sep = "")
}

#' @title Print an APA 7 Reference
#' @noRd
.print_reference <- function(ref_key) {
  references <- list(
    # EDA y Estadistica Descriptiva
    eda = "Tukey, J. W. (1977). Exploratory data analysis. Addison-Wesley.",
    missing = "Little, R. J. A., & Rubin, D. B. (2019). Statistical analysis with missing data (3rd ed.). Wiley. https://doi.org/10.1002/9781119482260",
    normality = "Shapiro, S. S., & Wilk, M. B. (1965). An analysis of variance test for normality (complete samples). Biometrika, 52(3-4), 591-611. https://doi.org/10.1093/biomet/52.3-4.591",
    outliers = "Theriault, R., Ben-Shachar, M. S., Patil, I., Ludecke, D., Wiernik, B. M., & Makowski, D. (2024). Check your outliers! An introduction to identifying statistical outliers in R with easystats. Behavior Research Methods, 56, 4162-4172. https://doi.org/10.3758/s13428-024-02356-w",
    correlation = "Cohen, J. (1988). Statistical power analysis for the behavioral sciences (2nd ed.). Lawrence Erlbaum Associates.",
    vif = "O'Brien, R. M. (2007). A caution regarding rules of thumb for variance inflation factors. Quality & Quantity, 41(5), 673-690. https://doi.org/10.1007/s11135-006-9018-6 | Chan, J. Y.-L., Leow, S. M. H., Bea, K. T., Cheng, W. K., Phoong, S. W., Hong, Z.-W., & Chen, Y.-L. (2022). Mitigating the multicollinearity problem and its machine learning approach: A review. Mathematics, 10(8), 1283. https://doi.org/10.3390/math10081283",

    # Preprocesamiento
    train_test = "Hastie, T., Tibshirani, R., & Friedman, J. (2009). The elements of statistical learning: Data mining, inference, and prediction (2nd ed.). Springer. https://doi.org/10.1007/978-0-387-84858-7 | Gholamy, A., Kreinovich, V., & Kosheleva, O. (2018). Why 70/30 or 80/20 relation between training and testing sets: A pedagogical explanation. International Journal of Intelligent Technologies and Applied Statistics, 11(2), 105-111. | James, G., Witten, D., Hastie, T., & Tibshirani, R. (2021). An introduction to statistical learning: With applications in R (2nd ed.). Springer. https://doi.org/10.1007/978-1-0716-1418-1",
    cv = "Kohavi, R. (1995). A study of cross-validation and bootstrap for accuracy estimation and model selection. In Proceedings of the 14th International Joint Conference on Artificial Intelligence (Vol. 2, pp. 1137-1145). Morgan Kaufmann. | Raschka, S. (2020). Model evaluation, model selection, and algorithm selection in machine learning. arXiv:1811.12808v3. https://doi.org/10.48550/arXiv.1811.12808",
    stratified_cv = "Kohavi, R. (1995). A study of cross-validation and bootstrap for accuracy estimation and model selection. In Proceedings of the 14th International Joint Conference on Artificial Intelligence (Vol. 2, pp. 1137-1145). Morgan Kaufmann.",
    normalization = "Raschka, S. (2020). Model evaluation, model selection, and algorithm selection in machine learning. arXiv:1811.12808v3. https://doi.org/10.48550/arXiv.1811.12808 | Kuhn, M., & Johnson, K. (2013). Applied predictive modeling. Springer. https://doi.org/10.1007/978-1-4614-6849-3",
    smote = "Chawla, N. V., Bowyer, K. W., Hall, L. O., & Kegelmeyer, W. P. (2002). SMOTE: Synthetic minority over-sampling technique. Journal of Artificial Intelligence Research, 16, 321-357. https://doi.org/10.1613/jair.953 | He, H., & Garcia, E. A. (2009). Learning from imbalanced data. IEEE Transactions on Knowledge and Data Engineering, 21(9), 1263-1284. https://doi.org/10.1109/TKDE.2008.239",
    boruta = "Kursa, M. B., & Rudnicki, W. R. (2010). Feature selection with the Boruta package. Journal of Statistical Software, 36(11), 1-13. https://doi.org/10.18637/jss.v036.i11",
    imputation = "Li, J., Tong, Y., Guan, Y., Wu, S., & Li, D. (2024). Comparison of imputation methods for missing data in predictive modelling of health outcomes. BMC Medical Research Methodology, 24, 41. https://doi.org/10.1186/s12874-024-02156-y | Kapoor, S., & Narayanan, A. (2023). Leakage and the reproducibility crisis in machine-learning-based science. Patterns, 4(9), 100804. https://doi.org/10.1016/j.patter.2023.100804",
    feature_engineering = "Zheng, A., & Casari, A. (2018). Feature engineering for machine learning: Principles and techniques for data scientists. O'Reilly Media. | Kuhn, M., & Johnson, K. (2013). Applied predictive modeling. Springer. https://doi.org/10.1007/978-1-4614-6849-3",

    # Tratamiento de Outliers y Multicolinealidad
    winsorization = "Theriault, R., Ben-Shachar, M. S., Patil, I., Ludecke, D., Wiernik, B. M., & Makowski, D. (2024). Check your outliers! An introduction to identifying statistical outliers in R with easystats. Behavior Research Methods, 56, 4162-4172. https://doi.org/10.3758/s13428-024-02356-w",
    correlation_removal = "Kuhn, M., & Johnson, K. (2013). Applied predictive modeling. Springer. https://doi.org/10.1007/978-1-4614-6849-3",
    vif_removal = "O'Brien, R. M. (2007). A caution regarding rules of thumb for variance inflation factors. Quality & Quantity, 41(5), 673-690. https://doi.org/10.1007/s11135-006-9018-6 | Chan, J. Y.-L., Leow, S. M. H., Bea, K. T., Cheng, W. K., Phoong, S. W., Hong, Z.-W., & Chen, Y.-L. (2022). Mitigating the multicollinearity problem and its machine learning approach: A review. Mathematics, 10(8), 1283. https://doi.org/10.3390/math10081283",

    # Interacciones y Splines
    interactions = "Aiken, L. S., & West, S. G. (1991). Multiple regression: Testing and interpreting interactions. Sage Publications.",
    splines = "Harrell, F. E. (2015). Regression modeling strategies: With applications to linear models, logistic and ordinal regression, and survival analysis (2nd ed.). Springer. https://doi.org/10.1007/978-3-319-19425-7",

    # Modelos
    rf = "Breiman, L. (2001). Random forests. Machine Learning, 45(1), 5-32. https://doi.org/10.1023/A:1010933404324",
    xgboost = "Chen, T., & Guestrin, C. (2016). XGBoost: A scalable tree boosting system. In Proceedings of the 22nd ACM SIGKDD International Conference on Knowledge Discovery and Data Mining (pp. 785-794). ACM. https://doi.org/10.1145/2939672.2939785",
    glm = "Nelder, J. A., & Wedderburn, R. W. M. (1972). Generalized linear models. Journal of the Royal Statistical Society: Series A (General), 135(3), 370-384. https://doi.org/10.2307/2344614",
    svm = "Cortes, C., & Vapnik, V. (1995). Support-vector networks. Machine Learning, 20(3), 273-297. https://doi.org/10.1007/BF00994018",
    nnet = "Rumelhart, D. E., Hinton, G. E., & Williams, R. J. (1986). Learning representations by back-propagating errors. Nature, 323(6088), 533-536. https://doi.org/10.1038/323533a0",
    tree = "Breiman, L., Friedman, J. H., Olshen, R. A., & Stone, C. J. (1984). Classification and regression trees. CRC Press. https://doi.org/10.1201/9781315139470",
    nb = "Hand, D. J., & Yu, K. (2001). Idiot's Bayes: Not so stupid after all? International Statistical Review, 69(3), 385-398. https://doi.org/10.1111/j.1751-5823.2001.tb00465.x",

    # Tuning
    grid_search = "Bergstra, J., & Bengio, Y. (2012). Random search for hyper-parameter optimization. Journal of Machine Learning Research, 13(10), 281-305.",
    random_search = "Bergstra, J., & Bengio, Y. (2012). Random search for hyper-parameter optimization. Journal of Machine Learning Research, 13(10), 281-305.",
    bayes_opt = "Snoek, J., Larochelle, H., & Adams, R. P. (2012). Practical Bayesian optimization of machine learning algorithms. In Advances in Neural Information Processing Systems (Vol. 25, pp. 2951-2959). Curran Associates.",
    racing = "Maron, O., & Moore, A. W. (1997). The racing algorithm: Model selection for lazy learners. Artificial Intelligence Review, 11(1-5), 193-225. https://doi.org/10.1023/A:1006556606079",

    # Metricas de Evaluacion
    roc_auc = "Hanley, J. A., & McNeil, B. J. (1982). The meaning and use of the area under a receiver operating characteristic (ROC) curve. Radiology, 143(1), 29-36. https://doi.org/10.1148/radiology.143.1.7063747",
    confusion_matrix = "Fawcett, T. (2006). An introduction to ROC analysis. Pattern Recognition Letters, 27(8), 861-874. https://doi.org/10.1016/j.patrec.2005.10.010",
    f1_score = "Van Rijsbergen, C. J. (1979). Information retrieval (2nd ed.). Butterworth-Heinemann.",
    f2_score = "Van Rijsbergen, C. J. (1979). Information retrieval (2nd ed.). Butterworth-Heinemann. | Chinchor, N. (1992). MUC-4 evaluation metrics. In Proceedings of the 4th Conference on Message Understanding (MUC-4) (pp. 22-29). Association for Computational Linguistics. https://doi.org/10.3115/1072064.1072067",
    mcc = "Matthews, B. W. (1975). Comparison of the predicted and observed secondary structure of T4 phage lysozyme. Biochimica et Biophysica Acta (BBA) - Protein Structure, 405(2), 442-451. https://doi.org/10.1016/0005-2795(75)90109-9 | Chicco, D., & Jurman, G. (2020). The advantages of the Matthews correlation coefficient (MCC) over F1 score and accuracy in binary classification evaluation. BMC Genomics, 21, Article 6. https://doi.org/10.1186/s12864-019-6413-7",
    rmse = "Chai, T., & Draxler, R. R. (2014). Root mean square error (RMSE) or mean absolute error (MAE)? Arguments against avoiding RMSE in the literature. Geoscientific Model Development, 7(3), 1247-1250. https://doi.org/10.5194/gmd-7-1247-2014",
    r_squared = "Nagelkerke, N. J. D. (1991). A note on a general definition of the coefficient of determination. Biometrika, 78(3), 691-692. https://doi.org/10.1093/biomet/78.3.691",
    calibration = "Niculescu-Mizil, A., & Caruana, R. (2005). Predicting good probabilities with supervised learning. In Proceedings of the 22nd International Conference on Machine Learning (ICML) (pp. 625-632). ACM. https://doi.org/10.1145/1102351.1102430",

    # Interpretabilidad
    shap = "Lundberg, S. M., & Lee, S.-I. (2017). A unified approach to interpreting model predictions. In Advances in Neural Information Processing Systems (Vol. 30, pp. 4765-4774). Curran Associates.",
    importance = "Breiman, L. (2001). Random forests. Machine Learning, 45(1), 5-32. https://doi.org/10.1023/A:1010933404324",

    # Analisis Avanzado
    threshold = "Youden, W. J. (1950). Index for rating diagnostic tests. Cancer, 3(1), 32-35. https://doi.org/10.1002/1097-0142(1950)3:1<32::AID-CNCR2820030106>3.0.CO;2-3",
    leakage = "Kaufman, S., Rosset, S., Perlich, C., & Stitelman, O. (2012). Leakage in data mining: Formulation, detection, and avoidance. ACM Transactions on Knowledge Discovery from Data, 6(4), Article 15, 1-21. https://doi.org/10.1145/2382577.2382579 | Kapoor, S., & Narayanan, A. (2023). Leakage and the reproducibility crisis in machine-learning-based science. Patterns, 4(9), 100804. https://doi.org/10.1016/j.patter.2023.100804",
    nested_cv = "Varma, S., & Simon, R. (2006). Bias in error estimation when using cross-validation for model selection. BMC Bioinformatics, 7, Article 91. https://doi.org/10.1186/1471-2105-7-91",
    residuals = "Cook, R. D., & Weisberg, S. (1982). Residuals and influence in regression. Chapman and Hall.",

    # Frameworks y Herramientas
    tidy_modeling = "Kuhn, M., & Silge, J. (2022). Tidy modeling with R: A framework for modeling in the tidyverse. O'Reilly Media. https://www.tmwr.org/",

    # Fairness
    fairness = "Barocas, S., Hardt, M., & Narayanan, A. (2019). Fairness and machine learning: Limitations and opportunities. MIT Press.",
    disparate_impact = "Feldman, M., Friedler, S. A., Moeller, J., Scheidegger, C., & Venkatasubramanian, S. (2015). Certifying and removing disparate impact. KDD, 259-268.",

    # Decision Curve Analysis
    dca = "Vickers, A. J., & Elkin, E. B. (2006). Decision curve analysis: A novel method for evaluating prediction models. Medical Decision Making, 26(6), 565-574.",

    # Causal Inference
    ps = "Rosenbaum, P. R., & Rubin, D. B. (1983). The central role of the propensity score in observational studies for causal effects. Biometrika, 70(1), 41-55.",
    dml = "Chernozhukov, V., Chetverikov, D., Demirer, M., Duflo, E., Hansen, C., Newey, W., & Robins, J. (2018). Double/debiased machine learning for treatment and structural parameters. The Econometrics Journal, 21(1), C1-C68.",
    cf_causal = "Wager, S., & Athey, S. (2018). Estimation and inference of heterogeneous treatment effects using random forests. JASA, 113(523), 1228-1242."
  )

  if (ref_key %in% names(references)) {
    cat("\n    Referencia: ", references[[ref_key]], "\n", sep = "")
  }
}


# --- Funciones de validacion ---

#' @noRd
.validate_inputs <- function(data, target, models) {
  if (!is.data.frame(data)) {
    stop("'data' must be a data.frame")
  }

  if (!target %in% names(data)) {
    stop("Target variable '", target, "' not found in the data")
  }

  valid_models <- c("rf", "xgboost", "svm", "nnet", "glm", "glmnet", "tree", "nb")
  invalid <- setdiff(models, valid_models)
  if (length(invalid) > 0) {
    stop("Invalid models: ", paste(invalid, collapse = ", "),
         "\nOptions: ", paste(valid_models, collapse = ", "))
  }
}

#' @noRd
.detect_task <- function(y) {
  if (is.factor(y) || is.character(y)) {
    return("classification")
  }

  n_unique <- length(unique(y))
  if (n_unique <= 10 && n_unique < length(y) * 0.05) {
    return("classification")
  }

  return("regression")
}

#' @noRd
.prepare_data <- function(data, target, task) {
  # Convertir caracteres a factores
  data <- data |>
    dplyr::mutate(dplyr::across(dplyr::where(is.character), as.factor))

  # Para clasificacion, asegurar que target sea factor
  if (task == "classification") {
    data[[target]] <- as.factor(data[[target]])
  }

  return(data)
}


# --- Funciones de preprocesamiento ---

#' @noRd
.create_recipe <- function(data, target, task, impute, normalize, balance_classes) {

  # Crear formula
  formula_ml <- stats::as.formula(paste(target, "~ ."))

  # Iniciar receta
  rec <- recipes::recipe(formula_ml, data = data)

  # Imputacion
  if (impute) {
    rec <- rec |>
      recipes::step_impute_median(recipes::all_numeric_predictors()) |>
      recipes::step_impute_mode(recipes::all_nominal_predictors())
  }

  # Dummies para variables categoricas
  rec <- rec |>
    recipes::step_dummy(recipes::all_nominal_predictors(), one_hot = FALSE)

  # Normalizacion
  if (normalize) {
    rec <- rec |>
      recipes::step_normalize(recipes::all_numeric_predictors())
  }

  # SMOTE para clasificacion desbalanceada
  if (balance_classes && task == "classification") {
    if (requireNamespace("themis", quietly = TRUE)) {
      rec <- rec |>
        themis::step_smote(!!rlang::sym(target), over_ratio = 1)
    } else {
      warning("Package 'themis' is not installed. SMOTE not applied.")
    }
  }

  return(rec)
}


# --- Funciones de modelos ---

#' @noRd
.get_model_spec <- function(model_name, task) {

  if (model_name == "rf") {
    if (task == "classification") {
      spec <- parsnip::rand_forest(trees = 500) |>
        parsnip::set_engine("ranger", importance = "impurity") |>
        parsnip::set_mode("classification")
    } else {
      spec <- parsnip::rand_forest(trees = 500) |>
        parsnip::set_engine("ranger", importance = "impurity") |>
        parsnip::set_mode("regression")
    }
  }

  else if (model_name == "xgboost") {
    if (task == "classification") {
      spec <- parsnip::boost_tree(trees = 500, tree_depth = 6, learn_rate = 0.1) |>
        parsnip::set_engine("xgboost") |>
        parsnip::set_mode("classification")
    } else {
      spec <- parsnip::boost_tree(trees = 500, tree_depth = 6, learn_rate = 0.1) |>
        parsnip::set_engine("xgboost") |>
        parsnip::set_mode("regression")
    }
  }

  else if (model_name == "svm") {
    if (task == "classification") {
      spec <- parsnip::svm_rbf() |>
        parsnip::set_engine("kernlab") |>
        parsnip::set_mode("classification")
    } else {
      spec <- parsnip::svm_rbf() |>
        parsnip::set_engine("kernlab") |>
        parsnip::set_mode("regression")
    }
  }

  else if (model_name == "nnet") {
    if (task == "classification") {
      spec <- parsnip::mlp(hidden_units = 10, epochs = 100) |>
        parsnip::set_engine("nnet") |>
        parsnip::set_mode("classification")
    } else {
      spec <- parsnip::mlp(hidden_units = 10, epochs = 100) |>
        parsnip::set_engine("nnet") |>
        parsnip::set_mode("regression")
    }
  }

  else if (model_name == "glm") {
    if (task == "classification") {
      spec <- parsnip::logistic_reg() |>
        parsnip::set_engine("glm") |>
        parsnip::set_mode("classification")
    } else {
      spec <- parsnip::linear_reg() |>
        parsnip::set_engine("lm") |>
        parsnip::set_mode("regression")
    }
  }

  else if (model_name == "glmnet") {
    if (task == "classification") {
      spec <- parsnip::logistic_reg(penalty = 0.01, mixture = 0.5) |>
        parsnip::set_engine("glmnet") |>
        parsnip::set_mode("classification")
    } else {
      spec <- parsnip::linear_reg(penalty = 0.01, mixture = 0.5) |>
        parsnip::set_engine("glmnet") |>
        parsnip::set_mode("regression")
    }
  }

  else if (model_name == "tree") {
    if (task == "classification") {
      spec <- parsnip::decision_tree() |>
        parsnip::set_engine("rpart") |>
        parsnip::set_mode("classification")
    } else {
      spec <- parsnip::decision_tree() |>
        parsnip::set_engine("rpart") |>
        parsnip::set_mode("regression")
    }
  }

  else if (model_name == "nb") {
    if (task == "classification") {
      spec <- parsnip::naive_Bayes() |>
        parsnip::set_engine("naivebayes") |>
        parsnip::set_mode("classification")
    } else {
      stop("Naive Bayes is only available for classification")
    }
  }

  else {
    stop("Unsupported model: ", model_name)
  }

  return(spec)
}


# --- Funciones de evaluacion ---

#' @noRd
.summarize_cv_results <- function(cv_results, task) {
  results_list <- lapply(names(cv_results), function(model_name) {
    metrics <- tune::collect_metrics(cv_results[[model_name]])
    metrics$model <- model_name
    metrics
  })

  all_metrics <- do.call(rbind, results_list)

  if (task == "classification") {
    summary_df <- all_metrics |>
      dplyr::select(model, .metric, mean) |>
      tidyr::pivot_wider(names_from = .metric, values_from = mean) |>
      dplyr::arrange(dplyr::desc(mcc))
  } else {
    summary_df <- all_metrics |>
      dplyr::select(model, .metric, mean) |>
      tidyr::pivot_wider(names_from = .metric, values_from = mean) |>
      dplyr::arrange(dplyr::desc(rsq))
  }

  return(as.data.frame(summary_df))
}


#' @noRd
.calculate_test_metrics <- function(predictions, target, task) {

  if (task == "classification") {
    # Obtener nombre de clase positiva
    target_col <- predictions[[target]]
    levels_y <- levels(target_col)
    positive_class <- levels_y[2]  # Segunda clase como positiva
    prob_col <- paste0(".pred_", positive_class)
    n_levels <- length(levels_y)

    # F2-Score
    f2_meas <- yardstick::metric_tweak("f2_meas", yardstick::f_meas, beta = 2)

    metrics <- dplyr::bind_rows(
      yardstick::roc_auc(predictions, truth = !!rlang::sym(target),
                         !!rlang::sym(prob_col)),
      yardstick::accuracy(predictions, truth = !!rlang::sym(target),
                          estimate = .pred_class),
      yardstick::sensitivity(predictions, truth = !!rlang::sym(target),
                             estimate = .pred_class),
      yardstick::specificity(predictions, truth = !!rlang::sym(target),
                             estimate = .pred_class),
      yardstick::f_meas(predictions, truth = !!rlang::sym(target),
                        estimate = .pred_class),
      yardstick::bal_accuracy(predictions, truth = !!rlang::sym(target),
                              estimate = .pred_class),
      yardstick::mcc(predictions, truth = !!rlang::sym(target),
                     estimate = .pred_class),
      yardstick::kap(predictions, truth = !!rlang::sym(target),
                     estimate = .pred_class),
      f2_meas(predictions, truth = !!rlang::sym(target),
              estimate = .pred_class)
    )

    # PR-AUC solo para binario
    if (n_levels == 2) {
      metrics <- dplyr::bind_rows(
        metrics,
        yardstick::pr_auc(predictions, truth = !!rlang::sym(target),
                          !!rlang::sym(prob_col))
      )
    }
  } else {
    metrics <- dplyr::bind_rows(
      yardstick::rmse(predictions, truth = !!rlang::sym(target),
                      estimate = .pred),
      yardstick::rsq(predictions, truth = !!rlang::sym(target),
                     estimate = .pred),
      yardstick::mae(predictions, truth = !!rlang::sym(target),
                     estimate = .pred)
    )
  }

  return(metrics)
}


#' @noRd
.get_variable_importance <- function(final_fit) {

  imp_df <- get_importance(final_fit, verbose = FALSE)
  imp_df[, c("Variable", "Importance"), drop = FALSE]
}


# --- Funciones de tuning ---

#' @noRd
.tune_model <- function(model_name, recipe, cv_folds, metrics, task,
                        grid_size, verbose) {

  if (model_name == "rf") {
    tune_spec <- parsnip::rand_forest(
      mtry = tune::tune(),
      min_n = tune::tune(),
      trees = 500
    ) |>
      parsnip::set_engine("ranger", importance = "impurity") |>
      parsnip::set_mode(task)

  } else if (model_name == "xgboost") {
    tune_spec <- parsnip::boost_tree(
      mtry = tune::tune(),
      min_n = tune::tune(),
      tree_depth = tune::tune(),
      learn_rate = tune::tune(),
      trees = 500
    ) |>
      parsnip::set_engine("xgboost") |>
      parsnip::set_mode(task)

  } else if (model_name == "glmnet") {
    tune_spec <- if (task == "classification") {
      parsnip::logistic_reg(
        penalty = tune::tune(),
        mixture = tune::tune()
      ) |>
        parsnip::set_engine("glmnet") |>
        parsnip::set_mode("classification")
    } else {
      parsnip::linear_reg(
        penalty = tune::tune(),
        mixture = tune::tune()
      ) |>
        parsnip::set_engine("glmnet") |>
        parsnip::set_mode("regression")
    }
  }

  wf <- workflows::workflow() |>
    workflows::add_recipe(recipe) |>
    workflows::add_model(tune_spec)

  # Grid search
  tune_results <- tune::tune_grid(
    wf,
    resamples = cv_folds,
    grid = grid_size,
    metrics = metrics,
    control = tune::control_grid(verbose = FALSE)
  )

  # Seleccionar mejores parametros
  if (task == "classification") {
    best_params <- tune::select_best(tune_results, metric = "mcc")
  } else {
    best_params <- tune::select_best(tune_results, metric = "rsq")
  }

  # Finalizar workflow
  final_wf <- tune::finalize_workflow(wf, best_params)

  # Convertir a lista simple
  params_list <- as.list(best_params)
  params_list$.config <- NULL

  return(list(
    workflow = final_wf,
    best_params = params_list,
    tune_results = tune_results
  ))
}


# =============================================================================
# Analisis de Interacciones y Efectos No Lineales
# =============================================================================

#' @title Analyze Interactions and Nonlinear Effects
#' @description
#' Screens for potentially important interactions among the top predictors
#' (categorical x numeric and categorical x categorical pairs) and for
#' nonlinear effects of numeric predictors (comparison of a linear and a
#' quadratic fit with an F test), and suggests interaction terms or splines
#' to improve linear models.
#' @param data Data frame with the data.
#' @param target Name of the target variable.
#' @param task Type of task: "classification" or "regression".
#' @param importance Optional data frame of variable importance with a
#'   `Variable` column (e.g. from [get_importance()]); its first `top_n`
#'   variables are analyzed. If NULL, the first `top_n` predictors are used.
#' @param top_n Number of top variables to analyze (default: 5).
#' @param verbose Logical. Print results and suggestions (default: TRUE).
#' @return A list with `interactions` (named list; each element has `var1`,
#'   `var2`, `type` and `strength`), `nonlinear` (named list; each element
#'   has `variable`, `p_value` of the quadratic term, `r2_gain` in percentage
#'   points and a `recommendation`) and `summary` (list with
#'   `n_interactions`, `n_nonlinear`, `top_interaction` and `top_nonlinear`).
#' @examples
#' set.seed(1)
#' n <- 200
#' df <- data.frame(x1 = runif(n, -2, 2), x2 = rnorm(n),
#'                  g = factor(sample(c("a", "b"), n, TRUE)))
#' df$y <- df$x1^2 + df$x2 + rnorm(n)
#' res <- analyze_interactions_splines(df, target = "y", task = "regression",
#'                                     verbose = FALSE)
#' res$summary
#' @family supervised learning
#' @export
analyze_interactions_splines <- function(data, target, task,
                                         importance = NULL,
                                         top_n = 5,
                                         verbose = TRUE) {

  results <- list()

  # Obtener variables predictoras
  predictors <- setdiff(names(data), target)
  num_vars <- names(data)[sapply(data, is.numeric)]
  num_vars <- setdiff(num_vars, target)
  cat_vars <- names(data)[sapply(data, function(x) is.factor(x) | is.character(x))]
  cat_vars <- setdiff(cat_vars, target)

  # =========================================================================
  # 1. ANALISIS DE INTERACCIONES
  # =========================================================================

  if (verbose) {
    cat("\n    ANALISIS DE INTERACCIONES\n")
    cat("    -------------------------\n")
    cat("    Las interacciones ocurren cuando el efecto de una variable\n")
    cat("    depende del valor de otra. Por ejemplo, el efecto de 'Age'\n")
    cat("    en la supervivencia puede ser diferente para hombres y mujeres.\n\n")
  }

  # Seleccionar top variables para analizar
  if (!is.null(importance) && nrow(importance) > 0) {
    top_vars <- head(importance$Variable, top_n)
    # Limpiar nombres (quitar sufijos de dummies como _Female, _X1st, etc.)
    top_vars_clean <- unique(gsub("_.*$", "", top_vars))
    top_vars_clean <- intersect(top_vars_clean, predictors)
  } else {
    top_vars_clean <- head(predictors, min(top_n, length(predictors)))
  }

  # Detectar interacciones potenciales
  interactions_detected <- list()

  # Interacciones categorica x numerica (mas interpretables)
  for (cat_var in intersect(cat_vars, top_vars_clean)) {
    for (num_var in intersect(num_vars, top_vars_clean)) {
      if (cat_var != num_var) {
        # Calcular correlacion por grupo
        interaction_strength <- tryCatch({
          group_cors <- tapply(data[[num_var]], data[[cat_var]], function(x) {
            if (length(unique(x)) < 3) return(NA)
            cor(x, as.numeric(data[[target]][data[[cat_var]] == unique(data[[cat_var]])[1]]),
                use = "complete.obs")
          })
          # Variabilidad en correlaciones = evidencia de interaccion
          if (all(is.na(group_cors))) return(0)
          sd(group_cors, na.rm = TRUE)
        }, error = function(e) 0)

        if (!is.na(interaction_strength) && interaction_strength > 0.1) {
          interactions_detected[[paste0(cat_var, " x ", num_var)]] <- list(
            var1 = cat_var,
            var2 = num_var,
            type = "categorica x numerica",
            strength = round(interaction_strength, 3)
          )
        }
      }
    }
  }

  # Interacciones categorica x categorica
  cat_vars_in_top <- intersect(cat_vars, top_vars_clean)
  if (length(cat_vars_in_top) >= 2) {
    cat_pairs <- combn(cat_vars_in_top, 2, simplify = FALSE)
    for (pair in cat_pairs) {
      if (length(pair) == 2) {
        interaction_name <- paste0(pair[1], " x ", pair[2])
        interactions_detected[[interaction_name]] <- list(
          var1 = pair[1],
          var2 = pair[2],
          type = "categorica x categorica",
          strength = NA  # Mas dificil de cuantificar
        )
      }
    }
  }

  results$interactions <- interactions_detected

  if (verbose) {
    if (length(interactions_detected) > 0) {
      cat("    Interacciones potenciales detectadas:\n\n")
      for (int_name in names(interactions_detected)) {
        int_info <- interactions_detected[[int_name]]
        cat("      *", int_name, "\n")
        cat("        Tipo:", int_info$type, "\n")
        if (!is.na(int_info$strength)) {
          cat("        Intensidad:", int_info$strength, "\n")
        }
        cat("\n")
      }

      cat("    Interpretacion:\n")
      cat("      Una interaccion '", names(interactions_detected)[1], "' significa que\n")
      cat("      el efecto de una variable cambia segun el nivel de la otra.\n\n")
    } else {
      cat("    No se detectaron interacciones significativas entre las\n")
      cat("    variables principales.\n\n")
    }
  }

  # =========================================================================
  # 2. ANALISIS DE EFECTOS NO LINEALES (SPLINES)
  # =========================================================================

  if (verbose) {
    cat("    EFECTOS NO LINEALES (SPLINES)\n")
    cat("    ------------------------------\n")
    cat("    Los splines permiten modelar relaciones curvas entre variables.\n")
    cat("    Por ejemplo, la edad puede tener un efecto no lineal donde\n")
    cat("    ninos y ancianos tienen patrones diferentes a adultos.\n\n")
  }

  nonlinear_candidates <- list()

  for (num_var in intersect(num_vars, top_vars_clean)) {
    # Detectar no linealidad usando test de curvatura simple
    x <- data[[num_var]]
    y <- if (task == "classification") {
      as.numeric(data[[target]]) - 1
    } else {
      data[[target]]
    }

    # Eliminar NAs
    complete_idx <- complete.cases(x, y)
    x <- x[complete_idx]
    y <- y[complete_idx]

    if (length(x) < 30) next

    nonlinearity_test <- tryCatch({
      # Comparar modelo lineal vs cuadratico
      x_centered <- x - mean(x)
      lm_linear <- lm(y ~ x_centered)
      lm_quad <- lm(y ~ x_centered + I(x_centered^2))

      # Test F para termino cuadratico
      anova_result <- anova(lm_linear, lm_quad)
      p_value <- anova_result$`Pr(>F)`[2]

      # R2 adicional del termino cuadratico
      r2_linear <- summary(lm_linear)$r.squared
      r2_quad <- summary(lm_quad)$r.squared
      r2_gain <- r2_quad - r2_linear

      # Proteger contra NA (ej. variable binaria donde el termino cuadratico es singular)
      if (is.na(p_value)) p_value <- 1
      if (is.na(r2_gain)) r2_gain <- 0

      list(p_value = p_value, r2_gain = r2_gain)
    }, error = function(e) {
      list(p_value = 1, r2_gain = 0)
    })

    if (nonlinearity_test$p_value < 0.05 || nonlinearity_test$r2_gain > 0.01) {
      nonlinear_candidates[[num_var]] <- list(
        variable = num_var,
        p_value = round(nonlinearity_test$p_value, 4),
        r2_gain = round(nonlinearity_test$r2_gain * 100, 2),
        recommendation = if (nonlinearity_test$p_value < 0.01) {
          "Altamente recomendado"
        } else if (nonlinearity_test$p_value < 0.05) {
          "Recomendado"
        } else {
          "Considerar"
        }
      )
    }
  }

  results$nonlinear <- nonlinear_candidates

  if (verbose) {
    if (length(nonlinear_candidates) > 0) {
      cat("    Variables con efectos no lineales detectados:\n\n")
      for (var_name in names(nonlinear_candidates)) {
        nl_info <- nonlinear_candidates[[var_name]]
        cat("      *", var_name, "\n")
        cat("        p-valor curvatura:", nl_info$p_value, "\n")
        cat("        R2 ganancia:", nl_info$r2_gain, "%\n")
        cat("        Sugerencia:", nl_info$recommendation, "\n\n")
      }
    } else {
      cat("    No se detectaron efectos no lineales significativos.\n")
      cat("    Las relaciones parecen ser aproximadamente lineales.\n\n")
    }
  }

  # =========================================================================
  # 3. SUGERENCIAS DE MEJORA
  # =========================================================================

  if (verbose) {
    cat("    SUGERENCIAS PARA MEJORAR EL MODELO\n")
    cat("    -----------------------------------\n\n")

    has_suggestions <- FALSE

    # Sugerencias de interacciones
    if (length(interactions_detected) > 0) {
      has_suggestions <- TRUE
      cat("    1. INTERACCIONES:\n")
      cat("       Para incluir interacciones en el modelo, puedes:\n\n")

      # Mostrar ejemplo de codigo
      int_example <- names(interactions_detected)[1]
      vars_example <- interactions_detected[[1]]

      cat("       # En R, crear variable de interaccion:\n")
      cat("       data$", vars_example$var1, "_", vars_example$var2,
          " <- interaction(data$", vars_example$var1, ", data$", vars_example$var2, ")\n\n", sep = "")

      cat("       # O en la formula del modelo:\n")
      cat("       formula <- ", target, " ~ ", vars_example$var1, " * ", vars_example$var2, " + ...\n\n", sep = "")
    }

    # Sugerencias de splines
    if (length(nonlinear_candidates) > 0) {
      has_suggestions <- TRUE
      cat("    2. TRANSFORMACIONES NO LINEALES (SPLINES):\n")
      cat("       Para capturar efectos no lineales, puedes usar:\n\n")

      var_example <- names(nonlinear_candidates)[1]

      cat("       # Opcion A: Splines con paquete splines\n")
      cat("       library(splines)\n")
      cat("       formula <- ", target, " ~ ns(", var_example, ", df = 4) + ...\n\n", sep = "")

      cat("       # Opcion B: Restricted Cubic Splines con rms\n")
      cat("       library(rms)\n")
      cat("       formula <- ", target, " ~ rcs(", var_example, ", 4) + ...\n\n", sep = "")
    }

    if (!has_suggestions) {
      cat("    El modelo actual parece estar bien especificado.\n")
      cat("    No se detectaron interacciones o efectos no lineales\n")
      cat("    que requieran atencion inmediata.\n\n")
    }

    cat("    Nota: Los modelos como Random Forest y XGBoost capturan\n")
    cat("    automaticamente interacciones y efectos no lineales.\n")
    cat("    Estas sugerencias son mas relevantes para modelos lineales (GLM).\n")
  }

  # Resumen
  results$summary <- list(
    n_interactions = length(interactions_detected),
    n_nonlinear = length(nonlinear_candidates),
    top_interaction = if (length(interactions_detected) > 0) names(interactions_detected)[1] else NA,
    top_nonlinear = if (length(nonlinear_candidates) > 0) names(nonlinear_candidates)[1] else NA
  )

  return(results)
}
