# =============================================================================
# Funciones de Modelado para PsyML
# =============================================================================

#' @title Train and Compare Models with Cross-Validation
#'
#' @description
#' Modeling step of the pipeline: creates the cross-validation folds
#' ([setup_cv()]), defines the model specifications ([define_models()]),
#' fits every model with cross-validation ([fit_models_cv()]) and compares
#' them to select the best one ([compare_models()]).
#'
#' @param preprocess_result Result of [preprocess_data()]. It must contain
#'   `task`, `target`, `train_data` and `recipe`.
#' @param models Character vector with the models to train (default:
#'   `c("rf", "xgboost", "glm")`). Available: `"rf"` (random forest), `"xgboost"`, `"svm"` (RBF kernel),
#'   `"nnet"` (neural network), `"glm"` (logistic or linear regression),
#'   `"glmnet"` (regularized regression), `"tree"` (decision tree) and
#'   `"nb"` (naive Bayes). Some models need suggested packages (e.g.
#'   \pkg{kernlab}, \pkg{glmnet}, \pkg{rpart}).
#' @param cv_folds Number of cross-validation folds (default: 10).
#' @param select_metric Metric used to select the best model. If NULL
#'   (default) ROC-AUC is used for classification and R-squared for
#'   regression. See [supervised_ml()] for the available metrics.
#' @param seed Optional integer seed. If NULL (default) the random number
#'   generator state is not modified.
#' @param verbose Logical. Print the progress and an explanation of each
#'   step in the console (default: TRUE).
#'
#' @return An object of class `supervisedml_modeling` (a list) with
#'   `cv_folds` (the `rset` of folds), `model_specs` (named list of parsnip
#'   specifications), `cv_results` (named list of resampling results),
#'   `comparison` (result of [compare_models()]), `best_model` (name of the
#'   best model), `task`, `target`, `recipe` and `train_data`.
#'
#' @examples
#' \donttest{
#' set.seed(1)
#' n <- 200
#' df <- data.frame(x1 = rnorm(n), x2 = rnorm(n), x3 = rnorm(n))
#' df$y <- factor(ifelse(df$x1 + 0.5 * df$x2 + rnorm(n) > 0, "yes", "no"))
#' pp <- preprocess_data(df, target = "y", task = "classification",
#'                       verbose = FALSE)
#' mod <- train_models(pp, models = c("glm", "rf"), cv_folds = 3,
#'                     verbose = FALSE)
#' mod$best_model
#' mod$comparison$summary
#' }
#'
#' @family supervised learning
#' @export
train_models <- function(preprocess_result,
                         models = c("rf", "xgboost", "glm"),
                         cv_folds = 10,
                         select_metric = NULL,
                         seed = NULL,
                         verbose = TRUE) {

  if (!is.null(seed)) set.seed(seed)

  task <- preprocess_result$task
  target <- preprocess_result$target
  train_data <- preprocess_result$train_data
  recipe <- preprocess_result$recipe

  # Establecer metrica por defecto si no se especifica
  if (is.null(select_metric)) {
    select_metric <- if (task == "classification") "roc_auc" else "rsq"
  }

  if (verbose) {
    .print_section(3, "Modelado")
  }

  results <- list()

  # 3.1 Configurar CV
  if (verbose) .print_subsection(3, 1, "Configurar Validacion Cruzada")
  cv_result <- setup_cv(train_data, target, task, cv_folds, seed, verbose)
  results$cv_folds <- cv_result
  if (verbose) .print_reference("cv")

  # 3.2 Definir Modelos
  if (verbose) .print_subsection(3, 2, "Definir Modelos")
  model_specs <- define_models(models, task, verbose)
  results$model_specs <- model_specs

  # 3.3 Entrenar Modelos
  if (verbose) .print_subsection(3, 3, "Entrenar y Evaluar Modelos")
  cv_results <- fit_models_cv(
    model_specs = model_specs,
    recipe = recipe,
    cv_folds = cv_result,
    task = task,
    select_metric = select_metric,
    verbose = verbose
  )
  results$cv_results <- cv_results

  # 3.4 Comparar Modelos
  if (verbose) .print_subsection(3, 4, "Comparar Modelos")
  comparison <- compare_models(cv_results, task, select_metric, verbose)
  if (verbose) .print_reference("roc_auc")
  results$comparison <- comparison
  results$best_model <- comparison$best_model

  results$task <- task
  results$target <- target
  results$recipe <- recipe
  results$train_data <- train_data
  class(results) <- c("supervisedml_modeling", "list")

  return(results)
}


#' @title Set Up Cross-Validation Folds
#'
#' @description
#' Creates k-fold cross-validation folds of the training data, stratified by
#' the outcome in classification.
#'
#' @param train_data Data frame with the training data.
#' @param target Character string with the name of the outcome variable.
#' @param task Type of task: `"classification"` or `"regression"`.
#' @param cv_folds Number of folds (default: 10).
#' @param seed Optional integer seed. If NULL (default) the random number
#'   generator state is not modified.
#' @param verbose Logical. Print the progress and an explanation of each
#'   step in the console (default: TRUE).
#'
#' @return An `rset` object (tibble with columns `splits` and `id`) created
#'   by [rsample::vfold_cv()].
#'
#' @examples
#' set.seed(1)
#' n <- 200
#' df <- data.frame(x1 = rnorm(n), x2 = rnorm(n), x3 = rnorm(n))
#' df$y <- factor(ifelse(df$x1 + 0.5 * df$x2 + rnorm(n) > 0, "yes", "no"))
#' folds <- setup_cv(df, target = "y", task = "classification",
#'                   cv_folds = 3, seed = 1)
#' folds
#'
#' @family supervised learning
#' @export
setup_cv <- function(train_data, target, task, cv_folds = 10, seed = NULL, verbose = TRUE) {

  if (!is.null(seed)) set.seed(seed)

  if (task == "classification") {
    folds <- rsample::vfold_cv(
      train_data,
      v = cv_folds,
      strata = !!rlang::sym(target)
    )
    cv_method <- "stratified"
  } else {
    folds <- rsample::vfold_cv(train_data, v = cv_folds)
    cv_method <- "simple"
  }

  if (verbose) {
    cat("    La validacion cruzada divide los datos en K partes (folds) para\n")
    cat("    evaluar el modelo de forma robusta. El modelo se entrena K veces,\n")
    cat("    cada vez usando K-1 folds para entrenar y 1 fold para validar.\n\n")

    if (cv_method == "stratified") {
      cat("    Metodo: K-Fold Cross-Validation Estratificado\n")
      cat("      - Folds:", cv_folds, "\n")
      cat("      - Estratificado por:", target, "\n")
      cat("      - Observaciones por fold: ~", round(nrow(train_data) / cv_folds), "\n")
      cat("      (Mantiene la proporcion de clases en cada fold)\n")
    } else {
      cat("    Metodo: K-Fold Cross-Validation Simple\n")
      cat("      - Folds:", cv_folds, "\n")
      cat("      - Observaciones por fold: ~", round(nrow(train_data) / cv_folds), "\n")
      cat("      (Division aleatoria sin estratificacion)\n")
    }
  }

  folds
}


#' @title Define Model Specifications
#'
#' @description
#' Creates the parsnip model specifications of the requested models for the
#' given task, with default hyperparameters.
#'
#' @param models Character vector with the models. Available: `"rf"` (random forest), `"xgboost"`, `"svm"` (RBF kernel),
#'   `"nnet"` (neural network), `"glm"` (logistic or linear regression),
#'   `"glmnet"` (regularized regression), `"tree"` (decision tree) and
#'   `"nb"` (naive Bayes). Some models need suggested packages (e.g.
#'   \pkg{kernlab}, \pkg{glmnet}, \pkg{rpart}).
#' @param task Type of task: `"classification"` or `"regression"`.
#' @param verbose Logical. Print the progress and an explanation of each
#'   step in the console (default: TRUE).
#'
#' @return A named list of parsnip model specifications (`model_spec`
#'   objects), one per model.
#'
#' @examples
#' specs <- define_models(c("glm", "rf"), task = "classification")
#' names(specs)
#'
#' @family supervised learning
#' @export
define_models <- function(models, task, verbose = TRUE) {

  model_specs <- list()

  # Descripciones de cada modelo
  model_descriptions <- list(
    rf = "Ensemble de arboles de decision con muestreo aleatorio",
    xgboost = "Gradient boosting optimizado, entrena arboles secuencialmente",
    glm = if (task == "classification") "Modelo lineal para probabilidades (regresion logistica)" else "Modelo lineal clasico para predecir valores continuos",
    glmnet = "Regresion con regularizacion Lasso/Ridge (penaliza coeficientes grandes)",
    svm = "Busca el hiperplano optimo para separar clases o predecir valores",
    nnet = "Modelo inspirado en neuronas, captura relaciones no lineales",
    tree = "Arbol de decision simple, facil de interpretar",
    nb = "Clasificador probabilistico basado en el teorema de Bayes"
  )

  for (model_name in models) {
    spec <- .get_model_spec(model_name, task)
    model_specs[[model_name]] <- spec

    if (verbose) {
      cat("    -", .get_model_label(model_name), "\n")
      if (model_name %in% names(model_descriptions)) {
        cat("       ", model_descriptions[[model_name]], "\n")
      }
    }
  }

  model_specs
}


# Recalcula el pr_auc de cada pliegue con las predicciones guardadas
# (save_pred = TRUE), usando la probabilidad de la clase positiva. Si algo
# falla devuelve el resultado sin tocar.
.corregir_pr_auc_cv <- function(cv_result, target, event_info) {
  tryCatch({
    for (i in seq_len(nrow(cv_result))) {
      pred <- cv_result$.predictions[[i]]
      m <- cv_result$.metrics[[i]]
      if (is.null(pred) || !"pr_auc" %in% m$.metric) next
      m$.estimate[m$.metric == "pr_auc"] <- yardstick::pr_auc_vec(
        truth = pred[[target]], estimate = pred[[event_info$prob_col]],
        event_level = event_info$event_level)
      cv_result$.metrics[[i]] <- m
    }
    cv_result
  }, error = function(e) cv_result)
}


#' @title Fit Models with Cross-Validation
#'
#' @description
#' Fits each model specification with the preprocessing recipe on every
#' cross-validation fold and computes the performance metrics
#' (classification: ROC-AUC, accuracy, sensitivity, specificity, F1,
#' balanced accuracy, PR-AUC (binary only), MCC, kappa and F2; regression:
#' RMSE, R-squared and MAE).
#'
#' @param model_specs Named list of model specifications, as returned by
#'   [define_models()].
#' @param recipe A `recipe` object (e.g. the `recipe` element of
#'   [preprocess_data()]).
#' @param cv_folds An `rset` of folds, as returned by [setup_cv()].
#' @param task Type of task: `"classification"` or `"regression"`.
#' @param select_metric Metric shown in the console for each model. If NULL
#'   (default) ROC-AUC is shown for classification and R-squared for
#'   regression.
#' @param verbose Logical. Print the progress and an explanation of each
#'   step in the console (default: TRUE).
#'
#' @return A named list with one resampling result (`resample_results`
#'   object from [tune::fit_resamples()], with saved predictions) per model.
#'
#' @examples
#' set.seed(1)
#' n <- 200
#' df <- data.frame(x1 = rnorm(n), x2 = rnorm(n), x3 = rnorm(n))
#' df$y <- factor(ifelse(df$x1 + 0.5 * df$x2 + rnorm(n) > 0, "yes", "no"))
#' pp <- preprocess_data(df, target = "y", task = "classification",
#'                       verbose = FALSE)
#' folds <- setup_cv(pp$train_data, target = "y", task = "classification",
#'                   cv_folds = 3, verbose = FALSE)
#' specs <- define_models("glm", task = "classification", verbose = FALSE)
#' cv_res <- fit_models_cv(specs, pp$recipe, folds, task = "classification",
#'                         verbose = FALSE)
#' tune::collect_metrics(cv_res$glm)
#'
#' @family supervised learning
#' @export
fit_models_cv <- function(model_specs, recipe, cv_folds, task, select_metric = NULL, verbose = TRUE) {

  # Detectar numero de niveles del target
  target_var <- recipe$var_info$variable[recipe$var_info$role == "outcome"]
  train_data <- recipe$template
  n_levels <- length(levels(factor(train_data[[target_var]])))

  # Definir metricas
  if (task == "classification") {

    if (n_levels == 2) {
      # Binary: detectar event_level para consistencia con eval_metrics()
      event_info <- .detect_event_level(factor(train_data[[target_var]]))
      el <- event_info$event_level

      # Tweak metricas que dependen de event_level
      # Nota: roc_auc y pr_auc NO se tweakean porque son metricas de probabilidad
      # y su calculo dentro de tune::fit_resamples() se invierte incorrectamente.
      # ROC-AUC es simetrico y no cambia con event_level.
      sensitivity_el <- yardstick::metric_tweak("sensitivity", yardstick::sensitivity, event_level = el)
      specificity_el <- yardstick::metric_tweak("specificity", yardstick::specificity, event_level = el)
      f_meas_el <- yardstick::metric_tweak("f_meas", yardstick::f_meas, event_level = el)
      f2_meas <- yardstick::metric_tweak("f2_meas", yardstick::f_meas, beta = 2, event_level = el)

      metrics_set <- yardstick::metric_set(
        yardstick::roc_auc,
        yardstick::accuracy,
        sensitivity_el,
        specificity_el,
        f_meas_el,
        yardstick::bal_accuracy,
        yardstick::pr_auc,
        yardstick::mcc,
        yardstick::kap,
        f2_meas
      )
    } else {
      # Multiclass: excluir pr_auc (no soporta multiclass)
      # F2-Score: F-beta con beta=2 (prioriza recall sobre precision)
      f2_meas <- yardstick::metric_tweak("f2_meas", yardstick::f_meas, beta = 2)

      metrics_set <- yardstick::metric_set(
        yardstick::roc_auc,
        yardstick::accuracy,
        yardstick::sensitivity,
        yardstick::specificity,
        yardstick::f_meas,
        yardstick::bal_accuracy,
        yardstick::mcc,
        yardstick::kap,
        f2_meas
      )
    }
  } else {
    metrics_set <- yardstick::metric_set(
      yardstick::rmse,
      yardstick::rsq,
      yardstick::mae
    )
  }

  cv_results <- list()

  for (model_name in names(model_specs)) {
    if (verbose) {
      cat("    Entrenando:", .get_model_label(model_name), "...")
    }

    # Crear workflow
    wf <- workflows::workflow() |>
      workflows::add_recipe(recipe) |>
      workflows::add_model(model_specs[[model_name]])

    # Entrenar con CV (suprimir mensajes informativos)
    cv_result <- suppressMessages(
      tune::fit_resamples(
        wf,
        resamples = cv_folds,
        metrics = metrics_set,
        control = tune::control_resamples(save_pred = TRUE)
      )
    )

    # PR-AUC binario: fit_resamples() pasa a las metricas de probabilidad la
    # PRIMERA columna (.pred_<nivel 1>), asi que pr_auc salia calculado para
    # la clase negativa (p. ej., .999 con un 4 % de positivos). Se recalcula
    # por pliegue con la columna y el event_level de la clase positiva.
    if (task == "classification" && n_levels == 2) {
      cv_result <- .corregir_pr_auc_cv(cv_result, target_var, event_info)
    }

    cv_results[[model_name]] <- cv_result

    if (verbose) {
      metrics <- tune::collect_metrics(cv_result)
      # Mostrar la metrica seleccionada por el usuario
      show_metric <- select_metric
      if (is.null(show_metric)) {
        show_metric <- if (task == "classification") "roc_auc" else "rsq"
      }
      metric_labels_cv <- c(
        roc_auc = "AUC", mcc = "MCC", f_meas = "F1", f2_meas = "F2",
        accuracy = "Acc", sensitivity = "Sens", specificity = "Spec",
        bal_accuracy = "BalAcc", pr_auc = "PR-AUC", kap = "Kappa",
        rmse = "RMSE", rsq = "R2", mae = "MAE"
      )
      show_label <- metric_labels_cv[show_metric]
      if (is.na(show_label)) show_label <- toupper(show_metric)
      val <- metrics$mean[metrics$.metric == show_metric]
      if (length(val) > 0) {
        cat(paste0(" ", show_label, " = ", round(val, 3), "\n"))
      } else {
        cat("\n")
      }
    }
  }

  cv_results
}


#' @title Compare Cross-Validated Models
#'
#' @description
#' Summarizes the cross-validated metrics of all models, adds the mean rank
#' across metrics (`rank_mean`) and selects the best model according to
#' `select_metric`. With `verbose = TRUE` it also reports the stability of
#' the best model across folds.
#'
#' @param cv_results Named list of resampling results, as returned by
#'   [fit_models_cv()].
#' @param task Type of task: `"classification"` or `"regression"`.
#' @param select_metric Metric used to select the best model. If NULL
#'   (default) ROC-AUC is used for classification and R-squared for
#'   regression. `"rank_mean"` selects the model with the best mean rank.
#' @param verbose Logical. Print the progress and an explanation of each
#'   step in the console (default: TRUE).
#'
#' @return A list with `summary` (data frame with one row per model, ordered
#'   from best to worst, with column `model`, one column per metric and
#'   `rank_mean`), `all_metrics` (long data frame from
#'   [tune::collect_metrics()] with a `model` column), `best_model`,
#'   `best_metric` (value of the selection metric for the best model) and
#'   `select_metric`.
#'
#' @examples
#' \donttest{
#' set.seed(1)
#' n <- 200
#' df <- data.frame(x1 = rnorm(n), x2 = rnorm(n), x3 = rnorm(n))
#' df$y <- factor(ifelse(df$x1 + 0.5 * df$x2 + rnorm(n) > 0, "yes", "no"))
#' pp <- preprocess_data(df, target = "y", task = "classification",
#'                       verbose = FALSE)
#' folds <- setup_cv(pp$train_data, target = "y", task = "classification",
#'                   cv_folds = 3, verbose = FALSE)
#' specs <- define_models(c("glm", "rf"), task = "classification",
#'                        verbose = FALSE)
#' cv_res <- fit_models_cv(specs, pp$recipe, folds, task = "classification",
#'                         verbose = FALSE)
#' cmp <- compare_models(cv_res, task = "classification", select_metric = "mcc")
#' cmp$best_model
#' }
#'
#' @family supervised learning
#' @export
compare_models <- function(cv_results, task, select_metric = NULL, verbose = TRUE) {

  # Establecer metrica por defecto
  if (is.null(select_metric)) {
    select_metric <- if (task == "classification") "roc_auc" else "rsq"
  }

  # Etiquetas legibles para las metricas
  metric_labels <- c(
    roc_auc = "ROC-AUC",
    f_meas = "F1-Score",
    f2_meas = "F2-Score",
    accuracy = "Accuracy",
    sensitivity = "Sensitivity",
    specificity = "Specificity",
    bal_accuracy = "Balanced Accuracy",
    pr_auc = "PR-AUC",
    mcc = "MCC",
    kap = "Cohen's Kappa",
    rmse = "RMSE",
    rsq = "R-squared",
    mae = "MAE",
    rank_mean = "Ranking Promedio"
  )

  # Descripciones de cuando usar cada metrica
  metric_descriptions <- c(
    roc_auc = "Buena para desbalance moderado, evalua discriminacion global",
    f_meas = "Balance entre precision y recall, buena para desbalance",
    f2_meas = "Como F1 pero prioriza recall (detectar positivos), ideal cuando perder un positivo es costoso",
    accuracy = "Simple pero sensible al desbalance de clases",
    sensitivity = "Prioriza detectar todos los positivos (recall)",
    specificity = "Prioriza evitar falsos positivos",
    bal_accuracy = "Promedio de sensitivity y specificity",
    pr_auc = "Ideal para desbalance severo (clases muy desiguales)",
    mcc = "Correlacion entre prediccion y realidad, robusto a desbalance (-1 a 1)",
    kap = "Acuerdo corregido por azar, robusto a desbalance (-1 a 1)",
    rmse = "Penaliza errores grandes, en unidades originales",
    rsq = "Proporcion de varianza explicada (0-1)",
    mae = "Error promedio absoluto, robusto a outliers",
    rank_mean = "Ranking promedio a traves de todas las metricas (menor = mejor en mas metricas)"
  )

  # Metricas que se maximizan vs minimizan
  maximize_metrics <- c("roc_auc", "f_meas", "f2_meas", "accuracy", "sensitivity",
                        "specificity", "bal_accuracy", "pr_auc", "mcc", "kap", "rsq")

  # Recopilar metricas de todos los modelos
  all_metrics <- lapply(names(cv_results), function(model_name) {
    metrics <- tune::collect_metrics(cv_results[[model_name]])
    metrics$model <- model_name
    metrics
  })

  metrics_df <- do.call(rbind, all_metrics)

  # Crear resumen
  summary_df <- metrics_df |>
    dplyr::select(model, .metric, mean) |>
    tidyr::pivot_wider(names_from = .metric, values_from = mean)

  # Calcular rank_mean: ranking promedio a traves de todas las metricas
  metric_cols <- setdiff(names(summary_df)[vapply(summary_df, is.numeric, logical(1))], "model")
  if (length(metric_cols) > 0) {
    minimize_metrics <- c("rmse", "mae", "mape")
    # do.call(cbind, lapply(...)) devuelve SIEMPRE una matriz de
    # n_modelos x n_metricas. Con sapply(), cuando habia un solo modelo el
    # resultado era un vector de longitud n_metricas y la asignacion fallaba
    # ("Can't recycle input of size 10 to size 1").
    rank_mat <- do.call(cbind, lapply(metric_cols, function(m) {
      if (m %in% minimize_metrics) {
        rank(summary_df[[m]], ties.method = "average")
      } else {
        rank(-summary_df[[m]], ties.method = "average")
      }
    }))
    summary_df$rank_mean <- round(rowMeans(rank_mat), 2)
  }

  # Ordenar segun la metrica seleccionada
  if (select_metric == "rank_mean") {
    summary_df <- summary_df |>
      dplyr::arrange(rank_mean)
  } else if (select_metric %in% maximize_metrics) {
    summary_df <- summary_df |>
      dplyr::arrange(dplyr::desc(!!rlang::sym(select_metric)))
  } else {
    summary_df <- summary_df |>
      dplyr::arrange(!!rlang::sym(select_metric))
  }

  best_model <- summary_df$model[1]
  best_metric_value <- summary_df[[select_metric]][1]

  if (verbose) {
    cat("    Metrica de seleccion:", metric_labels[select_metric], "\n\n")
    cat("    Resumen de modelos:\n\n")
    print(as.data.frame(summary_df))

    # Mostrar leyenda de metricas
    cat("\n    Leyenda de metricas:\n")
    if (task == "classification") {
      cat("      - accuracy:     Proporcion de predicciones correctas\n")
      cat("      - bal_accuracy: Promedio de sensitivity y specificity\n")
      cat("      - f_meas:       F1-Score, balance entre precision y recall\n")
      cat("      - f2_meas:      F2-Score, como F1 pero prioriza recall\n")
      cat("      - mcc:          Matthews Correlation Coefficient (-1 a 1)\n")
      cat("      - kap:          Cohen's Kappa, acuerdo corregido por azar (-1 a 1)\n")
      if ("pr_auc" %in% names(summary_df)) {
        cat("      - pr_auc:       Area bajo curva Precision-Recall\n")
      }
      cat("      - roc_auc:      Area bajo curva ROC\n")
      cat("      - sensitivity:  Tasa de verdaderos positivos (recall)\n")
      cat("      - specificity:  Tasa de verdaderos negativos\n")
    } else {
      cat("      - rmse: Error cuadratico medio (penaliza errores grandes)\n")
      cat("      - rsq:  R-cuadrado, proporcion de varianza explicada\n")
      cat("      - mae:  Error absoluto medio (robusto a outliers)\n")
    }

    # Mostrar rank_mean en la leyenda si existe
    if ("rank_mean" %in% names(summary_df)) {
      cat("      - rank_mean:    Ranking promedio (menor = mejor en mas metricas)\n")
    }

    cat("\n    Mejor modelo seleccionado por:", metric_labels[select_metric], "\n")
    cat("    Mejor modelo:", .get_model_label(best_model), "\n")
    cat("   ", metric_labels[select_metric], ":", round(best_metric_value, 4), "\n")

    if (select_metric == "rank_mean") {
      # Para rank_mean, mostrar en cuantas metricas gana el mejor modelo
      n_metrics <- length(metric_cols)
      n_wins <- sum(sapply(metric_cols, function(m) {
        if (m %in% c("rmse", "mae", "mape")) {
          which.min(summary_df[[m]]) == 1
        } else {
          which.max(summary_df[[m]]) == 1
        }
      }))
      cat("    El modelo es #1 en", n_wins, "de", n_metrics, "metricas\n\n")
    } else {
      # Interpretacion del valor de la metrica
      interpretation <- .interpret_metric_value(select_metric, best_metric_value)
      cat("    Interpretacion:", interpretation, "\n\n")
    }

    # Diagnostico de estabilidad entre folds (varianza CV) -- no aplica para rank_mean
    if (select_metric != "rank_mean") {
      best_row <- metrics_df[metrics_df$model == best_model & metrics_df$.metric == select_metric, ]
      if (nrow(best_row) > 0 && !is.na(best_row$std_err[1]) && best_row$mean[1] != 0) {
        n_folds <- best_row$n[1]
        std_err_val <- best_row$std_err[1]
        mean_val <- best_row$mean[1]
        cv_pct <- abs(std_err_val * sqrt(n_folds) / mean_val) * 100

        cat("    Estabilidad entre folds (CV%): ", round(cv_pct, 1), "%\n", sep = "")
        if (cv_pct < 5) {
          cat("    [ok] Modelo estable entre folds\n\n")
        } else if (cv_pct <= 15) {
          cat("    [~] Variabilidad moderada entre folds -- el modelo podria ser inestable\n\n")
        } else {
          cat("    [!] ADVERTENCIA: Alta variabilidad entre folds -- rendimiento inestable\n")
          cat("    Considere: aumentar los datos de entrenamiento o simplificar el modelo\n\n")
        }
      }
    }

    cat("    Nota: El", metric_labels[select_metric], "es", metric_descriptions[select_metric], "\n")

    # Referencia academica para metricas auto-seleccionadas
    if (select_metric == "mcc") {
      cat("    Referencia: Chicco & Jurman (2023). Patterns, 4(12), 100804.\n")
    } else if (select_metric == "rsq") {
      cat("    Referencia: Chicco et al. (2021). PeerJ Comput Sci, 7, e623.\n")
    }
  }

  list(
    summary = as.data.frame(summary_df),
    all_metrics = metrics_df,
    best_model = best_model,
    best_metric = best_metric_value,
    select_metric = select_metric
  )
}


#' Get a Readable Model Label
#' @noRd
.get_model_label <- function(model_name) {
  labels <- c(
    rf = "Random Forest",
    xgboost = "XGBoost",
    svm = "SVM (RBF)",
    nnet = "Red Neuronal",
    glm = "Regresion Logistica/Lineal",
    glmnet = "GLMNet (Regularizado)",
    tree = "Arbol de Decision",
    nb = "Naive Bayes"
  )
  if (model_name %in% names(labels)) {
    return(labels[[model_name]])
  }
  return(model_name)
}


#' Interpret a Metric Value
#' @description Gives plain-language interpretations for non-expert users.
#' @noRd
.interpret_metric_value <- function(metric, value) {

  if (is.null(value) || length(value) == 0 || is.na(value)) {
    return("No disponible (NA)")
  }

  # ROC-AUC y PR-AUC: miden que tan bien el modelo distingue entre clases
  if (metric %in% c("roc_auc", "pr_auc")) {
    if (value >= 0.99) return("[!] ADVERTENCIA: Valor sospechosamente alto (posible overfitting o data leakage). Revise los datos.")
    if (value >= 0.90) return("Excelente: el modelo distingue muy bien entre las clases")
    if (value >= 0.80) return("Bueno: el modelo distingue bien entre las clases")
    if (value >= 0.70) return("Aceptable: el modelo distingue moderadamente entre las clases")
    if (value >= 0.60) return("Debil: el modelo tiene dificultad para distinguir las clases")
    return("Insuficiente: el modelo no logra distinguir las clases correctamente")
  }

  # Accuracy y Balanced Accuracy: proporcion de aciertos
  if (metric %in% c("accuracy", "bal_accuracy")) {
    pct <- round(value * 100)
    if (value >= 0.99) return(paste0("[!] ADVERTENCIA: ", pct, "% de acierto es sospechosamente alto (posible overfitting o data leakage). Revise los datos."))
    if (value >= 0.90) return(paste0("Excelente: el modelo acierta en ", pct, "% de los casos"))
    if (value >= 0.80) return(paste0("Bueno: el modelo acierta en ", pct, "% de los casos"))
    if (value >= 0.70) return(paste0("Aceptable: el modelo acierta en ", pct, "% de los casos"))
    if (value >= 0.60) return(paste0("Moderado: el modelo acierta en ", pct, "% de los casos"))
    return(paste0("Bajo: el modelo solo acierta en ", pct, "% de los casos"))
  }

  # MCC: correlacion entre prediccion y realidad (-1 a 1)
  if (metric == "mcc") {
    if (value >= 0.99) return("[!] ADVERTENCIA: Valor sospechosamente alto (posible overfitting o data leakage). Revise los datos.")
    if (value >= 0.70) return("Excelente: alta correlacion entre predicciones y valores reales")
    if (value >= 0.50) return("Bueno: buena correlacion entre predicciones y valores reales")
    if (value >= 0.30) return("Aceptable: correlacion moderada entre predicciones y valores reales")
    if (value >= 0.10) return("Debil: baja correlacion entre predicciones y valores reales")
    if (value >= 0) return("Muy debil: el modelo apenas supera al azar")
    return("Negativo: el modelo predice peor que el azar (predicciones inversas)")
  }

  # Kappa: acuerdo corregido por azar (-1 a 1)
  if (metric == "kap") {
    if (value >= 0.99) return("[!] ADVERTENCIA: Valor sospechosamente alto (posible overfitting o data leakage). Revise los datos.")
    if (value >= 0.81) return("Excelente: acuerdo casi perfecto con la realidad")
    if (value >= 0.61) return("Bueno: acuerdo sustancial con la realidad")
    if (value >= 0.41) return("Aceptable: acuerdo moderado con la realidad")
    if (value >= 0.21) return("Debil: acuerdo bajo con la realidad")
    if (value >= 0) return("Muy debil: el modelo apenas supera al azar")
    return("Negativo: el modelo predice peor que el azar")
  }

  # F2-Score: como F1 pero prioriza recall (detectar positivos)
  if (metric == "f2_meas") {
    if (value >= 0.99) return("[!] ADVERTENCIA: Valor sospechosamente alto (posible overfitting o data leakage). Revise los datos.")
    if (value >= 0.90) return("Excelente: detecta casi todos los positivos con buena precision")
    if (value >= 0.80) return("Bueno: buena deteccion de positivos con precision razonable")
    if (value >= 0.70) return("Aceptable: detecta la mayoria de positivos pero con algunos errores")
    if (value >= 0.60) return("Moderado: pierde algunos positivos o tiene errores frecuentes")
    return("Bajo: pierde muchos positivos o comete demasiados errores")
  }

  # F1-Score: equilibrio entre encontrar todos los positivos y no equivocarse
  if (metric == "f_meas") {
    if (value >= 0.99) return("[!] ADVERTENCIA: Valor sospechosamente alto (posible overfitting o data leakage). Revise los datos.")
    if (value >= 0.90) return("Excelente: encuentra los positivos sin casi equivocarse")
    if (value >= 0.80) return("Bueno: buen equilibrio entre encontrar positivos y precision")
    if (value >= 0.70) return("Aceptable: equilibrio moderado entre encontrar positivos y precision")
    if (value >= 0.60) return("Moderado: puede perder positivos o tener algunos errores")
    return("Bajo: pierde muchos positivos o comete muchos errores")
  }

  # Sensitivity: capacidad de detectar los casos positivos
  if (metric == "sensitivity") {
    pct <- round(value * 100)
    if (value >= 0.99) return(paste0("[!] ADVERTENCIA: ", pct, "% de sensitivity es sospechosamente alto (posible overfitting o data leakage). Revise los datos."))
    if (value >= 0.90) return(paste0("Excelente: detecta el ", pct, "% de los casos positivos"))
    if (value >= 0.80) return(paste0("Bueno: detecta el ", pct, "% de los casos positivos"))
    if (value >= 0.70) return(paste0("Aceptable: detecta el ", pct, "% de los casos positivos"))
    if (value >= 0.60) return(paste0("Moderado: detecta el ", pct, "% de los casos positivos"))
    return(paste0("Bajo: solo detecta el ", pct, "% de los casos positivos"))
  }

  # Specificity: capacidad de detectar los casos negativos
  if (metric == "specificity") {
    pct <- round(value * 100)
    if (value >= 0.99) return(paste0("[!] ADVERTENCIA: ", pct, "% de specificity es sospechosamente alto (posible overfitting o data leakage). Revise los datos."))
    if (value >= 0.90) return(paste0("Excelente: identifica correctamente el ", pct, "% de los negativos"))
    if (value >= 0.80) return(paste0("Bueno: identifica correctamente el ", pct, "% de los negativos"))
    if (value >= 0.70) return(paste0("Aceptable: identifica correctamente el ", pct, "% de los negativos"))
    if (value >= 0.60) return(paste0("Moderado: identifica correctamente el ", pct, "% de los negativos"))
    return(paste0("Bajo: solo identifica correctamente el ", pct, "% de los negativos"))
  }

  # R-squared: cuanto de la variabilidad explica el modelo
  if (metric == "rsq") {
    pct <- round(value * 100)
    if (value >= 0.99) return(paste0("[!] ADVERTENCIA: R-squared de ", pct, "% es sospechosamente alto (posible overfitting o data leakage). Revise los datos."))
    if (value >= 0.90) return(paste0("Excelente: el modelo explica el ", pct, "% de la variacion en los datos"))
    if (value >= 0.75) return(paste0("Bueno: el modelo explica el ", pct, "% de la variacion en los datos"))
    if (value >= 0.50) return(paste0("Moderado: el modelo explica el ", pct, "% de la variacion en los datos"))
    if (value >= 0.25) return(paste0("Debil: el modelo solo explica el ", pct, "% de la variacion"))
    return(paste0("Muy debil: el modelo apenas explica el ", pct, "% de la variacion"))
  }

  # RMSE y MAE: error promedio en las predicciones
  if (metric %in% c("rmse", "mae")) {
    if (value < 1e-6) return("[!] ADVERTENCIA: Error practicamente cero es sospechoso (posible overfitting o data leakage). Revise los datos.")
    return(paste0("Error promedio de ", round(value, 2), " unidades (comparar con el rango de la variable)"))
  }

  return("")
}
