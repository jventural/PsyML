# =============================================================================
# Funciones de Evaluacion en Test para PsyML
# =============================================================================

#' Detect the Positive Class and the Event Level
#' @noRd
.detect_event_level <- function(target_vector) {

  levels_y <- levels(target_vector)
  n_levels <- length(levels_y)

  # --- Multiclass: 3+ niveles ---
  if (n_levels >= 3) {
    return(list(
      type = "multiclass",
      levels = levels_y,
      n_classes = n_levels,
      prob_cols = paste0(".pred_", levels_y)
    ))
  }

  if (n_levels < 2) {
    stop("The outcome variable must have at least 2 levels")
  }

  # --- Binary: exactamente 2 niveles ---

  # Funcion para normalizar texto: minusculas, sin tildes
.normalize_text <- function(x) {
    x <- tolower(x)
    # Remover tildes y caracteres especiales
    x <- gsub("[\u00E1\u00E0\u00E4\u00E2\u00E3]", "a", x)
    x <- gsub("[\u00E9\u00E8\u00EB\u00EA]", "e", x)
    x <- gsub("[\u00ED\u00EC\u00EF\u00EE]", "i", x)
    x <- gsub("[\u00F3\u00F2\u00F6\u00F4\u00F5]", "o", x)
    x <- gsub("[\u00FA\u00F9\u00FC\u00FB]", "u", x)
    x <- gsub("[\u00F1]", "n", x)
    x
  }

  # Patrones comunes para clase positiva (espanol e ingles)
  positive_patterns <- c(
    # Afirmaciones basicas
    "yes", "si", "1", "true", "verdadero", "verdad",
    # Estado positivo
    "positive", "positivo", "pos", "positiva",
    # Presencia
    "present", "presente", "exists", "existe",
    # Casos/eventos
    "case", "caso", "event", "evento",
    # Enfermedad/condicion
    "disease", "enfermo", "enferma", "enfermedad", "sick", "ill",
    "patient", "paciente", "affected", "afectado", "afectada",
    "diagnosed", "diagnosticado", "diagnosticada",
    # Exito/logro
    "success", "exito", "exitoso", "exitosa", "successful",
    "approved", "aprobado", "aprobada", "passed", "aprobado",
    "accepted", "aceptado", "aceptada",
    # Supervivencia
    "survived", "sobrevivio", "alive", "vivo", "viva",
    # Riesgo/alto
    "high", "alto", "alta", "risk", "riesgo",
    # Ocurrencia
    "occurred", "ocurrio", "happened", "sucedio",
    "detected", "detectado", "detectada",
    # Respuesta
    "response", "respuesta", "responded", "respondio",
    "reactive", "reactivo", "reactiva",
    # Compra/conversion
    "buy", "compra", "compro", "purchased", "bought",
    "converted", "convertido", "convertida", "conversion",
    "churned", "churn", "abandono",
    # Fraude/anomalia
    "fraud", "fraude", "fraudulent", "fraudulento",
    "anomaly", "anomalia", "suspicious", "sospechoso",
    # Otros comunes
    "winner", "ganador", "ganadora", "won", "gano",
    "default", "impago", "moroso", "morosa",
    "pregnant", "embarazada", "embarazo",
    "malignant", "maligno", "maligna", "cancer",
    "spam", "recurrence", "recurrencia", "recidiva",
    "dropout", "desercion", "desertor"
  )

  # Normalizar niveles
  level1_norm <- .normalize_text(levels_y[1])
  level2_norm <- .normalize_text(levels_y[2])

  # Determinar cual es la clase positiva
  level2_is_positive <- any(sapply(positive_patterns, function(p) grepl(p, level2_norm, fixed = TRUE)))
  level1_is_positive <- any(sapply(positive_patterns, function(p) grepl(p, level1_norm, fixed = TRUE)))

  # Si level2 parece positivo, usar event_level = "second"
  # Si level1 parece positivo, usar event_level = "first"
  # Si ninguno es claro, asumir que level2 es positivo (convencion comun)
  if (level2_is_positive && !level1_is_positive) {
    event_level <- "second"
    positive_class <- levels_y[2]
  } else if (level1_is_positive && !level2_is_positive) {
    event_level <- "first"
    positive_class <- levels_y[1]
  } else {
    # Por defecto, asumir segundo nivel como positivo
    event_level <- "second"
    positive_class <- levels_y[2]
  }

  list(
    type = "binary",
    levels = levels_y,
    positive_class = positive_class,
    negative_class = ifelse(positive_class == levels_y[1], levels_y[2], levels_y[1]),
    event_level = event_level,
    prob_col = paste0(".pred_", positive_class)
  )
}


#' @title Evaluate the Final Model on the Test Set
#'
#' @description
#' Fits the final workflow on the whole training set and evaluates it on the
#' test set: predictions, performance metrics, overfitting diagnosis
#' (cross-validation vs test, out-of-bag vs test for random forests, train vs
#' test for other models) and, depending on the task, ROC curve, confusion
#' matrix and calibration (classification) or predicted vs observed values
#' and residual analysis (regression).
#'
#' @param tuning_result A list with a `final_workflow` element (an unfitted
#'   tidymodels workflow), such as the result of [tune_best_model()] or the
#'   `tuning` element of a [supervised_ml()] result.
#' @param train_data Data frame with the (preprocessed) training set.
#' @param test_data Data frame with the (preprocessed) test set.
#' @param target Character string with the name of the outcome variable.
#' @param task Type of task: `"classification"` or `"regression"`.
#' @param modeling_result Optional result of [train_models()]; its
#'   cross-validation summary is used in the overfitting diagnosis
#'   (default: NULL).
#' @param best_model Optional name of the best model, e.g. `"rf"` or
#'   `"xgboost"` (default: NULL).
#' @param verbose Logical. Print the results and their interpretation in
#'   the console (default: TRUE).
#'
#' @return An object of class `supervisedml_evaluation` (a list) with:
#'   `final_fit` (fitted workflow), `predictions` (test predictions, see
#'   [eval_predictions()]), `metrics` (test metrics, see [eval_metrics()]),
#'   `train_metrics` (training metrics, NULL for random forests),
#'   `overfitting` (list with the overfitting diagnosis: `cv_auc`,
#'   `test_auc`, `cv_gap`, train vs test `comparison`, and `oob_auc`,
#'   `oob_gap` for random forests), `task` and `target`. Classification adds
#'   `roc_data` ([eval_roc()]), `confusion` ([eval_confusion()]) and, for
#'   binary outcomes, `calibration` ([eval_calibration()]). Regression adds
#'   `pred_obs` ([eval_pred_vs_obs()]) and `residuals` ([eval_residuals()]).
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
#' ev <- evaluate_model(res$tuning, res$train_data, res$test_data,
#'                      target = "y", task = "classification",
#'                      verbose = FALSE)
#' ev$metrics
#' }
#'
#' @family supervised learning
#' @export
evaluate_model <- function(tuning_result,
                           train_data,
                           test_data,
                           target,
                           task,
                           modeling_result = NULL,
                           best_model = NULL,
                           verbose = TRUE) {

  if (verbose) {
    .print_section(5, "Evaluacion en Test")
  }

  results <- list()

  # Obtener workflow final
  if (inherits(tuning_result, "supervisedml_tuning")) {
    final_wf <- tuning_result$final_workflow
  } else {
    final_wf <- tuning_result$final_workflow
  }

  # 5.1 Entrenar Modelo Final
  if (verbose) .print_subsection(5, 1, "Entrenar Modelo Final")

  if (verbose) {
    cat("    El modelo final se entrena usando TODOS los datos de entrenamiento.\n")
    cat("    Esto permite que el modelo aprenda de la mayor cantidad de datos\n")
    cat("    posible antes de ser evaluado en el conjunto de test.\n\n")
  }

  final_fit <- parsnip::fit(final_wf, data = train_data)
  results$final_fit <- final_fit

  if (verbose) {
    cat("    Modelo entrenado con", nrow(train_data), "observaciones\n")
  }

  # 5.2 Predicciones en Test
  if (verbose) {
    .print_subsection(5, 2, "Predicciones en Test")
    cat("    El conjunto de test contiene datos que el modelo NUNCA ha visto\n")
    cat("    durante el entrenamiento. Esto nos da una estimacion honesta de\n")
    cat("    como se comportara el modelo con datos nuevos en el mundo real.\n\n")
  }

  predictions <- eval_predictions(final_fit, test_data, target, task, verbose)
  results$predictions <- predictions
  if (verbose) .print_reference("train_test")

  # 5.3 Metricas de Rendimiento
  if (verbose) .print_subsection(5, 3, "Metricas de Rendimiento")

  metrics <- eval_metrics(predictions, target, task, verbose)
  results$metrics <- metrics
  if (verbose) .print_reference("roc_auc")

  # 5.4 Diagnostico de Overfitting
  if (verbose) .print_subsection(5, 4, "Diagnostico de Overfitting")
  overfitting_diag <- .diagnose_overfitting(
    final_fit = final_fit,
    train_data = train_data,
    test_metrics = metrics,
    target = target,
    task = task,
    modeling_result = modeling_result,
    best_model = best_model,
    verbose = verbose
  )
  results$train_metrics <- overfitting_diag$train_metrics
  results$overfitting <- overfitting_diag

  if (task == "classification") {
    # 5.5 Curva ROC
    if (verbose) .print_subsection(5, 5, "Curva ROC")
    results$roc_data <- eval_roc(predictions, target, verbose)
    if (verbose) .print_reference("roc_auc")

    # 5.6 Matriz de Confusion
    if (verbose) .print_subsection(5, 6, "Matriz de Confusion")
    results$confusion <- eval_confusion(predictions, target, verbose)
    if (verbose) .print_reference("confusion_matrix")

    # 5.7 Calibracion (solo binario)
    n_levels_eval <- length(levels(predictions[[target]]))
    if (n_levels_eval == 2) {
      if (verbose) .print_subsection(5, 7, "Analisis de Calibracion")
      results$calibration <- eval_calibration(predictions, target, verbose)
      if (verbose) .print_reference("calibration")
    } else {
      if (verbose) {
        .print_subsection(5, 7, "Analisis de Calibracion")
        cat("    Analisis de calibracion disponible solo para clasificacion binaria.\n")
      }
    }

  } else {
    # 5.5 Predicciones vs Observaciones
    if (verbose) .print_subsection(5, 5, "Predicciones vs Observaciones")
    results$pred_obs <- eval_pred_vs_obs(predictions, target, verbose)
    if (verbose) .print_reference("r_squared")

    # 5.6 Analisis de Residuos
    if (verbose) .print_subsection(5, 6, "Analisis de Residuos")
    results$residuals <- eval_residuals(predictions, target, verbose)
    if (verbose) .print_reference("residuals")
  }

  results$task <- task
  results$target <- target
  class(results) <- c("supervisedml_evaluation", "list")

  return(results)
}


#' @title Generate Predictions on a Data Set
#'
#' @description
#' Predicts the outcome of a data set with a fitted workflow and binds the
#' predictions (and class probabilities for classification) to the data.
#'
#' @param final_fit A fitted tidymodels workflow, e.g. the `final_fit`
#'   element of a [supervised_ml()] result.
#' @param test_data Data frame with the observations to predict.
#' @param target Character string with the name of the outcome variable.
#' @param task Type of task: `"classification"` or `"regression"`.
#' @param verbose Logical. Print the results and their interpretation in
#'   the console (default: TRUE).
#'
#' @return A tibble with the columns of `test_data` plus `.pred_class` and
#'   one `.pred_<level>` probability column per class (classification), or
#'   `.pred` (regression).
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
#' preds <- eval_predictions(res$final_fit, res$test_data, target = "y",
#'                           task = "classification")
#' head(preds)
#' }
#'
#' @family supervised learning
#' @export
eval_predictions <- function(final_fit, test_data, target, task, verbose = TRUE) {

  # Predicciones de clase/valor
  preds <- stats::predict(final_fit, new_data = test_data)

  if (task == "classification") {
    # Predicciones de probabilidad
    probs <- stats::predict(final_fit, new_data = test_data, type = "prob")
    predictions <- dplyr::bind_cols(test_data, preds, probs)
  } else {
    predictions <- dplyr::bind_cols(test_data, preds)
  }

  if (verbose) {
    cat("    Predicciones generadas:", nrow(predictions), "observaciones\n")
  }

  predictions
}


#' @title Compute Performance Metrics
#'
#' @description
#' Computes performance metrics from a data frame of predictions. For binary
#' classification the positive class is detected from the level names (e.g.
#' "yes", "1", "positive"; otherwise the second level). Multiclass metrics
#' use macro averaging and the Hand-Till ROC-AUC.
#'
#' @param predictions A data frame of predictions with the observed
#'   outcome column and, for classification, the `.pred_class` column and
#'   one `.pred_<level>` probability column per class (as returned by
#'   [eval_predictions()]).
#' @param target Character string with the name of the outcome variable.
#' @param task Type of task: `"classification"` or `"regression"`.
#' @param verbose Logical. Print the results and their interpretation in
#'   the console (default: TRUE).
#'
#' @return A tibble with columns `.metric`, `.estimator` and `.estimate`.
#'   Classification: `roc_auc`, `accuracy`, `sensitivity`, `specificity`,
#'   `f_meas`, `mcc`, `kap` and `f2_meas`. Regression: `rmse`, `rsq` and
#'   `mae`.
#'
#' @examples
#' set.seed(1)
#' y <- factor(sample(c("no", "yes"), 100, replace = TRUE))
#' p <- stats::plogis(ifelse(y == "yes", 1, -1) + rnorm(100))
#' preds <- data.frame(y = y,
#'                     .pred_class = factor(ifelse(p >= 0.5, "yes", "no"),
#'                                          levels = levels(y)),
#'                     .pred_no = 1 - p, .pred_yes = p)
#' eval_metrics(preds, target = "y", task = "classification")
#'
#' # Regression
#' yr <- rnorm(100, mean = 10)
#' preds_r <- data.frame(yr = yr, .pred = yr + rnorm(100, sd = 0.5))
#' eval_metrics(preds_r, target = "yr", task = "regression", verbose = FALSE)
#'
#' @family supervised learning
#' @export
eval_metrics <- function(predictions, target, task, verbose = TRUE) {

  if (task == "classification") {
    # Detectar clase positiva y event_level
    event_info <- .detect_event_level(predictions[[target]])

    if (event_info$type == "binary") {
      # --- BINARY ---
      if (verbose) {
        cat("    Estas son las metricas calculadas en el conjunto de TEST.\n")
        cat("    Representan el rendimiento real del modelo con datos nuevos.\n\n")

        cat("    Configuracion de clases:\n")
        cat("      - Clase positiva (lo que queremos detectar):", event_info$positive_class, "\n")
        cat("      - Clase negativa:", event_info$negative_class, "\n\n")
      }

      # F2-Score: F-beta con beta=2 (prioriza recall sobre precision)
      f2_meas <- yardstick::metric_tweak("f2_meas", yardstick::f_meas, beta = 2)

      metrics <- dplyr::bind_rows(
        yardstick::roc_auc(predictions, truth = !!rlang::sym(target),
                           !!rlang::sym(event_info$prob_col),
                           event_level = event_info$event_level),
        yardstick::accuracy(predictions, truth = !!rlang::sym(target),
                            estimate = .pred_class),
        yardstick::sensitivity(predictions, truth = !!rlang::sym(target),
                               estimate = .pred_class,
                               event_level = event_info$event_level),
        yardstick::specificity(predictions, truth = !!rlang::sym(target),
                               estimate = .pred_class,
                               event_level = event_info$event_level),
        yardstick::f_meas(predictions, truth = !!rlang::sym(target),
                          estimate = .pred_class,
                          event_level = event_info$event_level),
        yardstick::mcc(predictions, truth = !!rlang::sym(target),
                       estimate = .pred_class),
        yardstick::kap(predictions, truth = !!rlang::sym(target),
                       estimate = .pred_class),
        f2_meas(predictions, truth = !!rlang::sym(target),
                estimate = .pred_class,
                event_level = event_info$event_level)
      )
    } else {
      # --- MULTICLASS ---
      if (verbose) {
        cat("    Estas son las metricas calculadas en el conjunto de TEST.\n")
        cat("    Representan el rendimiento real del modelo con datos nuevos.\n\n")

        cat("    Clasificacion multiclase (", event_info$n_classes, " clases):\n", sep = "")
        cat("      - Clases:", paste(event_info$levels, collapse = ", "), "\n\n")
      }

      # F2-Score multiclass
      f2_meas <- yardstick::metric_tweak("f2_meas", yardstick::f_meas, beta = 2)

      # roc_auc multiclass: pasar todas las columnas .pred_*
      prob_cols_syms <- rlang::syms(event_info$prob_cols)

      metrics <- dplyr::bind_rows(
        yardstick::roc_auc(predictions, truth = !!rlang::sym(target),
                           !!!prob_cols_syms),
        yardstick::accuracy(predictions, truth = !!rlang::sym(target),
                            estimate = .pred_class),
        yardstick::sensitivity(predictions, truth = !!rlang::sym(target),
                               estimate = .pred_class,
                               estimator = "macro"),
        yardstick::specificity(predictions, truth = !!rlang::sym(target),
                               estimate = .pred_class,
                               estimator = "macro"),
        yardstick::f_meas(predictions, truth = !!rlang::sym(target),
                          estimate = .pred_class,
                          estimator = "macro"),
        yardstick::mcc(predictions, truth = !!rlang::sym(target),
                       estimate = .pred_class),
        yardstick::kap(predictions, truth = !!rlang::sym(target),
                       estimate = .pred_class),
        f2_meas(predictions, truth = !!rlang::sym(target),
                estimate = .pred_class,
                estimator = "macro")
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

  if (verbose) {
    cat("    Metricas en Test:\n")
    for (i in 1:nrow(metrics)) {
      metric_name <- metrics$.metric[i]
      metric_value <- metrics$.estimate[i]
      interpretation <- .interpret_metric_value(metric_name, metric_value)
      cat("      -", metric_name, ":", round(metric_value, 4), "\n")
      if (nchar(interpretation) > 0) {
        cat("        ", interpretation, "\n")
      }
    }

    if (task == "classification") {
      is_multi <- event_info$type == "multiclass"
      cat("\n    Leyenda rapida:\n")
      cat("      - roc_auc: Capacidad de distinguir entre clases (0.5=azar, 1=perfecto)",
          if (is_multi) " [Hand-Till]" else "", "\n", sep = "")
      cat("      - accuracy: Porcentaje de predicciones correctas\n")
      cat("      - sensitivity: Porcentaje de positivos detectados correctamente",
          if (is_multi) " [promedio macro]" else "", "\n", sep = "")
      cat("      - specificity: Porcentaje de negativos detectados correctamente",
          if (is_multi) " [promedio macro]" else "", "\n", sep = "")
      cat("      - f_meas: Balance entre precision y sensibilidad (F1)",
          if (is_multi) " [promedio macro]" else "", "\n", sep = "")
      cat("      - mcc: Correlacion prediccion-realidad, robusto a desbalance (-1 a 1)\n")
      cat("      - f2_meas: Como F1 pero prioriza detectar positivos (F2)",
          if (is_multi) " [promedio macro]" else "", "\n", sep = "")
      if (is_multi) {
        cat("\n    Nota: Las metricas usan promedio macro (promedio entre clases,\n")
        cat("    sin ponderar por frecuencia). ROC-AUC usa el metodo Hand-Till\n")
        cat("    para extension multiclase.\n")
      }
    } else {
      cat("\n    Leyenda rapida:\n")
      cat("      - rmse: Error promedio (penaliza errores grandes)\n")
      cat("      - rsq: Porcentaje de variacion explicada (0-1)\n")
      cat("      - mae: Error promedio absoluto\n")
    }
  }

  metrics
}


#' @title Compute the ROC Curve
#'
#' @description
#' Computes the ROC curve and the area under the curve from classification
#' predictions (one-vs-all curves and Hand-Till AUC for multiclass
#' outcomes).
#'
#' @param predictions A data frame of predictions with the observed
#'   outcome column and, for classification, the `.pred_class` column and
#'   one `.pred_<level>` probability column per class (as returned by
#'   [eval_predictions()]).
#' @param target Character string with the name of the outcome variable.
#' @param verbose Logical. Print the results and their interpretation in
#'   the console (default: TRUE).
#'
#' @return A list with `curve` (tibble from [yardstick::roc_curve()] with
#'   `.threshold`, `specificity`, `sensitivity` and, for multiclass, `.level`),
#'   `auc` (numeric area under the curve) and `event_info` (list describing
#'   the positive class and event level).
#'
#' @examples
#' set.seed(1)
#' y <- factor(sample(c("no", "yes"), 100, replace = TRUE))
#' p <- stats::plogis(ifelse(y == "yes", 1, -1) + rnorm(100))
#' preds <- data.frame(y = y,
#'                     .pred_class = factor(ifelse(p >= 0.5, "yes", "no"),
#'                                          levels = levels(y)),
#'                     .pred_no = 1 - p, .pred_yes = p)
#' roc <- eval_roc(preds, target = "y")
#' roc$auc
#'
#' @family supervised learning
#' @export
eval_roc <- function(predictions, target, verbose = TRUE) {

  # Detectar clase positiva y event_level
  event_info <- .detect_event_level(predictions[[target]])

  if (event_info$type == "binary") {
    # --- BINARY ---
    roc_data <- yardstick::roc_curve(
      predictions,
      truth = !!rlang::sym(target),
      !!rlang::sym(event_info$prob_col),
      event_level = event_info$event_level
    )

    auc <- yardstick::roc_auc(
      predictions,
      truth = !!rlang::sym(target),
      !!rlang::sym(event_info$prob_col),
      event_level = event_info$event_level
    )$.estimate
  } else {
    # --- MULTICLASS ---
    prob_cols_syms <- rlang::syms(event_info$prob_cols)

    roc_data <- yardstick::roc_curve(
      predictions,
      truth = !!rlang::sym(target),
      !!!prob_cols_syms
    )

    auc <- yardstick::roc_auc(
      predictions,
      truth = !!rlang::sym(target),
      !!!prob_cols_syms
    )$.estimate
  }

  if (verbose) {
    cat("    La curva ROC muestra el balance entre detectar positivos correctamente\n")
    cat("    (sensitivity) y evitar falsas alarmas (1 - specificity) a diferentes\n")
    cat("    umbrales de decision. Un modelo perfecto tendria la curva pegada a la\n")
    cat("    esquina superior izquierda.\n\n")

    cat("    AUC (Area Bajo la Curva):", round(auc, 4), "\n")
    interpretation <- .interpret_metric_value("roc_auc", auc)
    cat("    Interpretacion:", interpretation, "\n\n")

    if (event_info$type == "multiclass") {
      cat("    Nota: En clasificacion multiclase, el AUC se calcula usando el\n")
      cat("    metodo Hand-Till, y se genera una curva ROC por clase (one-vs-all).\n")
    } else {
      cat("    Nota: La curva ROC se incluye siempre como herramienta de diagnostico\n")
      cat("    porque permite visualizar el rendimiento del modelo en todos los\n")
      cat("    umbrales de decision posibles, no solo en el umbral por defecto (0.5).\n")
      cat("    Esto es util si necesitas ajustar el punto de corte segun tu caso de uso.\n")
    }
  }

  list(curve = roc_data, auc = auc, event_info = event_info)
}


#' @title Compute the Confusion Matrix
#'
#' @description
#' Cross-tabulates predicted and observed classes and, when `verbose = TRUE`,
#' explains the true/false positives and negatives.
#'
#' @param predictions A data frame of predictions with the observed
#'   outcome column and, for classification, the `.pred_class` column and
#'   one `.pred_<level>` probability column per class (as returned by
#'   [eval_predictions()]).
#' @param target Character string with the name of the outcome variable.
#' @param verbose Logical. Print the results and their interpretation in
#'   the console (default: TRUE).
#'
#' @return An object of class `conf_mat` (see [yardstick::conf_mat()]),
#'   whose `table` element holds the counts (predictions in rows, truth in
#'   columns).
#'
#' @examples
#' set.seed(1)
#' y <- factor(sample(c("no", "yes"), 100, replace = TRUE))
#' p <- stats::plogis(ifelse(y == "yes", 1, -1) + rnorm(100))
#' preds <- data.frame(y = y,
#'                     .pred_class = factor(ifelse(p >= 0.5, "yes", "no"),
#'                                          levels = levels(y)),
#'                     .pred_no = 1 - p, .pred_yes = p)
#' eval_confusion(preds, target = "y")
#'
#' @family supervised learning
#' @export
eval_confusion <- function(predictions, target, verbose = TRUE) {

  # Detectar clase positiva
  event_info <- .detect_event_level(predictions[[target]])

  conf_mat <- yardstick::conf_mat(
    predictions,
    truth = !!rlang::sym(target),
    estimate = .pred_class
  )

  if (verbose) {
    cat("    La matriz de confusion muestra como se distribuyen las predicciones\n")
    cat("    del modelo comparadas con los valores reales (Truth).\n\n")

    if (event_info$type == "binary") {
      cat("    Clase positiva:", event_info$positive_class, "\n")
      cat("    Clase negativa:", event_info$negative_class, "\n\n")

      print(conf_mat)

      # Extraer valores de la matriz
      mat <- conf_mat$table
      tn <- mat[1, 1]  # Verdaderos negativos
      fp <- mat[2, 1]  # Falsos positivos
      fn <- mat[1, 2]  # Falsos negativos
      tp <- mat[2, 2]  # Verdaderos positivos

      cat("\n    Como leer la matriz:\n")
      cat("      - Verdaderos Negativos (", tn, "): Predijo '", event_info$negative_class,
          "' y era '", event_info$negative_class, "' (acierto)\n", sep = "")
      cat("      - Verdaderos Positivos (", tp, "): Predijo '", event_info$positive_class,
          "' y era '", event_info$positive_class, "' (acierto)\n", sep = "")
      cat("      - Falsos Positivos (", fp, "): Predijo '", event_info$positive_class,
          "' pero era '", event_info$negative_class, "' (error)\n", sep = "")
      cat("      - Falsos Negativos (", fn, "): Predijo '", event_info$negative_class,
          "' pero era '", event_info$positive_class, "' (error)\n", sep = "")
    } else {
      # --- MULTICLASS ---
      cat("    Clases:", paste(event_info$levels, collapse = ", "), "\n\n")

      print(conf_mat)

      mat <- conf_mat$table
      total <- sum(mat)
      correct <- sum(diag(mat))
      cat("\n    Como leer la matriz:\n")
      cat("      - La diagonal muestra los aciertos por clase\n")
      cat("      - Total aciertos:", correct, "de", total,
          "(", round(correct / total * 100, 1), "%)\n")
      for (k in seq_along(event_info$levels)) {
        cls <- event_info$levels[k]
        cls_correct <- mat[k, k]
        cls_total <- sum(mat[, k])
        if (cls_total > 0) {
          cat("      -", cls, ":", cls_correct, "/", cls_total,
              "(", round(cls_correct / cls_total * 100, 1), "% detectados)\n")
        }
      }
    }
  }

  conf_mat
}


#' @title Calibration Analysis by Probability Bins
#'
#' @description
#' Compares the mean predicted probability of the positive class with the
#' observed proportion of positives within ten probability bins of width
#' 0.1. Binary classification only.
#'
#' @param predictions A data frame of predictions with the observed
#'   outcome column and, for classification, the `.pred_class` column and
#'   one `.pred_<level>` probability column per class (as returned by
#'   [eval_predictions()]).
#' @param target Character string with the name of the outcome variable.
#' @param verbose Logical. Print the results and their interpretation in
#'   the console (default: TRUE).
#'
#' @return A tibble with columns `prob_bin` (probability bin), `n` (number
#'   of cases), `mean_pred` (mean predicted probability) and `mean_obs`
#'   (observed proportion of positives); NULL for multiclass outcomes.
#'
#' @examples
#' set.seed(1)
#' y <- factor(sample(c("no", "yes"), 100, replace = TRUE))
#' p <- stats::plogis(ifelse(y == "yes", 1, -1) + rnorm(100))
#' preds <- data.frame(y = y,
#'                     .pred_class = factor(ifelse(p >= 0.5, "yes", "no"),
#'                                          levels = levels(y)),
#'                     .pred_no = 1 - p, .pred_yes = p)
#' eval_calibration(preds, target = "y", verbose = FALSE)
#'
#' @family supervised learning
#' @export
eval_calibration <- function(predictions, target, verbose = TRUE) {

  # Detectar clase positiva
  event_info <- .detect_event_level(predictions[[target]])

  # Guard: calibracion solo para clasificacion binaria
  if (event_info$type == "multiclass") {
    if (verbose) {
      cat("    Analisis de calibracion disponible solo para clasificacion binaria.\n")
      cat("    (", event_info$n_classes, " clases detectadas)\n", sep = "")
    }
    return(NULL)
  }

  # Crear bins de probabilidad
  predictions$prob_bin <- cut(
    predictions[[event_info$prob_col]],
    breaks = seq(0, 1, by = 0.1),
    include.lowest = TRUE
  )

  # Calcular proporcion observada vs predicha
  calibration <- predictions |>
    dplyr::group_by(prob_bin) |>
    dplyr::summarise(
      n = dplyr::n(),
      mean_pred = mean(!!rlang::sym(event_info$prob_col), na.rm = TRUE),
      mean_obs = mean(as.numeric(!!rlang::sym(target) == event_info$positive_class), na.rm = TRUE),
      .groups = "drop"
    )

  if (verbose) {
    cat("    La calibracion mide si las probabilidades del modelo son confiables.\n")
    cat("    Un modelo bien calibrado deberia cumplir: cuando predice 70% de\n")
    cat("    probabilidad, aproximadamente 70% de esos casos deberian ser positivos.\n\n")

    cat("    Calibracion por deciles de probabilidad:\n\n")
    print(as.data.frame(calibration))

    cat("\n    Como leer la tabla:\n")
    cat("      - prob_bin: Rango de probabilidad predicha\n")
    cat("      - n: Cantidad de casos en ese rango\n")
    cat("      - mean_pred: Probabilidad promedio predicha por el modelo\n")
    cat("      - mean_obs: Proporcion real de positivos observados\n\n")

    cat("    Interpretacion: Si mean_pred y mean_obs son similares, el modelo\n")
    cat("    esta bien calibrado. Si mean_obs es mayor, el modelo subestima.\n")
    cat("    Si mean_obs es menor, el modelo sobreestima.\n")
  }

  calibration
}


#' @title Compare Predicted and Observed Values
#'
#' @description
#' Computes the correlation between observed and predicted values and the
#' mean bias of the predictions (regression).
#'
#' @param predictions A data frame of regression predictions with the
#'   observed outcome column and the `.pred` column (as returned by
#'   [eval_predictions()]).
#' @param target Character string with the name of the outcome variable.
#' @param verbose Logical. Print the results and their interpretation in
#'   the console (default: TRUE).
#'
#' @return A list with `observed` and `predicted` (numeric vectors),
#'   `correlation` (Pearson correlation) and `bias` (mean of predicted minus
#'   observed).
#'
#' @examples
#' set.seed(1)
#' y <- rnorm(100, mean = 10)
#' preds <- data.frame(y = y, .pred = y + rnorm(100, sd = 0.5))
#' pvo <- eval_pred_vs_obs(preds, target = "y")
#' pvo$correlation
#'
#' @family supervised learning
#' @export
eval_pred_vs_obs <- function(predictions, target, verbose = TRUE) {

  obs <- predictions[[target]]
  pred <- predictions$.pred

  # Calcular estadisticas
  correlation <- cor(obs, pred, use = "complete.obs")
  bias <- mean(pred - obs, na.rm = TRUE)

  if (verbose) {
    cat("    Correlacion obs-pred:", round(correlation, 4), "\n")
    cat("    Sesgo medio:", round(bias, 4), "\n")
  }

  list(
    observed = obs,
    predicted = pred,
    correlation = correlation,
    bias = bias
  )
}


#' @title Residual Analysis
#'
#' @description
#' Computes the residuals (observed minus predicted), their descriptive
#' statistics and a Shapiro-Wilk normality test (on a random subsample of
#' 5000 residuals when there are more).
#'
#' @param predictions A data frame of regression predictions with the
#'   observed outcome column and the `.pred` column (as returned by
#'   [eval_predictions()]).
#' @param target Character string with the name of the outcome variable.
#' @param verbose Logical. Print the results and their interpretation in
#'   the console (default: TRUE).
#'
#' @return A list with `residuals` (numeric vector), `stats` (list with
#'   `mean`, `sd`, `min`, `max` and `median` of the residuals) and
#'   `normality_test` (an `htest` object from [stats::shapiro.test()]).
#'
#' @examples
#' set.seed(1)
#' y <- rnorm(100, mean = 10)
#' preds <- data.frame(y = y, .pred = y + rnorm(100, sd = 0.5))
#' rs <- eval_residuals(preds, target = "y")
#' rs$stats
#'
#' @family supervised learning
#' @export
eval_residuals <- function(predictions, target, verbose = TRUE) {

  obs <- predictions[[target]]
  pred <- predictions$.pred
  residuals <- obs - pred

  # Estadisticas de residuos
  stats_resid <- list(
    mean = mean(residuals, na.rm = TRUE),
    sd = sd(residuals, na.rm = TRUE),
    min = min(residuals, na.rm = TRUE),
    max = max(residuals, na.rm = TRUE),
    median = median(residuals, na.rm = TRUE)
  )

  # Test de normalidad
  if (length(residuals) <= 5000) {
    shapiro <- shapiro.test(residuals)
  } else {
    shapiro <- shapiro.test(sample(residuals, 5000))
  }

  if (verbose) {
    cat("    Estadisticas de residuos:\n")
    cat("      - Media:", round(stats_resid$mean, 4), "\n")
    cat("      - DE:", round(stats_resid$sd, 4), "\n")
    cat("      - Rango: [", round(stats_resid$min, 2), ",",
        round(stats_resid$max, 2), "]\n")
    cat("    Shapiro-Wilk p-value:", format.pval(shapiro$p.value), "\n")
  }

  list(
    residuals = residuals,
    stats = stats_resid,
    normality_test = shapiro
  )
}


#' Three-Tier Overfitting Diagnosis
#'
#' Tier 1: CV vs test (all models), main diagnosis. Tier 2: OOB vs test
#' (random forest only). Tier 3: train vs test (non-RF models only).
#'
#' @noRd
.diagnose_overfitting <- function(final_fit, train_data, test_metrics, target, task,
                                  modeling_result = NULL, best_model = NULL,
                                  verbose = TRUE) {

  is_rf <- !is.null(best_model) && tolower(best_model) == "rf"

  # Metrica principal segun tarea
  main_metric <- if (task == "classification") "roc_auc" else "rsq"
  test_main <- test_metrics[test_metrics$.metric == main_metric, ".estimate"]
  if (length(test_main) == 0) test_main <- NA_real_ else test_main <- test_main[1]

  # ---- Tier 1: CV vs Test ----
  cv_auc <- NA_real_
  if (!is.null(modeling_result) && !is.null(modeling_result$comparison)) {
    cv_summary <- modeling_result$comparison$summary
    if (!is.null(cv_summary) && !is.null(best_model) && main_metric %in% names(cv_summary)) {
      best_row <- cv_summary[cv_summary$model == best_model, ]
      if (nrow(best_row) > 0) {
        cv_auc <- best_row[[main_metric]][1]
      }
    }
  }
  cv_gap <- cv_auc - test_main

  # ---- Tier 2: OOB vs Test (solo RF) ----
  oob_auc <- NA_real_
  if (is_rf && task == "classification") {
    tryCatch({
      # Extraer el motor ranger del workflow fitted
      fit_engine <- final_fit$fit$fit$fit
      if (inherits(fit_engine, "ranger") && !is.null(fit_engine$predictions)) {
        # fit_engine$predictions es una matriz n x n_classes con probabilidades OOB
        oob_probs <- fit_engine$predictions
        truth_vec <- train_data[[target]]
        event_info <- .detect_event_level(truth_vec)

        if (event_info$type == "binary") {
          # Obtener columna de la clase positiva
          pos_class <- event_info$positive_class
          if (pos_class %in% colnames(oob_probs)) {
            oob_prob_pos <- oob_probs[, pos_class]
          } else {
            # Intentar con el indice segun event_level
            col_idx <- if (event_info$event_level == "first") 1 else 2
            oob_prob_pos <- oob_probs[, col_idx]
          }

          # Calcular AUC OOB con yardstick
          oob_df <- data.frame(
            truth = truth_vec,
            .pred_pos = oob_prob_pos
          )
          names(oob_df)[2] <- paste0(".pred_", pos_class)

          oob_auc <- yardstick::roc_auc(
            oob_df,
            truth = truth_vec,
            !!rlang::sym(paste0(".pred_", pos_class)),
            event_level = event_info$event_level
          )$.estimate
        } else {
          # Multiclass OOB: usar macro-average Hand-Till
          prob_cols <- paste0(".pred_", event_info$levels)
          oob_df <- data.frame(truth = truth_vec)
          for (k in seq_along(event_info$levels)) {
            col_name <- prob_cols[k]
            lev <- event_info$levels[k]
            if (lev %in% colnames(oob_probs)) {
              oob_df[[col_name]] <- oob_probs[, lev]
            } else {
              oob_df[[col_name]] <- oob_probs[, k]
            }
          }
          prob_cols_syms <- rlang::syms(prob_cols)
          oob_auc <- yardstick::roc_auc(
            oob_df,
            truth = truth_vec,
            !!!prob_cols_syms
          )$.estimate
        }
      }
    }, error = function(e) {
      # Si falla la extraccion OOB, simplemente no mostramos Tier 2
      NULL  # oob_auc keeps its initial NA value
    })
  }
  oob_gap <- oob_auc - test_main

  # ---- Tier 3: Train vs Test (solo no-RF) ----
  train_metrics <- NULL
  train_comparison <- NULL
  if (!is_rf) {
    train_preds <- eval_predictions(final_fit, train_data, target, task, verbose = FALSE)
    train_metrics <- eval_metrics(train_preds, target, task, verbose = FALSE)

    lower_is_better <- c("rmse", "mae")
    train_comparison <- merge(
      train_metrics[, c(".metric", ".estimate")],
      test_metrics[, c(".metric", ".estimate")],
      by = ".metric",
      suffixes = c("_train", "_test")
    )
    names(train_comparison) <- c("metric", "train", "test")
    train_comparison$gap <- ifelse(
      train_comparison$metric %in% lower_is_better,
      train_comparison$test - train_comparison$train,
      train_comparison$train - train_comparison$test
    )
  }

  # ---- Verbose output ----
  if (verbose) {
    cat("    Se comparan distintas estimaciones del rendimiento\n")
    cat("    para detectar si el modelo memorizo los datos.\n\n")

    # -- Tier 1: CV vs Test --
    if (!is.na(cv_auc) && !is.na(test_main)) {
      cat("    Comparacion CV vs Test (todos los modelos):\n")
      cat("      metric       cv      test      gap\n")
      cat(sprintf("      %-10s %6.3f    %6.3f   %+6.3f\n",
                  main_metric, cv_auc, test_main, cv_gap))
      cat("\n")

      abs_cv_gap <- abs(cv_gap)
      if (abs_cv_gap < 0.03) {
        cat("    [ok] Excelente generalizacion (gap CV-test:", sprintf("%+.3f", cv_gap), ")\n")
      } else if (abs_cv_gap < 0.05) {
        cat("    [ok] Buena generalizacion (gap CV-test:", sprintf("%+.3f", cv_gap), ")\n")
      } else if (abs_cv_gap < 0.10) {
        cat("    [~] Diferencia moderada entre CV y test (gap:", sprintf("%+.3f", cv_gap), ")\n")
      } else {
        cat("    [!] Diferencia sustancial -- posible data leakage o shift (gap:", sprintf("%+.3f", cv_gap), ")\n")
      }
      cat("\n")
    }

    # -- Tier 2: Nota para RF --
    if (is_rf) {
      cat("    Nota para Random Forest:\n")
      cat("    El AUC en train (~1.0) es artificialmente alto\n")
      cat("    debido al error de resubstitucion. Esto es normal\n")
      cat("    y NO indica sobreajuste. El diagnostico correcto\n")
      cat("    es CV vs Test (mostrado arriba).\n")
      cat("\n    Referencia: Breiman, L. (2001). Random forests.\n")
      cat("    Machine Learning, 45(1), 5-32.\n")
    }

    # -- Tier 3: Train vs Test (solo no-RF) --
    if (!is_rf && !is.null(train_comparison)) {
      cat("    Comparacion Train vs Test:\n\n")
      comp_display <- train_comparison
      comp_display$train <- round(comp_display$train, 4)
      comp_display$test <- round(comp_display$test, 4)
      comp_display$gap <- round(comp_display$gap, 4)
      print(as.data.frame(comp_display), row.names = FALSE)

      train_main_row <- train_comparison[train_comparison$metric == main_metric, ]
      if (nrow(train_main_row) > 0) {
        train_gap <- train_main_row$gap[1]
        cat("\n")
        if (abs(train_gap) < 0.05) {
          cat("    [ok] Sin indicios de overfitting (gap:", round(train_gap, 4), ")\n")
        } else if (abs(train_gap) < 0.10) {
          cat("    [~] Overfitting leve (gap:", round(train_gap, 4), ")\n")
          cat("    Considere simplificar el modelo o aumentar los datos\n")
        } else {
          cat("    [!] Overfitting sustancial (gap:", round(train_gap, 4), ")\n")
          .print_overfitting_suggestions()
        }
      }
    }

    cat("\n")
  }

  list(
    train_metrics = train_metrics,
    comparison = train_comparison,
    cv_auc = cv_auc,
    test_auc = test_main,
    cv_gap = cv_gap,
    oob_auc = if (is_rf) oob_auc else NULL,
    oob_gap = if (is_rf) oob_gap else NULL,
    is_rf = is_rf
  )
}


#' Print Suggestions Against Overfitting
#' @noRd
.print_overfitting_suggestions <- function() {
  cat("\n    Sugerencias:\n")
  cat("    - Reducir la complejidad del modelo (menos arboles, mayor regularizacion)\n")
  cat("    - Aumentar los datos de entrenamiento\n")
  cat("    - Activar feature_selection = TRUE para eliminar variables irrelevantes\n")
  cat("    - Verificar si alguna variable contiene informacion del target (data leakage)\n")
}
