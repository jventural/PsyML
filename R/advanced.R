# =============================================================================
# Advanced functions for PsyML
# =============================================================================
# Includes: threshold optimization, probability calibration, nested CV,
#           advanced residual analysis, data leakage detection
# =============================================================================


# =============================================================================
# 1. OPTIMIZACION DE THRESHOLD (Clasificacion)
# =============================================================================

# Predicciones fuera de pliegue del workflow final (ya afinado), reajustado con
# los pliegues de la validacion cruzada del entrenamiento. Sirven para ELEGIR
# el umbral sin tocar el conjunto de prueba. NULL si algo falla.
.oof_predictions <- function(final_workflow, cv_folds, target, seed = NULL) {
  if (is.null(final_workflow) || is.null(cv_folds)) return(NULL)
  tryCatch({
    if (!is.null(seed)) set.seed(seed)
    res <- suppressMessages(tune::fit_resamples(
      final_workflow, resamples = cv_folds,
      control = tune::control_resamples(save_pred = TRUE)))
    pred <- tune::collect_predictions(res)
    if (!target %in% names(pred)) return(NULL)
    as.data.frame(pred)
  }, error = function(e) NULL)
}

# Sensibilidad, especificidad y precision de unas predicciones con un umbral dado.
.threshold_metrics_on <- function(predictions, target, threshold) {
  tryCatch({
    ev <- .detect_event_level(predictions[[target]])
    probs <- predictions[[ev$prob_col]]
    truth <- predictions[[target]] == ev$positive_class
    pred <- probs >= threshold
    tp <- sum(pred & truth); fn <- sum(!pred & truth)
    tn <- sum(!pred & !truth); fp <- sum(pred & !truth)
    data.frame(threshold = threshold,
               sensitivity = tp / (tp + fn), specificity = tn / (tn + fp),
               precision = if (tp + fp > 0) tp / (tp + fp) else NA_real_,
               balanced_accuracy = (tp / (tp + fn) + tn / (tn + fp)) / 2,
               tp = tp, fp = fp, tn = tn, fn = fn)
  }, error = function(e) NULL)
}

#' @title Optimize the Classification Threshold
#'
#' @description
#' Finds the optimal probability threshold for binary classification by
#' evaluating thresholds from 0.01 to 0.99 (step 0.01) under one of several
#' criteria: Youden's J, F1, a user-defined misclassification cost, or
#' balanced accuracy. The positive class is detected from the levels of the
#' target (recognized labels such as "yes", "1" or "positive"; otherwise
#' the second level).
#'
#' @param predictions Data frame of predictions with the observed target
#'   (a factor with two levels) and a probability column named
#'   `.pred_<positive class>`, such as the `predictions` element of a
#'   [supervised_ml()] result.
#' @param target Name of the target variable.
#' @param method Optimization criterion: "youden" (default; maximizes
#'   sensitivity + specificity - 1), "f1" (maximizes F1), "cost" (minimizes
#'   `cost_fp * FP + cost_fn * FN`) or "balanced" (maximizes balanced
#'   accuracy). Unknown values fall back to "youden".
#' @param cost_fp Cost of a false positive (used when `method = "cost"`;
#'   default: 1).
#' @param cost_fn Cost of a false negative (used when `method = "cost"`;
#'   default: 1).
#' @param verbose Logical. Print results (default: TRUE).
#'
#' @return A list with `optimal_threshold` (numeric), `optimal_metrics`
#'   (one-row data frame with the metrics at the optimal threshold),
#'   `all_results` (data frame with columns `threshold`, `sensitivity`,
#'   `specificity`, `precision`, `f1`, `youden_j`, `balanced_acc`,
#'   `total_cost`, `tp`, `tn`, `fp` and `fn` for every threshold) and
#'   `method`.
#' @examples
#' set.seed(1)
#' n <- 200
#' x <- rnorm(n)
#' y <- factor(ifelse(x + rnorm(n) > 0.8, "yes", "no"), levels = c("no", "yes"))
#' preds <- data.frame(y = y, .pred_yes = plogis(1.5 * x - 1))
#' thr <- optimize_threshold(preds, target = "y", method = "f1", verbose = FALSE)
#' thr$optimal_threshold
#' @family supervised learning
#' @export
optimize_threshold <- function(predictions,
                               target,
                               method = "youden",
                               cost_fp = 1,
                               cost_fn = 1,
                               verbose = TRUE) {

  # Detectar clase positiva y event_level
  event_info <- .detect_event_level(predictions[[target]])
  if (!identical(event_info$type, "binary")) {
    stop("Threshold optimization requires a binary target")
  }

  if (!event_info$prob_col %in% names(predictions)) {
    stop("No probabilities were found in the predictions")
  }

  probs <- predictions[[event_info$prob_col]]
  truth <- as.numeric(predictions[[target]] == event_info$positive_class)

  method_descriptions <- c(
    youden = "Maximiza Youden's J (sensitivity + specificity - 1)",
    f1 = "Maximiza F1-Score (balance precision-recall)",
    balanced = "Maximiza Balanced Accuracy",
    cost = "Minimiza costo total de errores"
  )

  # Evaluar diferentes thresholds
  thresholds <- seq(0.01, 0.99, by = 0.01)

  results <- lapply(thresholds, function(thresh) {
    pred_class <- ifelse(probs >= thresh, 1, 0)

    tp <- sum(pred_class == 1 & truth == 1)
    tn <- sum(pred_class == 0 & truth == 0)
    fp <- sum(pred_class == 1 & truth == 0)
    fn <- sum(pred_class == 0 & truth == 1)

    sensitivity <- ifelse((tp + fn) > 0, tp / (tp + fn), 0)
    specificity <- ifelse((tn + fp) > 0, tn / (tn + fp), 0)
    precision <- ifelse((tp + fp) > 0, tp / (tp + fp), 0)
    recall <- sensitivity
    f1 <- ifelse((precision + recall) > 0,
                 2 * precision * recall / (precision + recall), 0)

    # Youden's J statistic
    youden_j <- sensitivity + specificity - 1

    # Costo total
    total_cost <- cost_fp * fp + cost_fn * fn

    # Balanced accuracy
    balanced_acc <- (sensitivity + specificity) / 2

    data.frame(
      threshold = thresh,
      sensitivity = sensitivity,
      specificity = specificity,
      precision = precision,
      f1 = f1,
      youden_j = youden_j,
      balanced_acc = balanced_acc,
      total_cost = total_cost,
      tp = tp, tn = tn, fp = fp, fn = fn
    )
  })

  results_df <- do.call(rbind, results)

  # Seleccionar threshold optimo segun metodo
  optimal_idx <- switch(method,
    "youden" = which.max(results_df$youden_j),
    "f1" = which.max(results_df$f1),
    "cost" = which.min(results_df$total_cost),
    "balanced" = which.max(results_df$balanced_acc),
    which.max(results_df$youden_j)
  )

  optimal_threshold <- results_df$threshold[optimal_idx]
  optimal_metrics <- results_df[optimal_idx, ]

  if (verbose) {
    cat("    Metodo:", method, "-", method_descriptions[method], "\n\n")

    cat("    Resultado:\n")
    cat("      - Threshold por defecto: 0.50\n")
    cat("      - Threshold optimo encontrado:", round(optimal_threshold, 2), "\n\n")

    cat("    Metricas con threshold optimo (", round(optimal_threshold, 2), "):\n", sep = "")
    cat("      - Sensibilidad:", round(optimal_metrics$sensitivity * 100, 1), "% de positivos detectados\n")
    cat("      - Especificidad:", round(optimal_metrics$specificity * 100, 1), "% de negativos correctos\n")
    cat("      - Precision:", round(optimal_metrics$precision * 100, 1), "% de predicciones positivas son correctas\n")
    cat("      - F1-Score:", round(optimal_metrics$f1, 4), "\n")

    # Comparar con threshold 0.5
    default_idx <- which(results_df$threshold == 0.5)
    if (length(default_idx) > 0) {
      default_metrics <- results_df[default_idx, ]
      cat("\n    Comparacion:\n")
      cat("      - F1 con threshold 0.5:", round(default_metrics$f1, 4), "\n")
      cat("      - F1 con threshold optimo:", round(optimal_metrics$f1, 4), "\n")
      improvement <- (optimal_metrics$f1 - default_metrics$f1) / default_metrics$f1 * 100
      if (abs(improvement) > 0.1) {
        if (improvement > 0) {
          cat("      - Mejora:", round(improvement, 1), "%\n")
        } else {
          cat("      - Nota: El threshold por defecto ya era optimo para F1\n")
        }
      } else {
        cat("      - Diferencia minima entre ambos thresholds\n")
      }
    }

    cat("\n    Uso: Puedes aplicar este threshold con apply_threshold(predictions, target,",
        round(optimal_threshold, 2), ")\n")
  }

  list(
    optimal_threshold = optimal_threshold,
    optimal_metrics = optimal_metrics,
    all_results = results_df,
    method = method
  )
}


#' @title Apply a Classification Threshold
#' @description Reclassifies binary predictions with a given probability
#'   threshold (for instance, one found with [optimize_threshold()]).
#' @param predictions Data frame of predictions with the observed target
#'   (a factor with two levels) and a probability column named
#'   `.pred_<positive class>`.
#' @param target Name of the target variable.
#' @param threshold Probability threshold between 0 and 1; observations
#'   with probability >= `threshold` are assigned to the positive class.
#' @return The `predictions` data frame with an added factor column
#'   `.pred_class_optimized`.
#' @examples
#' set.seed(1)
#' x <- rnorm(100)
#' y <- factor(ifelse(x + rnorm(100) > 0.8, "yes", "no"), levels = c("no", "yes"))
#' preds <- data.frame(y = y, .pred_yes = plogis(1.5 * x - 1))
#' out <- apply_threshold(preds, target = "y", threshold = 0.3)
#' table(out$.pred_class_optimized)
#' @family supervised learning
#' @export
apply_threshold <- function(predictions, target, threshold) {

  # Detectar clase positiva
  event_info <- .detect_event_level(predictions[[target]])

  probs <- predictions[[event_info$prob_col]]
  new_class <- ifelse(probs >= threshold,
                      event_info$positive_class,
                      event_info$negative_class)
  predictions$.pred_class_optimized <- factor(new_class, levels = event_info$levels)

  predictions
}


#' @title Plot the Threshold Optimization
#' @description Plots sensitivity, specificity, F1 and precision as a
#'   function of the threshold, marking the optimal threshold and 0.5.
#' @param threshold_result A list returned by [optimize_threshold()].
#' @return A `ggplot` object.
#' @examples
#' set.seed(1)
#' x <- rnorm(200)
#' y <- factor(ifelse(x + rnorm(200) > 0.8, "yes", "no"), levels = c("no", "yes"))
#' preds <- data.frame(y = y, .pred_yes = plogis(1.5 * x - 1))
#' thr <- optimize_threshold(preds, target = "y", verbose = FALSE)
#' plot_threshold_optimization(thr)
#' @family supervised learning
#' @export
plot_threshold_optimization <- function(threshold_result) {

  results <- threshold_result$all_results
  optimal <- threshold_result$optimal_threshold

  # Preparar datos para grafico
  plot_data <- tidyr::pivot_longer(
    results[, c("threshold", "sensitivity", "specificity", "f1", "precision")],
    cols = -threshold,
    names_to = "metric",
    values_to = "value"
  )

  ggplot2::ggplot(plot_data, ggplot2::aes(x = threshold, y = value, color = metric)) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::geom_vline(xintercept = optimal, linetype = "dashed", color = "red") +
    ggplot2::geom_vline(xintercept = 0.5, linetype = "dotted", color = "gray50") +
    ggplot2::annotate("text", x = optimal + 0.05, y = 0.95,
                      label = paste("Optimo:", round(optimal, 2)),
                      color = "red", hjust = 0) +
    ggplot2::labs(
      title = "Optimizacion de Threshold",
      subtitle = paste("Metodo:", threshold_result$method),
      x = "Threshold",
      y = "Valor",
      color = "Metrica"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::scale_color_brewer(palette = "Set1") +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
      legend.position = "bottom"
    )
}


# =============================================================================
# 2. CALIBRACION DE PROBABILIDADES
# =============================================================================

#' @title Calibrate Predicted Probabilities
#'
#' @description
#' Calibrates the predicted probabilities of a binary classifier using
#' Platt scaling (logistic regression of the outcome on the probabilities)
#' or isotonic regression, and reports the Brier score and the expected
#' calibration error (ECE, 10 bins) before and after calibration. The
#' calibration model is fitted and evaluated on the same predictions, so
#' the reported improvement is optimistic.
#'
#' @param predictions Data frame of predictions with the observed target
#'   (a factor with two levels) and a probability column named
#'   `.pred_<positive class>`.
#' @param target Name of the target variable.
#' @param method Calibration method: "platt" (default, sigmoid) or
#'   "isotonic".
#' @param verbose Logical. Print results (default: TRUE).
#'
#' @return A list with `calibration_model` (a `glm` or `isoreg` object),
#'   `predictions` (the input with an added `.pred_calibrated` column),
#'   `target`, `method`, `brier_original`, `brier_calibrated`,
#'   `ece_original` and `ece_calibrated`.
#' @examples
#' set.seed(1)
#' x <- rnorm(300)
#' y <- factor(ifelse(x + rnorm(300) > 0, "yes", "no"), levels = c("no", "yes"))
#' preds <- data.frame(y = y, .pred_yes = plogis(3 * x))
#' cal <- calibrate_probabilities(preds, target = "y", verbose = FALSE)
#' c(cal$brier_original, cal$brier_calibrated)
#' @family supervised learning
#' @export
calibrate_probabilities <- function(predictions,
                                    target,
                                    method = "platt",
                                    verbose = TRUE) {

  if (!method %in% c("platt", "isotonic")) {
    stop("'method' must be \"platt\" or \"isotonic\"")
  }

  # Detectar clase positiva
  event_info <- .detect_event_level(predictions[[target]])
  if (!identical(event_info$type, "binary")) {
    stop("Probability calibration requires a binary target")
  }

  probs <- predictions[[event_info$prob_col]]
  truth <- as.numeric(predictions[[target]] == event_info$positive_class)

  if (verbose) {
    cat("    Configuracion de clases:\n")
    cat("      - Clase positiva:", event_info$positive_class, "\n")
    cat("      - event_level:", event_info$event_level, "\n\n")
  }

  if (method == "platt") {
    # Platt Scaling (Regresion logistica)
    cal_data <- data.frame(prob = probs, truth = truth)
    cal_model <- glm(truth ~ prob, data = cal_data, family = binomial())

    calibrated_probs <- predict(cal_model, type = "response")

  } else if (method == "isotonic") {
    # Isotonic Regression
    cal_model <- isoreg(probs, truth)

    # Funcion para predecir con isotonic
    calibrated_probs <- approx(
      x = cal_model$x[cal_model$ord],
      y = cal_model$yf,
      xout = probs,
      rule = 2
    )$y
  }

  # Calcular metricas de calibracion
  # Brier Score
  brier_original <- mean((probs - truth)^2)
  brier_calibrated <- mean((calibrated_probs - truth)^2)

  # Expected Calibration Error (ECE)
  ece_original <- calculate_ece(probs, truth)
  ece_calibrated <- calculate_ece(calibrated_probs, truth)

  if (verbose) {
    cat("    Metodo de calibracion:", method, "\n\n")
    cat("    Brier Score:\n")
    cat("      - Original:", round(brier_original, 4), "\n")
    cat("      - Calibrado:", round(brier_calibrated, 4), "\n")

    improvement_brier <- (brier_original - brier_calibrated) / brier_original * 100
    if (improvement_brier > 0) {
      cat("      - Mejora:", round(improvement_brier, 1), "%\n")
    }

    cat("\n    Expected Calibration Error (ECE):\n")
    cat("      - Original:", round(ece_original, 4), "\n")
    cat("      - Calibrado:", round(ece_calibrated, 4), "\n")
  }

  # Agregar probabilidades calibradas al dataframe
  predictions$.pred_calibrated <- calibrated_probs

  list(
    calibration_model = cal_model,
    predictions = predictions,
    target = target,
    method = method,
    brier_original = brier_original,
    brier_calibrated = brier_calibrated,
    ece_original = ece_original,
    ece_calibrated = ece_calibrated
  )
}


#' @title Compute the Expected Calibration Error
#' @noRd
calculate_ece <- function(probs, truth, n_bins = 10) {
  bins <- cut(probs, breaks = seq(0, 1, length.out = n_bins + 1),
              include.lowest = TRUE)

  ece <- 0
  n_total <- length(probs)

  for (bin in levels(bins)) {
    idx <- which(bins == bin)
    if (length(idx) > 0) {
      avg_prob <- mean(probs[idx])
      avg_truth <- mean(truth[idx])
      weight <- length(idx) / n_total
      ece <- ece + weight * abs(avg_prob - avg_truth)
    }
  }

  ece
}


#' @title Plot the Calibration Curve
#' @description Reliability diagram comparing the mean predicted
#'   probability with the observed proportion of the positive class per
#'   probability bin, before (red) and after (blue) calibration.
#' @param calibration_result A list returned by [calibrate_probabilities()].
#' @param n_bins Number of probability bins (default: 10).
#' @return A `ggplot` object.
#' @examples
#' set.seed(1)
#' x <- rnorm(300)
#' y <- factor(ifelse(x + rnorm(300) > 0, "yes", "no"), levels = c("no", "yes"))
#' preds <- data.frame(y = y, .pred_yes = plogis(3 * x))
#' cal <- calibrate_probabilities(preds, target = "y", verbose = FALSE)
#' plot_calibration_curve(cal)
#' @family supervised learning
#' @export
plot_calibration_curve <- function(calibration_result, n_bins = 10) {

  predictions <- calibration_result$predictions

  # Usar target guardado por calibrate_probabilities
  target_col <- calibration_result$target
  if (is.null(target_col)) {
    # Fallback: buscar factor que no sea .pred_class
    factor_cols <- names(predictions)[sapply(predictions, is.factor)]
    factor_cols <- factor_cols[!factor_cols %in% c(".pred_class", ".pred_class_optimized")]
    target_col <- factor_cols[1]
  }

  # Detectar clase positiva
  event_info <- .detect_event_level(predictions[[target_col]])

  probs_orig <- predictions[[event_info$prob_col]]
  probs_cal <- predictions$.pred_calibrated
  truth <- as.numeric(predictions[[target_col]] == event_info$positive_class)

  # Crear bins
  bins <- cut(probs_orig, breaks = seq(0, 1, length.out = n_bins + 1),
              include.lowest = TRUE)

  # Calcular promedios por bin
  cal_data <- data.frame(
    bin = bins,
    prob_orig = probs_orig,
    prob_cal = probs_cal,
    truth = truth
  )

  cal_summary <- cal_data |>
    dplyr::group_by(bin) |>
    dplyr::summarise(
      mean_pred_orig = mean(prob_orig),
      mean_pred_cal = mean(prob_cal),
      mean_obs = mean(truth),
      n = dplyr::n(),
      .groups = "drop"
    )

  ggplot2::ggplot(cal_summary) +
    ggplot2::geom_abline(intercept = 0, slope = 1,
                         linetype = "dashed", color = "gray50") +
    ggplot2::geom_point(ggplot2::aes(x = mean_pred_orig, y = mean_obs),
                        color = "red", size = 3, alpha = 0.7) +
    ggplot2::geom_line(ggplot2::aes(x = mean_pred_orig, y = mean_obs),
                       color = "red", alpha = 0.5) +
    ggplot2::geom_point(ggplot2::aes(x = mean_pred_cal, y = mean_obs),
                        color = "blue", size = 3, alpha = 0.7) +
    ggplot2::geom_line(ggplot2::aes(x = mean_pred_cal, y = mean_obs),
                       color = "blue", alpha = 0.5) +
    ggplot2::labs(
      title = "Curva de Calibracion",
      subtitle = "Rojo: Original | Azul: Calibrado",
      x = "Probabilidad Predicha",
      y = "Proporcion Observada"
    ) +
    ggplot2::xlim(0, 1) + ggplot2::ylim(0, 1) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold")
    )
}


# =============================================================================
# 3. NESTED CROSS-VALIDATION
# =============================================================================

#' @title Nested Cross-Validation
#'
#' @description
#' Runs nested cross-validation to obtain a less biased estimate of the
#' performance of a model whose hyperparameters are tuned: an inner
#' cross-validation tunes the model by grid search and an outer
#' cross-validation evaluates the tuned model on data not used for tuning.
#'
#' @param data Training data (data frame).
#' @param target Name of the target variable.
#' @param task Type of task: "classification" or "regression".
#' @param recipe A `recipes` recipe for the preprocessing (e.g. from
#'   [prep_recipe()] or [recipes::recipe()]).
#' @param model_name Model to tune: "rf" (default, random forest with 500
#'   trees) or "xgboost".
#' @param outer_folds Number of outer folds (default: 5).
#' @param inner_folds Number of inner folds (default: 5).
#' @param grid_size Size of the tuning grid (default: 10).
#' @param seed Optional integer seed. If NULL (default) the random number
#'   generator state is not modified.
#' @param verbose Logical. Print progress (default: TRUE).
#'
#' @return A list with `results` (data frame with one row per outer fold:
#'   `fold` and `roc_auc` and `accuracy` for classification, or `rmse` and
#'   `rsq` for regression), `summary` (list with the mean and SD of those
#'   metrics), `task`, `model` and `config` (numbers of outer and inner
#'   folds).
#' @examples
#' \donttest{
#' set.seed(1)
#' n <- 100
#' df <- data.frame(x1 = rnorm(n), x2 = rnorm(n), x3 = rnorm(n))
#' df$y <- df$x1 + 0.5 * df$x2 + rnorm(n)
#' rec <- recipes::recipe(y ~ ., data = df)
#' ncv <- run_nested_cv(df, target = "y", task = "regression", recipe = rec,
#'                      outer_folds = 2, inner_folds = 2, grid_size = 2,
#'                      seed = 1, verbose = FALSE)
#' ncv$results
#' }
#' @family supervised learning
#' @export
run_nested_cv <- function(data,
                      target,
                      task,
                      recipe,
                      model_name = "rf",
                      outer_folds = 5,
                      inner_folds = 5,
                      grid_size = 10,
                      seed = NULL,
                      verbose = TRUE) {

  if (!is.null(seed)) set.seed(seed)

  if (!model_name %in% c("rf", "xgboost")) {
    stop("Nested CV is only available for 'rf' and 'xgboost'")
  }

  if (verbose) {
    cat("    Configuracion:\n")
    cat("      - Folds externos:", outer_folds, "\n")
    cat("      - Folds internos:", inner_folds, "\n")
    cat("      - Grid size:", grid_size, "\n")
    cat("      - Modelo:", model_name, "\n\n")
  }

  # Crear folds externos
  if (task == "classification") {
    outer_cv <- .strat_no_pool(rsample::vfold_cv, data, v = outer_folds,
                               strata = !!rlang::sym(target))
  } else {
    outer_cv <- rsample::vfold_cv(data, v = outer_folds)
  }

  # Definir metricas
  if (task == "classification") {
    metrics_set <- yardstick::metric_set(yardstick::roc_auc, yardstick::accuracy)
    select_metric <- "roc_auc"
  } else {
    metrics_set <- yardstick::metric_set(yardstick::rmse, yardstick::rsq)
    select_metric <- "rmse"
  }

  # Resultados por fold externo
  outer_results <- list()

  for (i in 1:outer_folds) {
    if (verbose) {
      cat("    Fold externo", i, "/", outer_folds, "...")
    }

    # Datos de este fold
    train_outer <- rsample::analysis(outer_cv$splits[[i]])
    test_outer <- rsample::assessment(outer_cv$splits[[i]])

    # Crear folds internos
    if (task == "classification") {
      inner_cv <- .strat_no_pool(rsample::vfold_cv, train_outer, v = inner_folds,
                                 strata = !!rlang::sym(target))
    } else {
      inner_cv <- rsample::vfold_cv(train_outer, v = inner_folds)
    }

    # Crear especificacion con tune
    if (model_name == "rf") {
      tune_spec <- parsnip::rand_forest(
        mtry = tune::tune(),
        min_n = tune::tune(),
        trees = 500
      ) |>
        parsnip::set_engine("ranger") |>
        parsnip::set_mode(task)
    } else {
      tune_spec <- parsnip::boost_tree(
        mtry = tune::tune(),
        min_n = tune::tune(),
        tree_depth = tune::tune(),
        learn_rate = tune::tune(),
        trees = 500
      ) |>
        parsnip::set_engine("xgboost") |>
        parsnip::set_mode(task)
    }

    # Workflow
    wf <- workflows::workflow() |>
      workflows::add_recipe(recipe) |>
      workflows::add_model(tune_spec)

    # Tuning en folds internos
    tune_results <- suppressMessages(
      tune::tune_grid(
        wf,
        resamples = inner_cv,
        grid = grid_size,
        metrics = metrics_set,
        control = tune::control_grid(verbose = FALSE)
      )
    )

    # Mejor configuracion
    best_params <- tune::select_best(tune_results, metric = select_metric)
    final_wf <- tune::finalize_workflow(wf, best_params)

    # Entrenar en train_outer y evaluar en test_outer
    final_fit <- parsnip::fit(final_wf, data = train_outer)

    # Predicciones en test externo
    if (task == "classification") {
      preds <- stats::predict(final_fit, new_data = test_outer)
      probs <- stats::predict(final_fit, new_data = test_outer, type = "prob")
      test_preds <- dplyr::bind_cols(test_outer, preds, probs)

      # Detectar clase positiva y event_level
      event_info <- .detect_event_level(test_outer[[target]])

      # Calcular metricas con event_level correcto
      if (event_info$type == "binary") {
        auc <- yardstick::roc_auc(test_preds,
                                   truth = !!rlang::sym(target),
                                   !!rlang::sym(event_info$prob_col),
                                   event_level = event_info$event_level)$.estimate
      } else {
        prob_cols_syms <- rlang::syms(event_info$prob_cols)
        auc <- yardstick::roc_auc(test_preds,
                                   truth = !!rlang::sym(target),
                                   !!!prob_cols_syms)$.estimate
      }
      acc <- yardstick::accuracy(test_preds,
                                  truth = !!rlang::sym(target),
                                  estimate = .pred_class)$.estimate

      outer_results[[i]] <- data.frame(
        fold = i,
        roc_auc = auc,
        accuracy = acc
      )

      if (verbose) cat(" AUC =", round(auc, 3), "\n")

    } else {
      preds <- stats::predict(final_fit, new_data = test_outer)
      test_preds <- dplyr::bind_cols(test_outer, preds)

      rmse_val <- yardstick::rmse(test_preds,
                                   truth = !!rlang::sym(target),
                                   estimate = .pred)$.estimate
      rsq_val <- yardstick::rsq(test_preds,
                                 truth = !!rlang::sym(target),
                                 estimate = .pred)$.estimate

      outer_results[[i]] <- data.frame(
        fold = i,
        rmse = rmse_val,
        rsq = rsq_val
      )

      if (verbose) cat(" RMSE =", round(rmse_val, 3), "\n")
    }
  }

  results_df <- do.call(rbind, outer_results)

  # Resumen
  if (verbose) {
    cat("\n    Resultados de Nested CV:\n")
    if (task == "classification") {
      cat("      - ROC-AUC: ", round(mean(results_df$roc_auc), 4),
          " (SD:", round(sd(results_df$roc_auc), 4), ")\n")
      cat("      - Accuracy:", round(mean(results_df$accuracy), 4),
          " (SD:", round(sd(results_df$accuracy), 4), ")\n")
    } else {
      cat("      - RMSE:", round(mean(results_df$rmse), 4),
          " (SD:", round(sd(results_df$rmse), 4), ")\n")
      cat("      - R-squared:", round(mean(results_df$rsq), 4),
          " (SD:", round(sd(results_df$rsq), 4), ")\n")
    }
  }

  list(
    results = results_df,
    summary = if (task == "classification") {
      list(
        roc_auc_mean = mean(results_df$roc_auc),
        roc_auc_sd = sd(results_df$roc_auc),
        accuracy_mean = mean(results_df$accuracy),
        accuracy_sd = sd(results_df$accuracy)
      )
    } else {
      list(
        rmse_mean = mean(results_df$rmse),
        rmse_sd = sd(results_df$rmse),
        rsq_mean = mean(results_df$rsq),
        rsq_sd = sd(results_df$rsq)
      )
    },
    task = task,
    model = model_name,
    config = list(outer_folds = outer_folds, inner_folds = inner_folds)
  )
}


# =============================================================================
# 4. ANALISIS DE RESIDUOS AVANZADO (Regresion)
# =============================================================================

#' @title Advanced Residual Analysis
#'
#' @description
#' Runs a residual analysis for regression predictions: descriptive
#' statistics, Shapiro-Wilk normality test (on at most 5000 residuals), a
#' simplified heteroscedasticity check (correlation between absolute
#' residuals and predictions), residual outliers (3 x IQR rule), influential
#' points (standardized residuals with absolute value > 3) and an
#' approximate Durbin-Watson statistic.
#'
#' @param predictions Data frame with the observed target and a `.pred`
#'   column with the predictions (e.g. the `predictions` element of a
#'   regression [supervised_ml()] result).
#' @param target Name of the target variable.
#' @param verbose Logical. Print results (default: TRUE).
#'
#' @return An object of class `c("supervisedml_residuals", "list")` with
#'   elements `basic_stats`, `normality_test` (`htest`), `homoscedasticity`
#'   (`correlation`, `p_value`, `heteroscedastic`), `outliers`
#'   (`n_outliers`, `pct_outliers`, `indices`), `influential_points`
#'   (`n_influential`, `indices`), `autocorrelation` (`durbin_watson`,
#'   `interpretation`), `residuals`, `predicted` and `observed`.
#' @examples
#' set.seed(1)
#' obs <- rnorm(150, 10, 2)
#' preds <- data.frame(y = obs, .pred = obs + rnorm(150))
#' ra <- advanced_residual_analysis(preds, target = "y", verbose = FALSE)
#' ra$autocorrelation
#' @family supervised learning
#' @export
advanced_residual_analysis <- function(predictions, target, verbose = TRUE) {

  obs <- predictions[[target]]
  pred <- predictions$.pred
  residuals <- obs - pred
  n <- length(residuals)

  results <- list()

  # 1. Estadisticas basicas de residuos
  results$basic_stats <- list(
    mean = mean(residuals),
    sd = sd(residuals),
    median = median(residuals),
    min = min(residuals),
    max = max(residuals),
    skewness = (mean(residuals) - median(residuals)) / sd(residuals)
  )

  # 2. Test de normalidad (Shapiro-Wilk)
  if (n <= 5000) {
    shapiro_result <- shapiro.test(residuals)
  } else {
    shapiro_result <- shapiro.test(sample(residuals, 5000))
  }
  results$normality_test <- shapiro_result

  # 3. Test de homocedasticidad (simplificado - basado en correlacion)
  # Correlacion entre residuos absolutos y predicciones
  cor_test <- cor.test(abs(residuals), pred)
  results$homoscedasticity <- list(
    correlation = cor_test$estimate,
    p_value = cor_test$p.value,
    heteroscedastic = cor_test$p.value < 0.05
  )

  # 4. Deteccion de outliers en residuos (usando IQR)
  q1 <- quantile(residuals, 0.25)
  q3 <- quantile(residuals, 0.75)
  iqr <- q3 - q1
  lower <- q1 - 3 * iqr
  upper <- q3 + 3 * iqr
  outlier_idx <- which(residuals < lower | residuals > upper)

  results$outliers <- list(
    n_outliers = length(outlier_idx),
    pct_outliers = length(outlier_idx) / n * 100,
    indices = outlier_idx
  )

  # 5. Puntos influyentes (basado en residuos estandarizados + leverage aproximado)
  std_residuals <- (residuals - mean(residuals)) / sd(residuals)
  influential_idx <- which(abs(std_residuals) > 3)

  results$influential_points <- list(
    n_influential = length(influential_idx),
    indices = influential_idx
  )

  # 6. Autocorrelacion de residuos (Durbin-Watson aproximado)
  dw_statistic <- sum(diff(residuals)^2) / sum(residuals^2)
  results$autocorrelation <- list(
    durbin_watson = dw_statistic,
    interpretation = if (dw_statistic < 1.5) "Autocorrelacion positiva" else
                     if (dw_statistic > 2.5) "Autocorrelacion negativa" else
                     "Sin autocorrelacion significativa"
  )

  if (verbose) {
    cat("    Estadisticas de Residuos:\n")
    cat("      - Media:", round(results$basic_stats$mean, 4), "\n")
    cat("      - DE:", round(results$basic_stats$sd, 4), "\n")
    cat("      - Rango: [", round(results$basic_stats$min, 2), ",",
        round(results$basic_stats$max, 2), "]\n\n")

    cat("    Test de Normalidad (Shapiro-Wilk):\n")
    cat("      - W =", round(shapiro_result$statistic, 4), "\n")
    cat("      - p-value =", format.pval(shapiro_result$p.value), "\n")
    if (shapiro_result$p.value < 0.05) {
      cat("      - [!] Residuos NO normales\n\n")
    } else {
      cat("      - [OK] Residuos aproximadamente normales\n\n")
    }

    cat("    Test de Homocedasticidad:\n")
    cat("      - Correlacion |residuos| vs pred:", round(cor_test$estimate, 4), "\n")
    if (results$homoscedasticity$heteroscedastic) {
      cat("      - [!] Posible heterocedasticidad (p <", format.pval(cor_test$p.value), ")\n\n")
    } else {
      cat("      - [OK] Homocedasticidad asumida\n\n")
    }

    cat("    Outliers en Residuos:\n")
    cat("      -", results$outliers$n_outliers, "outliers detectados",
        "(", round(results$outliers$pct_outliers, 1), "%)\n\n")

    cat("    Puntos Influyentes:\n")
    cat("      -", results$influential_points$n_influential,
        "puntos con residuos estandarizados > 3\n\n")

    cat("    Autocorrelacion (Durbin-Watson):\n")
    cat("      - DW =", round(dw_statistic, 4), "\n")
    cat("      -", results$autocorrelation$interpretation, "\n")
  }

  results$residuals <- residuals
  results$predicted <- pred
  results$observed <- obs

  class(results) <- c("supervisedml_residuals", "list")
  results
}


#' @title Residual Diagnostic Plots
#' @description Residuals versus predictions, normal Q-Q plot,
#'   histogram of residuals and scale-location plot.
#' @param residual_analysis A list returned by
#'   [advanced_residual_analysis()].
#' @return If the patchwork package is installed, a patchwork object
#'   combining the four plots; otherwise a list of four `ggplot` objects
#'   (`residuals_vs_fitted`, `qq_plot`, `histogram`, `scale_location`).
#' @examples
#' set.seed(1)
#' obs <- rnorm(150, 10, 2)
#' preds <- data.frame(y = obs, .pred = obs + rnorm(150))
#' ra <- advanced_residual_analysis(preds, target = "y", verbose = FALSE)
#' p <- plot_residual_diagnostics(ra)
#' @family supervised learning
#' @export
plot_residual_diagnostics <- function(residual_analysis) {

  residuals <- residual_analysis$residuals
  predicted <- residual_analysis$predicted
  observed <- residual_analysis$observed

  plot_data <- data.frame(
    residuals = residuals,
    predicted = predicted,
    observed = observed,
    std_residuals = (residuals - mean(residuals)) / sd(residuals)
  )

  # 1. Residuos vs Predicciones
  p1 <- ggplot2::ggplot(plot_data, ggplot2::aes(x = predicted, y = residuals)) +
    ggplot2::geom_point(alpha = 0.5, color = "#3498db") +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    ggplot2::geom_smooth(method = "loess", se = FALSE, color = "orange") +
    ggplot2::labs(title = "Residuos vs Predicciones", x = "Predicciones", y = "Residuos") +
    ggplot2::theme_minimal()

  # 2. Q-Q Plot
  p2 <- ggplot2::ggplot(plot_data, ggplot2::aes(sample = std_residuals)) +
    ggplot2::stat_qq(alpha = 0.5, color = "#e74c3c") +
    ggplot2::stat_qq_line(color = "black", linetype = "dashed") +
    ggplot2::labs(title = "Q-Q Plot de Residuos", x = "Cuantiles Teoricos", y = "Cuantiles Observados") +
    ggplot2::theme_minimal()

  # 3. Histograma de residuos
  p3 <- ggplot2::ggplot(plot_data, ggplot2::aes(x = residuals)) +
    ggplot2::geom_histogram(bins = 30, fill = "#2ecc71", color = "white", alpha = 0.7) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
    ggplot2::labs(title = "Distribucion de Residuos", x = "Residuos", y = "Frecuencia") +
    ggplot2::theme_minimal()

  # 4. Scale-Location
  p4 <- ggplot2::ggplot(plot_data, ggplot2::aes(x = predicted, y = sqrt(abs(std_residuals)))) +
    ggplot2::geom_point(alpha = 0.5, color = "#9b59b6") +
    ggplot2::geom_smooth(method = "loess", se = FALSE, color = "red") +
    ggplot2::labs(title = "Scale-Location", x = "Predicciones", y = "sqrt(|Residuos Std|)") +
    ggplot2::theme_minimal()

  # Combinar con patchwork si esta disponible
  if (requireNamespace("patchwork", quietly = TRUE)) {
    (p1 + p2) / (p3 + p4)
  } else {
    list(residuals_vs_fitted = p1, qq_plot = p2,
         histogram = p3, scale_location = p4)
  }
}


# =============================================================================
# 5. DETECCION DE DATA LEAKAGE (Post-Modelo)
# =============================================================================

#' @title Detect Possible Data Leakage (Post-Model Checks)
#'
#' @description
#' Inspects the results of a trained model for indicators of data leakage
#' that can only be checked after training. Pre-model checks (correlation
#' with the target, possible identifiers) are done by [eda_leakage()].
#'
#' Checks performed:
#' - test AUC or R-squared > 0.99 (suspiciously high performance);
#' - one variable accounting for more than 80% of the total importance
#'   (dominant variable).
#'
#' @param model_results A list with a `test_metrics` element (data frame
#'   with columns `.metric` and `.estimate`), such as a [supervised_ml()]
#'   result. Optional (default: NULL).
#' @param importance Data frame of variable importance with columns
#'   `Variable` and `Importance`, sorted by decreasing importance (e.g. the
#'   `importance` element of a [supervised_ml()] result). Optional (default:
#'   NULL).
#' @param verbose Logical. Print results (default: TRUE).
#'
#' @return A list with `warnings` (list with elements `high_performance`
#'   and/or `dominant_variable` describing each indicator), `n_warnings`
#'   (number of indicators) and `leakage_suspected` (logical).
#' @examples
#' \donttest{
#' # Inputs built by hand
#' metrics <- data.frame(.metric = c("roc_auc", "accuracy"),
#'                       .estimate = c(0.995, 0.97))
#' imp <- data.frame(Variable = c("a", "b", "c"), Importance = c(90, 6, 4))
#' detect_data_leakage(list(test_metrics = metrics), imp,
#'                     verbose = FALSE)$n_warnings
#' # From a supervised_ml() result
#' set.seed(1)
#' n <- 200
#' df <- data.frame(x1 = rnorm(n), x2 = rnorm(n), x3 = rnorm(n))
#' df$y <- factor(ifelse(df$x1 + 0.5 * df$x2 + rnorm(n) > 0, "yes", "no"))
#' res <- supervised_ml(df, target = "y", models = "glm", cv_folds = 3,
#'                      tune_best = FALSE, run_shap = FALSE, n_cores = 1,
#'                      verbose = FALSE)
#' lk <- detect_data_leakage(res, importance = res$importance, verbose = FALSE)
#' lk$leakage_suspected
#' }
#' @family supervised learning
#' @export
detect_data_leakage <- function(model_results = NULL,
                                 importance = NULL,
                                 verbose = TRUE) {

  warnings <- list()
  warning_count <- 0

  if (verbose) {
    cat("    (Verificaciones post-modelo - las verificaciones pre-modelo se\n")
    cat("     realizaron en EDA seccion 1.2.8)\n\n")
    cat("    Verificando:\n")
    cat("      - AUC/R-squared > 0.99 (rendimiento sospechoso)\n")
    cat("      - Variable con importancia > 80% (variable dominante)\n\n")
  }

  # 1. Rendimiento sospechosamente alto (AUC > 0.99 o R-squared > 0.99)
  if (!is.null(model_results) && !is.null(model_results$test_metrics)) {
    metrics <- model_results$test_metrics

    # Verificar AUC para clasificacion
    if ("roc_auc" %in% metrics$.metric) {
      auc <- metrics$.estimate[metrics$.metric == "roc_auc"]
      if (auc > 0.99) {
        warnings$high_performance <- list(
          type = "RENDIMIENTO MUY ALTO",
          metric = "AUC",
          value = auc,
          message = "AUC > 0.99 puede indicar data leakage o target trivial"
        )
        warning_count <- warning_count + 1
      }
    }

    # Verificar R-squared para regresion
    if ("rsq" %in% metrics$.metric) {
      rsq <- metrics$.estimate[metrics$.metric == "rsq"]
      if (rsq > 0.99) {
        warnings$high_performance <- list(
          type = "RENDIMIENTO MUY ALTO",
          metric = "R-squared",
          value = rsq,
          message = "R-squared > 0.99 puede indicar data leakage"
        )
        warning_count <- warning_count + 1
      }
    }
  }

  # 2. Variable con importancia dominante (> 80% del total)
  if (!is.null(importance) && nrow(importance) > 1) {
    total_importance <- sum(importance$Importance)
    top_importance <- importance$Importance[1]
    top_var <- importance$Variable[1]
    pct_top <- (top_importance / total_importance) * 100

    if (pct_top > 80) {
      warnings$dominant_variable <- list(
        type = "VARIABLE DOMINANTE",
        variable = top_var,
        importance_pct = round(pct_top, 1),
        message = paste0("Una variable explica ", round(pct_top, 1),
                         "% de la importancia total - posible leakage")
      )
      warning_count <- warning_count + 1
    }
  }

  # Mostrar resultados
  if (verbose) {
    if (warning_count == 0) {
      cat("    [ok] No se detectaron indicadores post-modelo de data leakage\n")
      cat("         - Rendimiento dentro de rangos normales\n")
      cat("         - Importancia distribuida entre variables\n")
    } else {
      cat("    [!] Se detectaron", warning_count, "posibles indicadores de leakage:\n\n")

      if (!is.null(warnings$high_performance)) {
        w <- warnings$high_performance
        cat("      RENDIMIENTO SOSPECHOSAMENTE ALTO:\n")
        cat("        -", w$metric, "=", round(w$value, 4), "\n")
        cat("        - [!]", w$message, "\n\n")
      }

      if (!is.null(warnings$dominant_variable)) {
        w <- warnings$dominant_variable
        cat("      VARIABLE DOMINANTE:\n")
        cat("        - Variable:", w$variable, "\n")
        cat("        - Importancia:", w$importance_pct, "% del total\n")
        cat("        - [!]", w$message, "\n")
        cat("        - [!] Verifique que esta variable no contiene\n")
        cat("              informacion del futuro o del target\n\n")
      }
    }
  }

  list(
    warnings = warnings,
    n_warnings = warning_count,
    leakage_suspected = warning_count > 0
  )
}


# =============================================================================
# 6. FEATURE SELECTION DENTRO DEL CV (Wrapper)
# =============================================================================

#' @title Feature Selection with Cross-Validation
#'
#' @description
#' Performs feature selection inside a cross-validation loop to reduce
#' overfitting. In each fold a random forest (ranger, 100 trees) is fitted
#' on complete cases and the most important predictors (the top half, but
#' at least `min_features`) are retained; predictors selected in at least
#' half of the folds are returned.
#'
#' @param data Training data (data frame).
#' @param target Name of the target variable.
#' @param task Type of task: "classification" (stratified folds) or
#'   "regression".
#' @param cv_folds Number of folds (default: 5).
#' @param min_features Minimum number of features retained in each fold
#'   (default: 5).
#' @param seed Optional integer seed. If NULL (default) the random number
#'   generator state is not modified.
#' @param verbose Logical. Print progress (default: TRUE).
#'
#' @return A list with `selected_features` (character vector),
#'   `selection_counts` (named numeric vector with the number of folds in
#'   which each predictor was selected, sorted), `threshold` (minimum
#'   number of folds required) and `n_folds`.
#' @examples
#' set.seed(1)
#' n <- 150
#' df <- as.data.frame(matrix(rnorm(n * 6), n, 6))
#' df$y <- df$V1 + df$V2 + rnorm(n)
#' fs <- cv_feature_selection(df, target = "y", task = "regression",
#'                            cv_folds = 3, min_features = 2, seed = 1,
#'                            verbose = FALSE)
#' fs$selected_features
#' @family supervised learning
#' @export
cv_feature_selection <- function(data,
                                  target,
                                  task,
                                  cv_folds = 5,
                                  min_features = 5,
                                  seed = NULL,
                                  verbose = TRUE) {

  if (!is.null(seed)) set.seed(seed)

  if (verbose) {
    cat("    Feature Selection con CV (evita overfitting):\n")
    cat("      - Folds:", cv_folds, "\n")
    cat("      - Minimo features:", min_features, "\n\n")
  }

  # Crear folds
  if (task == "classification") {
    folds <- .strat_no_pool(rsample::vfold_cv, data, v = cv_folds,
                            strata = !!rlang::sym(target))
  } else {
    folds <- rsample::vfold_cv(data, v = cv_folds)
  }

  # Variables predictoras
  predictors <- setdiff(names(data), target)
  n_predictors <- length(predictors)

  # Conteo de seleccion por variable
  selection_count <- setNames(rep(0, n_predictors), predictors)

  for (i in 1:cv_folds) {
    if (verbose) cat("      Fold", i, "/", cv_folds, "...")

    train_fold <- rsample::analysis(folds$splits[[i]])

    # Feature selection en este fold usando RF importance
    tryCatch({
      # Preparar datos
      train_clean <- train_fold |>
        dplyr::mutate(dplyr::across(dplyr::where(is.character), as.factor)) |>
        na.omit()

      # Entrenar RF rapido para importancia
      if (task == "classification") {
        rf_model <- ranger::ranger(
          formula = as.formula(paste(target, "~ .")),
          data = train_clean,
          num.trees = 100,
          importance = "impurity",
          seed = seed
        )
      } else {
        rf_model <- ranger::ranger(
          formula = as.formula(paste(target, "~ .")),
          data = train_clean,
          num.trees = 100,
          importance = "impurity",
          seed = seed
        )
      }

      # Obtener importancia
      imp <- rf_model$variable.importance
      imp_sorted <- sort(imp, decreasing = TRUE)

      # Seleccionar top features (al menos min_features)
      n_select <- max(min_features, ceiling(length(imp) * 0.5))
      selected_vars <- names(imp_sorted)[1:min(n_select, length(imp_sorted))]

      # Incrementar conteo
      for (v in selected_vars) {
        if (v %in% names(selection_count)) {
          selection_count[v] <- selection_count[v] + 1
        }
      }

      if (verbose) cat(" OK\n")

    }, error = function(e) {
      if (verbose) cat(" Error\n")
    })
  }

  # Features seleccionadas en mayoria de folds
  threshold <- ceiling(cv_folds / 2)
  final_selected <- names(selection_count)[selection_count >= threshold]

  if (verbose) {
    cat("\n    Features seleccionadas (en >=", threshold, "folds):\n")
    cat("      -", length(final_selected), "de", n_predictors, "variables\n")
    if (length(final_selected) <= 10) {
      cat("      - Variables:", paste(final_selected, collapse = ", "), "\n")
    } else {
      cat("      - Top 10:", paste(head(final_selected, 10), collapse = ", "), "...\n")
    }
  }

  list(
    selected_features = final_selected,
    selection_counts = sort(selection_count, decreasing = TRUE),
    threshold = threshold,
    n_folds = cv_folds
  )
}
