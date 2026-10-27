# =============================================================================
# Funciones de visualizacion para PsyML
# =============================================================================

#' @title Generate All Plots of a supervisedml Object
#'
#' @description
#' Generates all relevant plots of a [supervised_ml()] result together with
#' metadata for scientific reports. Figures are numbered (Figura 1, Figura 2,
#' ...) and given descriptive names and file names (plot titles and
#' catalog texts are in Spanish). Plots depend on the task and on the
#' analyses run: variable importance, model comparison, ROC curve, confusion
#' matrix, calibration, threshold optimization, predicted vs observed values,
#' residuals, tuning, SHAP, fairness and decision curve analysis.
#'
#' @param x An object of class `supervisedml` returned by [supervised_ml()].
#' @param verbose Logical. Print the list of generated figures
#'   (default: TRUE).
#'
#' @return A list with `plots` (named list of `ggplot` objects),
#'   `figures_catalog` (named list with, for each plot, `number`, `type`,
#'   `id`, `title`, `description` and `filename`) and `total_figures`
#'   (number of figures).
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
#' gp <- generate_all_plots(res, verbose = FALSE)
#' names(gp$plots)
#' gp$plots$roc_curve
#' }
#'
#' @family supervised learning
#' @export
generate_all_plots <- function(x, verbose = TRUE) {

  if (verbose) cat("\n    Generando graficos...\n")

  plots <- list()
  figures_catalog <- list()
  task <- x$task
  fig_num <- 0

  # 1. Importancia de Variables
  if (!is.null(x$importance) && nrow(x$importance) > 0) {
    fig_num <- fig_num + 1
    plots$importance <- .plot_importance(x, top_n = 15)
    figures_catalog$importance <- list(
      number = fig_num, type = "figura",
      id = paste0("Figura_", fig_num),
      title = "Importancia de Variables",
      description = paste0("Importancia de variables del modelo ", x$best_model),
      filename = paste0("Figura_", fig_num, "_Importancia_Variables.png")
    )
    if (verbose) cat("      - Figura", fig_num, ": Importancia de variables\n")
  }

  # 2. Comparacion de Modelos
  if (!is.null(x$cv_summary)) {
    fig_num <- fig_num + 1
    plots$model_comparison <- .plot_metrics(x)
    metric_name <- ifelse(task == "classification", "ROC-AUC", "RMSE")
    figures_catalog$model_comparison <- list(
      number = fig_num, type = "figura",
      id = paste0("Figura_", fig_num),
      title = "Comparacion de Modelos",
      description = paste0("Rendimiento de modelos usando ", metric_name, " en CV"),
      filename = paste0("Figura_", fig_num, "_Comparacion_Modelos.png")
    )
    if (verbose) cat("      - Figura", fig_num, ": Comparacion de modelos\n")
  }

  # 3. Graficos especificos por tarea
  if (task == "classification") {
    # Curva ROC
    fig_num <- fig_num + 1
    plots$roc_curve <- suppressWarnings(.plot_roc(x))
    auc_val <- x$test_metrics$.estimate[x$test_metrics$.metric == "roc_auc"]
    figures_catalog$roc_curve <- list(
      number = fig_num, type = "figura",
      id = paste0("Figura_", fig_num),
      title = "Curva ROC",
      description = paste0("Curva ROC del modelo. AUC = ", round(auc_val, 3)),
      filename = paste0("Figura_", fig_num, "_Curva_ROC.png")
    )
    if (verbose) cat("      - Figura", fig_num, ": Curva ROC\n")

    # Matriz de Confusion
    fig_num <- fig_num + 1
    plots$confusion_matrix <- suppressWarnings(.plot_confusion(x))
    figures_catalog$confusion_matrix <- list(
      number = fig_num, type = "figura",
      id = paste0("Figura_", fig_num),
      title = "Matriz de Confusion",
      description = "Matriz de confusion en conjunto de test",
      filename = paste0("Figura_", fig_num, "_Matriz_Confusion.png")
    )
    if (verbose) cat("      - Figura", fig_num, ": Matriz de confusion\n")

    # Calibracion
    if (!is.null(x$evaluation$calibration)) {
      fig_num <- fig_num + 1
      plots$calibration <- .plot_calibration_deciles(x$evaluation$calibration)
      figures_catalog$calibration <- list(
        number = fig_num, type = "figura",
        id = paste0("Figura_", fig_num),
        title = "Calibracion del Modelo",
        description = "Calibracion por deciles de probabilidad",
        filename = paste0("Figura_", fig_num, "_Calibracion.png")
      )
      if (verbose) cat("      - Figura", fig_num, ": Calibracion por deciles\n")
    }

    # Optimizacion de Threshold
    if (!is.null(x$threshold_optimization)) {
      fig_num <- fig_num + 1
      plots$threshold_optimization <- plot_threshold_optimization(x$threshold_optimization)
      opt_thresh <- x$threshold_optimization$optimal_threshold
      figures_catalog$threshold_optimization <- list(
        number = fig_num, type = "figura",
        id = paste0("Figura_", fig_num),
        title = "Optimizacion de Threshold",
        description = paste0("Threshold optimo = ", round(opt_thresh, 3)),
        filename = paste0("Figura_", fig_num, "_Threshold.png")
      )
      if (verbose) cat("      - Figura", fig_num, ": Optimizacion de threshold\n")
    }

    # Curva de Calibracion (post-calibracion)
    if (!is.null(x$calibration)) {
      fig_num <- fig_num + 1
      plots$calibration_curve <- plot_calibration_curve(x$calibration)
      figures_catalog$calibration_curve <- list(
        number = fig_num, type = "figura",
        id = paste0("Figura_", fig_num),
        title = "Curva de Calibracion",
        description = "Curva de calibracion post-ajuste",
        filename = paste0("Figura_", fig_num, "_Calibracion_Curva.png")
      )
      if (verbose) cat("      - Figura", fig_num, ": Curva de calibracion\n")
    }

  } else {
    # Regresion
    fig_num <- fig_num + 1
    plots$pred_vs_obs <- .plot_pred_vs_obs(x)
    r2_val <- x$test_metrics$.estimate[x$test_metrics$.metric == "rsq"]
    figures_catalog$pred_vs_obs <- list(
      number = fig_num, type = "figura",
      id = paste0("Figura_", fig_num),
      title = "Predicciones vs Observaciones",
      description = paste0("R2 = ", round(r2_val, 3)),
      filename = paste0("Figura_", fig_num, "_Pred_vs_Obs.png")
    )
    if (verbose) cat("      - Figura", fig_num, ": Predicciones vs Observaciones\n")

    fig_num <- fig_num + 1
    plots$residuals <- .plot_residuals(x)
    figures_catalog$residuals <- list(
      number = fig_num, type = "figura",
      id = paste0("Figura_", fig_num),
      title = "Analisis de Residuos",
      description = "Residuos vs valores predichos",
      filename = paste0("Figura_", fig_num, "_Residuos.png")
    )
    if (verbose) cat("      - Figura", fig_num, ": Analisis de residuos\n")

    if (!is.null(x$residual_analysis)) {
      fig_num <- fig_num + 1
      plots$residual_diagnostics <- plot_residual_diagnostics(x$residual_analysis)
      figures_catalog$residual_diagnostics <- list(
        number = fig_num, type = "figura",
        id = paste0("Figura_", fig_num),
        title = "Diagnosticos de Residuos",
        description = "Panel de diagnosticos avanzados",
        filename = paste0("Figura_", fig_num, "_Residuos_Diag.png")
      )
      if (verbose) cat("      - Figura", fig_num, ": Diagnosticos de residuos\n")
    }
  }

  # 4. Tuning (si aplica)
  if (!is.null(x$tuning) && x$tuning$tuned && !is.null(x$tuning$tune_results)) {
    fig_num <- fig_num + 1
    plots$tuning <- plot_tuning(x$tuning)
    figures_catalog$tuning <- list(
      number = fig_num, type = "figura",
      id = paste0("Figura_", fig_num),
      title = "Resultados de Tuning",
      description = "Optimizacion de hiperparametros",
      filename = paste0("Figura_", fig_num, "_Tuning.png")
    )
    if (verbose) cat("      - Figura", fig_num, ": Resultados de tuning\n")
  }

  # 5. SHAP (si aplica)
  if (!is.null(x$interpretation$shap)) {
    interpret_obj <- list(
      importance = x$importance,
      shap = x$interpretation$shap
    )
    class(interpret_obj) <- c("supervisedml_interpret", "list")

    # 5a. SHAP Bar Plot (importancia)
    fig_num <- fig_num + 1
    plots$shap_bar <- plot_shap_bar(interpret_obj, top_n = 15)
    figures_catalog$shap_bar <- list(
      number = fig_num, type = "figura",
      id = paste0("Figura_", fig_num),
      title = "SHAP Feature Importance (Bar)",
      description = "Importancia media absoluta SHAP por variable",
      filename = paste0("Figura_", fig_num, "_SHAP_Bar.png")
    )
    if (verbose) cat("      - Figura", fig_num, ": SHAP bar (importancia)\n")

    # 5b. SHAP Beeswarm Plot (distribucion)
    fig_num <- fig_num + 1
    plots$shap_summary <- plot_shap_summary(interpret_obj, top_n = 15)
    figures_catalog$shap_summary <- list(
      number = fig_num, type = "figura",
      id = paste0("Figura_", fig_num),
      title = "SHAP Beeswarm Plot",
      description = "Distribucion de valores SHAP por variable (beeswarm)",
      filename = paste0("Figura_", fig_num, "_SHAP_Beeswarm.png")
    )
    if (verbose) cat("      - Figura", fig_num, ": SHAP beeswarm\n")
  }

  # Fairness Analysis
  if (!is.null(x$fairness_analysis)) {
    fig_num <- fig_num + 1
    plots$fairness <- .plot_fairness(x$fairness_analysis)
    figures_catalog$fairness <- list(
      number = fig_num, type = "figura",
      id = paste0("Figura_", fig_num),
      title = "Analisis de Equidad (Fairness)",
      description = paste0("Metricas de equidad por subgrupos de ", x$fairness_analysis$protected_var),
      filename = paste0("Figura_", fig_num, "_Fairness.png")
    )
    if (verbose) cat("      - Figura", fig_num, ": Analisis de fairness\n")
  }

  # Decision Curve Analysis
  if (!is.null(x$dca)) {
    fig_num <- fig_num + 1
    plots$dca <- .plot_dca(x$dca)
    figures_catalog$dca <- list(
      number = fig_num, type = "figura",
      id = paste0("Figura_", fig_num),
      title = "Decision Curve Analysis",
      description = "Net Benefit vs threshold de probabilidad",
      filename = paste0("Figura_", fig_num, "_DCA.png")
    )
    if (verbose) cat("      - Figura", fig_num, ": Decision Curve Analysis\n")
  }

  if (verbose) {
    cat("      Total:", length(plots), "graficos generados\n")
  }

  # Retornar estructura con plots y catalogo
  list(
    plots = plots,
    figures_catalog = figures_catalog,
    total_figures = fig_num
  )
}


#' @title Plot a supervisedml Object
#'
#' @description
#' Returns the plots of a [supervised_ml()] result: a combined summary panel
#' (default), the list of all plots, or a single plot.
#'
#' @param x An object of class `supervisedml` returned by [supervised_ml()].
#' @param type Type of plot: `"panel"` (default; four-plot summary panel,
#'   requires \pkg{patchwork}), `"all"` (list with all plots) or a single
#'   plot: `"importance"`, `"metrics"` (or `"comparison"`), `"roc"`,
#'   `"confusion"`, `"calibration"`, `"calibration_curve"`, `"threshold"`,
#'   `"pred_vs_obs"`, `"residuals"`, `"residual_diagnostics"`, `"tuning"`,
#'   `"shap"` (or `"shap_beeswarm"`), `"shap_bar"`, `"fairness"` or `"dca"`.
#'   Availability depends on the task and on the analyses run.
#' @param top_n Number of variables shown in the importance plot when it has
#'   to be regenerated (default: 15).
#' @param ... Additional arguments (not used).
#'
#' @return A `ggplot` (or `patchwork`) object, a named list of `ggplot`
#'   objects when `type = "all"`, or NULL (with a message) when the
#'   requested plot is not available. The plot is drawn when the returned
#'   object is printed.
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
#' plot(res, type = "roc")
#' plot(res, type = "confusion")
#' names(plot(res, type = "all"))
#' if (requireNamespace("patchwork", quietly = TRUE)) plot(res)
#' }
#'
#' @family supervised learning
#' @export
plot.supervisedml <- function(x, type = "panel", top_n = 15, ...) {

  # Obtener plots - ahora pueden estar en estructura nueva o antigua
  plots_data <- x$plots
  if (is.null(plots_data) || length(plots_data) == 0) {
    plots_data <- generate_all_plots(x, verbose = FALSE)
  }

  # Extraer plots si estan en estructura nueva
  if (!is.null(plots_data$plots)) {
    actual_plots <- plots_data$plots
  } else {
    actual_plots <- plots_data
  }

  # Panel combinado
  if (type == "panel") {
    return(plot_all.supervisedml(x))
  }

  # Todos los graficos
  if (type == "all") {
    return(actual_plots)
  }

  # Graficos individuales por nombre
  plot_map <- list(
    importance = "importance",
    metrics = "model_comparison",
    comparison = "model_comparison",
    roc = "roc_curve",
    confusion = "confusion_matrix",
    calibration = "calibration",
    calibration_curve = "calibration_curve",
    pred_vs_obs = "pred_vs_obs",
    residuals = "residuals",
    residual_diagnostics = "residual_diagnostics",
    tuning = "tuning",
    threshold = "threshold_optimization",
    shap = "shap_summary",
    shap_bar = "shap_bar",
    shap_beeswarm = "shap_summary",
    fairness = "fairness",
    dca = "dca"
  )

  if (type %in% names(plot_map)) {
    plot_name <- plot_map[[type]]
    if (!is.null(actual_plots[[plot_name]])) {
      return(actual_plots[[plot_name]])
    }
  }

  # Fallback a funciones originales para compatibilidad
  if (type == "importance") {
    return(.plot_importance(x, top_n))
  }

  if (type == "metrics") {
    return(.plot_metrics(x))
  }

  if (type == "confusion" && x$task == "classification") {
    return(.plot_confusion(x))
  }

  if (type == "roc" && x$task == "classification") {
    return(.plot_roc(x))
  }

  if (type == "residuals" && x$task == "regression") {
    return(.plot_residuals(x))
  }

  if (type == "pred_vs_obs" && x$task == "regression") {
    return(.plot_pred_vs_obs(x))
  }

  message("Plot type not available. Options: panel, all, importance, metrics, roc, confusion, calibration, pred_vs_obs, residuals, tuning, threshold, shap")
  return(NULL)
}


#' @noRd
.plot_importance <- function(x, top_n = 15) {

  imp <- utils::head(x$importance, top_n)

  p <- ggplot2::ggplot(imp,
                       ggplot2::aes(x = stats::reorder(Variable, Importance),
                                    y = Importance)) +
    ggplot2::geom_col(fill = "#3498db", alpha = 0.8) +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = "Importancia de Variables",
      subtitle = paste("Modelo:", x$best_model),
      x = "",
      y = "Importancia"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 14),
      axis.text = ggplot2::element_text(size = 10)
    )

  return(p)
}


#' @noRd
.plot_metrics <- function(x) {

  cv_data <- x$cv_summary

  if (x$task == "classification") {
    metric_col <- "roc_auc"
    metric_label <- "ROC-AUC"
  } else {
    metric_col <- "rmse"
    metric_label <- "RMSE"
  }

  p <- ggplot2::ggplot(cv_data,
                       ggplot2::aes(x = stats::reorder(model, -.data[[metric_col]]),
                                    y = .data[[metric_col]])) +
    ggplot2::geom_col(fill = "#2ecc71", alpha = 0.8) +
    ggplot2::geom_text(ggplot2::aes(label = round(.data[[metric_col]], 3)),
                       vjust = -0.5, size = 4) +
    ggplot2::labs(
      title = "Comparacion de Modelos (CV)",
      x = "Modelo",
      y = metric_label
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 14),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    )

  return(p)
}


#' @noRd
.plot_confusion <- function(x) {

  target <- x$target
  preds <- x$predictions

  conf_mat <- yardstick::conf_mat(preds,
                                  truth = !!rlang::sym(target),
                                  estimate = .pred_class)

  p <- ggplot2::autoplot(conf_mat, type = "heatmap") +
    ggplot2::labs(
      title = "Matriz de Confusion",
      subtitle = paste("Modelo:", x$best_model)
    ) +
    ggplot2::theme_minimal()

  return(p)
}


#' @noRd
.plot_roc <- function(x) {

  target <- x$target
  preds <- x$predictions

  event_info <- .detect_event_level(preds[[target]])
  auc_val <- x$test_metrics$.estimate[x$test_metrics$.metric == "roc_auc"]

  if (event_info$type == "binary") {
    roc_data <- yardstick::roc_curve(preds,
                                     truth = !!rlang::sym(target),
                                     !!rlang::sym(event_info$prob_col),
                                     event_level = event_info$event_level)

    p <- suppressWarnings(
      ggplot2::autoplot(roc_data) +
        ggplot2::labs(
          title = "Curva ROC",
          subtitle = paste("AUC =", round(auc_val, 3), "| Clase positiva:", event_info$positive_class)
        ) +
        ggplot2::theme_minimal()
    )
  } else {
    # Multiclass: pasar todas las columnas .pred_*
    prob_cols_syms <- rlang::syms(event_info$prob_cols)

    roc_data <- yardstick::roc_curve(preds,
                                     truth = !!rlang::sym(target),
                                     !!!prob_cols_syms)

    p <- suppressWarnings(
      ggplot2::autoplot(roc_data) +
        ggplot2::labs(
          title = "Curva ROC (Multiclase)",
          subtitle = paste("AUC Hand-Till =", round(auc_val, 3))
        ) +
        ggplot2::theme_minimal()
    )
  }

  return(p)
}


#' @noRd
.plot_residuals <- function(x) {

  preds <- x$predictions
  target <- x$target

  preds$residuals <- preds[[target]] - preds$.pred

  p <- ggplot2::ggplot(preds, ggplot2::aes(x = .pred, y = residuals)) +
    ggplot2::geom_point(alpha = 0.5, color = "#3498db") +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    ggplot2::geom_smooth(method = "loess", se = TRUE, color = "#e74c3c") +
    ggplot2::labs(
      title = "Analisis de Residuos",
      subtitle = paste("Modelo:", x$best_model),
      x = "Valores Predichos",
      y = "Residuos"
    ) +
    ggplot2::theme_minimal()

  return(p)
}


#' @noRd
.plot_pred_vs_obs <- function(x) {

  preds <- x$predictions
  target <- x$target

  r2_val <- x$test_metrics$.estimate[x$test_metrics$.metric == "rsq"]

  p <- ggplot2::ggplot(preds, ggplot2::aes(x = .data[[target]], y = .pred)) +
    ggplot2::geom_point(alpha = 0.5, color = "#3498db") +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed",
                         color = "red", linewidth = 1) +
    ggplot2::labs(
      title = "Predicciones vs Observaciones",
      subtitle = paste("R2 =", round(r2_val, 3)),
      x = "Valores Observados",
      y = "Valores Predichos"
    ) +
    ggplot2::theme_minimal()

  return(p)
}


#' @title Plot a Summary Panel of All Results
#'
#' @description
#' Generic function that combines the main plots of a result into a single
#' panel. The `supervisedml` method arranges four plots with
#' \pkg{patchwork}: variable importance, model comparison and, for
#' classification, confusion matrix and ROC curve or, for regression,
#' predicted vs observed values and residuals.
#'
#' @param x An object with a `plot_all()` method, such as a `supervisedml`
#'   object returned by [supervised_ml()].
#' @param ... Additional arguments passed to methods (not used by the
#'   `supervisedml` method).
#'
#' @return A `patchwork` object (drawn when printed).
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
#' if (requireNamespace("patchwork", quietly = TRUE)) plot_all(res)
#' }
#'
#' @family supervised learning
#' @export
plot_all <- function(x, ...) {
  UseMethod("plot_all")
}


#' @rdname plot_all
#' @export
plot_all.supervisedml <- function(x, ...) {

  if (!requireNamespace("patchwork", quietly = TRUE)) {
    stop("Package 'patchwork' is required for this function.", call. = FALSE)
  }

  p1 <- .plot_importance(x, top_n = 10)
  p2 <- .plot_metrics(x)

  if (x$task == "classification") {
    p3 <- .plot_confusion(x)
    p4 <- .plot_roc(x)
  } else {
    p3 <- .plot_pred_vs_obs(x)
    p4 <- .plot_residuals(x)
  }

  combined <- (p1 | p2) / (p3 | p4) +
    patchwork::plot_annotation(
      title = paste("Resumen de", ifelse(x$task == "classification", "Clasificacion", "Regresion")),
      subtitle = paste("Mejor modelo:", .get_model_label(x$best_model)),
      theme = ggplot2::theme(
        plot.title = ggplot2::element_text(hjust = 0.5, face = "bold", size = 14),
        plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 11)
      )
    )

  return(combined)
}


#' Calibration Plot by Probability Bins
#' @noRd
.plot_calibration_deciles <- function(calibration_data) {

  if (is.null(calibration_data)) return(NULL)

  calibration_data <- calibration_data[!is.na(calibration_data$mean_pred), ]

  ggplot2::ggplot(calibration_data, ggplot2::aes(x = mean_pred, y = mean_obs)) +
    ggplot2::geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray50") +
    ggplot2::geom_point(ggplot2::aes(size = n), color = "#9b59b6", alpha = 0.7) +
    ggplot2::geom_line(color = "#9b59b6", alpha = 0.5) +
    ggplot2::labs(
      title = "Calibracion del Modelo",
      subtitle = "Probabilidad predicha vs proporcion observada",
      x = "Probabilidad Predicha Promedio",
      y = "Proporcion Observada",
      size = "N obs"
    ) +
    ggplot2::xlim(0, 1) + ggplot2::ylim(0, 1) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = ggplot2::element_text(hjust = 0.5)
    )
}


#' @title Save All Plots to PNG Files
#'
#' @description
#' Saves every plot of a [supervised_ml()] result as a PNG file with a
#' descriptive name (e.g. `Figura_1_Importancia_Variables.png`) plus the
#' combined summary panel (`Figura_0_Panel_Resumen.png`, only when
#' \pkg{patchwork} is installed). The directory is created if needed.
#'
#' @param x An object of class `supervisedml` returned by [supervised_ml()].
#' @param path Directory where the files are written. Required; there is no
#'   default.
#' @param prefix Optional character prefix added to every file name
#'   (default: NULL).
#' @param width Width of each figure in inches (default: 8).
#' @param height Height of each figure in inches (default: 6).
#' @param dpi Resolution in dots per inch (default: 300).
#' @param verbose Logical. Print the catalog of saved figures
#'   (default: TRUE).
#'
#' @return Invisibly, a named list with one element per saved file, each a
#'   list with `filename`, `full_path` and `catalog` (the figure metadata,
#'   see [generate_all_plots()]).
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
#' out_dir <- file.path(tempdir(), "psyml_figures")
#' saved <- save_all_plots(res, path = out_dir, dpi = 72, verbose = FALSE)
#' list.files(out_dir)
#' unlink(out_dir, recursive = TRUE)
#' }
#'
#' @family supervised learning
#' @export
save_all_plots <- function(x, path, prefix = NULL, width = 8, height = 6, dpi = 300,
                           verbose = TRUE) {

  if (missing(path) || is.null(path)) {
    stop("Please provide 'path', the directory where the figures are saved ",
         "(for example, path = tempdir()).", call. = FALSE)
  }

  plots_data <- x$plots
  if (is.null(plots_data) || length(plots_data) == 0) {
    plots_data <- generate_all_plots(x, verbose = FALSE)
  }

  if (!is.null(plots_data$plots)) {
    actual_plots <- plots_data$plots
    figures_catalog <- plots_data$figures_catalog
  } else {
    actual_plots <- plots_data
    figures_catalog <- NULL
  }

  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
  }

  saved_files <- list()

  for (name in names(actual_plots)) {
    p <- actual_plots[[name]]
    if (!is.null(p) && (inherits(p, "ggplot") || inherits(p, "patchwork"))) {

      if (!is.null(figures_catalog) && !is.null(figures_catalog[[name]])) {
        filename_base <- figures_catalog[[name]]$filename
      } else {
        filename_base <- paste0("plot_", name, ".png")
      }

      if (!is.null(prefix)) {
        filename_base <- paste0(prefix, "_", filename_base)
      }

      filename <- file.path(path, filename_base)

      tryCatch({
        ggplot2::ggsave(filename, p, width = width, height = height, dpi = dpi)
        saved_files[[name]] <- list(
          filename = basename(filename),
          full_path = normalizePath(filename, mustWork = FALSE),
          catalog = figures_catalog[[name]]
        )
      }, error = function(e) {
        message("Could not save plot: ", name)
      })
    }
  }

  tryCatch({
    panel <- plot_all.supervisedml(x)
    if (!is.null(panel)) {
      panel_name <- ifelse(is.null(prefix),
                           "Figura_0_Panel_Resumen.png",
                           paste0(prefix, "_Figura_0_Panel_Resumen.png"))
      filename <- file.path(path, panel_name)
      ggplot2::ggsave(filename, panel, width = 12, height = 10, dpi = dpi)
      saved_files$panel <- list(
        filename = basename(filename),
        full_path = normalizePath(filename, mustWork = FALSE),
        catalog = list(number = 0, title = "Panel Resumen", type = "figura")
      )
    }
  }, error = function(e) {
    message("Could not save the combined panel")
  })

  if (verbose) {
    cat("\n============================================================\n")
    cat(" CATALOGO DE FIGURAS GUARDADAS\n")
    cat("============================================================\n")
    cat("Directorio:", normalizePath(path), "\n\n")

    for (name in names(saved_files)) {
      info <- saved_files[[name]]
      if (!is.null(info$catalog)) {
        fig_id <- if (!is.null(info$catalog$id)) info$catalog$id else "Figura"
        fig_title <- if (!is.null(info$catalog$title)) info$catalog$title else name
        cat(sprintf("  %s - %s\n", fig_id, fig_title))
        cat(sprintf("    Archivo: %s\n", info$filename))
        if (!is.null(info$catalog$description)) {
          cat(sprintf("    Descripcion: %s\n", info$catalog$description))
        }
        cat("\n")
      }
    }

    cat("Total archivos:", length(saved_files), "\n")
    cat("============================================================\n")
  }

  invisible(saved_files)
}


#' @title Get the Catalog of Figures
#'
#' @description
#' Returns the catalog of the figures generated by [supervised_ml()], with
#' their numbers, titles, descriptions and suggested file names (texts in
#' Spanish).
#'
#' @param x An object of class `supervisedml` returned by [supervised_ml()].
#'
#' @return A data frame with one row per figure, ordered by number, and
#'   columns `key` (name of the plot in `x$figures`), `number`, `id`,
#'   `title`, `description` and `filename`; NULL (with a message) when `x`
#'   has no plots or no catalog.
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
#' get_figures_catalog(res)
#' }
#'
#' @family supervised learning
#' @export
get_figures_catalog <- function(x) {

  plots_data <- x$plots
  if (is.null(plots_data)) {
    message("No plots have been generated.")
    return(NULL)
  }

  if (!is.null(plots_data$figures_catalog)) {
    catalog <- plots_data$figures_catalog
  } else {
    message("No figure catalog available.")
    return(NULL)
  }

  catalog_df <- do.call(rbind, lapply(names(catalog), function(name) {
    item <- catalog[[name]]
    data.frame(
      key = name,
      number = item$number,
      id = item$id,
      title = item$title,
      description = item$description,
      filename = item$filename,
      stringsAsFactors = FALSE
    )
  }))

  catalog_df <- catalog_df[order(catalog_df$number), ]
  rownames(catalog_df) <- NULL

  catalog_df
}


#' @title Print the Catalog of Figures
#'
#' @description
#' Prints the catalog of figures returned by [get_figures_catalog()]: id,
#' title, description, file name and how to access each figure (console
#' text in Spanish).
#'
#' @param x An object of class `supervisedml` returned by [supervised_ml()].
#'
#' @return Invisibly returns NULL. Called for its side effect of printing.
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
#' print_figures_catalog(res)
#' }
#'
#' @family supervised learning
#' @export
print_figures_catalog <- function(x) {

  catalog <- get_figures_catalog(x)

  if (is.null(catalog)) return(invisible(NULL))

  cat("\n============================================================\n")
  cat(" CATALOGO DE FIGURAS\n")
  cat("============================================================\n\n")

  for (i in seq_len(nrow(catalog))) {
    row <- catalog[i, ]
    cat(sprintf("%s: %s\n", row$id, row$title))
    cat(sprintf("  Descripcion: %s\n", row$description))
    cat(sprintf("  Archivo: %s\n", row$filename))
    cat(sprintf("  Acceso: resultado$figures$%s\n\n", row$key))
  }

  cat("============================================================\n")
  cat("Uso en reportes:\n")
  cat("  [Insertar Figura 1 - Importancia de Variables]\n")
  cat("  [Insertar Figura 2 - Comparacion de Modelos]\n")
  cat("============================================================\n")

  invisible(NULL)
}


#' @title Plot the Profile of Performance Metrics
#'
#' @description
#' Draws a point-and-line plot with the profile of all cross-validated
#' performance metrics of each model. Each model is a line connecting its
#' values across metrics, which makes it easy to compare strengths and
#' weaknesses.
#'
#' @param x A `supervisedml` object (its `cv_summary` element is used) or a
#'   data frame with a `model` column and numeric metric columns.
#' @param metrics Optional character vector with the metrics to include. If
#'   NULL (default), all numeric columns are used.
#' @param highlight Optional name of the model to highlight. If NULL
#'   (default) and `x` is a `supervisedml` object, the best model is
#'   highlighted.
#' @param size_point Numeric. Size of the points (default: 3.5).
#' @param size_line Numeric. Width of the lines (default: 0.9).
#' @param show_values Logical. Show the numeric values above the points
#'   (default: TRUE).
#'
#' @return A `ggplot` object.
#' @family supervised learning
#' @export
#'
#' @examples
#' \donttest{
#' cv <- data.frame(model = c("glm", "rf", "xgboost"),
#'                  roc_auc = c(0.81, 0.86, 0.84),
#'                  accuracy = c(0.74, 0.79, 0.78),
#'                  mcc = c(0.48, 0.57, 0.55))
#' plot_metrics_profile(cv, highlight = "rf")
#' plot_metrics_profile(cv, metrics = c("roc_auc", "mcc"), show_values = FALSE)
#'
#' set.seed(1)
#' n <- 200
#' df <- data.frame(x1 = rnorm(n), x2 = rnorm(n), x3 = rnorm(n))
#' df$y <- factor(ifelse(df$x1 + 0.5 * df$x2 + rnorm(n) > 0, "yes", "no"))
#' res <- supervised_ml(df, target = "y", models = "glm", cv_folds = 3,
#'                      tune_best = FALSE, run_shap = FALSE, n_cores = 1,
#'                      verbose = FALSE)
#' plot_metrics_profile(res)
#' }
plot_metrics_profile <- function(x, metrics = NULL, highlight = NULL,
                                 size_point = 3.5, size_line = 0.9,
                                 show_values = TRUE) {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for this function.", call. = FALSE)
  }

  # Extraer datos
  if (inherits(x, "supervisedml")) {
    cv_data <- x$cv_summary
    if (is.null(highlight)) highlight <- x$best_model
  } else if (is.data.frame(x)) {
    cv_data <- x
  } else {
    stop("x must be a supervisedml object or a data.frame with a 'model' column")
  }

  if (is.null(cv_data) || !"model" %in% names(cv_data)) {
    stop("No cv_summary with a 'model' column was found")
  }

  # Etiquetas legibles
  metric_labels <- c(
    roc_auc = "ROC-AUC", f_meas = "F1-Score", f2_meas = "F2-Score",
    accuracy = "Accuracy", sensitivity = "Sensitivity",
    specificity = "Specificity", bal_accuracy = "Bal. Accuracy",
    pr_auc = "PR-AUC", mcc = "MCC", kap = "Kappa",
    rmse = "RMSE", rsq = "R\u00b2", mae = "MAE", mape = "MAPE",
    rank_mean = "Rank Promedio"
  )

  # Seleccionar metricas
  num_cols <- setdiff(names(cv_data)[vapply(cv_data, is.numeric, logical(1))], "model")
  if (!is.null(metrics)) {
    num_cols <- intersect(num_cols, metrics)
  }

  if (length(num_cols) == 0) {
    stop("No numeric metrics were found in the data")
  }

  # Pivotear a formato largo
  plot_df <- cv_data[, c("model", num_cols), drop = FALSE]
  plot_long <- stats::reshape(
    plot_df,
    direction = "long",
    varying = num_cols,
    v.names = "value",
    timevar = "metric",
    times = num_cols,
    idvar = "model"
  )
  rownames(plot_long) <- NULL

  # Aplicar etiquetas legibles
  plot_long$metric_label <- ifelse(
    plot_long$metric %in% names(metric_labels),
    metric_labels[plot_long$metric],
    plot_long$metric
  )
  # Ordenar metricas por valor medio descendente
  metric_order <- stats::aggregate(value ~ metric_label, data = plot_long, FUN = mean)
  metric_order <- metric_order$metric_label[order(metric_order$value, decreasing = TRUE)]
  plot_long$metric_label <- factor(plot_long$metric_label, levels = metric_order)

  # Etiquetas de modelo legibles
  plot_long$model_label <- vapply(plot_long$model, function(m) {
    lbl <- .get_model_label(m)
    if (is.null(lbl) || is.na(lbl)) m else lbl
  }, character(1))

  # Determinar resaltado
  plot_long$highlighted <- if (is.null(highlight)) {
    rep(FALSE, nrow(plot_long))
  } else {
    plot_long$model == highlight
  }

  # Paleta de colores
  models <- unique(plot_long$model_label)
  n_models <- length(models)
  if (n_models <= 8) {
    pal <- c("#3498db", "#e74c3c", "#2ecc71", "#9b59b6",
             "#e67e22", "#1abc9c", "#f39c12", "#34495e")[seq_len(n_models)]
  } else {
    pal <- grDevices::colorRampPalette(
      c("#3498db", "#e74c3c", "#2ecc71", "#9b59b6")
    )(n_models)
  }
  names(pal) <- models

  # Grafico
  p <- ggplot2::ggplot(plot_long,
                       ggplot2::aes(x = metric_label, y = value,
                                    color = model_label, group = model_label)) +
    ggplot2::geom_line(ggplot2::aes(linewidth = highlighted), alpha = 0.8) +
    ggplot2::geom_point(ggplot2::aes(size = highlighted), alpha = 0.9) +
    ggplot2::scale_linewidth_manual(values = c("FALSE" = size_line * 0.7,
                                               "TRUE" = size_line * 1.4),
                                    guide = "none") +
    ggplot2::scale_size_manual(values = c("FALSE" = size_point * 0.8,
                                          "TRUE" = size_point * 1.3),
                               guide = "none") +
    ggplot2::scale_color_manual(values = pal, name = "Modelo") +
    ggplot2::labs(
      title = "Perfil de Metricas de Rendimiento",
      subtitle = NULL,
      x = NULL,
      y = "Valor"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 14),
      plot.subtitle = ggplot2::element_text(size = 11, color = "gray40"),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 10),
      legend.position = "bottom",
      panel.grid.major.x = ggplot2::element_line(color = "gray90"),
      panel.grid.minor = ggplot2::element_blank()
    )

  if (show_values) {
    p <- p + ggplot2::geom_text(
      ggplot2::aes(label = sprintf("%.3f", value)),
      vjust = -1.2, size = 2.8, show.legend = FALSE
    )
  }

  return(p)
}


# =============================================================================
# Plot de Fairness
# =============================================================================

#' @noRd
.plot_fairness <- function(fairness_result) {

  gm <- fairness_result$group_metrics

  # Preparar datos en formato largo
  metrics_to_plot <- c("positive_rate", "tpr", "fpr", "ppv", "accuracy")
  metric_labels <- c("Positive Rate\n(Dem. Parity)", "TPR\n(Sensitivity)",
                      "FPR\n(False Pos.)", "PPV\n(Precision)", "Accuracy")

  df_long <- do.call(rbind, lapply(seq_along(metrics_to_plot), function(i) {
    data.frame(
      group = gm$group,
      metric = metric_labels[i],
      value = gm[[metrics_to_plot[i]]],
      stringsAsFactors = FALSE
    )
  }))

  df_long$metric <- factor(df_long$metric, levels = metric_labels)

  # Colores para grupos
  n_groups <- length(unique(df_long$group))
  colors <- c("#2196F3", "#FF5722", "#4CAF50", "#FF9800", "#9C27B0",
              "#00BCD4", "#795548", "#607D8B")[seq_len(n_groups)]

  p <- ggplot2::ggplot(df_long, ggplot2::aes(x = metric, y = value, fill = group)) +
    ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.7),
                      width = 0.6, alpha = 0.85) +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2f", value)),
                       position = ggplot2::position_dodge(width = 0.7),
                       vjust = -0.5, size = 3) +
    ggplot2::scale_fill_manual(values = colors) +
    ggplot2::labs(
      title = "Fairness Analysis by Protected Group",
      subtitle = paste("Protected variable:", fairness_result$protected_var,
                       "| DI Ratio:", round(fairness_result$disparate_impact_ratio, 3)),
      x = "", y = "Value", fill = "Group"
    ) +
    ggplot2::ylim(0, min(1.15, max(df_long$value, na.rm = TRUE) * 1.15)) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 12, face = "bold"),
      plot.subtitle = ggplot2::element_text(size = 10, color = "gray40"),
      axis.text.x = ggplot2::element_text(size = 9),
      legend.position = "bottom"
    )

  return(p)
}


# =============================================================================
# Plot de DCA (Decision Curve Analysis)
# =============================================================================

#' @noRd
.plot_dca <- function(dca_result) {

  dca <- dca_result$dca_data

  # Preparar datos para las 3 lineas
  df_plot <- data.frame(
    threshold = rep(dca$threshold, 3),
    net_benefit = c(dca$nb_model, dca$nb_all, dca$nb_none),
    strategy = rep(c("Model", "Treat All", "Treat None"), each = nrow(dca)),
    stringsAsFactors = FALSE
  )
  df_plot$strategy <- factor(df_plot$strategy,
                              levels = c("Model", "Treat All", "Treat None"))

  # Limitar el eje Y para mejor visualizacion
  y_max <- max(dca$nb_model, dca$nb_all, na.rm = TRUE) * 1.1
  y_min <- min(-0.05, min(dca$nb_model, dca$nb_all, na.rm = TRUE) * 1.1)

  p <- ggplot2::ggplot(df_plot, ggplot2::aes(x = threshold, y = net_benefit,
                                              color = strategy, linetype = strategy)) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::scale_color_manual(values = c("Model" = "#1565C0",
                                            "Treat All" = "#757575",
                                            "Treat None" = "#BDBDBD")) +
    ggplot2::scale_linetype_manual(values = c("Model" = "solid",
                                               "Treat All" = "dashed",
                                               "Treat None" = "dotted")) +
    ggplot2::coord_cartesian(ylim = c(y_min, y_max)) +
    ggplot2::labs(
      title = "Decision Curve Analysis",
      subtitle = paste("Prevalence:", round(dca_result$prevalence * 100, 1), "%"),
      x = "Threshold Probability",
      y = "Net Benefit",
      color = "Strategy",
      linetype = "Strategy"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 12, face = "bold"),
      plot.subtitle = ggplot2::element_text(size = 10, color = "gray40"),
      legend.position = "bottom"
    )

  # Agregar rango util si existe
  if (!is.na(dca_result$useful_range[1])) {
    p <- p + ggplot2::annotate("rect",
                                xmin = dca_result$useful_range[1],
                                xmax = dca_result$useful_range[2],
                                ymin = y_min, ymax = y_max,
                                alpha = 0.1, fill = "#4CAF50")
  }

  return(p)
}

