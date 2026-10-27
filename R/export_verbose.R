# =============================================================================
# Funciones de Exportacion del Verbose para PsyML
# =============================================================================
# Permite exportar el output verbose como TXT y JSON para generar reportes
# =============================================================================

#' @title Run supervised_ml() and Capture Its Console Output
#'
#' @description
#' Runs [supervised_ml()] with \code{verbose = TRUE}, capturing all the
#' console output so that it can be exported with [export_verbose_txt()] or
#' [export_verbose_json()]. The captured output is not shown in the console.
#'
#' @param ... Arguments passed to [supervised_ml()] (do not pass
#'   \code{verbose}, it is always set to TRUE).
#'
#' @return The \code{supervisedml} object returned by [supervised_ml()] with
#'   three additional elements: \code{verbose_text} (the captured output as a
#'   single string), \code{verbose_lines} (character vector, one element per
#'   line) and \code{verbose_json} (list with the output parsed into sections).
#'
#' @examples
#' \donttest{
#' set.seed(1)
#' n <- 200
#' df <- data.frame(x1 = rnorm(n), x2 = rnorm(n), x3 = rnorm(n))
#' df$y <- factor(ifelse(df$x1 + 0.5 * df$x2 + rnorm(n) > 0, "yes", "no"))
#' res <- supervised_ml_capture(df, target = "y", models = "glm", cv_folds = 3,
#'                              tune_best = FALSE, run_shap = FALSE,
#'                              n_cores = 1)
#' cat(head(res$verbose_lines, 10), sep = "\n")
#' }
#' @family reporting and export
#' @export
supervised_ml_capture <- function(...) {


  # Capturar el output
  verbose_output <- capture.output({
    result <- supervised_ml(..., verbose = TRUE)
  }, type = "output")

  # Agregar verbose al resultado
  result$verbose_text <- paste(verbose_output, collapse = "\n")
  result$verbose_lines <- verbose_output

  # Crear estructura JSON
  result$verbose_json <- .parse_verbose_to_json(verbose_output, result)


  return(result)
}


#' @title Export the Console Output of an Analysis as a TXT File
#'
#' @description
#' Writes the result of a PsyML analysis to a plain text file. When the
#' analysis was run with \code{verbose = TRUE}, [supervised_ml()] stores the
#' console output, so the file contains exactly the same output shown in the
#' console. Otherwise a summary is built from the result object.
#'
#' @param result Result of [supervised_ml()] or [supervised_ml_capture()].
#' @param file_path Path of the output file (required, no default).
#'
#' @return Invisibly returns \code{file_path}.
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
#' out <- file.path(tempdir(), "psyml_report.txt")
#' export_verbose_txt(res, out)
#' head(readLines(out))
#' unlink(out)
#' }
#' @family reporting and export
#' @export
export_verbose_txt <- function(result, file_path) {

  if (missing(file_path) || is.null(file_path)) {
    stop("Please provide 'file_path' (for example, ",
         "file.path(tempdir(), \"report.txt\")).", call. = FALSE)
  }

  # Verificar que tenga verbose_text (supervised_ml() ahora lo captura automaticamente)
  if (!is.null(result$verbose_text) && nzchar(result$verbose_text)) {
    txt_content <- result$verbose_text
  } else {
    # Fallback: construir desde el resultado si no tiene verbose capturado
    # Esto ocurre si se ejecuto con verbose = FALSE
    message("Note: the analysis was run with verbose = FALSE.")
    message("Building a summary from the results...")
    txt_content <- .build_txt_from_result(result)
  }

  writeLines(txt_content, file_path)
  message("TXT exported to: ", file_path)

  invisible(file_path)
}


#' @title Build the TXT report from a supervised_ml() result
#' @noRd
.build_txt_from_result <- function(result) {

  lines <- c()
  sep_line <- paste(rep("=", 60), collapse = "")
  dash_line <- paste(rep("-", 50), collapse = "")

  # Obtener datos basicos
 n_train <- if (!is.null(result$train_data)) nrow(result$train_data) else 0
  n_test <- if (!is.null(result$test_data)) nrow(result$test_data) else 0
  n_total <- n_train + n_test

  # ============================================================
  # HEADER
  # ============================================================
  lines <- c(lines, sep_line)
  lines <- c(lines, " PsyML - Machine Learning Automatizado")
  lines <- c(lines, sep_line)
  lines <- c(lines, "")
  lines <- c(lines, sprintf("Observaciones: %d", n_total))
  lines <- c(lines, sprintf("Variable objetivo: %s", result$target))
  lines <- c(lines, sprintf("Tarea: %s", result$task))
  lines <- c(lines, sprintf("Mejor modelo: %s", result$best_model))
  lines <- c(lines, "")

  # ============================================================
  # 1. INFORMACION DEL DATASET
  # ============================================================
  lines <- c(lines, sep_line)
  lines <- c(lines, "1. Informacion del Dataset")
  lines <- c(lines, sep_line)
  lines <- c(lines, "")

  lines <- c(lines, dash_line)
  lines <- c(lines, "1.1. Estructura de los Datos")
  lines <- c(lines, dash_line)
  lines <- c(lines, sprintf("    Observaciones totales: %d", n_total))
  lines <- c(lines, sprintf("    Conjunto de entrenamiento: %d (%.1f%%)", n_train, n_train/n_total*100))
  lines <- c(lines, sprintf("    Conjunto de prueba: %d (%.1f%%)", n_test, n_test/n_total*100))
  lines <- c(lines, "")

  # Distribucion de la variable objetivo
  if (result$task == "classification" && !is.null(result$test_data)) {
    lines <- c(lines, dash_line)
    lines <- c(lines, "1.2. Distribucion de la Variable Objetivo")
    lines <- c(lines, dash_line)

    # Calcular distribucion en train
    if (!is.null(result$train_data) && result$target %in% names(result$train_data)) {
      target_table <- table(result$train_data[[result$target]])
      target_props <- prop.table(target_table) * 100
      lines <- c(lines, "    Distribucion en Train:")
      for (i in seq_along(target_table)) {
        lines <- c(lines, sprintf("      - %s: %d (%.1f%%)",
                                  names(target_table)[i], target_table[i], target_props[i]))
      }
    }
    lines <- c(lines, "")
  }

  lines <- c(lines, "    Referencia: Hastie, T., Tibshirani, R., & Friedman, J. (2009).")
  lines <- c(lines, "    The elements of statistical learning (2nd ed.). Springer.")
  lines <- c(lines, "")

  # ============================================================
  # 2. PREPROCESAMIENTO
  # ============================================================
  lines <- c(lines, sep_line)
  lines <- c(lines, "2. Preprocesamiento de Datos")
  lines <- c(lines, sep_line)
  lines <- c(lines, "")

  lines <- c(lines, dash_line)
  lines <- c(lines, "2.1. Division Train/Test")
  lines <- c(lines, dash_line)
  lines <- c(lines, sprintf("    Proporcion test: %.0f%%", n_test/n_total*100))
  lines <- c(lines, sprintf("    Train: %d observaciones", n_train))
  lines <- c(lines, sprintf("    Test: %d observaciones", n_test))
  lines <- c(lines, "")

  # Feature selection si existe
  if (!is.null(result$selected_features)) {
    lines <- c(lines, dash_line)
    lines <- c(lines, "2.2. Variables Seleccionadas")
    lines <- c(lines, dash_line)
    lines <- c(lines, sprintf("    Variables utilizadas: %d", length(result$selected_features)))
    for (feat in result$selected_features) {
      lines <- c(lines, sprintf("      - %s", feat))
    }
    lines <- c(lines, "")
    lines <- c(lines, "    Referencia: Kursa, M. B., & Rudnicki, W. R. (2010).")
    lines <- c(lines, "    Feature selection with the Boruta package. JSS, 36(11), 1-13.")
    lines <- c(lines, "")
  }

  lines <- c(lines, dash_line)
  lines <- c(lines, "2.3. Receta de Preprocesamiento")
  lines <- c(lines, dash_line)
  lines <- c(lines, "    Pasos aplicados automaticamente:")
  lines <- c(lines, "      - Imputacion con mediana (variables numericas)")
  lines <- c(lines, "      - Imputacion con moda (variables categoricas)")
  lines <- c(lines, "      - Dummy encoding (variables categoricas)")
  lines <- c(lines, "      - Normalizacion z-score (si aplica)")
  lines <- c(lines, "")
  lines <- c(lines, "    Nota: Estos pasos se aplican dentro de la validacion cruzada")
  lines <- c(lines, "    para evitar data leakage.")
  lines <- c(lines, "")

  # ============================================================
  # 3. MODELADO
  # ============================================================
  lines <- c(lines, sep_line)
  lines <- c(lines, "3. Modelado")
  lines <- c(lines, sep_line)
  lines <- c(lines, "")

  lines <- c(lines, dash_line)
  lines <- c(lines, "3.1. Validacion Cruzada")
  lines <- c(lines, dash_line)
  lines <- c(lines, "    Metodo: K-Fold Cross-Validation Estratificado")
  lines <- c(lines, "    Folds: 10")
  lines <- c(lines, sprintf("    Estratificado por: %s", result$target))
  lines <- c(lines, "")
  lines <- c(lines, "    Referencia: Kohavi, R. (1995). A study of cross-validation")
  lines <- c(lines, "    and bootstrap for accuracy estimation. IJCAI, 14(2), 1137-1145.")
  lines <- c(lines, "")

  # Comparacion de modelos
  if (!is.null(result$model_comparison)) {
    lines <- c(lines, dash_line)
    lines <- c(lines, "3.2. Comparacion de Modelos")
    lines <- c(lines, dash_line)
    lines <- c(lines, "    Modelos evaluados en validacion cruzada:")
    lines <- c(lines, "")

    comp <- result$model_comparison
    if (is.data.frame(comp)) {
      # Header de tabla
      lines <- c(lines, sprintf("    %-12s %8s %8s %8s %8s",
                                "Modelo", "MCC", "ROC_AUC", "Accuracy", "F1"))
      lines <- c(lines, paste("    ", paste(rep("-", 50), collapse = ""), sep = ""))

      for (i in 1:nrow(comp)) {
        model_name <- if ("model" %in% names(comp)) comp$model[i] else rownames(comp)[i]
        mcc_val <- if ("mcc" %in% names(comp)) comp$mcc[i] else NA
        auc_val <- if ("roc_auc" %in% names(comp)) comp$roc_auc[i] else NA
        acc_val <- if ("accuracy" %in% names(comp)) comp$accuracy[i] else NA
        f1_val <- if ("f_meas" %in% names(comp)) comp$f_meas[i] else NA

        lines <- c(lines, sprintf("    %-12s %8.4f %8.4f %8.4f %8.4f",
                                  model_name,
                                  ifelse(is.na(mcc_val), 0, mcc_val),
                                  ifelse(is.na(auc_val), 0, auc_val),
                                  ifelse(is.na(acc_val), 0, acc_val),
                                  ifelse(is.na(f1_val), 0, f1_val)))
      }
    }
    lines <- c(lines, "")
    lines <- c(lines, sprintf("    Mejor modelo seleccionado: %s", result$best_model))
    lines <- c(lines, "")
  }

  # ============================================================
  # 4. TUNING DE HIPERPARAMETROS
  # ============================================================
  if (!is.null(result$best_params)) {
    lines <- c(lines, sep_line)
    lines <- c(lines, "4. Tuning de Hiperparametros")
    lines <- c(lines, sep_line)
    lines <- c(lines, "")

    lines <- c(lines, dash_line)
    lines <- c(lines, "4.1. Mejores Hiperparametros Encontrados")
    lines <- c(lines, dash_line)
    lines <- c(lines, sprintf("    Modelo: %s", result$best_model))
    lines <- c(lines, "    Metodo: Random Search con validacion cruzada")
    lines <- c(lines, "")
    lines <- c(lines, "    Valores optimizados:")
    for (param_name in names(result$best_params)) {
      param_val <- result$best_params[[param_name]]
      lines <- c(lines, sprintf("      - %s: %s", param_name, param_val))
    }
    lines <- c(lines, "")
    lines <- c(lines, "    Referencia: Bergstra, J., & Bengio, Y. (2012).")
    lines <- c(lines, "    Random search for hyper-parameter optimization. JMLR, 13, 281-305.")
    lines <- c(lines, "")
  }

  # ============================================================
  # 5. EVALUACION EN TEST
  # ============================================================
  lines <- c(lines, sep_line)
  lines <- c(lines, "5. Evaluacion en Test")
  lines <- c(lines, sep_line)
  lines <- c(lines, "")

  lines <- c(lines, dash_line)
  lines <- c(lines, "5.1. Metricas de Rendimiento")
  lines <- c(lines, dash_line)
  lines <- c(lines, "    El conjunto de test contiene datos que el modelo NUNCA vio")
  lines <- c(lines, "    durante el entrenamiento.")
  lines <- c(lines, "")
  lines <- c(lines, sprintf("    Predicciones evaluadas: %d observaciones", n_test))
  lines <- c(lines, "")

  if (!is.null(result$test_metrics)) {
    metrics_df <- result$test_metrics
    lines <- c(lines, "    Metricas en Test:")

    for (i in 1:nrow(metrics_df)) {
      metric_name <- metrics_df$.metric[i]
      metric_value <- metrics_df$.estimate[i]

      # Formatear e interpretar
      if (metric_name == "mcc") {
        interp <- ifelse(metric_value >= 0.7, "Excelente",
                         ifelse(metric_value >= 0.5, "Bueno",
                                ifelse(metric_value >= 0.3, "Aceptable", "Pobre")))
        lines <- c(lines, sprintf("      - MCC: %.4f (%s)", metric_value, interp))
      } else if (metric_name == "roc_auc") {
        interp <- ifelse(metric_value >= 0.9, "Excelente",
                         ifelse(metric_value >= 0.8, "Bueno",
                                ifelse(metric_value >= 0.7, "Aceptable", "Pobre")))
        lines <- c(lines, sprintf("      - ROC_AUC: %.4f (%s)", metric_value, interp))
      } else if (metric_name %in% c("accuracy", "sensitivity", "specificity")) {
        lines <- c(lines, sprintf("      - %s: %.2f%%", toupper(metric_name), metric_value * 100))
      } else {
        lines <- c(lines, sprintf("      - %s: %.4f", toupper(metric_name), metric_value))
      }
    }
    lines <- c(lines, "")
  }

  # Detectar si es multiclase
  n_levels_txt <- 2L
  if (!is.null(result$train_data) && result$target %in% names(result$train_data)) {
    n_levels_txt <- length(levels(factor(result$train_data[[result$target]])))
  }

  lines <- c(lines, "    Leyenda:")
  if (n_levels_txt >= 3) {
    lines <- c(lines, "      - ROC_AUC: Capacidad de distinguir entre clases (Hand-Till, 0.5=azar, 1=perfecto)")
    lines <- c(lines, "      - Accuracy: Porcentaje de predicciones correctas")
    lines <- c(lines, "      - Sensitivity: Promedio macro de sensibilidad entre clases")
    lines <- c(lines, "      - Specificity: Promedio macro de especificidad entre clases")
  } else {
    lines <- c(lines, "      - ROC_AUC: Capacidad de distinguir entre clases (0.5=azar, 1=perfecto)")
    lines <- c(lines, "      - Accuracy: Porcentaje de predicciones correctas")
    lines <- c(lines, "      - Sensitivity: Porcentaje de positivos detectados correctamente")
    lines <- c(lines, "      - Specificity: Porcentaje de negativos detectados correctamente")
  }
  lines <- c(lines, "")
  lines <- c(lines, "    Referencia: Hanley, J. A., & McNeil, B. J. (1982).")
  lines <- c(lines, "    The meaning and use of the area under a ROC curve. Radiology, 143(1), 29-36.")
  lines <- c(lines, "")

  # Matriz de confusion
  if (!is.null(result$confusion_matrix)) {
    lines <- c(lines, dash_line)
    lines <- c(lines, "5.2. Matriz de Confusion")
    lines <- c(lines, dash_line)
    lines <- c(lines, "")

    cm <- result$confusion_matrix
    if (is.matrix(cm) || is.table(cm)) {
      n_classes <- ncol(cm)
      class_names <- colnames(cm)
      if (n_classes == 2) {
        # Binario: formato compacto
        lines <- c(lines, "              Prediccion")
        lines <- c(lines, sprintf("    Truth     %-8s %-8s", class_names[1], class_names[2]))
        lines <- c(lines, sprintf("    %-8s  %-8d %-8d", rownames(cm)[1], cm[1,1], cm[1,2]))
        lines <- c(lines, sprintf("    %-8s  %-8d %-8d", rownames(cm)[2], cm[2,1], cm[2,2]))
      } else {
        # Multiclase: formato NxN
        col_width <- max(8, max(nchar(class_names)) + 2)
        fmt <- paste0("%-", col_width, "s")
        header <- paste("    ", sprintf(fmt, "Truth"), " ",
                        paste(sprintf(fmt, class_names), collapse = " "), sep = "")
        lines <- c(lines, "              Prediccion")
        lines <- c(lines, header)
        for (r in seq_len(nrow(cm))) {
          row_vals <- paste(sprintf(paste0("%-", col_width, "d"), cm[r, ]), collapse = " ")
          lines <- c(lines, sprintf("    %s %s", sprintf(fmt, rownames(cm)[r]), row_vals))
        }
      }
    }
    lines <- c(lines, "")
    lines <- c(lines, "    Como leer la matriz:")
    lines <- c(lines, "      - Diagonal: Predicciones correctas")
    lines <- c(lines, "      - Fuera de diagonal: Errores de clasificacion")
    lines <- c(lines, "")
  }

  # Threshold optimizado
  if (!is.null(result$optimal_threshold)) {
    lines <- c(lines, dash_line)
    lines <- c(lines, "5.3. Optimizacion de Threshold")
    lines <- c(lines, dash_line)
    lines <- c(lines, "    Metodo: Youden's J (maximiza sensitivity + specificity - 1)")
    lines <- c(lines, "")
    lines <- c(lines, "    Resultado:")
    lines <- c(lines, "      - Threshold por defecto: 0.50")
    lines <- c(lines, sprintf("      - Threshold optimo: %.4f", result$optimal_threshold))
    lines <- c(lines, "")
    lines <- c(lines, "    Referencia: Youden, W. J. (1950).")
    lines <- c(lines, "    Index for rating diagnostic tests. Cancer, 3(1), 32-35.")
    lines <- c(lines, "")
  }

  # ============================================================
  # 6. IMPORTANCIA DE VARIABLES
  # ============================================================
  if (!is.null(result$importance) && nrow(result$importance) > 0) {
    lines <- c(lines, sep_line)
    lines <- c(lines, "6. Importancia de Variables")
    lines <- c(lines, sep_line)
    lines <- c(lines, "")

    lines <- c(lines, dash_line)
    lines <- c(lines, "6.1. Variables mas Importantes")
    lines <- c(lines, dash_line)
    lines <- c(lines, "    La importancia indica cuanto contribuye cada variable")
    lines <- c(lines, "    a las predicciones del modelo.")
    lines <- c(lines, "")

    top_vars <- utils::head(result$importance, 10)
    lines <- c(lines, "    Top 10 variables:")
    for (i in 1:nrow(top_vars)) {
      # Barra visual
      bar_length <- round(top_vars$Importance[i] / max(top_vars$Importance) * 20)
      bar <- paste(rep("|", bar_length), collapse = "")
      lines <- c(lines, sprintf("      %2d. %-30s %.4f %s",
                                i, top_vars$Variable[i], top_vars$Importance[i], bar))
    }
    lines <- c(lines, "")
    lines <- c(lines, "    Referencia: Breiman, L. (2001). Random forests.")
    lines <- c(lines, "    Machine Learning, 45(1), 5-32.")
    lines <- c(lines, "")
  }

  # ============================================================
  # CONCLUSION
  # ============================================================
  lines <- c(lines, sep_line)
  lines <- c(lines, " Analisis Completado")
  lines <- c(lines, sep_line)
  lines <- c(lines, "")

  # Obtener metrica principal para conclusion
  select_metric_conc <- result$metadata$select_metric %||%
    (if (result$task == "classification") "mcc" else "rsq")
  metric_labels_conc <- c(
    roc_auc = "ROC-AUC", mcc = "MCC", f_meas = "F1-Score",
    accuracy = "Accuracy", rsq = "R-squared", rmse = "RMSE", mae = "MAE",
    kap = "Cohen's Kappa", bal_accuracy = "Balanced Accuracy"
  )
  main_val <- NA
  if (!is.null(result$test_metrics)) {
    main_row <- result$test_metrics[result$test_metrics$.metric == select_metric_conc, ]
    if (nrow(main_row) > 0) main_val <- main_row$.estimate[1]
  }

  lines <- c(lines, sprintf("Mejor modelo: %s", result$best_model))
  if (!is.na(main_val)) {
    label <- metric_labels_conc[select_metric_conc]
    if (is.na(label)) label <- toupper(select_metric_conc)
    lines <- c(lines, sprintf("%s en test: %.4f", label, main_val))
    interpretation <- .interpret_metric_value(select_metric_conc, main_val)
    lines <- c(lines, sprintf("Interpretacion: %s", interpretation))
  }
  lines <- c(lines, "")

  lines <- c(lines, dash_line)
  lines <- c(lines, "CONCLUSION")
  lines <- c(lines, dash_line)
  lines <- c(lines, sprintf("Se entreno un modelo de %s para predecir", .get_model_label(result$best_model)))
  lines <- c(lines, sprintf("'%s'. El modelo fue evaluado en %d observaciones nuevas", result$target, n_test))
  lines <- c(lines, "(que no uso durante el entrenamiento).")
  lines <- c(lines, "")
  lines <- c(lines, "Para usar el modelo:")
  lines <- c(lines, "  - Ver resultados: print(resultado)")
  lines <- c(lines, "  - Ver graficos: plot(resultado)")
  lines <- c(lines, "  - Hacer predicciones: predict(resultado, nuevos_datos)")
  lines <- c(lines, "")

  # Footer
  lines <- c(lines, sep_line)
  lines <- c(lines, sprintf("Generado: %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
  lines <- c(lines, "PsyML v2.0.0 - github.com/jventural/PsyML")
  lines <- c(lines, sep_line)

  paste(lines, collapse = "\n")
}


#' @title Export the Results of an Analysis as a JSON File
#'
#' @description
#' Writes the result of a PsyML analysis to a structured JSON file (metadata,
#' metrics, model information, variable importance, etc.). When the analysis
#' was run with \code{verbose = TRUE}, the JSON also includes the complete
#' console output (\code{verbose_text} and \code{verbose_lines}). The file can
#' be used by [generate_report_from_json()], [generate_report_with_ai()] and
#' [launch_report_generator()].
#'
#' @param result Result of [supervised_ml()] or [supervised_ml_capture()].
#' @param file_path Path of the output file (required, no default).
#'
#' @return Invisibly returns \code{file_path}.
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
#' out <- file.path(tempdir(), "psyml_report.json")
#' export_verbose_json(res, out)
#' names(jsonlite::fromJSON(out))
#' unlink(out)
#' }
#' @family reporting and export
#' @export
export_verbose_json <- function(result, file_path) {

  if (missing(file_path) || is.null(file_path)) {
    stop("Please provide 'file_path' (for example, ",
         "file.path(tempdir(), \"report.json\")).", call. = FALSE)
  }

  # Construir estructura JSON desde el resultado
  json_structure <- .build_json_from_result(result)

  # Agregar el verbose_text capturado (identico al de consola)
  if (!is.null(result$verbose_text) && nzchar(result$verbose_text)) {
    json_structure$verbose_text <- result$verbose_text
    json_structure$verbose_lines <- result$verbose_lines
  } else {
    # Si no hay verbose capturado (ejecutado con verbose=FALSE)
    message("Note: the analysis was run with verbose = FALSE.")
    message("The JSON will not include the complete console output.")
  }

  json_content <- jsonlite::toJSON(json_structure, pretty = TRUE, auto_unbox = TRUE)
  writeLines(json_content, file_path)
  message("JSON exported to: ", file_path)

  invisible(file_path)
}


#' @title Build the JSON structure from a supervised_ml() result
#' @noRd
.build_json_from_result <- function(result) {


  # Estructura base del JSON
  meta <- result$metadata
  json_structure <- list(
    metadata = list(
      generated_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      supervisedml_version = "2.0.0",
      task = result$task,
      target = result$target,
      best_model = result$best_model,
      n_observations = if (!is.null(meta)) meta$n_obs else
        (nrow(result$train_data) + nrow(result$test_data)),
      n_train = if (!is.null(meta)) meta$n_train else nrow(result$train_data),
      n_test = if (!is.null(meta)) meta$n_test else nrow(result$test_data),
      n_features = if (!is.null(meta)) meta$n_features else (ncol(result$train_data) - 1),
      n_classes = if (result$task == "classification" && !is.null(result$train_data)) {
        length(levels(factor(result$train_data[[result$target]])))
      } else NULL,
      models_trained = if (!is.null(meta)) meta$models_trained else NULL,
      cv_folds = if (!is.null(meta)) meta$cv_folds else NULL,
      tune_method = if (!is.null(meta) && !is.na(meta$tune_method)) meta$tune_method else NULL,
      seed = if (!is.null(meta)) meta$seed else NULL,
      elapsed_time = if (!is.null(meta)) as.numeric(meta$elapsed_time) else NULL
    ),
    sections = list(),
    metrics = list(),
    references = list()
  )

  # Agregar metricas
  if (!is.null(result$test_metrics)) {
    metrics_df <- result$test_metrics
    for (i in 1:nrow(metrics_df)) {
      metric_name <- metrics_df$.metric[i]
      metric_value <- metrics_df$.estimate[i]
      json_structure$metrics[[metric_name]] <- round(metric_value, 4)
    }
  }

  # Agregar informacion del modelo
  json_structure$model_info <- list(
    name = result$best_model,
    label = .get_model_label(result$best_model),
    tuned = !is.null(result$best_params),
    hyperparameters = result$best_params
  )

  # Agregar importancia de variables
  if (!is.null(result$importance) && nrow(result$importance) > 0) {
    top_vars <- utils::head(result$importance, 10)
    json_structure$variable_importance <- lapply(1:nrow(top_vars), function(i) {
      list(
        variable = top_vars$Variable[i],
        importance = round(top_vars$Importance[i], 4),
        rank = i
      )
    })
  }

  # Agregar threshold si existe
  if (!is.null(result$optimal_threshold)) {
    json_structure$threshold <- list(
      default = 0.5,
      optimized = round(result$optimal_threshold, 4)
    )
  }

  # Agregar comparacion de modelos (CV summary)
  if (!is.null(result$cv_summary) && is.data.frame(result$cv_summary)) {
    json_structure$model_comparison <- tryCatch({
      comp <- result$cv_summary
      lapply(1:nrow(comp), function(i) {
        row <- as.list(comp[i, ])
        row[] <- lapply(row, function(v) if (is.numeric(v)) round(v, 4) else v)
        row
      })
    }, error = function(e) NULL)
  }

  # Agregar confusion matrix si existe
  if (!is.null(result$evaluation$confusion)) {
    json_structure$confusion_matrix <- tryCatch({
      cm <- result$evaluation$confusion
      if (is.matrix(cm) || is.table(cm)) {
        list(
          matrix = as.list(as.data.frame(unclass(cm))),
          row_names = rownames(cm),
          col_names = colnames(cm)
        )
      } else NULL
    }, error = function(e) NULL)
  }

  # Agregar calibracion si existe
  if (!is.null(result$calibration)) {
    json_structure$calibration <- tryCatch({
      list(method = result$calibration$method %||% "unknown")
    }, error = function(e) NULL)
  }

  # Agregar catalogo de figuras
  if (!is.null(result$figures_catalog)) {
    json_structure$figures_catalog <- lapply(names(result$figures_catalog), function(name) {
      item <- result$figures_catalog[[name]]
      list(
        key = name,
        number = item$number,
        id = item$id,
        title = item$title,
        description = item$description,
        filename = item$filename
      )
    })
  }

  # Agregar referencias estandar
  json_structure$references <- c(
    "Tukey, J. W. (1977). Exploratory data analysis. Addison-Wesley.",
    "Breiman, L. (2001). Random forests. Machine Learning, 45(1), 5-32.",
    "Hanley, J. A., & McNeil, B. J. (1982). The meaning and use of the area under a ROC curve. Radiology, 143(1), 29-36.",
    "Kohavi, R. (1995). A study of cross-validation and bootstrap for accuracy estimation. IJCAI, 14(2), 1137-1145.",
    "Kuhn, M., & Wickham, H. (2020). Tidymodels: A collection of packages for modeling and machine learning.",
    "Hastie, T., Tibshirani, R., & Friedman, J. (2009). The elements of statistical learning (2nd ed.). Springer."
  )

  # Agregar referencia especifica del modelo
 if (result$best_model == "xgboost") {
    json_structure$references <- c(json_structure$references,
      "Chen, T., & Guestrin, C. (2016). XGBoost: A scalable tree boosting system. KDD, 785-794.")
  }

  return(json_structure)
}


#' @title Parse the verbose output into a JSON structure
#' @noRd
.parse_verbose_to_json <- function(verbose_lines, result) {

  # Estructura base del JSON
  json_structure <- list(
    metadata = list(
      generated_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      supervisedml_version = "2.0.0",
      task = result$task,
      target = result$target,
      best_model = result$best_model,
      n_observations = nrow(result$train_data) + nrow(result$test_data),
      n_train = nrow(result$train_data),
      n_test = nrow(result$test_data),
      n_classes = if (result$task == "classification" && !is.null(result$train_data)) {
        length(levels(factor(result$train_data[[result$target]])))
      } else NULL
    ),
    sections = list(),
    metrics = list(),
    references = list()
  )

  # Parsear secciones del verbose
  current_section <- NULL
  current_subsection <- NULL
  current_content <- c()

  for (line in verbose_lines) {
    # Detectar secciones principales (ej: "1. Carga y Exploracion")
    if (grepl("^[0-9]+\\. ", line) && !grepl("^[0-9]+\\.[0-9]+", line)) {
      # Guardar seccion anterior
      if (!is.null(current_section)) {
        json_structure$sections[[current_section]]$content <- paste(current_content, collapse = "\n")
      }

      current_section <- gsub("^([0-9]+)\\. (.+)$", "\\1_\\2", line)
      current_section <- gsub(" ", "_", current_section)
      current_section <- gsub("[^a-zA-Z0-9_]", "", current_section)

      json_structure$sections[[current_section]] <- list(
        title = trimws(line),
        subsections = list(),
        content = ""
      )
      current_content <- c()
      current_subsection <- NULL
    }
    # Detectar subsecciones (ej: "1.2. Analisis Exploratorio")
    else if (grepl("^[0-9]+\\.[0-9]+\\.?\\s", line)) {
      if (!is.null(current_section)) {
        subsec_name <- gsub("^([0-9]+\\.[0-9]+)\\.?\\s+(.+)$", "\\1_\\2", line)
        subsec_name <- gsub(" ", "_", subsec_name)
        subsec_name <- gsub("[^a-zA-Z0-9_.]", "", subsec_name)

        json_structure$sections[[current_section]]$subsections[[subsec_name]] <- list(
          title = trimws(line),
          content = ""
        )
        current_subsection <- subsec_name
      }
    }
    # Detectar referencias
    else if (grepl("^\\s*Referencia:", line)) {
      ref_text <- trimws(gsub("^\\s*Referencia:\\s*", "", line))
      json_structure$references <- c(json_structure$references, ref_text)
    }
    # Contenido regular
    else {
      current_content <- c(current_content, line)
    }
  }

  # Guardar ultima seccion
  if (!is.null(current_section) && length(current_content) > 0) {
    json_structure$sections[[current_section]]$content <- paste(current_content, collapse = "\n")
  }

  # Agregar metricas
  if (!is.null(result$test_metrics)) {
    metrics_df <- result$test_metrics
    for (i in 1:nrow(metrics_df)) {
      metric_name <- metrics_df$.metric[i]
      metric_value <- metrics_df$.estimate[i]
      json_structure$metrics[[metric_name]] <- round(metric_value, 4)
    }
  }

  # Agregar informacion del modelo
  json_structure$model_info <- list(
    name = result$best_model,
    label = .get_model_label(result$best_model),
    tuned = !is.null(result$best_params),
    hyperparameters = result$best_params
  )

  # Agregar importancia de variables
  if (!is.null(result$importance) && nrow(result$importance) > 0) {
    top_vars <- head(result$importance, 10)
    json_structure$variable_importance <- lapply(1:nrow(top_vars), function(i) {
      list(
        variable = top_vars$Variable[i],
        importance = round(top_vars$Importance[i], 4),
        rank = i
      )
    })
  }

  # Agregar threshold si existe
  if (!is.null(result$optimal_threshold)) {
    json_structure$threshold <- list(
      default = 0.5,
      optimized = round(result$optimal_threshold, 4)
    )
  }

  # Eliminar referencias duplicadas
  json_structure$references <- unique(unlist(json_structure$references))

  return(json_structure)
}


#' @title Generate a Report from an Exported JSON File
#'
#' @description
#' Builds an R Markdown report from a JSON file written by
#' [export_verbose_json()] and renders it to Word, HTML or PDF. Requires the
#' rmarkdown package and pandoc (and a LaTeX installation for PDF).
#'
#' @param json_path Path to the JSON file.
#' @param output_path Path of the output file (required, no default).
#' @param format Output format: \code{"docx"} (default), \code{"html"} or
#'   \code{"pdf"}.
#' @param template Report template: \code{"scientific"} (default),
#'   \code{"brief"} or \code{"full"}.
#'
#' @return Invisibly returns \code{output_path}.
#'
#' @examples
#' \donttest{
#' if (requireNamespace("rmarkdown", quietly = TRUE) &&
#'     rmarkdown::pandoc_available()) {
#'   set.seed(1)
#'   n <- 200
#'   df <- data.frame(x1 = rnorm(n), x2 = rnorm(n), x3 = rnorm(n))
#'   df$y <- factor(ifelse(df$x1 + 0.5 * df$x2 + rnorm(n) > 0, "yes", "no"))
#'   res <- supervised_ml(df, target = "y", models = "glm", cv_folds = 3,
#'                        tune_best = FALSE, run_shap = FALSE, n_cores = 1,
#'                        verbose = FALSE)
#'   json_file <- file.path(tempdir(), "psyml_report.json")
#'   html_file <- file.path(tempdir(), "psyml_report.html")
#'   export_verbose_json(res, json_file)
#'   generate_report_from_json(json_file, html_file, format = "html")
#'   unlink(c(json_file, html_file))
#' }
#' }
#' @family reporting and export
#' @export
generate_report_from_json <- function(json_path,
                                       output_path,
                                       format = c("docx", "html", "pdf"),
                                       template = c("scientific", "brief", "full")) {

  format <- match.arg(format)
  template <- match.arg(template)
  if (missing(output_path) || is.null(output_path)) {
    stop("Please provide 'output_path' (for example, ",
         "file.path(tempdir(), \"report.docx\")).", call. = FALSE)
  }
  if (!requireNamespace("rmarkdown", quietly = TRUE)) {
    stop("Package 'rmarkdown' is required for this function.", call. = FALSE)
  }

  # Leer JSON
  json_data <- jsonlite::fromJSON(json_path)

  # Generar contenido Rmd
  rmd_content <- .generate_rmd_content(json_data, template)

  # Escribir archivo temporal Rmd
  temp_rmd <- tempfile(fileext = ".Rmd")
  writeLines(rmd_content, temp_rmd)

  # Renderizar
  output_format <- switch(format,
    "docx" = "word_document",
    "html" = "html_document",
    "pdf" = "pdf_document"
  )

  rmarkdown::render(
    input = temp_rmd,
    output_format = output_format,
    output_file = basename(output_path),
    output_dir = dirname(output_path),
    quiet = TRUE
  )

  # Limpiar
  unlink(temp_rmd)

  message("Report generated: ", output_path)
  invisible(output_path)
}


#' @title Build the R Markdown content
#' @noRd
.generate_rmd_content <- function(json_data, template) {

  # Header YAML
  yaml_header <- paste0(
    "---\n",
    "title: \"Reporte de Analisis de Machine Learning\"\n",
    "date: \"", json_data$metadata$generated_at, "\"\n",
    "output:\n",
    "  word_document:\n",
    "    reference_docx: null\n",
    "---\n\n"
  )

  # Contenido segun template
  if (template == "scientific") {
    content <- .generate_scientific_report(json_data)
  } else if (template == "brief") {
    content <- .generate_brief_report(json_data)
  } else {
    content <- .generate_full_report(json_data)
  }

  paste0(yaml_header, content)
}


#' @title Build the scientific report template
#' @noRd
.generate_scientific_report <- function(json_data) {

  meta <- json_data$metadata
  metrics <- json_data$metrics
  model <- json_data$model_info
  refs <- json_data$references

  # Seccion Metodo
  content <- "# Metodo\n\n"

  content <- paste0(content, "## Participantes\n\n")
  content <- paste0(content,
    "La muestra estuvo conformada por ", meta$n_observations, " observaciones, ",
    "las cuales fueron divididas en un conjunto de entrenamiento (", meta$n_train,
    " observaciones, ", round(meta$n_train/meta$n_observations*100, 1), "%) ",
    "y un conjunto de prueba (", meta$n_test, " observaciones, ",
    round(meta$n_test/meta$n_observations*100, 1), "%) ",
    "para la validacion del modelo.\n\n"
  )

  content <- paste0(content, "## Analisis de Datos\n\n")
  content <- paste0(content,
    "El analisis estadistico se realizo en R utilizando el paquete PsyML. ",
    "Se empleo un enfoque de aprendizaje automatico para ",
    if (meta$task == "classification") {
      n_cls_ad <- meta$n_classes %||% 2L
      if (n_cls_ad >= 3) paste0("clasificacion multiclase (", n_cls_ad, " clases)") else "clasificacion binaria"
    } else "regresion",
    " con el objetivo de predecir la variable '", meta$target, "'.\n\n"
  )

  # Modelo utilizado
  content <- paste0(content,
    "Entre los modelos evaluados, ", model$label, " emergio como la opcion preferida ",
    "debido a su rendimiento superior en las metricas clave. "
  )

  if (model$tuned && !is.null(model$hyperparameters)) {
    content <- paste0(content,
      "Se realizo un ajuste de hiperparametros para optimizar el rendimiento del modelo. ",
      "Los hiperparametros optimizados fueron: "
    )
    params_text <- paste(names(model$hyperparameters), "=",
                          unlist(model$hyperparameters), collapse = ", ")
    content <- paste0(content, params_text, ".\n\n")
  } else {
    content <- paste0(content, "\n\n")
  }

  # Validacion cruzada
  content <- paste0(content,
    "Para mitigar el sobreajuste, se empleo validacion cruzada de 10 folds durante ",
    "el entrenamiento, asegurando que el rendimiento del modelo se evaluara en ",
    "multiples subconjuntos de datos (Kohavi, 1995). Este metodo divide los datos en ",
    "10 partes iguales, donde en cada iteracion se usa una parte para validacion ",
    "y las nueve restantes para entrenamiento.\n\n"
  )

  # Metricas
  content <- paste0(content, "## Metricas de Evaluacion\n\n")

  if (meta$task == "classification") {
    n_cls_met <- meta$n_classes %||% 2L
    if (n_cls_met >= 3) {
      content <- paste0(content,
        "Los modelos se evaluaron utilizando metricas criticas para datos de clasificacion multiclase. ",
        "Se utilizo el metodo Hand-Till para el calculo de AUC y el estimador macro para ",
        "sensibilidad, especificidad y F1 (Hand & Till, 2001; Hanley & McNeil, 1982). ",
        "Las metricas obtenidas en el conjunto de prueba fueron:\n\n"
      )
    } else {
      content <- paste0(content,
        "Los modelos se evaluaron utilizando metricas criticas para datos de clasificacion ",
        "(Hanley & McNeil, 1982). Las metricas obtenidas en el conjunto de prueba fueron:\n\n"
      )
    }

    if (!is.null(metrics$mcc)) {
      content <- paste0(content, "- **MCC (Matthews Correlation Coefficient)**: ",
                        round(metrics$mcc, 4), "\n")
    }
    if (!is.null(metrics$roc_auc)) {
      auc_label <- if (n_cls_met >= 3) "AUC Hand-Till (multiclase)" else "AUC (Area Under the Curve)"
      content <- paste0(content, "- **", auc_label, "**: ",
                        round(metrics$roc_auc, 4), "\n")
    }
    if (!is.null(metrics$accuracy)) {
      content <- paste0(content, "- **Accuracy**: ",
                        round(metrics$accuracy * 100, 2), "%\n")
    }
    if (!is.null(metrics$sensitivity)) {
      content <- paste0(content, "- **Sensibilidad**: ",
                        round(metrics$sensitivity * 100, 2), "%\n")
    }
    if (!is.null(metrics$specificity)) {
      content <- paste0(content, "- **Especificidad**: ",
                        round(metrics$specificity * 100, 2), "%\n")
    }
    if (!is.null(metrics$f_meas)) {
      content <- paste0(content, "- **F1-Score**: ",
                        round(metrics$f_meas, 4), "\n")
    }
  } else {
    content <- paste0(content,
      "Los modelos se evaluaron utilizando metricas estandar para regresion. ",
      "Las metricas obtenidas en el conjunto de prueba fueron:\n\n"
    )

    if (!is.null(metrics$rmse)) {
      content <- paste0(content, "- **RMSE**: ", round(metrics$rmse, 4), "\n")
    }
    if (!is.null(metrics$rsq)) {
      content <- paste0(content, "- **R-squared**: ", round(metrics$rsq, 4), "\n")
    }
    if (!is.null(metrics$mae)) {
      content <- paste0(content, "- **MAE**: ", round(metrics$mae, 4), "\n")
    }
  }

  content <- paste0(content, "\n")

  # Importancia de variables
  if (!is.null(json_data$variable_importance) && length(json_data$variable_importance) > 0) {
    content <- paste0(content, "## Importancia de Variables\n\n")
    content <- paste0(content,
      "Para comprender el impacto de cada variable en la prediccion, se calculo ",
      "la importancia de variables del modelo (Breiman, 2001). Las variables mas ",
      "influyentes fueron:\n\n"
    )

    content <- paste0(content, "| Rango | Variable | Importancia |\n")
    content <- paste0(content, "|-------|----------|-------------|\n")

    # jsonlite::fromJSON() simplifies the list of records to a data frame
    vi_list <- json_data$variable_importance
    if (is.data.frame(vi_list)) {
      vi_list <- lapply(seq_len(nrow(vi_list)),
                        function(i) as.list(vi_list[i, , drop = FALSE]))
    }
    for (var_info in vi_list) {
      content <- paste0(content, "| ", var_info$rank, " | ",
                        var_info$variable, " | ", var_info$importance, " |\n")
    }
    content <- paste0(content, "\n")
  }

  # Threshold optimizado (solo binario)
  n_cls_rpt <- meta$n_classes %||% 2L
  if (!is.null(json_data$threshold) && n_cls_rpt == 2) {
    content <- paste0(content, "## Optimizacion de Umbral\n\n")
    content <- paste0(content,
      "Se realizo una optimizacion del umbral de clasificacion para mejorar ",
      "el rendimiento del modelo. El umbral optimo encontrado fue ",
      json_data$threshold$optimized, " (comparado con el umbral por defecto de ",
      json_data$threshold$default, ").\n\n"
    )
  }

  # Referencias
  content <- paste0(content, "# Referencias\n\n")

  # Referencias fijas del framework
  fixed_refs <- c(
    "Breiman, L. (2001). Random forests. Machine Learning, 45(1), 5-32.",
    "Hanley, J. A., & McNeil, B. J. (1982). The meaning and use of the area under a ROC curve. Radiology, 143(1), 29-36.",
    "Kohavi, R. (1995). A study of cross-validation and bootstrap for accuracy estimation and model selection. IJCAI, 14(2), 1137-1145.",
    "Kuhn, M., & Wickham, H. (2020). Tidymodels: a collection of packages for modeling and machine learning using tidyverse principles."
  )

  all_refs <- unique(c(fixed_refs, refs))
  all_refs <- sort(all_refs)

  for (ref in all_refs) {
    if (nchar(ref) > 5) {
      content <- paste0(content, ref, "\n\n")
    }
  }

  return(content)
}


#' @title Build the brief report template
#' @noRd
.generate_brief_report <- function(json_data) {

  meta <- json_data$metadata
  metrics <- json_data$metrics
  model <- json_data$model_info

  content <- "# Resumen del Analisis\n\n"

  content <- paste0(content,
    "Se realizo un analisis de ", meta$task, " para predecir '", meta$target, "' ",
    "utilizando ", meta$n_observations, " observaciones.\n\n"
  )

  content <- paste0(content, "## Modelo Seleccionado\n\n")
  content <- paste0(content, "- **Modelo**: ", model$label, "\n")
  content <- paste0(content, "- **Datos de entrenamiento**: ", meta$n_train, "\n")
  content <- paste0(content, "- **Datos de prueba**: ", meta$n_test, "\n\n")

  content <- paste0(content, "## Metricas\n\n")
  for (metric_name in names(metrics)) {
    content <- paste0(content, "- **", metric_name, "**: ",
                      round(metrics[[metric_name]], 4), "\n")
  }

  return(content)
}


#' @title Build the full report template
#' @noRd
.generate_full_report <- function(json_data) {
  # Combina el reporte cientifico con detalles adicionales
  content <- .generate_scientific_report(json_data)

  # Agregar verbose completo si existe
  if (!is.null(json_data$verbose_text)) {
    content <- paste0(content, "\n\n# Anexo: Output Completo\n\n")
    content <- paste0(content, "```\n", json_data$verbose_text, "\n```\n")
  }

  return(content)
}


#' @title Launch the Shiny Report Generator
#'
#' @description
#' Opens the Shiny application that generates scientific reports from JSON
#' files exported with [export_verbose_json()]. Requires the shiny package.
#'
#' @param launch.browser Logical (default TRUE). Open the app in the web
#'   browser.
#'
#' @return Called for its side effect (runs the Shiny app); returns the value
#'   returned by \code{shiny::runApp()} when the app is closed.
#'
#' @examples
#' if (interactive()) {
#'   launch_report_generator()
#' }
#' @family reporting and export
#' @export
launch_report_generator <- function(launch.browser = TRUE) {

  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("Package 'shiny' is required. Please install package 'shiny'.",
         call. = FALSE)
  }

  app_dir <- system.file("shiny/report_generator", package = "PsyML")

  if (app_dir == "") {
    # Si no esta instalado como paquete, buscar en directorio local
    app_dir <- file.path(getwd(), "inst/shiny/report_generator")
  }

  if (!dir.exists(app_dir)) {
    stop("The Shiny report generator was not found. ",
         "Make sure the PsyML package is installed correctly.", call. = FALSE)
  }

  shiny::runApp(app_dir, launch.browser = launch.browser)
}


#' @title Run and Export a Complete Analysis
#'
#' @description
#' Convenience function that runs [supervised_ml()] through
#' [supervised_ml_capture()] and exports both a TXT and a JSON file to
#' \code{output_dir}. The file names are \code{<prefix>_<timestamp>.txt} and
#' \code{<prefix>_<timestamp>.json}.
#'
#' @param ... Arguments passed to [supervised_ml()] (do not pass
#'   \code{verbose}).
#' @param output_dir Output directory (required, no default). It is created if
#'   it does not exist.
#' @param prefix Prefix of the file names (default \code{"supervisedml"}).
#'
#' @return Invisibly returns the result of [supervised_ml_capture()] (a
#'   \code{supervisedml} object with the captured console output).
#'
#' @examples
#' \donttest{
#' set.seed(1)
#' n <- 200
#' df <- data.frame(x1 = rnorm(n), x2 = rnorm(n), x3 = rnorm(n))
#' df$y <- factor(ifelse(df$x1 + 0.5 * df$x2 + rnorm(n) > 0, "yes", "no"))
#' out_dir <- file.path(tempdir(), "psyml_export")
#' res <- supervised_ml_export(df, target = "y", models = "glm", cv_folds = 3,
#'                             tune_best = FALSE, run_shap = FALSE,
#'                             n_cores = 1, output_dir = out_dir)
#' list.files(out_dir)
#' unlink(out_dir, recursive = TRUE)
#' }
#' @family reporting and export
#' @export
supervised_ml_export <- function(..., output_dir, prefix = "supervisedml") {

  if (missing(output_dir) || is.null(output_dir)) {
    stop("Please provide 'output_dir' (for example, output_dir = tempdir()).",
         call. = FALSE)
  }
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

  # Ejecutar con captura
  result <- supervised_ml_capture(...)

  # Crear nombres de archivo
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  txt_file <- file.path(output_dir, paste0(prefix, "_", timestamp, ".txt"))
  json_file <- file.path(output_dir, paste0(prefix, "_", timestamp, ".json"))


  # Exportar
  export_verbose_txt(result, txt_file)
  export_verbose_json(result, json_file)

  message("\nExported files:")
  message("  TXT:  ", txt_file)
  message("  JSON: ", json_file)
  message("\nTo generate a report, run:")
  message("  launch_report_generator()")

  invisible(result)
}


#' @title Generate a Scientific Report with AI
#'
#' @description
#' Generates a scientific report (Word document) with the OpenAI chat
#' completions API directly from R, without opening the Shiny application.
#' The report text is written by the language model from the JSON file and
#' the tables of metrics and variable importance are inserted from the data.
#' Requires an OpenAI API key, an internet connection and the packages httr2
#' and officer.
#'
#' @param json_path Path to the JSON file exported with [export_verbose_json()].
#' @param api_key OpenAI API key. Defaults to the environment variable
#'   \code{PSYML_API_KEY}; an error is raised if it is empty.
#' @param output_path Path of the output .docx file (required; an error is
#'   raised if NULL, the default).
#' @param title Report title (default \code{"Analisis de Machine Learning"}).
#' @param author Author name (optional, default NULL).
#' @param language Language of the report: \code{"es"} (Spanish, default) or
#'   \code{"en"} (English).
#' @param model OpenAI model to use (default \code{"gpt-4.1-mini"}).
#' @param custom_instructions Optional character string with additional
#'   instructions for the language model (e.g., journal style or sections to
#'   emphasize). They are appended to the request with high priority. NULL
#'   (default) adds nothing.
#'
#' @return Invisibly returns \code{output_path}.
#'
#' @examples
#' \dontrun{
#' # Requires an OpenAI API key and an internet connection
#' set.seed(1)
#' n <- 200
#' df <- data.frame(x1 = rnorm(n), x2 = rnorm(n), x3 = rnorm(n))
#' df$y <- factor(ifelse(df$x1 + 0.5 * df$x2 + rnorm(n) > 0, "yes", "no"))
#' res <- supervised_ml_capture(df, target = "y", models = "glm", cv_folds = 3,
#'                              tune_best = FALSE, run_shap = FALSE,
#'                              n_cores = 1)
#' json_file <- file.path(tempdir(), "my_analysis.json")
#' export_verbose_json(res, json_file)
#'
#' Sys.setenv(PSYML_API_KEY = "your-openai-key")
#' generate_report_with_ai(
#'   json_path = json_file,
#'   output_path = file.path(tempdir(), "my_report.docx"),
#'   title = "Machine Learning Analysis",
#'   author = "Jane Doe",
#'   language = "en"
#' )
#' }
#'
#' @family reporting and export
#' @export
generate_report_with_ai <- function(json_path,
                                     api_key = Sys.getenv("PSYML_API_KEY"),
                                     output_path = NULL,
                                     title = "Analisis de Machine Learning",
                                     author = NULL,
                                     language = c("es", "en"),
                                     model = "gpt-4.1-mini",
                                     custom_instructions = NULL) {

  # Validate arguments
  language <- match.arg(language)

  if (is.null(output_path)) {
    stop("Please provide 'output_path', the .docx file to write (for example, ",
         "file.path(tempdir(), \"report.docx\")).", call. = FALSE)
  }

  if (!file.exists(json_path)) {
    stop("JSON file not found: ", json_path, call. = FALSE)
  }

  if (is.null(api_key) || !nzchar(api_key)) {
    stop("A valid OpenAI API key is required. Pass it in 'api_key' or set ",
         "the environment variable PSYML_API_KEY.", call. = FALSE)
  }

  # Required packages
  if (!requireNamespace("httr2", quietly = TRUE)) {
    stop("Package 'httr2' is required for this function.", call. = FALSE)
  }
  if (!requireNamespace("officer", quietly = TRUE)) {
    stop("Package 'officer' is required for this function.", call. = FALSE)
  }

  message("Reading JSON file...")
  json_data <- jsonlite::fromJSON(json_path)

  meta <- json_data$metadata
  metrics <- json_data$metrics
  model_info <- json_data$model_info
  var_imp <- json_data$variable_importance

  # Construir prompt del sistema
  message("Preparing the context for the language model...")

  # Detectar tipo de clasificacion
  classification_type <- "binary"
  if (meta$task == "classification") {
    n_cls <- meta$n_classes %||% 2L
    if (n_cls >= 3) classification_type <- "multiclass"
  }

  system_prompt <- .build_system_prompt(language, classification_type)

  # Construir mensaje del usuario
  user_message <- .build_user_message(meta, metrics, model_info, var_imp, json_data, language,
                                      custom_instructions = custom_instructions)

  # Llamar a la API de OpenAI
  message("Generating the report with ", model, "...")

  clean_api_key <- trimws(api_key)

  req <- httr2::request("https://api.openai.com/v1/chat/completions") |>
    httr2::req_headers(
      Authorization = paste("Bearer", clean_api_key),
      "Content-Type" = "application/json"
    ) |>
    httr2::req_body_json(list(
      model = model,
      temperature = 0.2,
      max_tokens = 16000,
      messages = list(
        list(role = "system", content = system_prompt),
        list(role = "user", content = user_message)
      )
    )) |>
    httr2::req_timeout(300)

  resp <- tryCatch({
    httr2::req_perform(req)
  }, error = function(e) {
    stop("Connection error with OpenAI: ", e$message, call. = FALSE)
  })

  # Verificar respuesta
  status <- httr2::resp_status(resp)
  if (status == 401) {
    stop("Invalid API key (error 401). Check that the key is correct and has credit.",
         call. = FALSE)
  } else if (status != 200) {
    body_text <- tryCatch(httr2::resp_body_string(resp), error = function(e) "")
    stop("HTTP error ", status, ": ", body_text, call. = FALSE)
  }

  # Extraer contenido
  js <- httr2::resp_body_json(resp, simplifyVector = FALSE)

  if (!is.null(js$error)) {
    stop("OpenAI error: ", js$error$message, call. = FALSE)
  }

  report_text <- ""
  if (!is.null(js$choices) && length(js$choices) > 0) {
    if (!is.null(js$choices[[1]]$message$content)) {
      report_text <- js$choices[[1]]$message$content
    }
  }

  if (!nzchar(report_text)) {
    stop("The API returned an empty response.", call. = FALSE)
  }

  message("Report generated. Creating the Word document...")

  # Crear documento Word con tablas reales
  doc <- .create_word_document(
    report_text = report_text,
    title = title,
    author = author,
    json_data = json_data
  )

  # Guardar documento
  print(doc, target = output_path)

  message("\nReport generated successfully.")
  message("File: ", output_path)

  invisible(output_path)
}


#' @title Create the Word document with real tables
#' @noRd
.create_word_document <- function(report_text, title, author, json_data) {

  # Crear documento

  doc <- officer::read_docx()

  # Titulo
  doc <- officer::body_add_par(doc, title, style = "heading 1")

  if (!is.null(author) && nzchar(author)) {
    doc <- officer::body_add_par(doc, author, style = "Normal")
  }

  doc <- officer::body_add_par(doc, format(Sys.Date(), "%Y-%m-%d"), style = "Normal")
  doc <- officer::body_add_par(doc, "", style = "Normal")

  # Procesar Markdown linea por linea
  md_lines <- unlist(strsplit(report_text, "\n", fixed = TRUE))
  i <- 1

  while (i <= length(md_lines)) {
    ln <- md_lines[i]

    # Lineas vacias
    if (nchar(trimws(ln)) == 0) {
      i <- i + 1
      next
    }

    # Separadores
    if (grepl("^---+$", ln)) {
      i <- i + 1
      next
    }

    # Headers nivel 1
    if (grepl("^#\\s+", ln) && !grepl("^##", ln)) {
      texto <- gsub("^#\\s+", "", ln)
      texto <- gsub("\\*\\*", "", texto)
      doc <- officer::body_add_par(doc, texto, style = "heading 1")
      i <- i + 1
      next
    }

    # Headers nivel 2
    if (grepl("^##\\s+", ln) && !grepl("^###", ln)) {
      texto <- gsub("^##\\s+", "", ln)
      texto <- gsub("\\*\\*", "", texto)
      doc <- officer::body_add_par(doc, texto, style = "heading 2")
      i <- i + 1
      next
    }

    # Headers nivel 3
    if (grepl("^###\\s+", ln)) {
      texto <- gsub("^###\\s+", "", ln)
      texto <- gsub("\\*\\*", "", texto)
      doc <- officer::body_add_par(doc, texto, style = "heading 3")
      i <- i + 1
      next
    }

    # Placeholder de Tabla 1 - Insertar tabla de metricas real
    if (grepl("\\[Insertar Tabla 1", ln, ignore.case = TRUE) ||
        grepl("\\[Insert Table 1", ln, ignore.case = TRUE)) {
      doc <- .add_metrics_table(doc, json_data)
      i <- i + 1
      next
    }

    # Placeholder de Tabla 2 - Insertar tabla de importancia de variables
    if (grepl("\\[Insertar Tabla 2", ln, ignore.case = TRUE) ||
        grepl("\\[Insert Table 2", ln, ignore.case = TRUE)) {
      doc <- .add_importance_table(doc, json_data)
      i <- i + 1
      next
    }

    # Otros placeholders de figuras/tablas
    if (grepl("^\\[Insertar", ln, ignore.case = TRUE) ||
        grepl("^\\[Insert", ln, ignore.case = TRUE)) {
      # Agregar placeholder en italica
      doc <- officer::body_add_par(doc, ln, style = "Normal")
      i <- i + 1
      next
    }

    # Titulo de Tabla o Figura (en negrita)
    if (grepl("^Tabla\\s+\\d+", ln) || grepl("^Table\\s+\\d+", ln) ||
        grepl("^Figura\\s+\\d+", ln) || grepl("^Figure\\s+\\d+", ln)) {
      texto <- gsub("\\*\\*", "", ln)
      texto <- gsub("\\*", "", texto)
      # Agregar como parrafo con formato especial
      doc <- officer::body_add_par(doc, texto, style = "Normal")
      i <- i + 1
      next
    }

    # Detectar tabla markdown (linea con |)
    if (grepl("^\\|", ln) && grepl("\\|$", ln)) {
      # Recopilar todas las lineas de la tabla
      table_lines <- c()
      while (i <= length(md_lines) && grepl("^\\|", md_lines[i])) {
        table_lines <- c(table_lines, md_lines[i])
        i <- i + 1
      }
      # Parsear y agregar tabla
      doc <- .add_markdown_table(doc, table_lines)
      next
    }

    # Parrafos normales
    texto <- ln
    texto <- gsub("\\*\\*([^*]+)\\*\\*", "\\1", texto)
    texto <- gsub("\\*([^*]+)\\*", "\\1", texto)
    texto <- gsub("_([^_]+)_", "\\1", texto)
    doc <- officer::body_add_par(doc, texto, style = "Normal")
    i <- i + 1
  }

  return(doc)
}


#' @title Add the metrics table to the Word document
#' @noRd
.add_metrics_table <- function(doc, json_data) {

  metrics <- json_data$metrics
  if (is.null(metrics)) {
    doc <- officer::body_add_par(doc, "[Metricas no disponibles]", style = "Normal")
    return(doc)
  }

  # Crear data frame con metricas
  metric_names <- names(metrics)
  metric_values <- unlist(metrics)

  # Formatear nombres y valores
  formatted_names <- sapply(metric_names, function(m) {
    switch(m,
      "roc_auc" = "ROC-AUC",
      "accuracy" = "Accuracy",
      "sensitivity" = "Sensitivity",
      "specificity" = "Specificity",
      "f_meas" = "F1-Score",
      "pr_auc" = "PR-AUC",
      "bal_accuracy" = "Balanced Accuracy",
      "mcc" = "MCC",
      "kap" = "Cohen's Kappa",
      "rmse" = "RMSE",
      "rsq" = "R-squared",
      "mae" = "MAE",
      toupper(m)
    )
  })

  formatted_values <- sapply(seq_along(metric_values), function(i) {
    m <- metric_names[i]
    val <- metric_values[i]
    if (m %in% c("accuracy", "sensitivity", "specificity", "bal_accuracy")) {
      sprintf("%.2f%%", val * 100)
    } else if (m %in% c("rmse", "mae")) {
      sprintf("%.4f", val)
    } else {
      sprintf("%.4f", val)
    }
  })

  # Agregar interpretacion
  interpretations <- sapply(seq_along(metric_values), function(i) {
    m <- metric_names[i]
    val <- metric_values[i]
    if (m == "mcc") {
      if (val >= 0.7) "Excelente"
      else if (val >= 0.5) "Bueno"
      else if (val >= 0.3) "Aceptable"
      else "Bajo"
    } else if (m == "roc_auc") {
      if (val >= 0.9) "Excelente"
      else if (val >= 0.8) "Bueno"
      else if (val >= 0.7) "Aceptable"
      else "Bajo"
    } else if (m %in% c("accuracy", "sensitivity", "specificity")) {
      if (val >= 0.9) "Excelente"
      else if (val >= 0.8) "Bueno"
      else if (val >= 0.7) "Aceptable"
      else "Bajo"
    } else {
      ""
    }
  })

  df <- data.frame(
    Metrica = formatted_names,
    Valor = formatted_values,
    Interpretacion = interpretations,
    stringsAsFactors = FALSE
  )

  # Crear tabla con flextable
  if (requireNamespace("flextable", quietly = TRUE)) {
    ft <- flextable::flextable(df)
    ft <- flextable::set_header_labels(ft,
      Metrica = "Metrica",
      Valor = "Valor",
      Interpretacion = "Interpretacion"
    )
    ft <- flextable::theme_booktabs(ft)
    ft <- flextable::autofit(ft)
    ft <- flextable::align(ft, align = "center", part = "all")
    ft <- flextable::bold(ft, part = "header")
    ft <- flextable::fontsize(ft, size = 10, part = "all")

    doc <- flextable::body_add_flextable(doc, ft)
  } else {
    # Fallback sin flextable
    doc <- officer::body_add_par(doc, "", style = "Normal")
    for (j in 1:nrow(df)) {
      line <- sprintf("%s: %s %s", df$Metrica[j], df$Valor[j],
                      ifelse(nzchar(df$Interpretacion[j]), paste0("(", df$Interpretacion[j], ")"), ""))
      doc <- officer::body_add_par(doc, line, style = "Normal")
    }
  }

  doc <- officer::body_add_par(doc, "", style = "Normal")
  return(doc)
}


#' @title Add the variable importance table to the Word document
#' @noRd
.add_importance_table <- function(doc, json_data) {

  var_imp <- json_data$variable_importance
  if (is.null(var_imp)) {
    doc <- officer::body_add_par(doc, "[Importancia de variables no disponible]", style = "Normal")
    return(doc)
  }

  # Convertir a data frame si es lista
  if (is.list(var_imp) && !is.data.frame(var_imp)) {
    df <- do.call(rbind, lapply(var_imp, function(v) {
      data.frame(
        Rango = v$rank,
        Variable = v$variable,
        Importancia = v$importance,
        stringsAsFactors = FALSE
      )
    }))
  } else {
    df <- data.frame(
      Rango = var_imp$rank,
      Variable = var_imp$variable,
      Importancia = var_imp$importance,
      stringsAsFactors = FALSE
    )
  }

  # Formatear importancia
  df$Importancia <- sprintf("%.4f", df$Importancia)

  # Limitar a top 10
  df <- utils::head(df, 10)

  # Crear tabla con flextable
  if (requireNamespace("flextable", quietly = TRUE)) {
    ft <- flextable::flextable(df)
    ft <- flextable::set_header_labels(ft,
      Rango = "Rango",
      Variable = "Variable",
      Importancia = "Importancia"
    )
    ft <- flextable::theme_booktabs(ft)
    ft <- flextable::autofit(ft)
    ft <- flextable::align(ft, j = 1, align = "center", part = "all")
    ft <- flextable::align(ft, j = 2, align = "left", part = "all")
    ft <- flextable::align(ft, j = 3, align = "center", part = "all")
    ft <- flextable::bold(ft, part = "header")
    ft <- flextable::fontsize(ft, size = 10, part = "all")

    doc <- flextable::body_add_flextable(doc, ft)
  } else {
    # Fallback sin flextable
    doc <- officer::body_add_par(doc, "", style = "Normal")
    for (j in 1:nrow(df)) {
      line <- sprintf("%d. %s: %s", df$Rango[j], df$Variable[j], df$Importancia[j])
      doc <- officer::body_add_par(doc, line, style = "Normal")
    }
  }

  doc <- officer::body_add_par(doc, "", style = "Normal")
  return(doc)
}


#' @title Add a Markdown table to the Word document
#' @noRd
.add_markdown_table <- function(doc, table_lines) {

  if (length(table_lines) < 2) {
    return(doc)
  }

  # Parsear lineas de tabla markdown
  parse_row <- function(line) {
    cells <- strsplit(line, "\\|")[[1]]
    cells <- cells[nzchar(trimws(cells))]
    trimws(cells)
  }

  # Primera linea es header
  header <- parse_row(table_lines[1])

  # Segunda linea es separador (ignorar)
  # Resto son datos
  data_rows <- list()
  for (k in 3:length(table_lines)) {
    if (k <= length(table_lines)) {
      row <- parse_row(table_lines[k])
      if (length(row) > 0) {
        data_rows[[length(data_rows) + 1]] <- row
      }
    }
  }

  if (length(data_rows) == 0) {
    return(doc)
  }

  # Crear data frame
  df <- do.call(rbind, lapply(data_rows, function(r) {
    # Asegurar que tenga el mismo numero de columnas que header
    if (length(r) < length(header)) {
      r <- c(r, rep("", length(header) - length(r)))
    } else if (length(r) > length(header)) {
      r <- r[1:length(header)]
    }
    r
  }))
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  names(df) <- header

  # Crear tabla con flextable
  if (requireNamespace("flextable", quietly = TRUE)) {
    ft <- flextable::flextable(df)
    ft <- flextable::theme_booktabs(ft)
    ft <- flextable::autofit(ft)
    ft <- flextable::align(ft, align = "center", part = "all")
    ft <- flextable::bold(ft, part = "header")
    ft <- flextable::fontsize(ft, size = 10, part = "all")

    doc <- flextable::body_add_flextable(doc, ft)
  } else {
    # Fallback
    for (j in 1:nrow(df)) {
      line <- paste(df[j, ], collapse = " | ")
      doc <- officer::body_add_par(doc, line, style = "Normal")
    }
  }

  doc <- officer::body_add_par(doc, "", style = "Normal")
  return(doc)
}


#' @title Build the system prompt
#' @noRd
.build_system_prompt <- function(language, classification_type = "binary") {

  is_multi <- (classification_type == "multiclass")

  if (language == "es") {

    # --- Secciones condicionales ---
    metrics_method <- if (is_multi) {
      paste(
        "- Metricas de evaluacion utilizadas (ROC-AUC Hand-Till, accuracy, sensitivity/specificity/F1 con estimador macro)",
        "- Procedimiento de deteccion de data leakage (pre y post-modelo)",
        sep = "\n"
      )
    } else {
      paste(
        "- Metricas de evaluacion utilizadas (ROC-AUC, accuracy, sensitivity, specificity, F1, F2, MCC, kappa, PR-AUC)",
        "- Procedimiento de optimizacion de hiperparametros (Random Search o grid search, numero de combinaciones)",
        "- Metodo de optimizacion de threshold (indice de Youden)",
        "- Procedimiento de deteccion de data leakage (pre y post-modelo)",
        "- Nested cross-validation como estimador insesgado (si se realizo)",
        sep = "\n"
      )
    }

    confusion_section <- if (is_multi) {
      paste(
        "## Matriz de Confusion",
        "Presenta la matriz N x N (una fila/columna por clase).",
        "Reporta aciertos en la diagonal y errores fuera de ella.",
        "Calcula precision por clase (diagonal / total columna).",
        "Discute implicaciones practicas de las confusiones mas frecuentes.",
        "[Insertar Figura - Matriz de Confusion]",
        sep = "\n"
      )
    } else {
      paste(
        "## Matriz de Confusion",
        "Reporta los cuatro cuadrantes: VP, VN, FP, FN con los NUMEROS EXACTOS del verbose.",
        "Discute las implicaciones practicas de cada tipo de error.",
        "[Insertar Figura - Matriz de Confusion]",
        sep = "\n"
      )
    }

    calibration_section <- if (!is_multi) {
      paste(
        "",
        "## Calibracion del Modelo",
        "Reporta el metodo de calibracion usado (ej. Platt scaling).",
        "Presenta Brier Score original vs calibrado y el porcentaje de mejora.",
        "Presenta ECE original vs calibrado.",
        "Interpreta si las probabilidades predichas son confiables.",
        "[Insertar Figura - Calibracion]",
        "",
        "## Optimizacion del Umbral",
        "Reporta el metodo de optimizacion (ej. Youden).",
        "Presenta threshold optimo vs default (0.50) con VALORES EXACTOS.",
        "Reporta cambios en sensibilidad, especificidad, precision y F1.",
        "[Insertar Figura - Threshold]",
        sep = "\n"
      )
    } else {
      ""
    }

    roc_section <- if (is_multi) {
      paste(
        "## Capacidad Discriminativa",
        "Reporta el valor de AUC Hand-Till (promedio multiclase) e interpretacion.",
        "Describe las curvas ROC por clase.",
        "[Insertar Figura - Curva ROC]",
        sep = "\n"
      )
    } else {
      paste(
        "## Capacidad Discriminativa",
        "Reporta el valor EXACTO de AUC del verbose e interpreta segun escala estandar.",
        "Describe la curva ROC.",
        "[Insertar Figura - Curva ROC]",
        sep = "\n"
      )
    }

    paste(
      "Eres un experto en redaccion cientifica especializado en Machine Learning y analisis predictivo.",
      "Tu tarea es generar un reporte academico COMPLETO en ESTRUCTURA DE ARTICULO CIENTIFICO.",
      "",
      "##############################################################################",
      "REGLA ANTI-ALUCINACION (LA MAS IMPORTANTE DE TODAS)",
      "##############################################################################",
      "",
      "TODOS los numeros, metricas, valores, hiperparametros y estadisticos del reporte",
      "DEBEN ser COPIADOS TEXTUALMENTE del verbose y los datos proporcionados.",
      "NUNCA inventes, redondees, aproximes, ni calcules numeros por tu cuenta.",
      "",
      "Si el verbose dice AUC = 0.7512, escribe AUC = .751. NO escribas 0.81 ni 0.75.",
      "Si el verbose dice mtry = 4, escribe mtry = 4. NO escribas mtry = 2.",
      "Si la matriz de confusion dice VP = 21, escribe 21. NO escribas 18.",
      "",
      "Si algun dato NO aparece en el verbose, NO lo inventes. Simplemente omitelo.",
      "Antes de escribir CUALQUIER numero, verifica que existe en los datos proporcionados.",
      "",
      "##############################################################################",
      "REGLA CRITICA: SEPARACION ENTRE METODO Y RESULTADOS",
      "##############################################################################",
      "",
      "METODO = describe QUE SE HIZO y COMO (procedimiento, plan, en tiempo pasado)",
      "RESULTADOS = presenta LOS NUMEROS Y HALLAZGOS OBTENIDOS",
      "",
      "En METODO/Analisis de Datos NUNCA debes incluir:",
      "- Valores especificos de metricas (ej: AUC = 0.82)",
      "- Cual modelo fue seleccionado como mejor",
      "- Valores de hiperparametros optimizados (ej: mtry = 4)",
      "- Resultados de la validacion cruzada",
      "",
      "Esos valores van SOLO en la seccion RESULTADOS.",
      "",
      "##############################################################################",
      "ESTRUCTURA OBLIGATORIA DEL REPORTE",
      "##############################################################################",
      "",
      "# METODO",
      "",
      "## Participantes",
      "Redacta en prosa narrativa (parrafo, NO bullets). Describe:",
      "- Tamano de la muestra (N total)",
      "- Division train/test (n y porcentajes)",
      "- Numero y tipo de variables (numericas y categoricas)",
      "- Distribucion de la variable objetivo (clases y porcentajes)",
      "- Valores faltantes detectados (total, porcentaje, variables afectadas)",
      "- Outliers detectados (metodo, cuantas variables afectadas)",
      "- Correlaciones altas detectadas (pares y valores)",
      "- Multicolinealidad (variables con VIF > 5)",
      "- Resultado de la verificacion de data leakage pre-modelo",
      "",
      "## Analisis de Datos",
      "IMPORTANTE: Esta seccion describe el FLUJO COMPLETO de trabajo metodologico.",
      "Redacta en tiempo pasado, en PROSA NARRATIVA (parrafos extensos, NO bullets).",
      "Debe reflejar TODOS los pasos del pipeline de preprocesamiento tal como aparecen",
      "en el verbose, en el ORDEN en que fueron ejecutados.",
      "",
      "Parrafo 1 - Software y entorno:",
      "  Software, paquetes (R, PsyML v1.0.0, tidymodels), procesamiento paralelo si se uso.",
      "",
      "Parrafo 2 - Preprocesamiento (DESCRIBIR CADA PASO del verbose):",
      "  Describe TODOS los pasos de la receta de preprocesamiento que aparecen en el verbose,",
      "  en el orden correcto. Los pasos tipicos incluyen (pero SOLO menciona los que aparezcan):",
      "  * Tipo de imputacion usado (KNN para numericas, moda para categoricas, u otro)",
      "  * Winsorizacion de outliers (percentiles usados, ej. 5-95)",
      "  * Eliminacion por alta correlacion (umbral usado, ej. |r| > 0.85)",
      "  * Agrupacion de categorias raras (umbral, ej. < 5%)",
      "  * Dummy encoding de variables categoricas",
      "  * Eliminacion de varianza cero",
      "  * Normalizacion (z-score u otro metodo)",
      "  * SMOTE u otro metodo de balanceo de clases",
      "  Enfatiza que todos los pasos se ejecutaron DENTRO de la validacion cruzada",
      "  para prevenir data leakage (cita Kapoor & Narayanan, 2023).",
      "",
      "Parrafo 3 - Estrategia de modelado:",
      "  Describe los modelos candidatos evaluados (sin decir cual gano).",
      "  Describe la validacion cruzada (tipo, numero de folds, estratificacion).",
      "  Describe la metrica de seleccion utilizada.",
      "",
      "Parrafo 4 - Procedimientos de optimizacion y validacion:",
      "  Describe el procedimiento de tuning (Random Search, numero de combinaciones).",
      "  Describe el metodo de optimizacion de threshold (Youden) si aplica.",
      "  Describe nested cross-validation si se realizo.",
      "  Describe la verificacion de data leakage post-modelo.",
      "  Describe el analisis de fairness si se realizo (variable protegida, metricas).",
      "  Describe el decision curve analysis si se realizo.",
      "",
      metrics_method,
      "",
      "# RESULTADOS",
      "",
      "Esta seccion presenta TODOS los hallazgos numericos copiados del verbose.",
      "RECUERDA: Solo usa numeros que aparezcan en el verbose. NUNCA inventes valores.",
      "",
      "## Seleccion del Modelo",
      "Presenta en prosa narrativa los resultados de la comparacion de modelos en CV.",
      "Reporta las metricas de CADA modelo evaluado (accuracy, bal_accuracy, F1, MCC, ROC-AUC, etc.).",
      "Indica cual modelo fue seleccionado, por que metrica, y su interpretacion.",
      "Reporta la estabilidad entre folds (CV%) si aparece en el verbose.",
      "[Insertar Figura - Comparacion de Modelos]",
      "",
      "## Optimizacion de Hiperparametros",
      "Reporta el metodo de busqueda y numero de combinaciones evaluadas.",
      "Lista los hiperparametros tuneados con sus valores optimos EXACTOS.",
      "Reporta el ROC-AUC obtenido en CV tras el tuning.",
      "Describe la interpretacion de cada hiperparametro.",
      "[Insertar Figura - Resultados de tuning]",
      "",
      "## Rendimiento del Modelo Final",
      "Reporta TODAS las metricas del conjunto de prueba con VALORES EXACTOS del verbose.",
      "Incluye: ROC-AUC, accuracy, sensitivity, specificity, F1, MCC, kappa, F2, PR-AUC.",
      "Incluye la interpretacion de cada metrica tal como aparece en el verbose.",
      "[Insertar Tabla 1 - Metricas de rendimiento]",
      "",
      "## Diagnostico de Overfitting",
      "Si el verbose incluye comparacion CV vs Test, reporta:",
      "  - AUC en CV, AUC en test, y el gap entre ambos",
      "  - Interpretacion de la generalizacion del modelo",
      "  - Notas sobre el error de resubstitucion si aplica",
      "",
      roc_section,
      "",
      confusion_section,
      calibration_section,
      "",
      "## Importancia de Variables",
      "Reporta el ranking completo (top 10) con valores de importancia EXACTOS.",
      "Interpreta teoricamente el rol de cada variable en el contexto del problema.",
      "[Insertar Tabla 2 - Importancia de Variables]",
      "[Insertar Figura - Importancia de Variables]",
      "",
      "## Valores SHAP (si aparecen en el verbose)",
      "Reporta la importancia SHAP (top 10) con valores EXACTOS.",
      "Compara con la importancia clasica del modelo.",
      "Interpreta diferencias entre ambos rankings.",
      "[Insertar Figura - SHAP bar (importancia)]",
      "[Insertar Figura - SHAP beeswarm]",
      "",
      "## Analisis Avanzado (solo si aparecen en el verbose, incluir TODAS las subsecciones que existan)",
      "",
      "### Deteccion de Data Leakage Post-Modelo",
      "Si aparece en el verbose, reporta los resultados de las verificaciones post-modelo.",
      "",
      "### Interacciones y Efectos No Lineales",
      "Si aparece en el verbose, reporta:",
      "  - Interacciones significativas detectadas (o su ausencia)",
      "  - Variables con efectos no lineales (p-valor, R2 ganancia)",
      "  - Sugerencias para mejorar el modelo",
      "",
      "### Nested Cross-Validation",
      "Si aparece en el verbose, reporta:",
      "  - Configuracion (folds externos, internos, grid size)",
      "  - ROC-AUC y Accuracy con sus SD",
      "  - Comparar con la estimacion de CV simple",
      "",
      "### Analisis de Equidad (Fairness)",
      "Si aparece en el verbose, reporta:",
      "  - Variable protegida y grupos evaluados",
      "  - Metricas por subgrupo (TPR, FPR, PPV, Accuracy, etc.)",
      "  - Metricas globales de fairness (Demographic Parity, Equalized Odds, etc.)",
      "  - Disparate Impact y regla 4/5",
      "  - Interpretacion de disparidades encontradas",
      "[Insertar Figura - Analisis de fairness]",
      "",
      "### Utilidad Clinica (Decision Curve Analysis)",
      "Si aparece en el verbose, reporta:",
      "  - Concepto del Net Benefit",
      "  - Rango util del modelo (thresholds donde supera tratar-a-todos y tratar-a-nadie)",
      "  - NNT en threshold optimo",
      "  - Interpretacion clinica del rango de utilidad",
      "[Insertar Figura - Decision Curve Analysis]",
      "",
      "# DISCUSION",
      "",
      "IMPORTANTE: Seccion BREVE con PUNTOS CLAVE solamente.",
      "El usuario desarrollara estos puntos en su propia discusion.",
      "",
      "Formato obligatorio:",
      "",
      "Hallazgos principales:",
      "1. [Punto conciso sobre rendimiento del modelo]",
      "2. [Punto sobre variables mas importantes y su interpretacion]",
      "3. [Punto sobre capacidad predictiva y utilidad practica]",
      "4. [Punto sobre fairness o calibracion si fueron evaluados]",
      "",
      "Limitaciones:",
      "1. [Limitacion metodologica]",
      "2. [Limitacion de datos]",
      "3. [Limitacion de generalizabilidad]",
      "",
      "Direcciones futuras:",
      "1. [Sugerencia de investigacion]",
      "2. [Mejora metodologica posible]",
      "",
      "# REFERENCIAS",
      "- Incluye TODAS las referencias que aparecen en el verbose (busca cada linea 'Referencia:')",
      "- Formato APA 7ma edicion estricto",
      "- Usa DOIs cuando esten disponibles en el verbose",
      "- Minimo 15 referencias (el verbose tipicamente contiene 15-25)",
      "",
      "##############################################################################",
      "ESTILO DE ESCRITURA",
      "##############################################################################",
      "",
      "- PROSA NARRATIVA en Metodo y Resultados: parrafos extensos de 150-250 palabras cada uno",
      "- PROHIBIDO usar listas con bullets (-) o numeradas en Metodo y Resultados",
      "- Cada subseccion de Resultados debe tener AL MENOS un parrafo completo de prosa",
      "- Discusion: SOLO puntos numerados, NO parrafos largos",
      "- Citas integradas en el texto: (Autor, ano) o (Autor & Autor, ano)",
      "- Estadisticos en formato APA: AUC = .751, accuracy = 75.25%",
      "- Usar terminologia tecnica apropiada con citas de respaldo",
      "",
      "##############################################################################",
      "REGLA DE NUMERACION DE SECCIONES",
      "##############################################################################",
      "",
      "PROHIBIDO numerar las secciones o subsecciones con numeros (1., 2., 2.1., 2.1.1., etc.).",
      "Usa SOLO el formato markdown de headers SIN numeros:",
      "  CORRECTO: ## METODO",
      "  INCORRECTO: ## 2. METODO",
      "  CORRECTO: ### Participantes",
      "  INCORRECTO: ### 2.1. Participantes",
      "  CORRECTO: ### Seleccion del Modelo",
      "  INCORRECTO: ### 3.1. Seleccion del Modelo",
      "",
      "##############################################################################",
      "REGLA DE TABLAS Y FIGURAS EN RESULTADOS",
      "##############################################################################",
      "",
      "En la seccion RESULTADOS debes incluir placeholders para tablas y figuras.",
      "Usa EXACTAMENTE estos formatos para que el sistema los detecte y genere tablas reales:",
      "  [Insertar Tabla 1 - Metricas de rendimiento]",
      "  [Insertar Tabla 2 - Importancia de Variables]",
      "Estos placeholders DEBEN aparecer en lineas separadas, solos, sin texto adicional.",
      "Tambien incluye placeholders de figuras del catalogo proporcionado.",
      "",
      "##############################################################################",
      "VERIFICACION FINAL (haz esto mentalmente antes de entregar el reporte)",
      "##############################################################################",
      "",
      "1. Compara CADA numero del reporte contra el verbose. Si no coincide, corrigelo.",
      "2. Verifica que TODAS las secciones del verbose tienen su contraparte en Resultados.",
      "3. Verifica que NO hay bullets en Metodo ni Resultados (solo prosa narrativa).",
      "4. Verifica que TODAS las referencias del verbose estan en la seccion Referencias.",
      "5. Verifica que los placeholder de figuras usan los nombres EXACTOS del catalogo.",
      "",
      "IMPORTANTE: Usa los nombres EXACTOS del catalogo de figuras proporcionado.",
      "",
      sep = "\n"
    )
  } else {

    # --- English version ---
    results_sections <- if (is_multi) {
      paste(
        "## Model Selection - Which model won and why (report ALL metrics per model from verbose)",
        "## Hyperparameter Optimization - Optimal values found (EXACT values from verbose)",
        "## Final Model Performance - ALL test metrics with EXACT values from verbose",
        "## ROC Curve - AUC Hand-Till interpretation (per-class curves)",
        "## Confusion Matrix - N x N error analysis with EXACT numbers from verbose",
        "## Variable Importance - Top predictors with EXACT values",
        "## SHAP Values - If present in verbose, report with EXACT values",
        "## Advanced Analysis - Nested CV, Fairness, DCA, Interactions (only if present in verbose)",
        sep = "\n"
      )
    } else {
      paste(
        "## Model Selection - Which model won and why (report ALL metrics per model from verbose)",
        "## Hyperparameter Optimization - Optimal values found (EXACT values from verbose)",
        "## Final Model Performance - ALL test metrics with EXACT values from verbose",
        "## Overfitting Diagnostic - CV vs Test gap",
        "## ROC Curve - AUC interpretation with EXACT value",
        "## Confusion Matrix - Error analysis with EXACT numbers from verbose",
        "## Calibration - Probability reliability, Brier Score, ECE",
        "## Threshold Optimization - Optimal cutoff with EXACT values",
        "## Variable Importance - Top predictors with EXACT values",
        "## SHAP Values - If present in verbose, report with EXACT values",
        "## Advanced Analysis - Nested CV, Fairness, DCA, Interactions (only if present in verbose)",
        sep = "\n"
      )
    }

    paste(
      "You are a scientific writing expert specialized in Machine Learning.",
      "Generate a COMPLETE academic report in SCIENTIFIC ARTICLE STRUCTURE.",
      "",
      "##############################################################################",
      "ANTI-HALLUCINATION RULE (THE MOST IMPORTANT RULE)",
      "##############################################################################",
      "",
      "ALL numbers, metrics, values, and statistics in the report MUST be",
      "COPIED VERBATIM from the verbose output and provided data.",
      "NEVER invent, round, approximate, or calculate numbers on your own.",
      "If a value does NOT appear in the verbose, do NOT include it.",
      "Before writing ANY number, verify it exists in the provided data.",
      "",
      "CRITICAL RULE: METHOD describes WHAT was done (procedure, past tense).",
      "RESULTS presents THE NUMBERS AND FINDINGS obtained.",
      "",
      "In METHOD/Data Analysis NEVER include specific metric values,",
      "which model was selected, or optimized hyperparameter values.",
      "Those go ONLY in RESULTS.",
      "",
      "# METHOD",
      "## Participants - Sample description, train/test split, missing values, outliers",
      "## Data Analysis - Full preprocessing pipeline as narrative prose, NO bullets",
      "",
      "# RESULTS",
      results_sections,
      "",
      "# DISCUSSION - Brief key points only (numbered)",
      "# REFERENCES - ALL references from verbose, APA 7th edition, minimum 15",
      "",
      "STYLE: Narrative prose (150-250 words per paragraph) in Method and Results.",
      "NO bullet lists in Method or Results. Only Discussion uses numbered points.",
      "APA statistics format: AUC = .751, accuracy = 75.25%",
      "",
      "SECTION NUMBERING: NEVER number sections (no 1., 2., 2.1., etc.).",
      "Use ONLY markdown headers without numbers: ## METHOD (not ## 2. METHOD).",
      "",
      "TABLES AND FIGURES: In RESULTS, include these exact placeholders on separate lines:",
      "  [Insert Table 1 - Performance metrics]",
      "  [Insert Table 2 - Variable Importance]",
      "Also include figure placeholders from the provided catalog.",
      "",
      "FINAL CHECK: Compare EVERY number against the verbose before delivering.",
      "",
      sep = "\n"
    )
  }
}




#' @title Build the user message
#' @noRd
.build_user_message <- function(meta, metrics, model_info, var_imp, json_data, language,
                                custom_instructions = NULL) {

  # Formatear metricas
  metrics_text <- paste(sapply(names(metrics), function(m) {
    val <- metrics[[m]]
    if (m %in% c("accuracy", "sensitivity", "specificity")) {
      sprintf("%s: %.2f%%", toupper(m), val * 100)
    } else {
      sprintf("%s: %.4f", toupper(m), val)
    }
  }), collapse = "\n")

  # Formatear importancia de variables
  var_text <- if (!is.null(var_imp) && (is.data.frame(var_imp) || length(var_imp) > 0)) {
    if (is.data.frame(var_imp)) {
      paste(sapply(1:min(10, nrow(var_imp)), function(i) {
        sprintf("%d. %s (%.4f)", var_imp$rank[i], var_imp$variable[i], var_imp$importance[i])
      }), collapse = "\n")
    } else {
      paste(sapply(1:min(10, length(var_imp)), function(i) {
        v <- var_imp[[i]]
        sprintf("%d. %s (%.4f)", v$rank, v$variable, v$importance)
      }), collapse = "\n")
    }
  } else {
    "No disponible"
  }

  # Hiperparametros
  hyperparam_text <- if (model_info$tuned && !is.null(model_info$hyperparameters)) {
    paste(names(model_info$hyperparameters), "=", unlist(model_info$hyperparameters), collapse = ", ")
  } else {
    "No se realizo tuning"
  }

  # Obtener verbose_text completo si existe
  verbose_section <- ""
  if (!is.null(json_data$verbose_text) && nzchar(json_data$verbose_text)) {
    verbose_section <- paste(
      "",
      "## OUTPUT COMPLETO DEL ANALISIS (VERBOSE)",
      "A continuacion se presenta el output completo del analisis de Machine Learning.",
      "Usa TODA esta informacion para escribir un reporte MUY DETALLADO y COMPLETO.",
      "Incluye todos los detalles tecnicos, interpretaciones y referencias mencionadas.",
      "",
      "```",
      json_data$verbose_text,
      "```",
      "",
      sep = "\n"
    )
  }

  paste(
    "# DATOS COMPLETOS PARA REPORTE ACADEMICO DE MACHINE LEARNING",
    "",
    "## RESUMEN DEL ESTUDIO",
    sprintf("Tipo de tarea: %s", if (meta$task == "classification") {
      n_cls <- json_data$metadata$n_classes %||% 2L
      if (n_cls >= 3) paste0("Clasificacion multiclase (", n_cls, " clases)") else "Clasificacion binaria"
    } else "Regresion"),
    sprintf("Variable objetivo: %s", meta$target),
    sprintf("Total de observaciones: N = %d", meta$n_observations),
    sprintf("Conjunto de entrenamiento: n = %d (%.1f%%)", meta$n_train, meta$n_train/meta$n_observations*100),
    sprintf("Conjunto de prueba: n = %d (%.1f%%)", meta$n_test, meta$n_test/meta$n_observations*100),
    "",
    "## MODELO SELECCIONADO",
    sprintf("Algoritmo: %s", model_info$label),
    sprintf("Tuning: %s", ifelse(model_info$tuned, "Si", "No")),
    sprintf("Hiperparametros: %s", hyperparam_text),
    "",
    "## METRICAS DE RENDIMIENTO (en test set)",
    metrics_text,
    "",
    if (!is.null(json_data$threshold)) {
      sprintf("Threshold optimizado: %.4f (vs default 0.50)", json_data$threshold$optimized)
    } else {
      ""
    },
    "",
    "## IMPORTANCIA DE VARIABLES (Top 10)",
    var_text,
    "",
    "## CATALOGO DE FIGURAS DISPONIBLES",
    "Cuando menciones figuras en el reporte, usa estos nombres exactos:",
    if (!is.null(json_data$figures_catalog) && length(json_data$figures_catalog) > 0) {
      # Manejar tanto data.frame (desde JSON) como lista (desde resultado)
      if (is.data.frame(json_data$figures_catalog)) {
        paste(apply(json_data$figures_catalog, 1, function(row) {
          sprintf("  - %s: %s (%s)", row["id"], row["title"], row["filename"])
        }), collapse = "\n")
      } else {
        paste(sapply(json_data$figures_catalog, function(fig) {
          sprintf("  - %s: %s (%s)", fig$id, fig$title, fig$filename)
        }), collapse = "\n")
      }
    } else {
      "  (No hay catalogo de figuras disponible)"
    },
    "",
    "IMPORTANTE: Cuando indiques donde insertar una figura, usa el formato:",
    "  [Insertar Figura_N - Titulo] donde N es el numero de la figura",
    "  Ejemplo: [Insertar Figura_1 - Importancia de Variables]",
    "  El usuario vera los archivos guardados con esos nombres.",
    verbose_section,
    "",
    sprintf("IDIOMA DEL REPORTE: %s", ifelse(language == "es", "ESPANOL", "ENGLISH")),
    "",
    "INSTRUCCIONES FINALES:",
    "- ANTI-ALUCINACION: Copia CADA numero textualmente del verbose. NUNCA inventes valores.",
    "- Usa TODA la informacion del verbose para escribir el reporte (TODAS las secciones)",
    "- El verbose contiene secciones numeradas (1. EDA, 2. Preprocesamiento, 3. Modelado,",
    "  4. Tuning, 5. Evaluacion, 6. Interpretabilidad, 7. Analisis Avanzado). TODAS deben",
    "  reflejarse en el reporte.",
    "- Incluye TODAS las interpretaciones de cada metrica tal como aparecen en el verbose",
    "- Extrae TODAS las referencias del verbose (busca cada linea que diga 'Referencia:')",
    "- El reporte debe ser MUY COMPLETO y DETALLADO (minimo 3000 palabras)",
    "- Estilo prosa narrativa (parrafos), NO bullets en Metodo ni Resultados",
    "- Verifica cada numero del reporte contra el verbose antes de entregar",
    "",
    if (!is.null(custom_instructions) && nzchar(custom_instructions)) {
      paste(
        "##############################################################################",
        "INSTRUCCIONES ADICIONALES DEL USUARIO (PRIORIDAD ALTA)",
        "##############################################################################",
        "",
        custom_instructions,
        "",
        sep = "\n"
      )
    } else {
      ""
    },
    "GENERA EL REPORTE AHORA.",
    sep = "\n"
  )
}


#' @title Launch the Integrated PsyML Shiny App
#'
#' @description
#' Opens the integrated Shiny application that combines the machine learning
#' pipeline, the sample size estimation and the AI report generator. Requires
#' the shiny package.
#'
#' @param launch.browser Logical (default TRUE). Open the app in the web
#'   browser.
#'
#' @return Called for its side effect (runs the Shiny app); returns the value
#'   returned by \code{shiny::runApp()} when the app is closed.
#'
#' @examples
#' if (interactive()) {
#'   launch_supervisedml_app()
#' }
#' @family reporting and export
#' @export
launch_supervisedml_app <- function(launch.browser = TRUE) {

  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("Package 'shiny' is required. Please install package 'shiny'.",
         call. = FALSE)
  }

  app_dir <- system.file("shiny/easyml_app", package = "PsyML")

  if (app_dir == "") {
    app_dir <- file.path(getwd(), "inst/shiny/easyml_app")
  }

  if (!dir.exists(app_dir)) {
    stop("PsyML integrated app not found. ",
         "Make sure the PsyML package is installed correctly.",
         call. = FALSE)
  }

  shiny::runApp(app_dir, launch.browser = launch.browser)
}



# =============================================================================
# Smart Diagnostics Engine (internal)
# =============================================================================

# @noRd
.build_diagnostics <- function(result) {
  acc <- new.env(parent = emptyenv())
  acc$findings <- list()
  task <- result$task %||% "unknown"
  target <- result$target %||% "unknown"
  meta <- result$metadata

  add <- function(id, issue, severity, details, suggested_code = "") {
    acc$findings[[length(acc$findings) + 1]] <- list(
      id = id, issue = issue, severity = severity,
      details = details, suggested_code = suggested_code
    )
  }

  # Rule 1: Perfect scores
  if (!is.null(result$test_metrics) && is.data.frame(result$test_metrics)) {
    tm <- result$test_metrics
    if (task == "classification") {
      auc_val <- tm$.estimate[tm$.metric == "roc_auc"]
      if (length(auc_val) > 0 && !is.na(auc_val[1]) && auc_val[1] >= 0.995) {
        add("perfect_scores",
            sprintf("Perfect or near-perfect scores (AUC = %.3f)", auc_val[1]),
            "critical",
            "AUC this high almost always indicates data leakage, a duplicate/derived feature, or a trivially separable dataset.",
            sprintf(paste0(
              "# Check for data leakage - look for IDs or target-derived columns:\n",
              "str(your_data)\n",
              "# Try excluding suspicious variables:\n",
              "result2 <- supervised_ml(your_data, \"%s\", \"%s\", exclude_vars = c(\"suspicious_var\"))"),
              target, task))
      }
    } else {
      rsq_val <- tm$.estimate[tm$.metric == "rsq"]
      if (length(rsq_val) > 0 && !is.na(rsq_val[1]) && rsq_val[1] >= 0.995) {
        add("perfect_scores",
            sprintf("Perfect or near-perfect scores (R-sq = %.4f)", rsq_val[1]),
            "critical",
            "R-squared this high almost always indicates data leakage or a derived feature that trivially predicts the target.",
            sprintf(paste0(
              "# Check for columns that are transformations of '%s':\n",
              "str(your_data)\n",
              "# Try excluding suspicious variables:\n",
              "result2 <- supervised_ml(your_data, \"%s\", \"%s\", exclude_vars = c(\"suspicious_var\"))"),
              target, target, task))
      }
    }
  }

  # Rule 2: Overfitting (train-test gap)
  if (!is.null(result$evaluation$overfitting$comparison)) {
    comp <- result$evaluation$overfitting$comparison
    select_metric <- meta$select_metric %||%
      (if (task == "classification") "mcc" else "rsq")
    row_idx <- which(comp$metric == select_metric)
    if (length(row_idx) > 0) {
      gap <- comp$gap[row_idx[1]]
      if (!is.na(gap) && gap > 0.15) {
        sev <- if (gap > 0.30) "critical" else "warning"
        add("overfitting",
            sprintf("Overfitting detected (train-test gap = %.3f on %s)", gap, select_metric),
            sev,
            sprintf("Training %s is substantially higher than test. The model memorizes training data instead of learning general patterns.", select_metric),
            sprintf(paste0(
              "# Reduce overfitting with tuning and more CV folds:\n",
              "result2 <- supervised_ml(your_data, \"%s\", \"%s\", tune = TRUE, cv_folds = 10)\n",
              "# Or try simpler models:\n",
              "result3 <- supervised_ml(your_data, \"%s\", \"%s\", models = c(\"glm\", \"tree\"))"),
              target, task, target, task))
      }
    }
  }

  # Rule 3: All models identical
  if (!is.null(result$cv_summary) && is.data.frame(result$cv_summary) &&
      nrow(result$cv_summary) > 1) {
    cv <- result$cv_summary
    select_metric <- meta$select_metric %||%
      (if (task == "classification") "mcc" else "rsq")
    if (select_metric %in% names(cv)) {
      vals <- cv[[select_metric]]
      vals <- vals[!is.na(vals)]
      if (length(vals) > 1 && (max(vals) - min(vals)) < 0.01) {
        add("identical_models",
            sprintf("All models show identical performance (%s range = %.4f)",
                    select_metric, max(vals) - min(vals)),
            "warning",
            "When all models converge to the same score, the task is likely trivially easy (ceiling effect) or data leakage gives every algorithm the same perfect signal.",
            "# If scores are near-perfect, investigate data leakage (see above)\n# If scores are modest, try feature engineering to strengthen the signal")
      }
    }
  }

  # Rule 4: Class imbalance (classification only)
  if (task == "classification" && !is.null(result$train_data) &&
      target %in% names(result$train_data)) {
    y <- result$train_data[[target]]
    if (!is.null(y)) {
      tab <- table(y)
      props <- prop.table(tab)
      minority_pct <- min(props) * 100
      if (minority_pct < 20) {
        sev <- if (minority_pct < 10) "critical" else "warning"
        minority_class <- names(which.min(tab))
        add("class_imbalance",
            sprintf("Class imbalance (minority class '%s' = %.1f%%)",
                    minority_class, minority_pct),
            sev,
            "Imbalanced classes can bias models toward the majority class. Accuracy alone is misleading.",
            sprintf(paste0(
              "# Use balanced metrics:\n",
              "result2 <- supervised_ml(your_data, \"%s\", \"classification\",\n",
              "                   select_metric = \"bal_accuracy\")"),
              target))
      }
    }
  }

  # Rule 5: Small dataset
  if (!is.null(meta)) {
    n <- meta$n_obs %||% 0L
    p <- meta$n_features %||% 1L
    if (n > 0 && n < 50) {
      add("small_n",
          sprintf("Very small dataset (N = %d)", n),
          "warning",
          "With fewer than 50 observations, ML models are prone to overfitting and unstable estimates.",
          sprintf(paste0(
            "# Use more CV folds for stability:\n",
            "result2 <- supervised_ml(your_data, \"%s\", \"%s\", cv_folds = %d)\n",
            "# Prefer simpler models:\n",
            "result3 <- supervised_ml(your_data, \"%s\", \"%s\", models = c(\"glm\", \"tree\"))"),
            target, task, min(n, 10), target, task))
    } else if (n > 0 && p > 0 && (n / p) < 10) {
      add("low_ratio",
          sprintf("Low observations-to-features ratio (N/p = %.1f)", n / p),
          "info",
          "Fewer than 10 observations per feature increases overfitting risk for complex models.",
          sprintf(paste0(
            "# Consider dimensionality reduction:\n",
            "result2 <- supervised_ml(your_data, \"%s\", \"%s\", use_pca = TRUE)"),
            target, task))
    }
  }

  # Rule 6: Dominant variable
  if (!is.null(result$importance) && is.data.frame(result$importance) &&
      nrow(result$importance) > 1) {
    imp <- result$importance
    total_imp <- sum(imp$Importance, na.rm = TRUE)
    if (total_imp > 0) {
      top_pct <- (imp$Importance[1] / total_imp) * 100
      if (top_pct > 80) {
        add("dominant_variable",
            sprintf("Dominant variable: '%s' accounts for %.1f%% of total importance",
                    imp$Variable[1], top_pct),
            "warning",
            "When one variable dominates, the model may be fragile. If that variable is noisy or unavailable, performance collapses.",
            sprintf(paste0(
              "# Try removing the dominant variable:\n",
              "result2 <- supervised_ml(your_data, \"%s\", \"%s\",\n",
              "                   exclude_vars = c(\"%s\"))"),
              target, task, imp$Variable[1]))
      }
    }
  }

  # Rule 7: No tuning
  if (!is.null(meta) && isFALSE(meta$tuned)) {
    add("no_tuning",
        "Models were not hyperparameter-tuned",
        "info",
        "Default hyperparameters may not be optimal. Tuning often improves performance.",
        sprintf(paste0(
          "# Enable tuning:\n",
          "result2 <- supervised_ml(your_data, \"%s\", \"%s\",\n",
          "                   tune = TRUE, tune_method = \"bayes\")"),
          target, task))
  }

  acc$findings
}


# =============================================================================
# AI Assistant: explain_with_ai()
# =============================================================================

#' Interpret Results of supervised_ml() with a Large Language Model
#'
#' @description
#' Sends a structured summary of a \code{supervised_ml()} result to a large
#' language model (Groq or OpenAI chat completions API) and returns a
#' plain-language interpretation of the analysis. A built-in diagnostic engine
#' first checks for common problems (possible data leakage, overfitting, class
#' imbalance, untuned models, etc.), and its findings are included in the
#' request. An API key of the chosen provider is required; no key is bundled
#' with the package.
#'
#' @param result An object of class \code{supervisedml} returned by
#'   \code{supervised_ml()}.
#' @param api_key API key of the provider. If \code{NULL} (default), the
#'   environment variable \code{PSYML_API_KEY} is used; an error is raised if
#'   it is not set.
#' @param language Language of the interpretation: \code{"es"} (Spanish,
#'   default) or \code{"en"} (English).
#' @param model Model identifier. If \code{NULL} (default),
#'   \code{"llama-3.3-70b-versatile"} is used for Groq and
#'   \code{"gpt-4.1-mini"} for OpenAI.
#' @param provider API provider: \code{"groq"} (default) or \code{"openai"}.
#' @param max_tokens Maximum number of tokens of the response (default 2000).
#' @param verbose Logical. If \code{TRUE} (default), the diagnostics and the
#'   interpretation are printed to the console.
#'
#' @return Invisibly, a list with components \code{success} (logical),
#'   \code{text} (character, the interpretation or the error message), and
#'   \code{diagnostics} (list of findings of the diagnostic engine; each
#'   finding has elements \code{id}, \code{issue}, \code{severity},
#'   \code{details}, and \code{suggested_code}).
#'
#' @examples
#' \dontrun{
#' # Requires an API key and an internet connection
#' set.seed(1)
#' df <- data.frame(x1 = rnorm(150), x2 = rnorm(150))
#' df$y <- factor(ifelse(df$x1 + rnorm(150) > 0, "yes", "no"))
#' res <- supervised_ml(df, "y", models = "glm", cv_folds = 3,
#'                      tune_best = FALSE, run_shap = FALSE, n_cores = 1,
#'                      verbose = FALSE)
#' Sys.setenv(PSYML_API_KEY = "your-groq-key")
#' explain_with_ai(res, language = "en")
#' explain_with_ai(res, provider = "openai", api_key = "your-openai-key")
#' }
#'
#' @family reporting and export
#' @export
explain_with_ai <- function(result,
                            api_key = NULL,
                            language = c("es", "en"),
                            model = NULL,
                            provider = c("groq", "openai"),
                            max_tokens = 2000L,
                            verbose = TRUE) {

  # --- Validate inputs ---
  language <- match.arg(language)
  provider <- match.arg(provider)

  if (!inherits(result, "supervisedml")) {
    stop("'result' must be an object returned by supervised_ml().", call. = FALSE)
  }

  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("Package 'httr' is required. Please install package 'httr'.",
         call. = FALSE)
  }

  # --- Auto-select model based on provider ---
  if (is.null(model)) {
    model <- switch(provider,
      groq   = "llama-3.3-70b-versatile",
      openai = "gpt-4.1-mini"
    )
  }

  # --- API key (argument, then environment variable PSYML_API_KEY) ---
  if (is.null(api_key)) {
    api_key <- Sys.getenv("PSYML_API_KEY", unset = "")
  }
  if (!nzchar(api_key)) {
    stop(paste0(
      "An API key is required. Provide it via:\n",
      "  1) explain_with_ai(result, api_key = '...'), or\n",
      "  2) Sys.setenv(PSYML_API_KEY = '...')\n",
      "Groq keys: https://console.groq.com/keys; ",
      "OpenAI keys: https://platform.openai.com/api-keys"
    ), call. = FALSE)
  }

  # --- API URL ---
  base_url <- switch(provider,
    groq   = "https://api.groq.com/openai/v1/chat/completions",
    openai = "https://api.openai.com/v1/chat/completions"
  )

  # --- Run diagnostics ---
  diag_findings <- .build_diagnostics(result)

  # --- Extract structured data ---
  task       <- result$task %||% "unknown"
  target     <- result$target %||% "unknown"
  best_model <- result$best_model %||% "unknown"
  meta       <- result$metadata

  # Dataset info
  meta_parts <- c()
  if (!is.null(meta$n_obs))      meta_parts <- c(meta_parts, sprintf("N = %d", meta$n_obs))
  if (!is.null(meta$n_train))    meta_parts <- c(meta_parts, sprintf("Train = %d", meta$n_train))
  if (!is.null(meta$n_test))     meta_parts <- c(meta_parts, sprintf("Test = %d", meta$n_test))
  if (!is.null(meta$n_features)) meta_parts <- c(meta_parts, sprintf("Features = %d", meta$n_features))
  if (!is.null(meta$cv_folds))   meta_parts <- c(meta_parts, sprintf("CV folds = %d", meta$cv_folds))
  if (!is.null(meta$tuned))      meta_parts <- c(meta_parts, sprintf("Tuned = %s", meta$tuned))
  meta_text <- paste(meta_parts, collapse = ", ")

  # Test metrics
  metrics_text <- ""
  if (!is.null(result$test_metrics) && is.data.frame(result$test_metrics)) {
    tm <- result$test_metrics
    metrics_text <- paste(sapply(seq_len(nrow(tm)), function(i) {
      sprintf("  %s: %.4f", tm$.metric[i], tm$.estimate[i])
    }), collapse = "\n")
  }

  # Train vs Test comparison
  comparison_text <- ""
  if (!is.null(result$evaluation$overfitting$comparison)) {
    comp <- result$evaluation$overfitting$comparison
    comparison_text <- paste(sapply(seq_len(nrow(comp)), function(i) {
      sprintf("  %s: train=%.4f, test=%.4f, gap=%.4f",
              comp$metric[i], comp$train[i], comp$test[i], comp$gap[i])
    }), collapse = "\n")
  }

  # CV model comparison
  cv_text <- ""
  if (!is.null(result$cv_summary) && is.data.frame(result$cv_summary)) {
    cv <- result$cv_summary
    select_metric <- meta$select_metric %||%
      (if (task == "classification") "mcc" else "rsq")
    if (select_metric %in% names(cv) && "model" %in% names(cv)) {
      cv_text <- paste(sapply(seq_len(nrow(cv)), function(i) {
        sprintf("  %s: %s=%.4f", cv$model[i], select_metric, cv[[select_metric]][i])
      }), collapse = "\n")
    }
  }

  # Variable importance (top 10)
  importance_text <- ""
  if (!is.null(result$importance) && is.data.frame(result$importance) &&
      nrow(result$importance) > 0) {
    top_imp <- utils::head(result$importance, 10)
    importance_text <- paste(sapply(seq_len(nrow(top_imp)), function(i) {
      sprintf("  %d. %s (%.4f)", i, top_imp$Variable[i], top_imp$Importance[i])
    }), collapse = "\n")
  }

  # Format diagnostic findings for user message
  diag_text <- ""
  if (length(diag_findings) > 0) {
    diag_text <- paste(sapply(diag_findings, function(f) {
      sprintf("[%s] %s: %s\n  Detail: %s",
              toupper(f$severity), f$id, f$issue, f$details)
    }), collapse = "\n")
  }

  # --- Console header + diagnostics ---
  if (verbose) cat("\n")
  if (verbose) cat(strrep("=", 60), "\n")
  if (language == "es") {
    if (verbose) cat("  Asistente IA - Interpretacion de Resultados\n")
  } else {
    if (verbose) cat("  AI Assistant - Results Interpretation\n")
  }
  if (verbose) cat(strrep("=", 60), "\n")

  if (length(diag_findings) > 0) {
    if (verbose) cat("\n")
    if (language == "es") {
      if (verbose) cat("[DIAGNOSTICOS DETECTADOS]\n")
    } else {
      if (verbose) cat("[DIAGNOSTICS DETECTED]\n")
    }
    for (f in diag_findings) {
      icon <- switch(f$severity,
        critical = "!",
        warning  = "~",
        info     = "i",
        "?"
      )
      label <- toupper(f$severity)
      if (verbose) cat(sprintf("  %s %s: %s\n", icon, label, f$issue))
    }
    if (verbose) cat("\n")
  }


  if (language == "es") {
    if (verbose) cat("  Consultando", provider, "(", model, ") ...\n")
  } else {
    if (verbose) cat("  Querying", provider, "(", model, ") ...\n")
  }
  if (verbose) cat(strrep("-", 60), "\n\n")

  # --- System prompt ---
  system_prompt <- if (language == "es") {
    paste0(
      "Eres un asistente experto en Machine Learning integrado en el paquete R PsyML. ",
      "Interpretas resultados de analisis predictivos con consejo practico y directo.\n\n",
      "REGLAS ESTRICTAS:\n",
      "- TODO codigo R que sugieras DEBE usar SOLAMENTE funciones de PsyML: supervised_ml(), ",
      "explain_with_ai(), plot(), predict(), summary(). NUNCA sugieras paquetes externos ",
      "(caret, randomForest, pROC, glmnet, etc.). Si no existe funcion en PsyML para algo, ",
      "dilo pero NO inventes codigo con otros paquetes.\n",
      "- Si el diagnostico detecta un problema CRITICAL, se directo: 'Estos resultados son ",
      "sospechosos y requieren investigacion inmediata' o 'Este dataset es trivialmente ",
      "separable'. PROHIBIDO lenguaje evasivo como 'posiblemente' o 'podria ser'.\n",
      "- Si todos los modelos logran AUC=1.0 en un dataset clasico conocido (iris, mtcars, ",
      "penguins), aclara que el dataset es trivialmente separable por naturaleza, ",
      "no necesariamente hay fuga de datos.\n",
      "- Responde MAXIMO 300 palabras. Se conciso y directo.\n",
      "- Usa texto plano. PROHIBIDO: markdown, bullets, asteriscos, negritas, listas con guiones.\n\n",
      "Estructura tu respuesta en 3 secciones:\n",
      "1. EVALUACION: Valoracion directa de resultados y diagnosticos detectados.\n",
      "2. INTERPRETACION: Metricas, variables importantes, comparacion de modelos.\n",
      "3. PROXIMOS PASOS: Recomendaciones concretas con codigo R usando SOLO PsyML.\n\n",
      "Ejemplo de codigo correcto:\n",
      "  result2 <- supervised_ml(data, \"target\", \"classification\", exclude_vars = c(\"var1\"))\n",
      "  result3 <- supervised_ml(data, \"target\", \"classification\", use_pca = TRUE, tune = TRUE)"
    )
  } else {
    paste0(
      "You are an expert Machine Learning assistant integrated into the PsyML R package. ",
      "You interpret predictive analysis results with practical, direct advice.\n\n",
      "STRICT RULES:\n",
      "- ALL R code you suggest MUST use ONLY PsyML functions: supervised_ml(), ",
      "explain_with_ai(), plot(), predict(), summary(). NEVER suggest external packages ",
      "(caret, randomForest, pROC, glmnet, etc.). If PsyML has no function for something, ",
      "say so but DO NOT invent code with other packages.\n",
      "- If the diagnostic detects a CRITICAL issue, be direct: 'These results are suspicious ",
      "and require immediate investigation' or 'This dataset is trivially separable'. ",
      "FORBIDDEN: evasive language like 'possibly' or 'might be'.\n",
      "- If all models achieve AUC=1.0 on a well-known classic dataset (iris, mtcars, ",
      "penguins), clarify the dataset is trivially separable by nature, ",
      "not necessarily data leakage.\n",
      "- Respond in MAXIMUM 300 words. Be concise and direct.\n",
      "- Use plain text. FORBIDDEN: markdown, bullets, asterisks, bold, dash lists.\n\n",
      "Structure your response in 3 sections:\n",
      "1. ASSESSMENT: Direct evaluation of results and detected diagnostics.\n",
      "2. INTERPRETATION: Metrics, important variables, model comparison.\n",
      "3. NEXT STEPS: Concrete recommendations with R code using ONLY PsyML.\n\n",
      "Example of correct code:\n",
      "  result2 <- supervised_ml(data, \"target\", \"classification\", exclude_vars = c(\"var1\"))\n",
      "  result3 <- supervised_ml(data, \"target\", \"classification\", use_pca = TRUE, tune = TRUE)"
    )
  }

  # --- User message ---
  # Detectar multiclase
  n_classes_ai <- 2L
  if (task == "classification" && !is.null(result$train_data) &&
      target %in% names(result$train_data)) {
    n_classes_ai <- length(levels(factor(result$train_data[[target]])))
  }
  class_type_label <- if (n_classes_ai >= 3) {
    sprintf("multiclass (%d classes)", n_classes_ai)
  } else "binary"

  user_sections <- c(
    sprintf("Task: %s (%s) | Target: %s | Best model: %s", task, class_type_label, target, best_model),
    sprintf("Dataset: %s", meta_text),
    "",
    "Test metrics:",
    metrics_text
  )

  if (nzchar(comparison_text)) {
    user_sections <- c(user_sections, "", "Train vs Test comparison:", comparison_text)
  }

  if (nzchar(cv_text)) {
    user_sections <- c(user_sections, "", "CV model comparison:", cv_text)
  }

  if (nzchar(importance_text)) {
    user_sections <- c(user_sections, "", "Top important variables:", importance_text)
  }

  if (nzchar(diag_text)) {
    user_sections <- c(user_sections, "", "DIAGNOSTIC FINDINGS:", diag_text)
  }

  user_message <- paste(user_sections, collapse = "\n")

  # --- API call ---
  body <- list(
    model       = model,
    temperature = 0.5,
    max_tokens  = as.integer(max_tokens),
    messages    = list(
      list(role = "system", content = system_prompt),
      list(role = "user",   content = user_message)
    )
  )

  response <- tryCatch({
    httr::POST(
      url    = base_url,
      httr::add_headers(
        Authorization  = paste("Bearer", api_key),
        `Content-Type` = "application/json"
      ),
      body   = jsonlite::toJSON(body, auto_unbox = TRUE),
      encode = "raw",
      httr::timeout(120)
    )
  }, error = function(e) {
    return(list(.error = e$message))
  })

  # Connection error
  if (is.list(response) && !is.null(response$.error)) {
    msg <- paste("Connection error:", response$.error)
    message(msg)
    return(invisible(list(success = FALSE, text = msg, diagnostics = diag_findings)))
  }

  status <- httr::status_code(response)

  # --- Handle rate limit (429) ---
  if (status == 429) {
    resp_text <- httr::content(response, "text", encoding = "UTF-8")
    wait_time <- 30
    wait_match <- regmatches(resp_text, regexpr("[0-9]+\\.?[0-9]*s", resp_text))
    if (length(wait_match) > 0) {
      parsed <- as.numeric(gsub("s$", "", wait_match[1]))
      if (!is.na(parsed)) wait_time <- ceiling(parsed) + 1
    }
    if (language == "es") {
      if (verbose) cat(sprintf("  Rate limit alcanzado. Esperando %d segundos...\n", wait_time))
    } else {
      if (verbose) cat(sprintf("  Rate limit reached. Waiting %d seconds...\n", wait_time))
    }
    Sys.sleep(wait_time)

    response <- tryCatch({
      httr::POST(
        url    = base_url,
        httr::add_headers(
          Authorization  = paste("Bearer", api_key),
          `Content-Type` = "application/json"
        ),
        body   = jsonlite::toJSON(body, auto_unbox = TRUE),
        encode = "raw",
        httr::timeout(120)
      )
    }, error = function(e) {
      return(list(.error = e$message))
    })

    if (is.list(response) && !is.null(response$.error)) {
      msg <- paste("Connection error on retry:", response$.error)
      message(msg)
      return(invisible(list(success = FALSE, text = msg, diagnostics = diag_findings)))
    }
    status <- httr::status_code(response)
  }

  # --- Handle 503 ---
  if (status == 503) {
    if (language == "es") {
      if (verbose) cat("  Servicio no disponible (503). Reintentando en 10s...\n")
    } else {
      if (verbose) cat("  Service unavailable (503). Retrying in 10s...\n")
    }
    Sys.sleep(10)
    response <- tryCatch({
      httr::POST(
        url    = base_url,
        httr::add_headers(
          Authorization  = paste("Bearer", api_key),
          `Content-Type` = "application/json"
        ),
        body   = jsonlite::toJSON(body, auto_unbox = TRUE),
        encode = "raw",
        httr::timeout(120)
      )
    }, error = function(e) {
      return(list(.error = e$message))
    })
    if (is.list(response) && !is.null(response$.error)) {
      msg <- paste("Connection error on retry:", response$.error)
      message(msg)
      return(invisible(list(success = FALSE, text = msg, diagnostics = diag_findings)))
    }
    status <- httr::status_code(response)
  }

  # --- Handle other errors ---
  if (status != 200) {
    resp_text <- httr::content(response, "text", encoding = "UTF-8")
    msg <- sprintf("API error (HTTP %d): %s", status, resp_text)
    message(msg)
    return(invisible(list(success = FALSE, text = msg, diagnostics = diag_findings)))
  }

  # --- Parse response ---
  resp_json <- tryCatch({
    jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"),
                       simplifyVector = FALSE)
  }, error = function(e) {
    NULL
  })

  if (is.null(resp_json) || is.null(resp_json$choices)) {
    msg <- "Could not parse API response."
    message(msg)
    return(invisible(list(success = FALSE, text = msg, diagnostics = diag_findings)))
  }

  ai_text <- resp_json$choices[[1]]$message$content

  if (is.null(ai_text) || !nzchar(ai_text)) {
    msg <- "API returned empty response."
    message(msg)
    return(invisible(list(success = FALSE, text = msg, diagnostics = diag_findings)))
  }

  # --- Print AI response ---
  if (language == "es") {
    if (verbose) cat("[INTERPRETACION IA]\n\n")
  } else {
    if (verbose) cat("[AI INTERPRETATION]\n\n")
  }
  if (verbose) cat(ai_text, "\n")
  if (verbose) cat("\n", strrep("=", 60), "\n")
  if (language == "es") {
    if (verbose) cat("  Modelo:", model, "| Proveedor:", provider, "\n")
  } else {
    if (verbose) cat("  Model:", model, "| Provider:", provider, "\n")
  }
  if (verbose) cat(strrep("=", 60), "\n\n")

  invisible(list(success = TRUE, text = ai_text, diagnostics = diag_findings))
}
