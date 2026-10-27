
# =============================================================================
# Preprocesamiento para Aprendizaje No Supervisado
# =============================================================================

#' @noRd
.unsupervised_preprocess <- function(data, exclude_cols = NULL,
                                      impute = TRUE, impute_method = "median",
                                      normalize = TRUE, normalize_method = "zscore",
                                      treat_outliers = FALSE, outlier_percentile = 0.05,
                                      remove_high_cor = TRUE, cor_threshold = 0.90,
                                      verbose = TRUE) {

  if (verbose) {
    .print_section(2, "Preprocesamiento")
  }

  result <- list()
  data_clean <- data
  removed_cols <- character(0)

  # ----- 2.1 Excluir columnas -----
  if (verbose) .print_subsection(2, 1, "Seleccion de Variables")

  if (!is.null(exclude_cols)) {
    existing <- intersect(exclude_cols, names(data_clean))
    if (length(existing) > 0) {
      data_clean <- data_clean[, !names(data_clean) %in% existing, drop = FALSE]
      removed_cols <- c(removed_cols, existing)
      if (verbose) cat("  Excluidas por usuario:", paste(existing, collapse = ", "), "\n")
    }
  }

  # Remover columnas no numericas
  non_numeric <- names(data_clean)[!sapply(data_clean, is.numeric)]
  if (length(non_numeric) > 0) {
    data_clean <- data_clean[, sapply(data_clean, is.numeric), drop = FALSE]
    removed_cols <- c(removed_cols, non_numeric)
    if (verbose) {
      cat("  Excluidas (no numericas):", paste(non_numeric, collapse = ", "), "\n")
    }
  }

  # Remover columnas con varianza cero
  var_zero <- names(data_clean)[sapply(data_clean, function(x) {
    var(x, na.rm = TRUE) == 0 || all(is.na(x))
  })]
  if (length(var_zero) > 0) {
    data_clean <- data_clean[, !names(data_clean) %in% var_zero, drop = FALSE]
    removed_cols <- c(removed_cols, var_zero)
    if (verbose) cat("  Excluidas (varianza cero):", paste(var_zero, collapse = ", "), "\n")
  }

  if (verbose) {
    cat("  Variables para analisis:", ncol(data_clean), "\n")
  }

  n_vars_initial <- ncol(data_clean)

  # ----- 2.2 Imputacion -----
  if (impute && any(is.na(data_clean))) {
    if (verbose) .print_subsection(2, 2, "Imputacion de Valores Faltantes")

    n_missing_before <- sum(is.na(data_clean))

    if (impute_method == "median") {
      for (v in names(data_clean)) {
        if (any(is.na(data_clean[[v]]))) {
          data_clean[[v]][is.na(data_clean[[v]])] <- median(data_clean[[v]], na.rm = TRUE)
        }
      }
    } else if (impute_method == "mean") {
      for (v in names(data_clean)) {
        if (any(is.na(data_clean[[v]]))) {
          data_clean[[v]][is.na(data_clean[[v]])] <- mean(data_clean[[v]], na.rm = TRUE)
        }
      }
    } else if (impute_method == "knn") {
      # KNN imputation usando VIM si esta disponible, sino median
      if (requireNamespace("VIM", quietly = TRUE)) {
        data_clean <- VIM::kNN(data_clean, imp_var = FALSE)
      } else {
        if (verbose) cat("  [!] Paquete VIM no disponible. Usando mediana.\n")
        for (v in names(data_clean)) {
          if (any(is.na(data_clean[[v]]))) {
            data_clean[[v]][is.na(data_clean[[v]])] <- median(data_clean[[v]], na.rm = TRUE)
          }
        }
      }
    }

    if (verbose) {
      cat("  Metodo:", impute_method, "\n")
      cat("  Valores imputados:", n_missing_before, "\n")
    }
  }

  # ----- 2.3 Tratamiento de outliers -----
  if (treat_outliers) {
    if (verbose) .print_subsection(2, 3, "Tratamiento de Outliers (Winsorizacion)")

    n_winsorized <- 0
    lower_p <- outlier_percentile
    upper_p <- 1 - outlier_percentile

    for (v in names(data_clean)) {
      x <- data_clean[[v]]
      lower_val <- quantile(x, lower_p, na.rm = TRUE)
      upper_val <- quantile(x, upper_p, na.rm = TRUE)
      n_low <- sum(x < lower_val, na.rm = TRUE)
      n_high <- sum(x > upper_val, na.rm = TRUE)
      n_winsorized <- n_winsorized + n_low + n_high
      data_clean[[v]] <- pmax(pmin(x, upper_val), lower_val)
    }

    if (verbose) {
      cat("  Percentil:", outlier_percentile, "-", 1 - outlier_percentile, "\n")
      cat("  Valores winsorizados:", n_winsorized, "\n")
    }
  }

  # ----- 2.4 Remover alta correlacion -----
  if (remove_high_cor && ncol(data_clean) >= 2) {
    if (verbose) .print_subsection(2, 4, "Eliminacion de Variables Altamente Correlacionadas")

    cor_mat <- cor(data_clean, use = "pairwise.complete.obs")
    # Encontrar pares con correlacion > threshold
    to_remove <- character(0)
    for (i in seq_len(ncol(cor_mat) - 1)) {
      for (j in (i + 1):ncol(cor_mat)) {
        if (abs(cor_mat[i, j]) > cor_threshold) {
          # Remover la variable con mayor correlacion media con las demas
          mean_cor_i <- mean(abs(cor_mat[i, -i]))
          mean_cor_j <- mean(abs(cor_mat[j, -j]))
          if (mean_cor_i > mean_cor_j) {
            to_remove <- c(to_remove, colnames(cor_mat)[i])
          } else {
            to_remove <- c(to_remove, colnames(cor_mat)[j])
          }
        }
      }
    }
    to_remove <- unique(to_remove)

    if (length(to_remove) > 0) {
      data_clean <- data_clean[, !names(data_clean) %in% to_remove, drop = FALSE]
      removed_cols <- c(removed_cols, to_remove)
      if (verbose) {
        cat("  Umbral: |r| >", cor_threshold, "\n")
        cat("  Variables removidas:", paste(to_remove, collapse = ", "), "\n")
        cat("  Variables restantes:", ncol(data_clean), "\n")
      }
    } else {
      if (verbose) cat("  No se removieron variables (ninguna supera umbral).\n")
    }
  }

  # Guardar datos antes de normalizar (para interpretacion)
  result$data_before_norm <- data_clean

  # ----- 2.5 Normalizacion -----
  if (normalize) {
    if (verbose) .print_subsection(2, 5, "Normalizacion")

    result$norm_params <- list()

    if (normalize_method == "zscore") {
      for (v in names(data_clean)) {
        m <- mean(data_clean[[v]], na.rm = TRUE)
        s <- sd(data_clean[[v]], na.rm = TRUE)
        if (s > 0) {
          data_clean[[v]] <- (data_clean[[v]] - m) / s
        }
        result$norm_params[[v]] <- list(method = "zscore", center = m, scale = s)
      }
    } else if (normalize_method == "minmax") {
      for (v in names(data_clean)) {
        min_val <- min(data_clean[[v]], na.rm = TRUE)
        max_val <- max(data_clean[[v]], na.rm = TRUE)
        range_val <- max_val - min_val
        if (range_val > 0) {
          data_clean[[v]] <- (data_clean[[v]] - min_val) / range_val
        }
        result$norm_params[[v]] <- list(method = "minmax", min = min_val, max = max_val)
      }
    }

    if (verbose) {
      cat("  Metodo:", normalize_method, "\n")
      cat("  Variables normalizadas:", ncol(data_clean), "\n")
    }
  }

  result$data_clean <- data_clean
  result$removed_cols <- removed_cols
  result$n_vars_original <- ncol(data)
  result$n_vars_final <- ncol(data_clean)

  if (verbose) {
    cat("\n  Resumen preprocesamiento:\n")
    cat("    Variables originales:", ncol(data), "\n")
    cat("    Variables finales:", ncol(data_clean), "\n")
    cat("    Observaciones:", nrow(data_clean), "\n")
  }

  return(result)
}
