
# =============================================================================
# EDA para Aprendizaje No Supervisado
# =============================================================================

#' @noRd
.unsupervised_eda <- function(data, exclude_cols = NULL, verbose = TRUE) {

  if (verbose) {
    .print_section(1, "Analisis Exploratorio de Datos (EDA)")
  }

  # Remover columnas excluidas para el analisis
  data_analysis <- data
  if (!is.null(exclude_cols)) {
    data_analysis <- data[, !names(data) %in% exclude_cols, drop = FALSE]
  }

  result <- list()

  # ----- 1.1 Estructura -----
  if (verbose) .print_subsection(1, 1, "Estructura del Dataset")

  n_obs <- nrow(data_analysis)
  n_vars <- ncol(data_analysis)
  numeric_vars <- names(data_analysis)[sapply(data_analysis, is.numeric)]
  categorical_vars <- names(data_analysis)[!sapply(data_analysis, is.numeric)]

  result$structure <- list(
    n_obs = n_obs,
    n_vars = n_vars,
    numeric_vars = numeric_vars,
    categorical_vars = categorical_vars
  )

  if (verbose) {
    cat("  Observaciones:", n_obs, "\n")
    cat("  Variables:", n_vars, "\n")
    cat("  Numericas:", length(numeric_vars), "\n")
    cat("  Categoricas:", length(categorical_vars), "\n")
    if (length(categorical_vars) > 0) {
      cat("  Categoricas:", paste(categorical_vars, collapse = ", "), "\n")
      cat("  [!] Las variables categoricas seran excluidas del analisis numerico.\n")
      cat("      Para incluirlas, considere dummy encoding o distancia Gower.\n")
    }
  }

  # ----- 1.2 Valores faltantes -----
  if (verbose) .print_subsection(1, 2, "Valores Faltantes")

  missing_count <- colSums(is.na(data_analysis))
  missing_pct <- round(missing_count / n_obs * 100, 2)
  has_missing <- missing_count[missing_count > 0]

  result$missing <- data.frame(
    variable = names(missing_count),
    n_missing = as.integer(missing_count),
    pct_missing = missing_pct,
    stringsAsFactors = FALSE
  )

  if (verbose) {
    if (length(has_missing) == 0) {
      cat("  No se detectaron valores faltantes.\n")
    } else {
      cat("  Variables con missings:\n")
      for (v in names(has_missing)) {
        cat(sprintf("    %s: %d (%.1f%%)\n", v, has_missing[v], missing_pct[v]))
      }
    }
  }

  # ----- 1.3 Estadisticas descriptivas (solo numericas) -----
  if (verbose) .print_subsection(1, 3, "Estadisticas Descriptivas")

  if (length(numeric_vars) > 0) {
    stats_df <- data.frame(
      variable = numeric_vars,
      mean = sapply(data_analysis[numeric_vars], mean, na.rm = TRUE),
      sd = sapply(data_analysis[numeric_vars], sd, na.rm = TRUE),
      min = sapply(data_analysis[numeric_vars], min, na.rm = TRUE),
      q25 = sapply(data_analysis[numeric_vars], quantile, 0.25, na.rm = TRUE),
      median = sapply(data_analysis[numeric_vars], median, na.rm = TRUE),
      q75 = sapply(data_analysis[numeric_vars], quantile, 0.75, na.rm = TRUE),
      max = sapply(data_analysis[numeric_vars], max, na.rm = TRUE),
      skewness = sapply(data_analysis[numeric_vars], function(x) {
        x <- x[!is.na(x)]
        n <- length(x)
        if (n < 3) return(NA_real_)
        m <- mean(x)
        s <- sd(x)
        if (s == 0) return(0)
        (sum((x - m)^3) / n) / (s^3)
      }),
      stringsAsFactors = FALSE,
      row.names = NULL
    )

    result$descriptive_stats <- stats_df

    if (verbose) {
      # Mostrar tabla compacta
      cat(sprintf("  %-20s %10s %10s %10s %10s\n",
                  "Variable", "Media", "DE", "Min", "Max"))
      cat("  ", .line("-", 64), "\n", sep = "")
      for (i in seq_len(min(nrow(stats_df), 15))) {
        cat(sprintf("  %-20s %10.3f %10.3f %10.3f %10.3f\n",
                    substr(stats_df$variable[i], 1, 20),
                    stats_df$mean[i], stats_df$sd[i],
                    stats_df$min[i], stats_df$max[i]))
      }
      if (nrow(stats_df) > 15) {
        cat("  ... (", nrow(stats_df) - 15, " variables mas)\n")
      }
    }
  }

  # ----- 1.4 Deteccion de outliers (IQR) -----
  if (verbose) .print_subsection(1, 4, "Deteccion de Outliers (IQR)")

  if (length(numeric_vars) > 0) {
    outlier_info <- lapply(numeric_vars, function(v) {
      x <- data_analysis[[v]]
      x <- x[!is.na(x)]
      q1 <- quantile(x, 0.25)
      q3 <- quantile(x, 0.75)
      iqr_val <- q3 - q1
      lower <- q1 - 1.5 * iqr_val
      upper <- q3 + 1.5 * iqr_val
      n_outliers <- sum(x < lower | x > upper)
      data.frame(variable = v, n_outliers = n_outliers,
                 pct_outliers = round(n_outliers / length(x) * 100, 2),
                 lower_bound = lower, upper_bound = upper,
                 stringsAsFactors = FALSE)
    })
    outlier_df <- do.call(rbind, outlier_info)
    result$outliers <- outlier_df

    if (verbose) {
      vars_with_outliers <- outlier_df[outlier_df$n_outliers > 0, ]
      if (nrow(vars_with_outliers) == 0) {
        cat("  No se detectaron outliers (criterio IQR x 1.5).\n")
      } else {
        cat("  Variables con outliers:\n")
        for (i in seq_len(min(nrow(vars_with_outliers), 10))) {
          cat(sprintf("    %s: %d outliers (%.1f%%)\n",
                      vars_with_outliers$variable[i],
                      vars_with_outliers$n_outliers[i],
                      vars_with_outliers$pct_outliers[i]))
        }
      }
    }
  }

  # ----- 1.5 Correlacion -----
  if (verbose) .print_subsection(1, 5, "Matriz de Correlacion")

  if (length(numeric_vars) >= 2) {
    cor_matrix <- cor(data_analysis[numeric_vars], use = "pairwise.complete.obs")
    result$correlation <- cor_matrix

    # Pares altamente correlacionados
    high_cor_pairs <- which(abs(cor_matrix) > 0.80 & upper.tri(cor_matrix), arr.ind = TRUE)

    if (verbose) {
      if (nrow(high_cor_pairs) == 0) {
        cat("  No se detectaron pares con |r| > 0.80.\n")
      } else {
        cat("  Pares con alta correlacion (|r| > 0.80):\n")
        for (i in seq_len(min(nrow(high_cor_pairs), 10))) {
          r <- high_cor_pairs[i, ]
          v1 <- numeric_vars[r[1]]
          v2 <- numeric_vars[r[2]]
          rval <- cor_matrix[r[1], r[2]]
          cat(sprintf("    %s <-> %s: r = %.3f\n", v1, v2, rval))
        }
      }
    }
  }

  # ----- 1.6 Test de normalidad -----
  if (verbose) .print_subsection(1, 6, "Test de Normalidad (Shapiro-Wilk)")

  if (length(numeric_vars) > 0) {
    norm_results <- lapply(numeric_vars, function(v) {
      x <- data_analysis[[v]]
      x <- x[!is.na(x)]
      # Shapiro-Wilk requiere n entre 3 y 5000
      if (length(x) >= 3 && length(x) <= 5000 && sd(x) > 0) {
        test <- shapiro.test(x)
        data.frame(variable = v, W = test$statistic, p_value = test$p.value,
                   normal = test$p.value > 0.05, stringsAsFactors = FALSE)
      } else if (length(x) >= 3 && sd(x) == 0) {
        data.frame(variable = v, W = NA, p_value = NA,
                   normal = NA, stringsAsFactors = FALSE)
      } else {
        data.frame(variable = v, W = NA, p_value = NA,
                   normal = NA, stringsAsFactors = FALSE)
      }
    })
    norm_df <- do.call(rbind, norm_results)
    result$normality <- norm_df

    if (verbose) {
      n_normal <- sum(norm_df$normal == TRUE, na.rm = TRUE)
      n_nonnormal <- sum(norm_df$normal == FALSE, na.rm = TRUE)
      cat("  Variables normales (p > 0.05):", n_normal, "\n")
      cat("  Variables no normales (p <= 0.05):", n_nonnormal, "\n")
      if (n_nonnormal > n_normal) {
        cat("  [!] La mayoria de variables no siguen distribucion normal.\n")
        cat("      Considere distancias robustas o metodos no parametricos.\n")
      }
    }
  }

  return(result)
}
