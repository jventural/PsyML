# =============================================================================
# EDA Causal para causal_ml
# =============================================================================

#' @noRd
.causal_eda <- function(data, treatment, outcome, covariates,
                        treatment_type, verbose = TRUE) {

  result <- list()

  if (verbose) {
    .print_section(1, "Analisis Exploratorio Causal")
  }

  # --- 1.1 Estructura ---
  if (verbose) {
    .print_subsection(1, 1, "Estructura de los Datos")
    cat("  Observaciones:", nrow(data), "\n")
    cat("  Covariables:", length(covariates), "\n")
    cat("  Tratamiento:", treatment, "(", treatment_type, ")\n")
    cat("  Resultado:", outcome, "\n")
  }

  result$n_obs <- nrow(data)
  result$n_covariates <- length(covariates)
  result$treatment_type <- treatment_type

  # --- 1.2 Distribucion del tratamiento ---
  if (verbose) {
    .print_subsection(1, 2, "Distribucion del Tratamiento")
  }

  t_vec <- data[[treatment]]
  if (treatment_type == "binary") {
    t_tab <- table(t_vec)
    t_prop <- prop.table(t_tab)
    result$treatment_table <- t_tab
    result$treatment_prop <- t_prop

    if (verbose) {
      cat("  Grupos:\n")
      for (i in seq_along(t_tab)) {
        cat("    ", names(t_tab)[i], ": n = ", t_tab[i],
            " (", round(t_prop[i] * 100, 1), "%)\n", sep = "")
      }
      # Verificar desbalance
      min_prop <- min(t_prop)
      if (min_prop < 0.1) {
        cat("\n  ADVERTENCIA: Desbalance severo (<10% en un grupo).\n")
        cat("  Considerar metodos robustos a desbalance (IPW, AIPW).\n")
      } else if (min_prop < 0.2) {
        cat("\n  Nota: Desbalance moderado. IPW puede ser inestable.\n")
      }
    }
  } else {
    result$treatment_summary <- summary(t_vec)
    if (verbose) {
      cat("  ", paste(names(summary(t_vec)), collapse = "  "), "\n")
      cat("  ", paste(round(summary(t_vec), 3), collapse = "  "), "\n")
    }
  }

  # --- 1.3 Distribucion del outcome ---
  if (verbose) {
    .print_subsection(1, 3, "Distribucion del Resultado")
  }

  y_vec <- data[[outcome]]
  if (is.numeric(y_vec)) {
    result$outcome_summary <- summary(y_vec)
    result$outcome_sd <- stats::sd(y_vec, na.rm = TRUE)
    if (verbose) {
      cat("  Media:", round(mean(y_vec, na.rm = TRUE), 3), "\n")
      cat("  SD:", round(result$outcome_sd, 3), "\n")
      cat("  Rango: [", round(min(y_vec, na.rm = TRUE), 3), ", ",
          round(max(y_vec, na.rm = TRUE), 3), "]\n", sep = "")

      if (treatment_type == "binary") {
        t_binary <- .ensure_binary_treatment(t_vec)
        m0 <- mean(y_vec[t_binary == 0], na.rm = TRUE)
        m1 <- mean(y_vec[t_binary == 1], na.rm = TRUE)
        cat("\n  Diferencia cruda de medias (no ajustada):\n")
        cat("    Control:", round(m0, 3), "\n")
        cat("    Tratamiento:", round(m1, 3), "\n")
        cat("    Diferencia:", round(m1 - m0, 3), "\n")
        result$crude_diff <- m1 - m0
      }
    }
  } else {
    result$outcome_table <- table(y_vec)
    if (verbose) {
      print(table(y_vec))
    }
  }

  # --- 1.4 Valores faltantes ---
  if (verbose) {
    .print_subsection(1, 4, "Valores Faltantes")
  }

  all_vars <- c(treatment, outcome, covariates)
  missing_counts <- sapply(data[, all_vars, drop = FALSE], function(x) sum(is.na(x)))
  missing_pct <- round(missing_counts / nrow(data) * 100, 1)
  result$missing <- data.frame(
    variable = all_vars,
    n_missing = missing_counts,
    pct_missing = missing_pct,
    stringsAsFactors = FALSE
  )

  if (verbose) {
    if (any(missing_counts > 0)) {
      vars_with_missing <- which(missing_counts > 0)
      cat("  Variables con valores faltantes:\n")
      for (i in vars_with_missing) {
        cat("    ", all_vars[i], ": ", missing_counts[i],
            " (", missing_pct[i], "%)\n", sep = "")
      }
    } else {
      cat("  Sin valores faltantes en variables de analisis.\n")
    }
  }

  # --- 1.5 Balance pre-ajuste (SMD) ---
  if (treatment_type == "binary") {
    if (verbose) {
      .print_subsection(1, 5, "Balance Pre-Ajuste (SMD)")
      .print_causal_reference("smd")
    }

    num_covs <- covariates[sapply(data[, covariates, drop = FALSE], is.numeric)]
    if (length(num_covs) > 0) {
      t_binary <- .ensure_binary_treatment(t_vec)
      smd_df <- .calc_all_smd(data, t_binary, num_covs)
      smd_df <- smd_df[order(-smd_df$smd), ]
      result$smd_pre <- smd_df

      if (verbose) {
        cat("\n  Standardized Mean Differences (|d|):\n")
        cat("  (Umbral recomendado: |d| < 0.10)\n\n")
        n_show <- min(nrow(smd_df), 15)
        for (i in seq_len(n_show)) {
          flag <- if (smd_df$smd[i] > 0.25) " ***" else
            if (smd_df$smd[i] > 0.10) " *" else ""
          cat("    ", sprintf("%-25s", smd_df$variable[i]),
              sprintf("%.3f", smd_df$smd[i]), flag, "\n", sep = "")
        }
        n_imbalanced <- sum(smd_df$smd > 0.10)
        cat("\n  Variables desbalanceadas (|d| > 0.10): ",
            n_imbalanced, "/", nrow(smd_df), "\n", sep = "")
      }
    }
  }

  # --- 1.6 Correlaciones con tratamiento ---
  if (verbose) {
    .print_subsection(1, 6, "Correlaciones con Tratamiento y Resultado")
  }

  num_covs <- covariates[sapply(data[, covariates, drop = FALSE], is.numeric)]
  if (length(num_covs) > 0) {
    t_numeric <- as.numeric(data[[treatment]])
    y_numeric <- as.numeric(data[[outcome]])

    cor_treatment <- sapply(num_covs, function(v) {
      stats::cor(data[[v]], t_numeric, use = "complete.obs")
    })
    cor_outcome <- sapply(num_covs, function(v) {
      stats::cor(data[[v]], y_numeric, use = "complete.obs")
    })

    result$cor_treatment <- sort(abs(cor_treatment), decreasing = TRUE)
    result$cor_outcome <- sort(abs(cor_outcome), decreasing = TRUE)

    # Identificar confusores potenciales (correlacionados con ambos)
    confounders <- num_covs[abs(cor_treatment) > 0.1 & abs(cor_outcome) > 0.1]
    result$potential_confounders <- confounders

    if (verbose) {
      cat("  Top correlaciones con tratamiento:\n")
      top_t <- head(sort(abs(cor_treatment), decreasing = TRUE), 5)
      for (i in seq_along(top_t)) {
        cat("    ", names(top_t)[i], ": r = ", round(top_t[i], 3), "\n", sep = "")
      }

      cat("\n  Top correlaciones con resultado:\n")
      top_y <- head(sort(abs(cor_outcome), decreasing = TRUE), 5)
      for (i in seq_along(top_y)) {
        cat("    ", names(top_y)[i], ": r = ", round(top_y[i], 3), "\n", sep = "")
      }

      if (length(confounders) > 0) {
        cat("\n  Confusores potenciales (|r| > 0.1 con ambos):\n")
        cat("    ", paste(confounders, collapse = ", "), "\n")
      }
    }
  }

  return(result)
}
