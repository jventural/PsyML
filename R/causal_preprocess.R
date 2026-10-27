# =============================================================================
# Preprocesamiento para causal_ml
# =============================================================================

#' @noRd
.causal_preprocess <- function(data, treatment, outcome, covariates,
                               treatment_type,
                               exclude_cols = NULL,
                               impute = TRUE,
                               impute_method = "median",
                               normalize = TRUE,
                               normalize_method = "zscore",
                               verbose = TRUE) {

  if (verbose) {
    .print_section(2, "Preprocesamiento Causal")
  }

  result <- list()
  df <- data

  # --- 2.1 Exclusion de columnas ---
  if (!is.null(exclude_cols)) {
    covariates <- setdiff(covariates, exclude_cols)
    if (verbose) {
      .print_subsection(2, 1, "Exclusion de Columnas")
      cat("  Excluidas:", paste(exclude_cols, collapse = ", "), "\n")
      cat("  Covariables restantes:", length(covariates), "\n")
    }
  }

  # --- 2.2 Validacion del tratamiento ---
  if (verbose) {
    .print_subsection(2, 2, "Validacion del Tratamiento")
  }

  if (treatment_type == "binary") {
    df[[treatment]] <- .ensure_binary_treatment(df[[treatment]])
    if (verbose) {
      cat("  Tratamiento codificado como 0/1\n")
      cat("  Control (0): n = ", sum(df[[treatment]] == 0), "\n", sep = "")
      cat("  Tratamiento (1): n = ", sum(df[[treatment]] == 1), "\n", sep = "")
    }
  } else {
    if (!is.numeric(df[[treatment]])) {
      stop("A continuous treatment must be numeric")
    }
    if (verbose) {
      cat("  Tratamiento continuo - sin recodificacion\n")
    }
  }

  result$treatment_type <- treatment_type

  # --- 2.3 Imputacion ---
  if (impute) {
    if (verbose) {
      .print_subsection(2, 3, "Imputacion de Valores Faltantes")
    }

    n_missing_before <- sum(is.na(df[, c(covariates, outcome), drop = FALSE]))

    if (n_missing_before > 0) {
      for (v in covariates) {
        if (any(is.na(df[[v]]))) {
          if (is.numeric(df[[v]])) {
            if (impute_method == "median") {
              df[[v]][is.na(df[[v]])] <- stats::median(df[[v]], na.rm = TRUE)
            } else if (impute_method == "mean") {
              df[[v]][is.na(df[[v]])] <- mean(df[[v]], na.rm = TRUE)
            } else if (impute_method == "knn") {
              # Fallback a median si knn no disponible
              df[[v]][is.na(df[[v]])] <- stats::median(df[[v]], na.rm = TRUE)
            }
          } else {
            # Moda para categoricas
            mode_val <- names(sort(table(df[[v]]), decreasing = TRUE))[1]
            df[[v]][is.na(df[[v]])] <- mode_val
          }
        }
      }
      # Imputar outcome
      if (any(is.na(df[[outcome]])) && is.numeric(df[[outcome]])) {
        df[[outcome]][is.na(df[[outcome]])] <- stats::median(df[[outcome]], na.rm = TRUE)
      }

      n_missing_after <- sum(is.na(df[, c(covariates, outcome), drop = FALSE]))

      if (verbose) {
        cat("  Metodo:", impute_method, "\n")
        cat("  Antes:", n_missing_before, "valores faltantes\n")
        cat("  Despues:", n_missing_after, "valores faltantes\n")
      }
    } else {
      if (verbose) cat("  Sin valores faltantes - imputacion no necesaria\n")
    }
  }

  # --- 2.4 Normalizacion ---
  norm_params <- NULL
  if (normalize) {
    if (verbose) {
      .print_subsection(2, 4, "Normalizacion de Covariables")
    }

    num_covs <- covariates[sapply(df[, covariates, drop = FALSE], is.numeric)]
    norm_params <- list()

    if (length(num_covs) > 0) {
      for (v in num_covs) {
        if (normalize_method == "zscore") {
          mu <- mean(df[[v]], na.rm = TRUE)
          s <- stats::sd(df[[v]], na.rm = TRUE)
          if (s > 1e-10) {
            df[[v]] <- (df[[v]] - mu) / s
            norm_params[[v]] <- list(method = "zscore", mean = mu, sd = s)
          }
        } else if (normalize_method == "minmax") {
          mn <- min(df[[v]], na.rm = TRUE)
          mx <- max(df[[v]], na.rm = TRUE)
          rng <- mx - mn
          if (rng > 1e-10) {
            df[[v]] <- (df[[v]] - mn) / rng
            norm_params[[v]] <- list(method = "minmax", min = mn, max = mx)
          }
        }
      }
      if (verbose) {
        cat("  Metodo:", normalize_method, "\n")
        cat("  Variables normalizadas:", length(num_covs), "\n")
      }
    }
  }

  # --- 2.5 Remover varianza cero ---
  zero_var <- sapply(df[, covariates, drop = FALSE], function(x) {
    length(unique(stats::na.omit(x))) <= 1
  })
  if (any(zero_var)) {
    removed <- covariates[zero_var]
    covariates <- covariates[!zero_var]
    if (verbose) {
      cat("\n  Variables con varianza cero removidas:", paste(removed, collapse = ", "), "\n")
    }
  }

  # Convertir caracteres a factores
  for (v in covariates) {
    if (is.character(df[[v]])) {
      df[[v]] <- as.factor(df[[v]])
    }
  }

  result$data <- df
  result$covariates <- covariates
  result$norm_params <- norm_params
  result$n_obs <- nrow(df)

  if (verbose) {
    cat("\n  Datos preprocesados: ", nrow(df), " obs x ",
        length(covariates), " covariables\n", sep = "")
  }

  return(result)
}
