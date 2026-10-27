# =============================================================================
# Wrappers de Python (EconML) para causal_ml via reticulate
# =============================================================================

#' @noRd
.causal_python <- function(data, treatment, outcome, covariates,
                           python_methods = c("econml_dml", "econml_forest"),
                           confidence_level = 0.95,
                           seed = NULL,
                           verbose = TRUE) {

  if (verbose) {
    .print_section(7, "Metodos Python (EconML via reticulate)")
    .print_causal_reference("econml")
  }

  result <- list()
  summary_rows <- list()

  # Verificar reticulate

  if (!.check_package("reticulate", "Python integration")) {
    return(list(summary_rows = list()))
  }

  # Verificar Python y EconML
  python_available <- tryCatch({
    reticulate::py_available(initialize = TRUE)
  }, error = function(e) FALSE)

  if (!python_available) {
    if (verbose) cat("  Python no disponible. Saltando metodos Python.\n")
    return(list(summary_rows = list()))
  }

  econml_available <- tryCatch({
    reticulate::py_module_available("econml")
  }, error = function(e) FALSE)

  if (!econml_available) {
    if (verbose) {
      cat("  EconML no instalado en Python.\n")
      cat("  Instalar con: pip install econml\n")
    }
    return(list(summary_rows = list()))
  }

  # Preparar datos
  t_vec <- as.integer(data[[treatment]])
  y_vec <- as.numeric(data[[outcome]])
  X_mat <- .prepare_covariate_matrix(data, covariates)

  # Importar modulos
  econml <- reticulate::import("econml")
  np <- reticulate::import("numpy")

  X_py <- np$array(X_mat)
  Y_py <- np$array(y_vec)
  T_py <- np$array(t_vec)

  # --- 7.1 EconML DML ---
  if ("econml_dml" %in% python_methods) {
    if (verbose) {
      .print_subsection(7, 1, "EconML - LinearDML")
    }

    dml_py_result <- tryCatch({
      dml_module <- reticulate::import("econml.dml")
      sklearn_ensemble <- reticulate::import("sklearn.ensemble")

      model_y <- sklearn_ensemble$RandomForestRegressor(
        n_estimators = 200L, max_depth = 5L, random_state = if (is.null(seed)) NULL else as.integer(seed)
      )
      model_t <- sklearn_ensemble$RandomForestClassifier(
        n_estimators = 200L, max_depth = 5L, random_state = if (is.null(seed)) NULL else as.integer(seed)
      )

      est <- dml_module$LinearDML(
        model_y = model_y,
        model_t = model_t,
        cv = as.integer(5),
        random_state = if (is.null(seed)) NULL else as.integer(seed)
      )

      est$fit(Y_py, T_py, X = X_py)

      ate_py <- as.numeric(est$ate(X_py))
      ate_interval <- est$ate_interval(X_py, alpha = 1 - confidence_level)
      ci_lower_py <- as.numeric(ate_interval[[1]])
      ci_upper_py <- as.numeric(ate_interval[[2]])
      se_py <- (ci_upper_py - ci_lower_py) / (2 * stats::qnorm(1 - (1 - confidence_level) / 2))

      cate_py <- as.numeric(est$effect(X_py))
      ci_val <- .calc_ci(ate_py, se_py, confidence_level)

      if (verbose) {
        cat("  Estimador: LinearDML (EconML)\n")
        cat("  ATE (EconML DML):", round(ate_py, 4), "\n")
        cat("  SE:", round(se_py, 4), "\n")
        cat("  IC", round(confidence_level * 100), "%: [",
            round(ci_lower_py, 4), ", ", round(ci_upper_py, 4), "]\n", sep = "")
        cat("  CATE SD:", round(stats::sd(cate_py), 4), "\n")
      }

      list(
        model = est,
        cate = cate_py,
        ate = ate_py,
        se = se_py,
        ci_lower = ci_lower_py,
        ci_upper = ci_upper_py,
        p_value = ci_val$p_value
      )
    }, error = function(e) {
      if (verbose) cat("  Error en EconML DML:", e$message, "\n")
      NULL
    })

    result$econml_dml <- dml_py_result
    if (!is.null(dml_py_result)) {
      summary_rows$econml_dml <- .create_summary_row(
        "EconML DML", dml_py_result$ate, dml_py_result$se,
        dml_py_result$ci_lower, dml_py_result$ci_upper, dml_py_result$p_value
      )
    }
  }

  # --- 7.2 EconML Causal Forest ---
  if ("econml_forest" %in% python_methods) {
    if (verbose) {
      .print_subsection(7, 2, "EconML - CausalForestDML")
    }

    cf_py_result <- tryCatch({
      dml_module <- reticulate::import("econml.dml")

      est_cf <- dml_module$CausalForestDML(
        n_estimators = 2000L,
        random_state = if (is.null(seed)) NULL else as.integer(seed)
      )

      est_cf$fit(Y_py, T_py, X = X_py)

      ate_cf_py <- as.numeric(est_cf$ate(X_py))
      ate_interval_cf <- est_cf$ate_interval(X_py, alpha = 1 - confidence_level)
      ci_lower_cf <- as.numeric(ate_interval_cf[[1]])
      ci_upper_cf <- as.numeric(ate_interval_cf[[2]])
      se_cf_py <- (ci_upper_cf - ci_lower_cf) / (2 * stats::qnorm(1 - (1 - confidence_level) / 2))

      cate_cf_py <- as.numeric(est_cf$effect(X_py))
      ci_val <- .calc_ci(ate_cf_py, se_cf_py, confidence_level)

      if (verbose) {
        cat("  Estimador: CausalForestDML (EconML)\n")
        cat("  ATE (EconML CF):", round(ate_cf_py, 4), "\n")
        cat("  SE:", round(se_cf_py, 4), "\n")
        cat("  IC", round(confidence_level * 100), "%: [",
            round(ci_lower_cf, 4), ", ", round(ci_upper_cf, 4), "]\n", sep = "")
        cat("  CATE SD:", round(stats::sd(cate_cf_py), 4), "\n")
      }

      list(
        model = est_cf,
        cate = cate_cf_py,
        ate = ate_cf_py,
        se = se_cf_py,
        ci_lower = ci_lower_cf,
        ci_upper = ci_upper_cf,
        p_value = ci_val$p_value
      )
    }, error = function(e) {
      if (verbose) cat("  Error en EconML Causal Forest:", e$message, "\n")
      NULL
    })

    result$econml_forest <- cf_py_result
    if (!is.null(cf_py_result)) {
      summary_rows$econml_forest <- .create_summary_row(
        "EconML Causal Forest", cf_py_result$ate, cf_py_result$se,
        cf_py_result$ci_lower, cf_py_result$ci_upper, cf_py_result$p_value
      )
    }
  }

  result$summary_rows <- summary_rows
  return(result)
}
