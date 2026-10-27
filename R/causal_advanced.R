# =============================================================================
# Metodos Avanzados para causal_ml: DML, Causal Forest, TMLE
# =============================================================================

#' @noRd
.causal_advanced <- function(data, treatment, outcome, covariates,
                             methods = c("dml", "causal_forest"),
                             dml_model = "rf",
                             dml_n_folds = 5,
                             cf_num_trees = 2000,
                             cf_honesty = TRUE,
                             run_tmle = FALSE,
                             confidence_level = 0.95,
                             seed = NULL,
                             verbose = TRUE) {

  if (verbose) {
    .print_section(5, "Metodos Avanzados de Inferencia Causal")
  }

  if (!is.null(seed)) set.seed(seed)
  result <- list()
  summary_rows <- list()

  t_vec <- as.integer(data[[treatment]])
  y_vec <- data[[outcome]]
  X <- data[, covariates, drop = FALSE]

  # --- 5.1 Double Machine Learning ---
  if ("dml" %in% methods) {
    if (verbose) {
      .print_subsection(5, 1, "Double Machine Learning (DML)")
      .print_causal_reference("dml")
    }

    dml_result <- tryCatch({
      if (.check_package("DoubleML", "Double Machine Learning") &&
          .check_package("mlr3", "Double Machine Learning") &&
          .check_package("mlr3learners", "Double Machine Learning")) {

        # Preparar datos para DoubleML
        X_mat <- .prepare_covariate_matrix(data, covariates)
        dml_data <- DoubleML::DoubleMLData$new(
          data = data.frame(Y = y_vec, D = t_vec, X_mat),
          y_col = "Y",
          d_cols = "D"
        )

        # Elegir learner
        ml_learner <- switch(dml_model,
          "rf" = mlr3::lrn("regr.ranger", num.trees = 500),
          "xgboost" = mlr3::lrn("regr.xgboost", nrounds = 100, verbose = 0),
          "lasso" = mlr3::lrn("regr.glmnet", alpha = 1),
          mlr3::lrn("regr.ranger", num.trees = 500)
        )

        ml_g <- ml_learner$clone()
        ml_m <- ml_learner$clone()

        # Partially Linear Model
        dml_plr <- DoubleML::DoubleMLPLR$new(
          dml_data, ml_g, ml_m,
          n_folds = dml_n_folds
        )
        dml_plr$fit()

        ate_dml <- dml_plr$coef
        se_dml <- dml_plr$se
        ci <- .calc_ci(ate_dml, se_dml, confidence_level)

        if (verbose) {
          cat("  Modelo base:", dml_model, "\n")
          cat("  Folds:", dml_n_folds, "\n")
          cat("  Estimador: Partially Linear Regression (PLR)\n")
          cat("\n  ATE (DML):", round(ate_dml, 4), "\n")
          cat("  SE:", round(se_dml, 4), "\n")
          cat("  IC", round(confidence_level * 100), "%: [",
              round(ci$ci_lower, 4), ", ", round(ci$ci_upper, 4), "]\n", sep = "")
          cat("  p-valor:", format(ci$p_value, digits = 4), "\n")
        }

        list(
          model = dml_plr,
          ate = as.numeric(ate_dml),
          se = as.numeric(se_dml),
          ci_lower = ci$ci_lower,
          ci_upper = ci$ci_upper,
          p_value = ci$p_value
        )
      } else {
        # Fallback: DML nativo con cross-fitting
        if (verbose) cat("  Usando DML nativo (sin DoubleML)...\n")
        .dml_native(data, treatment, outcome, covariates,
                    n_folds = dml_n_folds, confidence_level = confidence_level,
                    verbose = verbose)
      }
    }, error = function(e) {
      if (verbose) {
        cat("  Error en DoubleML:", e$message, "\n")
        cat("  Intentando implementacion nativa...\n")
      }
      tryCatch(
        .dml_native(data, treatment, outcome, covariates,
                    n_folds = dml_n_folds, confidence_level = confidence_level,
                    verbose = verbose),
        error = function(e2) {
          if (verbose) cat("  Error en DML nativo:", e2$message, "\n")
          NULL
        }
      )
    })

    result$dml <- dml_result
    if (!is.null(dml_result)) {
      summary_rows$dml <- .create_summary_row(
        "Double ML", dml_result$ate, dml_result$se,
        dml_result$ci_lower, dml_result$ci_upper, dml_result$p_value
      )
    }
  }

  # --- 5.2 Causal Forest (grf) ---
  if ("causal_forest" %in% methods) {
    if (verbose) {
      .print_subsection(5, 2, "Causal Forest (GRF)")
      .print_causal_reference("cf")
    }

    cf_result <- tryCatch({
      if (!.check_package("grf", "Causal Forest")) {
        stop("Package 'grf' is required")
      }

      X_mat <- .prepare_covariate_matrix(data, covariates)

      cf <- grf::causal_forest(
        X = X_mat,
        Y = y_vec,
        W = t_vec,
        num.trees = cf_num_trees,
        honesty = cf_honesty,
        seed = if (is.null(seed)) sample.int(.Machine$integer.max, 1) else seed
      )

      # ATE
      ate_cf_obj <- grf::average_treatment_effect(cf, target.sample = "all")
      ate_cf <- ate_cf_obj["estimate"]
      se_cf <- ate_cf_obj["std.err"]
      ci <- .calc_ci(ate_cf, se_cf, confidence_level)

      # CATE individuales
      cate_cf <- stats::predict(cf)$predictions

      # Variable importance
      vimp <- grf::variable_importance(cf)
      vimp_df <- data.frame(
        variable = colnames(X_mat),
        importance = as.numeric(vimp),
        stringsAsFactors = FALSE
      )
      vimp_df <- vimp_df[order(-vimp_df$importance), ]

      if (verbose) {
        cat("  Arboles:", cf_num_trees, "\n")
        cat("  Honesty:", cf_honesty, "\n")
        cat("\n  ATE (Causal Forest):", round(ate_cf, 4), "\n")
        cat("  SE:", round(se_cf, 4), "\n")
        cat("  IC", round(confidence_level * 100), "%: [",
            round(ci$ci_lower, 4), ", ", round(ci$ci_upper, 4), "]\n", sep = "")
        cat("  p-valor:", format(ci$p_value, digits = 4), "\n")
        cat("\n  CATE Distribution:\n")
        cat("    Media:", round(mean(cate_cf), 4), "\n")
        cat("    SD:", round(stats::sd(cate_cf), 4), "\n")
        cat("    Rango: [", round(min(cate_cf), 4), ", ",
            round(max(cate_cf), 4), "]\n", sep = "")

        cat("\n  Top 5 variables importantes:\n")
        n_show <- min(5, nrow(vimp_df))
        for (i in seq_len(n_show)) {
          cat("    ", sprintf("%-25s", vimp_df$variable[i]),
              sprintf("%.4f", vimp_df$importance[i]), "\n", sep = "")
        }
      }

      list(
        model = cf,
        cate = cate_cf,
        ate = as.numeric(ate_cf),
        se = as.numeric(se_cf),
        ci_lower = ci$ci_lower,
        ci_upper = ci$ci_upper,
        p_value = ci$p_value,
        variable_importance = vimp_df
      )
    }, error = function(e) {
      if (verbose) cat("  Error en Causal Forest:", e$message, "\n")
      NULL
    })

    result$causal_forest <- cf_result
    if (!is.null(cf_result)) {
      summary_rows$causal_forest <- .create_summary_row(
        "Causal Forest", cf_result$ate, cf_result$se,
        cf_result$ci_lower, cf_result$ci_upper, cf_result$p_value
      )
    }
  }

  # --- 5.3 TMLE ---
  if (run_tmle) {
    if (verbose) {
      .print_subsection(5, 3, "Targeted Maximum Likelihood Estimation (TMLE)")
      .print_causal_reference("tmle")
    }

    tmle_result <- tryCatch({
      if (!.check_package("tmle", "TMLE")) stop("Package 'tmle' is required")

      X_mat <- .prepare_covariate_matrix(data, covariates)

      tmle_fit <- tmle::tmle(
        Y = y_vec,
        A = t_vec,
        W = as.data.frame(X_mat),
        family = "gaussian"
      )

      ate_tmle <- tmle_fit$estimates$ATE$psi
      se_tmle <- sqrt(tmle_fit$estimates$ATE$var.psi)
      ci_tmle <- tmle_fit$estimates$ATE$CI
      p_tmle <- tmle_fit$estimates$ATE$pvalue

      if (verbose) {
        cat("  ATE (TMLE):", round(ate_tmle, 4), "\n")
        cat("  SE:", round(se_tmle, 4), "\n")
        cat("  IC", round(confidence_level * 100), "%: [",
            round(ci_tmle[1], 4), ", ", round(ci_tmle[2], 4), "]\n", sep = "")
        cat("  p-valor:", format(p_tmle, digits = 4), "\n")
      }

      list(
        model = tmle_fit,
        ate = ate_tmle,
        se = se_tmle,
        ci_lower = ci_tmle[1],
        ci_upper = ci_tmle[2],
        p_value = p_tmle
      )
    }, error = function(e) {
      if (verbose) cat("  Error en TMLE:", e$message, "\n")
      NULL
    })

    result$tmle <- tmle_result
    if (!is.null(tmle_result)) {
      summary_rows$tmle <- .create_summary_row(
        "TMLE", tmle_result$ate, tmle_result$se,
        tmle_result$ci_lower, tmle_result$ci_upper, tmle_result$p_value
      )
    }
  }

  result$summary_rows <- summary_rows
  return(result)
}

# --- DML nativo (fallback) ---
#' @noRd
.dml_native <- function(data, treatment, outcome, covariates,
                        n_folds = 5, confidence_level = 0.95,
                        verbose = TRUE) {

  t_vec <- as.integer(data[[treatment]])
  y_vec <- data[[outcome]]
  X <- data[, covariates, drop = FALSE]
  n <- nrow(data)

  # Cross-fitting
  folds <- sample(rep(seq_len(n_folds), length.out = n))

  theta_hat <- numeric(n)

  for (k in seq_len(n_folds)) {
    train_idx <- folds != k
    test_idx <- folds == k

    X_train <- X[train_idx, , drop = FALSE]
    X_test <- X[test_idx, , drop = FALSE]
    y_train <- y_vec[train_idx]
    t_train <- t_vec[train_idx]
    y_test <- y_vec[test_idx]
    t_test <- t_vec[test_idx]

    # Modelo para Y ~ X (nuisance)
    df_y <- data.frame(Y = y_train, X_train)
    fit_y <- ranger::ranger(Y ~ ., data = df_y, num.trees = 300)
    y_hat <- stats::predict(fit_y, data = X_test)$predictions

    # Modelo para T ~ X (nuisance)
    df_t <- data.frame(D = as.factor(t_train), X_train)
    fit_t <- ranger::ranger(D ~ ., data = df_t, probability = TRUE, num.trees = 300)
    t_hat <- stats::predict(fit_t, data = X_test)$predictions[, "1"]

    # Residuales
    y_resid <- y_test - y_hat
    t_resid <- t_test - t_hat

    theta_hat[test_idx] <- y_resid / (t_resid + 1e-10)
  }

  # Estimar ATE: regresion de Y_resid ~ T_resid
  all_y_hat <- numeric(n)
  all_t_hat <- numeric(n)

  for (k in seq_len(n_folds)) {
    train_idx <- folds != k
    test_idx <- folds == k

    df_y <- data.frame(Y = y_vec[train_idx], X[train_idx, , drop = FALSE])
    fit_y <- ranger::ranger(Y ~ ., data = df_y, num.trees = 300)
    all_y_hat[test_idx] <- stats::predict(fit_y, data = X[test_idx, , drop = FALSE])$predictions

    df_t <- data.frame(D = as.factor(t_vec[train_idx]), X[train_idx, , drop = FALSE])
    fit_t <- ranger::ranger(D ~ ., data = df_t, probability = TRUE, num.trees = 300)
    all_t_hat[test_idx] <- stats::predict(fit_t, data = X[test_idx, , drop = FALSE])$predictions[, "1"]
  }

  y_resid <- y_vec - all_y_hat
  t_resid <- t_vec - all_t_hat

  ate_dml <- sum(t_resid * y_resid) / sum(t_resid^2)
  psi <- t_resid * (y_resid - ate_dml * t_resid) / mean(t_resid^2)
  se_dml <- sqrt(mean(psi^2) / n)
  ci <- .calc_ci(ate_dml, se_dml, confidence_level)

  if (verbose) {
    cat("  Implementacion nativa con cross-fitting\n")
    cat("  Folds:", n_folds, "\n")
    cat("  Modelo base: ranger (Random Forest)\n")
    cat("\n  ATE (DML nativo):", round(ate_dml, 4), "\n")
    cat("  SE:", round(se_dml, 4), "\n")
    cat("  IC", round(confidence_level * 100), "%: [",
        round(ci$ci_lower, 4), ", ", round(ci$ci_upper, 4), "]\n", sep = "")
    cat("  p-valor:", format(ci$p_value, digits = 4), "\n")
  }

  list(
    ate = ate_dml,
    se = se_dml,
    ci_lower = ci$ci_lower,
    ci_upper = ci$ci_upper,
    p_value = ci$p_value
  )
}
