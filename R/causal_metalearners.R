# =============================================================================
# Meta-Learners para causal_ml (S-learner, T-learner, X-learner)
# Implementacion nativa con ranger (sin dependencias adicionales)
# =============================================================================

#' @noRd
.causal_metalearners <- function(data, treatment, outcome, covariates,
                                 meta_learners = c("S", "T", "X"),
                                 meta_base_model = "rf",
                                 confidence_level = 0.95,
                                 seed = NULL,
                                 verbose = TRUE) {

  if (verbose) {
    .print_section(6, "Meta-Learners")
    .print_causal_reference("meta")
  }

  if (!is.null(seed)) set.seed(seed)
  result <- list()
  summary_rows <- list()

  t_vec <- as.integer(data[[treatment]])
  y_vec <- data[[outcome]]
  X <- data[, covariates, drop = FALSE]
  n <- nrow(data)

  # Funcion para entrenar modelo base
  .fit_base <- function(X_train, y_train, model_type = meta_base_model) {
    df_train <- data.frame(Y = y_train, X_train)
    if (model_type == "rf") {
      ranger::ranger(Y ~ ., data = df_train, num.trees = 500)
    } else if (model_type == "xgboost") {
      if (!requireNamespace("xgboost", quietly = TRUE)) {
        stop("Package 'xgboost' is required for meta_base_model = 'xgboost'")
      }
      X_mat <- stats::model.matrix(~ . - 1, data = X_train)
      dtrain <- xgboost::xgb.DMatrix(data = X_mat, label = y_train)
      xgboost::xgb.train(
        params = list(objective = "reg:squarederror", max_depth = 4, eta = 0.1),
        data = dtrain, nrounds = 100, verbose = 0
      )
    }
  }

  .predict_base <- function(model, X_new, model_type = meta_base_model) {
    if (model_type == "rf") {
      stats::predict(model, data = X_new)$predictions
    } else if (model_type == "xgboost") {
      X_mat <- stats::model.matrix(~ . - 1, data = X_new)
      stats::predict(model, X_mat)
    }
  }

  # --- 6.1 S-Learner ---
  if ("S" %in% meta_learners) {
    if (verbose) {
      .print_subsection(6, 1, "S-Learner (Single Model)")
    }

    s_result <- tryCatch({
      X_with_t <- X
      X_with_t[[treatment]] <- t_vec

      # Entrenar un solo modelo
      s_model <- .fit_base(X_with_t, y_vec)

      # Predecir con T=1 y T=0
      X_t1 <- X_with_t; X_t1[[treatment]] <- 1
      X_t0 <- X_with_t; X_t0[[treatment]] <- 0

      mu_1 <- .predict_base(s_model, X_t1)
      mu_0 <- .predict_base(s_model, X_t0)
      cate_s <- mu_1 - mu_0

      ate_s <- mean(cate_s)
      se_s <- stats::sd(cate_s) / sqrt(n)
      ci <- .calc_ci(ate_s, se_s, confidence_level)

      if (verbose) {
        cat("  Un solo modelo con tratamiento como covariable\n")
        cat("  Base model:", meta_base_model, "\n")
        cat("\n  ATE (S-learner):", round(ate_s, 4), "\n")
        cat("  SE:", round(se_s, 4), "\n")
        cat("  IC", round(confidence_level * 100), "%: [",
            round(ci$ci_lower, 4), ", ", round(ci$ci_upper, 4), "]\n", sep = "")
        cat("  CATE - Rango: [", round(min(cate_s), 4), ", ",
            round(max(cate_s), 4), "]\n", sep = "")
        cat("  CATE - SD:", round(stats::sd(cate_s), 4), "\n")
      }

      list(
        cate = cate_s,
        ate = ate_s,
        se = se_s,
        ci_lower = ci$ci_lower,
        ci_upper = ci$ci_upper,
        p_value = ci$p_value,
        model = s_model
      )
    }, error = function(e) {
      if (verbose) cat("  Error en S-learner:", e$message, "\n")
      NULL
    })

    result$S <- s_result
    if (!is.null(s_result)) {
      summary_rows$S <- .create_summary_row(
        "S-Learner", s_result$ate, s_result$se,
        s_result$ci_lower, s_result$ci_upper, s_result$p_value
      )
    }
  }

  # --- 6.2 T-Learner ---
  if ("T" %in% meta_learners) {
    if (verbose) {
      .print_subsection(6, 2, "T-Learner (Two Models)")
    }

    t_result <- tryCatch({
      # Modelo separado por grupo
      model_1 <- .fit_base(X[t_vec == 1, , drop = FALSE], y_vec[t_vec == 1])
      model_0 <- .fit_base(X[t_vec == 0, , drop = FALSE], y_vec[t_vec == 0])

      mu_1 <- .predict_base(model_1, X)
      mu_0 <- .predict_base(model_0, X)
      cate_t <- mu_1 - mu_0

      ate_t <- mean(cate_t)
      se_t <- stats::sd(cate_t) / sqrt(n)
      ci <- .calc_ci(ate_t, se_t, confidence_level)

      if (verbose) {
        cat("  Dos modelos separados: mu(1|X) y mu(0|X)\n")
        cat("  Base model:", meta_base_model, "\n")
        cat("  n tratamiento:", sum(t_vec == 1), "\n")
        cat("  n control:", sum(t_vec == 0), "\n")
        cat("\n  ATE (T-learner):", round(ate_t, 4), "\n")
        cat("  SE:", round(se_t, 4), "\n")
        cat("  IC", round(confidence_level * 100), "%: [",
            round(ci$ci_lower, 4), ", ", round(ci$ci_upper, 4), "]\n", sep = "")
        cat("  CATE - Rango: [", round(min(cate_t), 4), ", ",
            round(max(cate_t), 4), "]\n", sep = "")
      }

      list(
        cate = cate_t,
        ate = ate_t,
        se = se_t,
        ci_lower = ci$ci_lower,
        ci_upper = ci$ci_upper,
        p_value = ci$p_value,
        model_1 = model_1,
        model_0 = model_0
      )
    }, error = function(e) {
      if (verbose) cat("  Error en T-learner:", e$message, "\n")
      NULL
    })

    result$T <- t_result
    if (!is.null(t_result)) {
      summary_rows$T <- .create_summary_row(
        "T-Learner", t_result$ate, t_result$se,
        t_result$ci_lower, t_result$ci_upper, t_result$p_value
      )
    }
  }

  # --- 6.3 X-Learner ---
  if ("X" %in% meta_learners) {
    if (verbose) {
      .print_subsection(6, 3, "X-Learner (Cross-fitting)")
    }

    x_result <- tryCatch({
      # Paso 1: T-learner
      model_1 <- .fit_base(X[t_vec == 1, , drop = FALSE], y_vec[t_vec == 1])
      model_0 <- .fit_base(X[t_vec == 0, , drop = FALSE], y_vec[t_vec == 0])

      # Paso 2: Imputed treatment effects
      mu_0_for_treated <- .predict_base(model_0, X[t_vec == 1, , drop = FALSE])
      D_1 <- y_vec[t_vec == 1] - mu_0_for_treated  # ITE para tratados

      mu_1_for_control <- .predict_base(model_1, X[t_vec == 0, , drop = FALSE])
      D_0 <- mu_1_for_control - y_vec[t_vec == 0]  # ITE para controles

      # Paso 3: Modelos de CATE
      cate_model_1 <- .fit_base(X[t_vec == 1, , drop = FALSE], D_1)
      cate_model_0 <- .fit_base(X[t_vec == 0, , drop = FALSE], D_0)

      # Paso 4: Combinar con propensity score
      tau_1 <- .predict_base(cate_model_1, X)
      tau_0 <- .predict_base(cate_model_0, X)

      # Propensity como peso
      ps_for_weight <- tryCatch({
        ps_res <- .estimate_ps(data, treatment, covariates, method = "logistic")
        ps_res$ps
      }, error = function(e) rep(mean(t_vec), n))

      cate_x <- ps_for_weight * tau_0 + (1 - ps_for_weight) * tau_1

      ate_x <- mean(cate_x)
      se_x <- stats::sd(cate_x) / sqrt(n)
      ci <- .calc_ci(ate_x, se_x, confidence_level)

      if (verbose) {
        cat("  Cross-fitting: imputa ITE para ambos grupos\n")
        cat("  Combina con pesos de propensity score\n")
        cat("  Base model:", meta_base_model, "\n")
        cat("\n  ATE (X-learner):", round(ate_x, 4), "\n")
        cat("  SE:", round(se_x, 4), "\n")
        cat("  IC", round(confidence_level * 100), "%: [",
            round(ci$ci_lower, 4), ", ", round(ci$ci_upper, 4), "]\n", sep = "")
        cat("  CATE - Rango: [", round(min(cate_x), 4), ", ",
            round(max(cate_x), 4), "]\n", sep = "")
      }

      list(
        cate = cate_x,
        ate = ate_x,
        se = se_x,
        ci_lower = ci$ci_lower,
        ci_upper = ci$ci_upper,
        p_value = ci$p_value,
        cate_model_1 = cate_model_1,
        cate_model_0 = cate_model_0
      )
    }, error = function(e) {
      if (verbose) cat("  Error en X-learner:", e$message, "\n")
      NULL
    })

    result$X <- x_result
    if (!is.null(x_result)) {
      summary_rows$X <- .create_summary_row(
        "X-Learner", x_result$ate, x_result$se,
        x_result$ci_lower, x_result$ci_upper, x_result$p_value
      )
    }
  }

  result$summary_rows <- summary_rows
  return(result)
}
