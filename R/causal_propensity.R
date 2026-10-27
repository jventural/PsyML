# =============================================================================
# Propensity Score Methods para causal_ml
# =============================================================================

#' @noRd
.causal_propensity <- function(data, treatment, outcome, covariates,
                               ps_model = "logistic",
                               ps_methods = c("matching", "ipw", "doubly_robust"),
                               matching_ratio = 1,
                               caliper = 0.2,
                               confidence_level = 0.95,
                               verbose = TRUE) {

  if (verbose) {
    .print_section(4, "Metodos de Propensity Score")
    .print_causal_reference("ps")
  }

  result <- list()
  summary_rows <- list()

  # --- 4.1 Estimar Propensity Score ---
  if (verbose) {
    .print_subsection(4, 1, "Estimacion del Propensity Score")
    cat("  Modelo:", ps_model, "\n")
  }

  ps_result <- .estimate_ps(data, treatment, covariates, method = ps_model)
  ps <- ps_result$ps
  result$ps <- ps
  result$ps_model <- ps_result$model
  result$ps_method <- ps_model

  t_vec <- as.integer(data[[treatment]])
  y_vec <- data[[outcome]]

  if (verbose) {
    cat("  PS estimado - Resumen:\n")
    cat("    Control:     media = ", round(mean(ps[t_vec == 0]), 3),
        ", sd = ", round(stats::sd(ps[t_vec == 0]), 3), "\n", sep = "")
    cat("    Tratamiento: media = ", round(mean(ps[t_vec == 1]), 3),
        ", sd = ", round(stats::sd(ps[t_vec == 1]), 3), "\n", sep = "")
  }

  # --- 4.2 Matching ---
  if ("matching" %in% ps_methods) {
    if (verbose) {
      .print_subsection(4, 2, "Propensity Score Matching")
      .print_causal_reference("matching")
    }

    matching_result <- tryCatch({
      if (.check_package("MatchIt", "Propensity Score Matching")) {
        fml <- stats::as.formula(paste(treatment, "~",
                                        paste(covariates, collapse = " + ")))
        m_out <- MatchIt::matchit(fml, data = data,
                                   method = "nearest",
                                   distance = ps,
                                   ratio = matching_ratio,
                                   caliper = caliper)

        m_data <- MatchIt::match.data(m_out)

        # ATE del matching (diferencia de medias en matched sample)
        ate_match <- mean(m_data[[outcome]][m_data[[treatment]] == 1]) -
          mean(m_data[[outcome]][m_data[[treatment]] == 0])

        # SE por bootstrap
        n_boot <- 500
        boot_ates <- replicate(n_boot, {
          idx <- sample(nrow(m_data), replace = TRUE)
          bd <- m_data[idx, ]
          mean(bd[[outcome]][bd[[treatment]] == 1]) -
            mean(bd[[outcome]][bd[[treatment]] == 0])
        })
        se_match <- stats::sd(boot_ates)
        ci <- .calc_ci(ate_match, se_match, confidence_level)

        if (verbose) {
          cat("  Matched pairs:", sum(m_out$weights > 0) / 2, "\n")
          cat("  Unmatched dropped:", sum(m_out$weights == 0), "\n")
          cat("  Ratio:", matching_ratio, ":1\n")
          cat("  Caliper:", caliper, "SD\n")
          cat("\n  ATE (matching):", round(ate_match, 4), "\n")
          cat("  SE:", round(se_match, 4), "\n")
          cat("  IC", round(confidence_level * 100), "%: [",
              round(ci$ci_lower, 4), ", ", round(ci$ci_upper, 4), "]\n", sep = "")
          cat("  p-valor:", format(ci$p_value, digits = 4), "\n")
        }

        list(
          matchit = m_out,
          matched_data = m_data,
          ate = ate_match,
          se = se_match,
          ci_lower = ci$ci_lower,
          ci_upper = ci$ci_upper,
          p_value = ci$p_value
        )
      } else {
        # Fallback: matching nativo por nearest neighbor
        if (verbose) cat("  Usando matching nativo (sin MatchIt)...\n")

        matched_idx <- .native_matching(ps, t_vec, ratio = matching_ratio,
                                         caliper = caliper)
        m_data <- data[matched_idx, ]
        m_t <- t_vec[matched_idx]
        m_y <- y_vec[matched_idx]

        ate_match <- mean(m_y[m_t == 1]) - mean(m_y[m_t == 0])
        se_match <- sqrt(stats::var(m_y[m_t == 1]) / sum(m_t == 1) +
                           stats::var(m_y[m_t == 0]) / sum(m_t == 0))
        ci <- .calc_ci(ate_match, se_match, confidence_level)

        if (verbose) {
          cat("  ATE (matching nativo):", round(ate_match, 4), "\n")
          cat("  SE:", round(se_match, 4), "\n")
        }

        list(
          matched_data = m_data,
          ate = ate_match,
          se = se_match,
          ci_lower = ci$ci_lower,
          ci_upper = ci$ci_upper,
          p_value = ci$p_value
        )
      }
    }, error = function(e) {
      if (verbose) cat("  Error en matching:", e$message, "\n")
      NULL
    })

    result$matching <- matching_result
    if (!is.null(matching_result)) {
      summary_rows$matching <- .create_summary_row(
        "PS Matching", matching_result$ate, matching_result$se,
        matching_result$ci_lower, matching_result$ci_upper, matching_result$p_value
      )
    }
  }

  # --- 4.3 IPW ---
  if ("ipw" %in% ps_methods) {
    if (verbose) {
      .print_subsection(4, 3, "Inverse Probability Weighting (IPW)")
      .print_causal_reference("ipw")
    }

    ipw_result <- tryCatch({
      # Pesos IPW (ATE)
      w <- ifelse(t_vec == 1, 1 / ps, 1 / (1 - ps))
      # Truncar extremos
      w_trunc <- pmin(w, stats::quantile(w, 0.99, na.rm = TRUE))
      # Normalizar
      w_norm <- w_trunc
      w_norm[t_vec == 1] <- w_norm[t_vec == 1] / sum(w_norm[t_vec == 1])
      w_norm[t_vec == 0] <- w_norm[t_vec == 0] / sum(w_norm[t_vec == 0])

      # ATE ponderado (Horvitz-Thompson)
      ate_ipw <- sum(w_trunc * t_vec * y_vec) / sum(w_trunc * t_vec) -
        sum(w_trunc * (1 - t_vec) * y_vec) / sum(w_trunc * (1 - t_vec))

      # SE sandwich
      n <- length(y_vec)
      psi <- w_trunc * (t_vec - ps) / (ps * (1 - ps)) * y_vec
      se_ipw <- sqrt(stats::var(psi) / n)

      ci <- .calc_ci(ate_ipw, se_ipw, confidence_level)

      if (verbose) {
        cat("  Pesos IPW truncados al percentil 99\n")
        cat("  Rango pesos: [", round(min(w_trunc), 2), ", ",
            round(max(w_trunc), 2), "]\n", sep = "")
        cat("  ESS control:", round(sum(w_trunc[t_vec == 0])^2 /
                                       sum(w_trunc[t_vec == 0]^2)), "\n")
        cat("  ESS tratamiento:", round(sum(w_trunc[t_vec == 1])^2 /
                                           sum(w_trunc[t_vec == 1]^2)), "\n")
        cat("\n  ATE (IPW):", round(ate_ipw, 4), "\n")
        cat("  SE:", round(se_ipw, 4), "\n")
        cat("  IC", round(confidence_level * 100), "%: [",
            round(ci$ci_lower, 4), ", ", round(ci$ci_upper, 4), "]\n", sep = "")
        cat("  p-valor:", format(ci$p_value, digits = 4), "\n")
      }

      list(
        weights = w_trunc,
        ate = ate_ipw,
        se = se_ipw,
        ci_lower = ci$ci_lower,
        ci_upper = ci$ci_upper,
        p_value = ci$p_value
      )
    }, error = function(e) {
      if (verbose) cat("  Error en IPW:", e$message, "\n")
      NULL
    })

    result$ipw <- ipw_result
    if (!is.null(ipw_result)) {
      summary_rows$ipw <- .create_summary_row(
        "IPW", ipw_result$ate, ipw_result$se,
        ipw_result$ci_lower, ipw_result$ci_upper, ipw_result$p_value
      )
    }
  }

  # --- 4.4 Doubly Robust (AIPW) ---
  if ("doubly_robust" %in% ps_methods) {
    if (verbose) {
      .print_subsection(4, 4, "Doubly Robust (AIPW)")
      .print_causal_reference("aipw")
    }

    aipw_result <- tryCatch({
      # Modelos de outcome usando data frames
      df_outcome <- data.frame(Y_out = y_vec, data[, covariates, drop = FALSE])

      # Modelo de outcome para control
      fit_0 <- stats::lm(Y_out ~ ., data = df_outcome[t_vec == 0, , drop = FALSE])
      mu_0 <- stats::predict(fit_0, newdata = df_outcome)

      # Modelo de outcome para tratamiento
      fit_1 <- stats::lm(Y_out ~ ., data = df_outcome[t_vec == 1, , drop = FALSE])
      mu_1 <- stats::predict(fit_1, newdata = df_outcome)

      # AIPW estimator
      n <- length(y_vec)
      aipw_1 <- mean(t_vec * y_vec / ps - (t_vec - ps) / ps * mu_1)
      aipw_0 <- mean((1 - t_vec) * y_vec / (1 - ps) + (t_vec - ps) / (1 - ps) * mu_0)
      ate_aipw <- aipw_1 - aipw_0

      # Influence function
      phi_1 <- t_vec * y_vec / ps - (t_vec - ps) / ps * mu_1 - aipw_1
      phi_0 <- (1 - t_vec) * y_vec / (1 - ps) + (t_vec - ps) / (1 - ps) * mu_0 - aipw_0
      phi <- phi_1 - phi_0
      se_aipw <- sqrt(mean(phi^2) / n)

      ci <- .calc_ci(ate_aipw, se_aipw, confidence_level)

      if (verbose) {
        cat("  Modelo de outcome: regresion lineal\n")
        cat("  Modelo de PS:", result$ps_method, "\n")
        cat("  Ventaja: consistente si CUALQUIERA de los dos modelos\n")
        cat("  esta correctamente especificado.\n")
        cat("\n  ATE (AIPW):", round(ate_aipw, 4), "\n")
        cat("  SE:", round(se_aipw, 4), "\n")
        cat("  IC", round(confidence_level * 100), "%: [",
            round(ci$ci_lower, 4), ", ", round(ci$ci_upper, 4), "]\n", sep = "")
        cat("  p-valor:", format(ci$p_value, digits = 4), "\n")
      }

      list(
        ate = ate_aipw,
        se = se_aipw,
        ci_lower = ci$ci_lower,
        ci_upper = ci$ci_upper,
        p_value = ci$p_value,
        mu_0 = mu_0,
        mu_1 = mu_1
      )
    }, error = function(e) {
      if (verbose) cat("  Error en AIPW:", e$message, "\n")
      NULL
    })

    result$doubly_robust <- aipw_result
    if (!is.null(aipw_result)) {
      summary_rows$doubly_robust <- .create_summary_row(
        "AIPW (Doubly Robust)", aipw_result$ate, aipw_result$se,
        aipw_result$ci_lower, aipw_result$ci_upper, aipw_result$p_value
      )
    }
  }

  result$summary_rows <- summary_rows
  return(result)
}

# --- Matching nativo (fallback sin MatchIt) ---
#' @noRd
.native_matching <- function(ps, treatment, ratio = 1, caliper = 0.2) {
  treated_idx <- which(treatment == 1)
  control_idx <- which(treatment == 0)

  ps_sd <- stats::sd(ps)
  caliper_abs <- caliper * ps_sd

  matched <- c()
  for (ti in treated_idx) {
    dists <- abs(ps[control_idx] - ps[ti])
    within_caliper <- which(dists <= caliper_abs)
    if (length(within_caliper) > 0) {
      n_match <- min(ratio, length(within_caliper))
      best <- within_caliper[order(dists[within_caliper])][seq_len(n_match)]
      matched <- c(matched, ti, control_idx[best])
    }
  }

  unique(matched)
}
