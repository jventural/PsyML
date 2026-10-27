# =============================================================================
# Verificacion de supuestos causales para causal_ml
# =============================================================================

#' @noRd
.causal_assumptions <- function(data, treatment, outcome, covariates,
                                treatment_type, ps_result = NULL,
                                confidence_level = 0.95,
                                verbose = TRUE) {

  if (verbose) {
    .print_section(3, "Verificacion de Supuestos Causales")
  }

  result <- list()

  # --- 3.1 Overlap / Positivity ---
  if (verbose) {
    .print_subsection(3, 1, "Overlap / Positivity")
    .print_causal_reference("overlap")
  }

  if (treatment_type == "binary" && !is.null(ps_result)) {
    ps <- ps_result$ps
    t_vec <- data[[treatment]]

    ps_by_group <- split(ps, t_vec)
    result$ps_range_control <- range(ps_by_group[["0"]])
    result$ps_range_treated <- range(ps_by_group[["1"]])

    # Soporte comun
    common_min <- max(min(ps_by_group[["0"]]), min(ps_by_group[["1"]]))
    common_max <- min(max(ps_by_group[["0"]]), max(ps_by_group[["1"]]))
    result$common_support <- c(common_min, common_max)

    # Proporcion fuera de soporte comun
    outside <- sum(ps < common_min | ps > common_max) / length(ps)
    result$pct_outside_support <- round(outside * 100, 1)

    # Trimming recomendado
    result$trim_recommended <- any(ps < 0.01 | ps > 0.99)

    if (verbose) {
      cat("  Rango PS (control):     [", round(result$ps_range_control[1], 3),
          ", ", round(result$ps_range_control[2], 3), "]\n", sep = "")
      cat("  Rango PS (tratamiento): [", round(result$ps_range_treated[1], 3),
          ", ", round(result$ps_range_treated[2], 3), "]\n", sep = "")
      cat("  Soporte comun:          [", round(common_min, 3),
          ", ", round(common_max, 3), "]\n", sep = "")
      cat("  Fuera de soporte comun: ", result$pct_outside_support, "%\n", sep = "")

      if (result$pct_outside_support > 10) {
        cat("\n  ADVERTENCIA: Mas del 10% fuera de soporte comun.\n")
        cat("  Positivity puede estar violado. Considerar trimming.\n")
      } else if (result$pct_outside_support > 5) {
        cat("\n  Nota: Algunas observaciones fuera de soporte comun.\n")
      } else {
        cat("\n  Overlap adecuado.\n")
      }

      if (result$trim_recommended) {
        cat("  PS extremos detectados (<0.01 o >0.99): trimming recomendado.\n")
      }
    }
  } else {
    if (verbose) cat("  (Requiere tratamiento binario y PS estimado)\n")
  }

  # --- 3.2 Balance post-PS ---
  if (treatment_type == "binary" && !is.null(ps_result)) {
    if (verbose) {
      .print_subsection(3, 2, "Balance Post-Ajuste (PS Ponderado)")
    }

    ps <- ps_result$ps
    t_vec <- as.integer(data[[treatment]])

    # Pesos IPW
    w <- ifelse(t_vec == 1, 1 / ps, 1 / (1 - ps))
    # Truncar pesos extremos
    w <- pmin(w, stats::quantile(w, 0.99, na.rm = TRUE))

    num_covs <- covariates[sapply(data[, covariates, drop = FALSE], is.numeric)]
    if (length(num_covs) > 0) {
      smd_weighted <- sapply(num_covs, function(v) {
        x <- data[[v]]
        m1_w <- stats::weighted.mean(x[t_vec == 1], w[t_vec == 1], na.rm = TRUE)
        m0_w <- stats::weighted.mean(x[t_vec == 0], w[t_vec == 0], na.rm = TRUE)
        s1 <- sqrt(Hmisc_wtd_var(x[t_vec == 1], w[t_vec == 1]))
        s0 <- sqrt(Hmisc_wtd_var(x[t_vec == 0], w[t_vec == 0]))
        pooled <- sqrt((s1^2 + s0^2) / 2)
        if (pooled < 1e-10) return(0)
        abs(m1_w - m0_w) / pooled
      })

      smd_post_df <- data.frame(
        variable = num_covs,
        smd_weighted = round(as.numeric(smd_weighted), 3),
        stringsAsFactors = FALSE
      )
      smd_post_df <- smd_post_df[order(-smd_post_df$smd_weighted), ]
      result$smd_post_ipw <- smd_post_df

      if (verbose) {
        cat("  SMD despues de ponderacion IPW:\n")
        n_show <- min(nrow(smd_post_df), 10)
        for (i in seq_len(n_show)) {
          flag <- if (smd_post_df$smd_weighted[i] > 0.10) " *" else ""
          cat("    ", sprintf("%-25s", smd_post_df$variable[i]),
              sprintf("%.3f", smd_post_df$smd_weighted[i]), flag, "\n", sep = "")
        }
        n_still_imbalanced <- sum(smd_post_df$smd_weighted > 0.10)
        cat("\n  Aun desbalanceadas (|d| > 0.10): ",
            n_still_imbalanced, "/", nrow(smd_post_df), "\n", sep = "")
      }
    }
  }

  # --- 3.3 E-value ---
  if (verbose) {
    .print_subsection(3, 3, "Analisis de Sensibilidad (E-value)")
    .print_causal_reference("evalue")
  }

  result$evalue <- NULL  # Se calculara despues con el ATE estimado

  if (verbose) {
    cat("  El E-value se calculara tras estimar el efecto causal.\n")
    cat("  Indica la fuerza minima que un confusor no medido necesitaria\n")
    cat("  para explicar completamente el efecto observado.\n")
  }

  # --- 3.4 Unconfoundedness (no testeable directamente) ---
  if (verbose) {
    .print_subsection(3, 4, "Supuesto de Ignorabilidad (No Testeable)")
    cat("  El supuesto de ignorabilidad condicional (unconfoundedness)\n")
    cat("  no es testeable directamente con datos observacionales.\n")
    cat("  Estrategias de mitigacion:\n")
    cat("    1. Incluir todas las covariables relevantes (dominio experto)\n")
    cat("    2. Analisis de sensibilidad con E-value\n")
    cat("    3. Variables instrumentales (si disponibles)\n")
    cat("    4. Disenos de regresion discontinua o DiD (si aplicable)\n")
  }

  return(result)
}

# --- Weighted variance sin depender de Hmisc ---
#' @noRd
Hmisc_wtd_var <- function(x, weights) {
  x <- x[!is.na(x)]
  w <- weights[!is.na(x)]
  if (length(x) < 2) return(0)
  mu <- stats::weighted.mean(x, w, na.rm = TRUE)
  sum(w * (x - mu)^2) / (sum(w) - 1)
}
