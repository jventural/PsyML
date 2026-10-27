# =============================================================================
# Figuras para causal_ml (12 tipos)
# =============================================================================

#' @noRd
.causal_plots <- function(resultado, verbose = TRUE) {

  if (verbose) {
    .print_section(8, "Generacion de Figuras Causales")
  }

  plots <- list()
  figures_catalog <- list()
  fig_num <- 0

  data <- resultado$preprocessed$data
  treatment <- resultado$params$treatment
  outcome <- resultado$params$outcome
  t_vec <- as.integer(data[[treatment]])
  y_vec <- data[[outcome]]

  theme_causal <- ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 12, face = "bold"),
      plot.subtitle = ggplot2::element_text(size = 10, color = "gray40"),
      axis.title = ggplot2::element_text(size = 10),
      legend.position = "bottom"
    )

  # --- Figura 1: Treatment Distribution ---
  fig_num <- fig_num + 1
  tryCatch({
    if (resultado$params$treatment_type == "binary") {
      df_t <- data.frame(Treatment = factor(t_vec, labels = c("Control", "Treated")))
      plots$treatment_distribution <- ggplot2::ggplot(df_t, ggplot2::aes(x = Treatment, fill = Treatment)) +
        ggplot2::geom_bar(alpha = 0.8, width = 0.6) +
        ggplot2::scale_fill_manual(values = c("#2196F3", "#FF5722")) +
        ggplot2::labs(title = "Treatment Distribution",
                      x = "Group", y = "Count") +
        theme_causal + ggplot2::theme(legend.position = "none")
    } else {
      df_t <- data.frame(Treatment = data[[treatment]])
      plots$treatment_distribution <- ggplot2::ggplot(df_t, ggplot2::aes(x = Treatment)) +
        ggplot2::geom_histogram(fill = "#2196F3", alpha = 0.7, bins = 30) +
        ggplot2::labs(title = "Treatment Distribution (Continuous)",
                      x = treatment, y = "Count") +
        theme_causal
    }
    figures_catalog$treatment_distribution <- list(
      number = fig_num, type = "figura", id = paste0("Figura_", fig_num),
      title = "Treatment Distribution",
      description = "Distribution of treatment variable across groups",
      filename = paste0("Figura_", fig_num, "_Treatment_Distribution.png")
    )
    if (verbose) cat("  Figura", fig_num, ": Treatment Distribution\n")
  }, error = function(e) if (verbose) cat("  Error Figura", fig_num, ":", e$message, "\n"))

  # --- Figura 2: Outcome by Treatment ---
  fig_num <- fig_num + 1
  tryCatch({
    df_ot <- data.frame(
      Outcome = y_vec,
      Treatment = factor(t_vec, labels = c("Control", "Treated"))
    )
    plots$outcome_by_treatment <- ggplot2::ggplot(df_ot, ggplot2::aes(x = Treatment, y = Outcome, fill = Treatment)) +
      ggplot2::geom_boxplot(alpha = 0.7, outlier.alpha = 0.3) +
      ggplot2::scale_fill_manual(values = c("#2196F3", "#FF5722")) +
      ggplot2::labs(title = "Outcome by Treatment Group",
                    x = "Group", y = outcome) +
      theme_causal + ggplot2::theme(legend.position = "none")
    figures_catalog$outcome_by_treatment <- list(
      number = fig_num, type = "figura", id = paste0("Figura_", fig_num),
      title = "Outcome by Treatment",
      description = "Boxplot of outcome variable by treatment group",
      filename = paste0("Figura_", fig_num, "_Outcome_by_Treatment.png")
    )
    if (verbose) cat("  Figura", fig_num, ": Outcome by Treatment\n")
  }, error = function(e) if (verbose) cat("  Error Figura", fig_num, ":", e$message, "\n"))

  # --- Figura 3: PS Distribution ---
  if (!is.null(resultado$propensity$ps)) {
    fig_num <- fig_num + 1
    tryCatch({
      ps <- resultado$propensity$ps
      df_ps <- data.frame(
        PS = ps,
        Group = factor(t_vec, labels = c("Control", "Treated"))
      )
      plots$ps_distribution <- ggplot2::ggplot(df_ps, ggplot2::aes(x = PS, fill = Group)) +
        ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density)),
                                alpha = 0.6, bins = 40, position = "identity") +
        ggplot2::scale_fill_manual(values = c("#2196F3", "#FF5722")) +
        ggplot2::labs(title = "Propensity Score Distribution",
                      x = "Propensity Score", y = "Density") +
        theme_causal
      figures_catalog$ps_distribution <- list(
        number = fig_num, type = "figura", id = paste0("Figura_", fig_num),
        title = "Propensity Score Distribution",
        description = "Distribution of estimated propensity scores by treatment group",
        filename = paste0("Figura_", fig_num, "_PS_Distribution.png")
      )
      if (verbose) cat("  Figura", fig_num, ": PS Distribution\n")
    }, error = function(e) if (verbose) cat("  Error Figura", fig_num, ":", e$message, "\n"))

    # --- Figura 4: PS Overlap ---
    fig_num <- fig_num + 1
    tryCatch({
      plots$ps_overlap <- ggplot2::ggplot(df_ps, ggplot2::aes(x = PS, color = Group, fill = Group)) +
        ggplot2::geom_density(alpha = 0.3, linewidth = 1) +
        ggplot2::scale_fill_manual(values = c("#2196F3", "#FF5722")) +
        ggplot2::scale_color_manual(values = c("#1565C0", "#D84315")) +
        ggplot2::labs(title = "Propensity Score Overlap",
                      subtitle = "Common support region",
                      x = "Propensity Score", y = "Density") +
        theme_causal
      figures_catalog$ps_overlap <- list(
        number = fig_num, type = "figura", id = paste0("Figura_", fig_num),
        title = "Propensity Score Overlap",
        description = "Density plot showing overlap/common support of propensity scores",
        filename = paste0("Figura_", fig_num, "_PS_Overlap.png")
      )
      if (verbose) cat("  Figura", fig_num, ": PS Overlap\n")
    }, error = function(e) if (verbose) cat("  Error Figura", fig_num, ":", e$message, "\n"))
  }

  # --- Figura 5: Balance Love Plot ---
  if (!is.null(resultado$eda$smd_pre)) {
    fig_num <- fig_num + 1
    tryCatch({
      smd_pre <- resultado$eda$smd_pre
      # Si hay SMD post
      smd_post <- resultado$assumptions$smd_post_ipw

      if (!is.null(smd_post)) {
        smd_merged <- merge(smd_pre, smd_post, by = "variable", suffixes = c("_pre", "_post"))
        df_love <- data.frame(
          variable = rep(smd_merged$variable, 2),
          smd = c(smd_merged$smd, smd_merged$smd_weighted),
          stage = rep(c("Unadjusted", "IPW Adjusted"), each = nrow(smd_merged))
        )
      } else {
        df_love <- data.frame(
          variable = smd_pre$variable,
          smd = smd_pre$smd,
          stage = "Unadjusted"
        )
      }

      df_love$variable <- stats::reorder(df_love$variable, df_love$smd)

      plots$balance_love_plot <- ggplot2::ggplot(df_love, ggplot2::aes(x = smd, y = variable, color = stage, shape = stage)) +
        ggplot2::geom_point(size = 3, alpha = 0.8) +
        ggplot2::geom_vline(xintercept = 0.10, linetype = "dashed", color = "red", alpha = 0.5) +
        ggplot2::geom_vline(xintercept = 0, color = "gray50") +
        ggplot2::scale_color_manual(values = c("Unadjusted" = "#FF5722", "IPW Adjusted" = "#4CAF50")) +
        ggplot2::labs(title = "Love Plot: Covariate Balance",
                      x = "|Standardized Mean Difference|", y = "",
                      color = "Stage", shape = "Stage") +
        theme_causal
      figures_catalog$balance_love_plot <- list(
        number = fig_num, type = "figura", id = paste0("Figura_", fig_num),
        title = "Balance Love Plot",
        description = "Love plot showing SMD before and after adjustment",
        filename = paste0("Figura_", fig_num, "_Balance_Love_Plot.png")
      )
      if (verbose) cat("  Figura", fig_num, ": Balance Love Plot\n")
    }, error = function(e) if (verbose) cat("  Error Figura", fig_num, ":", e$message, "\n"))
  }

  # --- Figura 6: CATE Distribution ---
  cate_data <- .get_best_cate(resultado)
  if (!is.null(cate_data)) {
    fig_num <- fig_num + 1
    tryCatch({
      df_cate <- data.frame(CATE = cate_data)
      plots$cate_distribution <- ggplot2::ggplot(df_cate, ggplot2::aes(x = CATE)) +
        ggplot2::geom_histogram(fill = "#9C27B0", alpha = 0.7, bins = 40) +
        ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
        ggplot2::geom_vline(xintercept = mean(cate_data), color = "#E91E63", linewidth = 1) +
        ggplot2::labs(title = "CATE Distribution",
                      subtitle = paste("Mean CATE =", round(mean(cate_data), 4)),
                      x = "Conditional Average Treatment Effect", y = "Count") +
        theme_causal
      figures_catalog$cate_distribution <- list(
        number = fig_num, type = "figura", id = paste0("Figura_", fig_num),
        title = "CATE Distribution",
        description = "Distribution of individual-level conditional treatment effects",
        filename = paste0("Figura_", fig_num, "_CATE_Distribution.png")
      )
      if (verbose) cat("  Figura", fig_num, ": CATE Distribution\n")
    }, error = function(e) if (verbose) cat("  Error Figura", fig_num, ":", e$message, "\n"))

    # --- Figura 7: CATE Heterogeneity ---
    fig_num <- fig_num + 1
    tryCatch({
      covariates <- resultado$preprocessed$covariates
      num_covs <- covariates[sapply(data[, covariates, drop = FALSE], is.numeric)]
      if (length(num_covs) >= 1) {
        # Top 4 covariables por correlacion con CATE
        cor_cate <- sapply(num_covs, function(v) {
          abs(stats::cor(data[[v]], cate_data, use = "complete.obs"))
        })
        top_covs <- names(sort(cor_cate, decreasing = TRUE))[seq_len(min(4, length(cor_cate)))]

        # Plot del top 1
        df_het <- data.frame(X = data[[top_covs[1]]], CATE = cate_data)
        plots$cate_heterogeneity <- ggplot2::ggplot(df_het, ggplot2::aes(x = X, y = CATE)) +
          ggplot2::geom_point(alpha = 0.3, color = "#673AB7") +
          ggplot2::geom_smooth(method = "loess", color = "#E91E63", se = TRUE) +
          ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
          ggplot2::labs(title = "Treatment Effect Heterogeneity",
                        subtitle = paste("CATE vs", top_covs[1]),
                        x = top_covs[1], y = "CATE") +
          theme_causal
        figures_catalog$cate_heterogeneity <- list(
          number = fig_num, type = "figura", id = paste0("Figura_", fig_num),
          title = "CATE Heterogeneity",
          description = paste("CATE vs top covariate:", top_covs[1]),
          filename = paste0("Figura_", fig_num, "_CATE_Heterogeneity.png")
        )
        if (verbose) cat("  Figura", fig_num, ": CATE Heterogeneity\n")
      }
    }, error = function(e) if (verbose) cat("  Error Figura", fig_num, ":", e$message, "\n"))
  }

  # --- Figura 8: Variable Importance (Causal Forest) ---
  if (!is.null(resultado$advanced$causal_forest$variable_importance)) {
    fig_num <- fig_num + 1
    tryCatch({
      vimp <- resultado$advanced$causal_forest$variable_importance
      vimp_top <- utils::head(vimp, 15)
      vimp_top$variable <- factor(vimp_top$variable, levels = rev(vimp_top$variable))

      plots$variable_importance_cf <- ggplot2::ggplot(vimp_top, ggplot2::aes(x = importance, y = variable)) +
        ggplot2::geom_col(fill = "#00897B", alpha = 0.8) +
        ggplot2::labs(title = "Variable Importance (Causal Forest)",
                      x = "Importance", y = "") +
        theme_causal
      figures_catalog$variable_importance_cf <- list(
        number = fig_num, type = "figura", id = paste0("Figura_", fig_num),
        title = "Variable Importance (Causal Forest)",
        description = "Variable importance from causal forest model",
        filename = paste0("Figura_", fig_num, "_Variable_Importance_CF.png")
      )
      if (verbose) cat("  Figura", fig_num, ": Variable Importance (CF)\n")
    }, error = function(e) if (verbose) cat("  Error Figura", fig_num, ":", e$message, "\n"))
  }

  # --- Figura 9: ATE Forest Plot ---
  if (!is.null(resultado$summary_table) && nrow(resultado$summary_table) > 0) {
    fig_num <- fig_num + 1
    tryCatch({
      st <- resultado$summary_table
      st$method <- factor(st$method, levels = rev(st$method))

      plots$ate_forest_plot <- ggplot2::ggplot(st, ggplot2::aes(x = ate, y = method)) +
        ggplot2::geom_point(size = 3, color = "#1565C0") +
        ggplot2::geom_errorbarh(ggplot2::aes(xmin = ci_lower, xmax = ci_upper),
                                height = 0.2, color = "#1565C0", linewidth = 0.8) +
        ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
        ggplot2::labs(title = "ATE Comparison Across Methods",
                      subtitle = "Point estimates with confidence intervals",
                      x = "Average Treatment Effect (ATE)", y = "") +
        theme_causal
      figures_catalog$ate_forest_plot <- list(
        number = fig_num, type = "figura", id = paste0("Figura_", fig_num),
        title = "ATE Forest Plot",
        description = "Comparison of ATE estimates across all methods with CI",
        filename = paste0("Figura_", fig_num, "_ATE_Forest_Plot.png")
      )
      if (verbose) cat("  Figura", fig_num, ": ATE Forest Plot\n")
    }, error = function(e) if (verbose) cat("  Error Figura", fig_num, ":", e$message, "\n"))
  }

  # --- Figura 10: Meta-Learner CATE comparison ---
  if (!is.null(resultado$meta_learners)) {
    fig_num <- fig_num + 1
    tryCatch({
      ml <- resultado$meta_learners
      cate_list <- list()
      for (nm in c("S", "T", "X")) {
        if (!is.null(ml[[nm]]$cate)) {
          cate_list[[paste0(nm, "-Learner")]] <- ml[[nm]]$cate
        }
      }
      if (length(cate_list) > 0) {
        df_ml <- do.call(rbind, lapply(names(cate_list), function(nm) {
          data.frame(CATE = cate_list[[nm]], Learner = nm)
        }))
        plots$meta_learner_cate <- ggplot2::ggplot(df_ml, ggplot2::aes(x = CATE, color = Learner, fill = Learner)) +
          ggplot2::geom_density(alpha = 0.2, linewidth = 1) +
          ggplot2::geom_vline(xintercept = 0, linetype = "dashed") +
          ggplot2::scale_color_manual(values = c("#E91E63", "#00BCD4", "#FF9800")) +
          ggplot2::scale_fill_manual(values = c("#E91E63", "#00BCD4", "#FF9800")) +
          ggplot2::labs(title = "Meta-Learner CATE Comparison",
                        x = "CATE", y = "Density") +
          theme_causal
        figures_catalog$meta_learner_cate <- list(
          number = fig_num, type = "figura", id = paste0("Figura_", fig_num),
          title = "Meta-Learner CATE Comparison",
          description = "Density comparison of CATE estimates from S/T/X learners",
          filename = paste0("Figura_", fig_num, "_Meta_Learner_CATE.png")
        )
        if (verbose) cat("  Figura", fig_num, ": Meta-Learner CATE\n")
      }
    }, error = function(e) if (verbose) cat("  Error Figura", fig_num, ":", e$message, "\n"))
  }

  # --- Figura 11: E-value Sensitivity ---
  if (!is.null(resultado$evalue)) {
    fig_num <- fig_num + 1
    tryCatch({
      ev <- resultado$evalue
      df_ev <- data.frame(
        x = c(1, ev$evalue_point),
        label = c("No confounding", paste0("E-value = ", round(ev$evalue_point, 2)))
      )
      plots$sensitivity_evalue <- ggplot2::ggplot(df_ev, ggplot2::aes(x = x, y = 1)) +
        ggplot2::geom_segment(ggplot2::aes(x = 1, xend = ev$evalue_point, y = 1, yend = 1),
                              color = "#FF5722", linewidth = 2, alpha = 0.6) +
        ggplot2::geom_point(ggplot2::aes(x = ev$evalue_point), size = 5, color = "#D84315") +
        ggplot2::geom_point(ggplot2::aes(x = 1), size = 5, color = "#4CAF50") +
        ggplot2::annotate("text", x = ev$evalue_point, y = 1.1,
                          label = paste0("E-value = ", round(ev$evalue_point, 2)),
                          size = 4, fontface = "bold") +
        ggplot2::labs(title = "Sensitivity Analysis: E-value",
                      subtitle = "Minimum confounding strength to explain away the effect",
                      x = "Risk Ratio Scale", y = "") +
        ggplot2::xlim(0.5, max(ev$evalue_point * 1.2, 3)) +
        theme_causal + ggplot2::theme(axis.text.y = ggplot2::element_blank())
      figures_catalog$sensitivity_evalue <- list(
        number = fig_num, type = "figura", id = paste0("Figura_", fig_num),
        title = "E-value Sensitivity Analysis",
        description = paste("E-value =", round(ev$evalue_point, 2)),
        filename = paste0("Figura_", fig_num, "_Sensitivity_Evalue.png")
      )
      if (verbose) cat("  Figura", fig_num, ": E-value Sensitivity\n")
    }, error = function(e) if (verbose) cat("  Error Figura", fig_num, ":", e$message, "\n"))
  }

  # --- Figura 12: Matching Balance (antes/despues) ---
  if (!is.null(resultado$propensity$matching$matchit)) {
    fig_num <- fig_num + 1
    tryCatch({
      m_out <- resultado$propensity$matching$matchit
      if (.check_package("cobalt", "Love plots")) {
        plots$matching_balance <- cobalt::love.plot(m_out,
                                                     binary = "std",
                                                     thresholds = c(m = 0.1),
                                                     title = "Matching Balance")
      } else {
        # Fallback nativo
        m_data <- resultado$propensity$matching$matched_data
        covariates <- resultado$preprocessed$covariates
        num_covs <- covariates[sapply(data[, covariates, drop = FALSE], is.numeric)]
        t_binary <- as.integer(data[[treatment]])

        smd_before <- .calc_all_smd(data, t_binary, num_covs)
        t_matched <- as.integer(m_data[[treatment]])
        smd_after <- .calc_all_smd(m_data, t_matched, num_covs)

        df_bal <- data.frame(
          variable = rep(smd_before$variable, 2),
          smd = c(smd_before$smd, smd_after$smd),
          stage = rep(c("Before", "After"), each = nrow(smd_before))
        )
        df_bal$variable <- stats::reorder(df_bal$variable, df_bal$smd)

        plots$matching_balance <- ggplot2::ggplot(df_bal, ggplot2::aes(x = smd, y = variable, color = stage, shape = stage)) +
          ggplot2::geom_point(size = 2.5) +
          ggplot2::geom_vline(xintercept = 0.10, linetype = "dashed", color = "red", alpha = 0.5) +
          ggplot2::scale_color_manual(values = c("Before" = "#FF5722", "After" = "#4CAF50")) +
          ggplot2::labs(title = "Matching Balance: Before vs After",
                        x = "|SMD|", y = "") +
          theme_causal
      }
      figures_catalog$matching_balance <- list(
        number = fig_num, type = "figura", id = paste0("Figura_", fig_num),
        title = "Matching Balance",
        description = "Covariate balance before and after propensity score matching",
        filename = paste0("Figura_", fig_num, "_Matching_Balance.png")
      )
      if (verbose) cat("  Figura", fig_num, ": Matching Balance\n")
    }, error = function(e) if (verbose) cat("  Error Figura", fig_num, ":", e$message, "\n"))
  }

  if (verbose) {
    cat("\n  Total figuras generadas:", length(plots), "\n")
  }

  return(list(plots = plots, figures_catalog = figures_catalog))
}

# --- Helper: obtener mejor CATE disponible ---
#' @noRd
.get_best_cate <- function(resultado) {
  # Prioridad: Causal Forest > X-learner > T-learner > S-learner
  if (!is.null(resultado$advanced$causal_forest$cate)) {
    return(resultado$advanced$causal_forest$cate)
  }
  if (!is.null(resultado$meta_learners$X$cate)) {
    return(resultado$meta_learners$X$cate)
  }
  if (!is.null(resultado$meta_learners$T$cate)) {
    return(resultado$meta_learners$T$cate)
  }
  if (!is.null(resultado$meta_learners$S$cate)) {
    return(resultado$meta_learners$S$cate)
  }
  if (!is.null(resultado$python$econml_forest$cate)) {
    return(resultado$python$econml_forest$cate)
  }
  return(NULL)
}
