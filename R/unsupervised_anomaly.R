
# =============================================================================
# Deteccion de Anomalias para Aprendizaje No Supervisado
# =============================================================================

#' @noRd
.unsupervised_anomaly <- function(data_processed,
                                   anomaly_methods,
                                   contamination,
                                   lof_k,
                                   seed,
                                   verbose) {

  if (verbose) {
    .print_section(6, "Deteccion de Anomalias")
  }

  result <- list()
  result$results <- list()
  data_mat <- as.matrix(data_processed)
  n_obs <- nrow(data_mat)
  n_expected <- ceiling(n_obs * contamination)

  if (verbose) {
    cat("  Contaminacion esperada:", contamination * 100, "%\n")
    cat("  Anomalias esperadas: ~", n_expected, "\n\n")
  }

  # =========================================================================
  # 6.1 Isolation Forest
  # =========================================================================
  if ("isolation_forest" %in% anomaly_methods) {
    if (verbose) .print_subsection(6, 1, "Isolation Forest")

    if (!requireNamespace("isotree", quietly = TRUE)) {
      if (verbose) cat("  [!] Package 'isotree' not installed. Install package 'isotree' to use this option.\n")
    } else {
      tryCatch({
        if (!is.null(seed)) set.seed(seed)
        iso_fit <- isotree::isolation.forest(data_processed, ntrees = 100,
                                              sample_size = min(256, n_obs),
                                              ndim = 1,
                                              seed = if (is.null(seed)) sample.int(.Machine$integer.max, 1) else seed)
        iso_scores <- predict(iso_fit, data_processed)

        # Threshold basado en contaminacion
        threshold <- quantile(iso_scores, 1 - contamination)
        is_anomaly <- iso_scores > threshold

        result$results$isolation_forest <- list(
          scores = iso_scores,
          threshold = threshold,
          is_anomaly = is_anomaly,
          n_anomalies = sum(is_anomaly),
          model = iso_fit
        )

        if (verbose) {
          cat("  Arboles: 100\n")
          cat("  Threshold (percentil ", (1 - contamination) * 100, "):",
              round(threshold, 4), "\n")
          cat("  Anomalias detectadas:", sum(is_anomaly), "\n")
          cat("  Score medio:", round(mean(iso_scores), 4), "\n")
          cat("  Score DE:", round(sd(iso_scores), 4), "\n")

          # Top 5 anomalias
          top5 <- order(iso_scores, decreasing = TRUE)[1:min(5, n_obs)]
          cat("\n  Top 5 anomalias (indice, score):\n")
          for (idx in top5) {
            cat(sprintf("    Obs %d: score = %.4f\n", idx, iso_scores[idx]))
          }
        }
      }, error = function(e) {
        if (verbose) cat("  [ERROR] Isolation Forest fallo:", conditionMessage(e), "\n")
      })
    }
  }

  # =========================================================================
  # 6.2 LOF (Local Outlier Factor)
  # =========================================================================
  if ("lof" %in% anomaly_methods) {
    if (verbose) .print_subsection(6, 2, "LOF (Local Outlier Factor)")

    if (!requireNamespace("dbscan", quietly = TRUE)) {
      if (verbose) cat("  [!] Package 'dbscan' not installed. Install package 'dbscan' to use this option.\n")
    } else {
      tryCatch({
        effective_k <- min(lof_k, n_obs - 1)

        # dbscan::lof() diverge a Inf cuando hay >= minPts puntos identicos
        # (densidad local infinita), algo frecuente en datos discretos u
        # ordinales. Se aplica un jitter minimo a los duplicados exactos para
        # estabilizar el calculo sin alterar la estructura.
        lof_input <- data_mat
        dup <- duplicated(data_mat) | duplicated(data_mat, fromLast = TRUE)
        n_dup <- sum(dup)
        if (n_dup > 0) {
          if (!is.null(seed)) set.seed(seed)
          jitter_mat <- matrix(rnorm(n_dup * ncol(data_mat), 0, 1e-4),
                               nrow = n_dup, ncol = ncol(data_mat))
          lof_input[dup, ] <- lof_input[dup, ] + jitter_mat
        }

        lof_scores <- dbscan::lof(lof_input, minPts = effective_k)

        # Sanear valores no finitos residuales (Inf/NaN -> maximo finito)
        if (any(!is.finite(lof_scores))) {
          finite_vals <- lof_scores[is.finite(lof_scores)]
          repl <- if (length(finite_vals)) max(finite_vals) else 1
          lof_scores[!is.finite(lof_scores)] <- repl
        }

        # Threshold basado en contaminacion
        threshold <- quantile(lof_scores, 1 - contamination, na.rm = TRUE)
        is_anomaly <- lof_scores > threshold

        result$results$lof <- list(
          scores = lof_scores,
          threshold = threshold,
          is_anomaly = is_anomaly,
          n_anomalies = sum(is_anomaly, na.rm = TRUE),
          k = effective_k
        )

        if (verbose) {
          cat("  k (minPts):", effective_k, "\n")
          if (n_dup > 0) {
            cat("  [i] Puntos duplicados estabilizados con jitter:", n_dup, "\n")
          }
          cat("  Threshold:", round(threshold, 4), "\n")
          cat("  Anomalias detectadas:", sum(is_anomaly, na.rm = TRUE), "\n")
          cat("  LOF medio:", round(mean(lof_scores, na.rm = TRUE), 4), "\n")
          cat("  LOF max:", round(max(lof_scores, na.rm = TRUE), 4), "\n")

          # Interpretacion
          cat("\n  Interpretacion de LOF scores:\n")
          cat("    LOF ~ 1: punto normal (densidad similar a vecinos)\n")
          cat("    LOF > 1: punto menos denso que vecinos (potencial anomalia)\n")
          cat("    LOF >> 2: anomalia clara\n")

          # Top 5
          top5 <- order(lof_scores, decreasing = TRUE)[1:min(5, n_obs)]
          cat("\n  Top 5 anomalias (indice, LOF):\n")
          for (idx in top5) {
            cat(sprintf("    Obs %d: LOF = %.4f\n", idx, lof_scores[idx]))
          }
        }
      }, error = function(e) {
        if (verbose) cat("  [ERROR] LOF fallo:", conditionMessage(e), "\n")
      })
    }
  }

  # =========================================================================
  # 6.3 Mahalanobis Distance
  # =========================================================================
  if ("mahalanobis" %in% anomaly_methods) {
    if (verbose) .print_subsection(6, 3, "Distancia de Mahalanobis")

    tryCatch({
      # Covarianza robusta si es posible
      center <- colMeans(data_mat)
      cov_mat <- cov(data_mat)

      # Verificar si la covarianza es invertible
      cov_det <- det(cov_mat)
      if (abs(cov_det) < .Machine$double.eps) {
        # Usar pseudo-inversa
        if (verbose) cat("  [!] Covarianza singular. Usando regularizacion.\n")
        cov_mat <- cov_mat + diag(ncol(cov_mat)) * 1e-6
      }

      maha_scores <- mahalanobis(data_mat, center = center, cov = cov_mat)

      # Threshold basado en chi-cuadrado (p < alpha) donde alpha = contamination
      p <- ncol(data_mat)
      threshold_chi2 <- qchisq(1 - contamination, df = p)

      is_anomaly <- maha_scores > threshold_chi2

      result$results$mahalanobis <- list(
        scores = maha_scores,
        threshold = threshold_chi2,
        is_anomaly = is_anomaly,
        n_anomalies = sum(is_anomaly),
        df = p
      )

      if (verbose) {
        cat("  Grados de libertad (p):", p, "\n")
        cat("  Threshold (chi2, alpha =", contamination, "):",
            round(threshold_chi2, 4), "\n")
        cat("  Anomalias detectadas:", sum(is_anomaly), "\n")
        cat("  Mahalanobis medio:", round(mean(maha_scores), 4), "\n")
        cat("  Mahalanobis max:", round(max(maha_scores), 4), "\n")

        top5 <- order(maha_scores, decreasing = TRUE)[1:min(5, n_obs)]
        cat("\n  Top 5 anomalias (indice, distancia):\n")
        for (idx in top5) {
          cat(sprintf("    Obs %d: D2 = %.4f\n", idx, maha_scores[idx]))
        }
      }
    }, error = function(e) {
      if (verbose) cat("  [ERROR] Mahalanobis fallo:", conditionMessage(e), "\n")
    })
  }

  # =========================================================================
  # CONSENSO
  # =========================================================================
  if (length(result$results) >= 2) {
    if (verbose) .print_subsection(6, length(anomaly_methods) + 1, "Consenso de Anomalias")

    # Contar cuantos metodos marcan cada obs como anomalia
    anomaly_matrix <- sapply(result$results, function(r) {
      as.integer(r$is_anomaly)
    })

    if (is.matrix(anomaly_matrix)) {
      n_methods <- ncol(anomaly_matrix)
      votes <- rowSums(anomaly_matrix)

      # Consenso = mayoria
      majority_threshold <- ceiling(n_methods / 2)
      consensus_anomaly <- votes >= majority_threshold

      result$consensus <- list(
        votes = votes,
        is_anomaly = consensus_anomaly,
        n_anomalies = sum(consensus_anomaly),
        majority_threshold = majority_threshold
      )

      if (verbose) {
        cat("  Metodos:", n_methods, "\n")
        cat("  Umbral de mayoria:", majority_threshold, "de", n_methods, "\n")
        cat("  Anomalias por consenso:", sum(consensus_anomaly), "\n")

        cat("\n  Distribucion de votos:\n")
        vote_table <- table(votes)
        for (v in names(vote_table)) {
          cat(sprintf("    %s metodos: %d observaciones\n", v, vote_table[v]))
        }
      }
    }
  }

  return(result)
}
