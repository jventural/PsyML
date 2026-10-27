
# =============================================================================
# Evaluacion de Clusters para Aprendizaje No Supervisado
# =============================================================================

#' @noRd
.unsupervised_evaluate <- function(data_processed,
                                    clustering_result,
                                    verbose) {

  if (verbose) {
    .print_section(5, "Evaluacion de Clusters")
  }

  result <- list()
  data_mat <- as.matrix(data_processed)
  dist_mat <- dist(data_mat)

  comparison_rows <- list()

  for (alg_name in names(clustering_result$results)) {
    alg <- clustering_result$results[[alg_name]]
    labels <- alg$labels

    if (verbose) .print_subsection(5, match(alg_name, names(clustering_result$results)), alg_name)

    # --- LCA usa metricas propias (BIC/AIC/Entropy) ---
    if (alg_name == "lca") {
      metrics <- list(
        bic = alg$bic,
        aic = alg$aic,
        entropy = alg$entropy,
        uncertainty_mean = mean(alg$uncertainty)
      )
      result[[alg_name]] <- metrics

      if (verbose) {
        cat("  BIC:", round(alg$bic, 2), "\n")
        cat("  AIC:", round(alg$aic, 2), "\n")
        cat("  Entropy (R2):", round(alg$entropy, 4), "\n")
        cat("  Incertidumbre media:", round(mean(alg$uncertainty), 4), "\n")
        cat("  [Nota] LCA usa metricas probabilisticas (BIC/AIC/Entropy),\n")
        cat("         no metricas basadas en distancia (Silhouette, CH, DB).\n")
      }

      # Fila para comparacion (NA en metricas de distancia)
      comparison_rows[[alg_name]] <- data.frame(
        algorithm = alg_name,
        silhouette = NA_real_,
        calinski_harabasz = NA_real_,
        davies_bouldin = NA_real_,
        dunn = NA_real_,
        stringsAsFactors = FALSE
      )
      next
    }

    # Filtrar ruido (DBSCAN label = 0)
    valid_idx <- labels > 0
    if (sum(valid_idx) < 2 || length(unique(labels[valid_idx])) < 2) {
      if (verbose) cat("  [!] Insuficientes clusters validos para evaluar.\n")
      next
    }

    labels_valid <- labels[valid_idx]
    data_valid <- data_mat[valid_idx, , drop = FALSE]
    dist_valid <- dist(data_valid)

    metrics <- list()

    # ----- Silhouette -----
    if (requireNamespace("cluster", quietly = TRUE)) {
      sil <- cluster::silhouette(labels_valid, dist_valid)
      metrics$silhouette <- mean(sil[, 3])

      # Silhouette por cluster
      sil_by_cluster <- tapply(sil[, 3], labels_valid, mean)
      metrics$silhouette_per_cluster <- sil_by_cluster
    }

    # ----- Calinski-Harabasz -----
    metrics$calinski_harabasz <- .calinski_harabasz(data_valid, labels_valid)

    # ----- Davies-Bouldin -----
    metrics$davies_bouldin <- .davies_bouldin(data_valid, labels_valid)

    # ----- Dunn Index -----
    metrics$dunn <- .dunn_index(data_valid, labels_valid)

    # ----- Estabilidad (Jaccard bootstrap) -----
    # Solo si n no es demasiado grande
    if (nrow(data_valid) <= 5000 && alg_name %in% c("kmeans", "hierarchical")) {
      metrics$stability <- .cluster_stability(data_valid, labels_valid, alg_name,
                                               clustering_result$optimal_k,
                                               n_boot = 20)
    }

    result[[alg_name]] <- metrics

    if (verbose) {
      cat("  Silhouette promedio:", round(metrics$silhouette, 4), "\n")
      cat("  Calinski-Harabasz:", round(metrics$calinski_harabasz, 2), "\n")
      cat("  Davies-Bouldin:", round(metrics$davies_bouldin, 4),
          "(menor = mejor)\n")
      cat("  Dunn Index:", round(metrics$dunn, 4),
          "(mayor = mejor)\n")

      if (!is.null(metrics$silhouette_per_cluster)) {
        cat("\n  Silhouette por cluster:\n")
        for (cl in names(metrics$silhouette_per_cluster)) {
          s <- metrics$silhouette_per_cluster[cl]
          quality <- if (s > 0.7) "Fuerte" else if (s > 0.5) "Razonable" else if (s > 0.25) "Debil" else "Pobre"
          cat(sprintf("    Cluster %s: %.4f (%s)\n", cl, s, quality))
        }
      }

      if (!is.null(metrics$stability)) {
        cat("\n  Estabilidad (Jaccard medio):", round(metrics$stability, 4), "\n")
        stab_quality <- if (metrics$stability > 0.85) "Excelente" else if (metrics$stability > 0.75) "Buena" else if (metrics$stability > 0.6) "Moderada" else "Pobre"
        cat("  Interpretacion:", stab_quality, "\n")
      }
    }

    # Fila para comparacion
    comparison_rows[[alg_name]] <- data.frame(
      algorithm = alg_name,
      silhouette = round(metrics$silhouette, 4),
      calinski_harabasz = round(metrics$calinski_harabasz, 2),
      davies_bouldin = round(metrics$davies_bouldin, 4),
      dunn = round(metrics$dunn, 4),
      stringsAsFactors = FALSE
    )
  }

  # ----- Comparacion final -----
  if (length(comparison_rows) > 0) {
    comparison_df <- do.call(rbind, comparison_rows)
    rownames(comparison_df) <- NULL
    result$comparison <- comparison_df

    # Mejor algoritmo (por silhouette, excluyendo LCA que usa BIC/AIC)
    sil_values <- comparison_df$silhouette
    sil_values[is.na(sil_values)] <- -Inf
    best_idx <- which.max(sil_values)
    if (all(is.na(comparison_df$silhouette))) {
      # Solo hay LCA, usar el primero
      best_idx <- 1
    }
    result$best_algorithm <- list(
      algorithm = comparison_df$algorithm[best_idx],
      silhouette = comparison_df$silhouette[best_idx],
      calinski_harabasz = comparison_df$calinski_harabasz[best_idx],
      davies_bouldin = comparison_df$davies_bouldin[best_idx]
    )

    if (verbose) {
      .print_subsection(5, length(clustering_result$results) + 1, "Comparacion Final")
      print(comparison_df, row.names = FALSE)
      cat("\n  >>> Mejor algoritmo:", comparison_df$algorithm[best_idx],
          "(Silhouette =", comparison_df$silhouette[best_idx], ")\n")

      # Interpretacion del silhouette
      best_sil <- comparison_df$silhouette[best_idx]
      if (is.na(best_sil)) {
        cat("  [Nota] Mejor algoritmo es LCA (evaluado por BIC/AIC/Entropy, no Silhouette).\n")
      } else if (best_sil > 0.7) {
        cat("  Interpretacion: Estructura de clusters FUERTE.\n")
      } else if (best_sil > 0.5) {
        cat("  Interpretacion: Estructura de clusters RAZONABLE.\n")
      } else if (best_sil > 0.25) {
        cat("  Interpretacion: Estructura de clusters DEBIL. Los clusters se solapan.\n")
      } else {
        cat("  Interpretacion: NO se encontro estructura clara de clusters.\n")
        cat("  Considere: cambiar k, usar otro algoritmo, o los datos no tienen clusters naturales.\n")
      }
    }
  }

  return(result)
}


# =============================================================================
# Metricas internas de clustering
# =============================================================================

#' Calinski-Harabasz Index
#' @noRd
.calinski_harabasz <- function(data, labels) {
  n <- nrow(data)
  k <- length(unique(labels))
  if (k <= 1) return(0)

  overall_center <- colMeans(data)

  # Between-cluster SS
  bss <- 0
  wss <- 0
  for (cl in unique(labels)) {
    members <- data[labels == cl, , drop = FALSE]
    n_cl <- nrow(members)
    center_cl <- colMeans(members)
    bss <- bss + n_cl * sum((center_cl - overall_center)^2)
    wss <- wss + sum(sweep(members, 2, center_cl)^2)
  }

  if (wss == 0) return(Inf)
  (bss / (k - 1)) / (wss / (n - k))
}

#' Davies-Bouldin Index
#' @noRd
.davies_bouldin <- function(data, labels) {
  clusters <- unique(labels)
  k <- length(clusters)
  if (k <= 1) return(Inf)

  # Centros y dispersiones
  centers <- matrix(0, nrow = k, ncol = ncol(data))
  dispersions <- numeric(k)

  for (i in seq_along(clusters)) {
    members <- data[labels == clusters[i], , drop = FALSE]
    centers[i, ] <- colMeans(members)
    dispersions[i] <- mean(sqrt(rowSums(sweep(members, 2, centers[i, ])^2)))
  }

  # DB index
  db <- 0
  for (i in seq_len(k)) {
    max_ratio <- 0
    for (j in seq_len(k)) {
      if (i != j) {
        d_ij <- sqrt(sum((centers[i, ] - centers[j, ])^2))
        if (d_ij > 0) {
          ratio <- (dispersions[i] + dispersions[j]) / d_ij
          max_ratio <- max(max_ratio, ratio)
        }
      }
    }
    db <- db + max_ratio
  }

  db / k
}

#' Dunn Index
#' @noRd
.dunn_index <- function(data, labels) {
  clusters <- unique(labels)
  k <- length(clusters)
  if (k <= 1) return(0)

  # Para eficiencia, usar centroides
  centers <- matrix(0, nrow = k, ncol = ncol(data))
  max_intra <- 0

  for (i in seq_along(clusters)) {
    members <- data[labels == clusters[i], , drop = FALSE]
    centers[i, ] <- colMeans(members)
    if (nrow(members) > 1) {
      intra_dists <- sqrt(rowSums(sweep(members, 2, centers[i, ])^2))
      max_intra <- max(max_intra, max(intra_dists))
    }
  }

  if (max_intra == 0) return(Inf)

  # Minima distancia inter-cluster (entre centroides)
  min_inter <- Inf
  for (i in seq_len(k - 1)) {
    for (j in (i + 1):k) {
      d <- sqrt(sum((centers[i, ] - centers[j, ])^2))
      min_inter <- min(min_inter, d)
    }
  }

  min_inter / max_intra
}


#' Cluster stability via Jaccard bootstrap
#' @noRd
.cluster_stability <- function(data, labels, algorithm, k, n_boot = 20) {
  n <- nrow(data)
  jaccard_scores <- numeric(n_boot)

  for (b in seq_len(n_boot)) {
    boot_idx <- sample(n, replace = TRUE)
    data_boot <- data[boot_idx, , drop = FALSE]

    tryCatch({
      if (algorithm == "kmeans") {
        boot_labels <- kmeans(data_boot, centers = k, nstart = 10, iter.max = 50)$cluster
      } else if (algorithm == "hierarchical") {
        boot_dist <- dist(data_boot)
        boot_hc <- hclust(boot_dist, method = "ward.D2")
        boot_labels <- cutree(boot_hc, k = k)
      } else {
        jaccard_scores[b] <- NA
        next
      }

      # Calcular Jaccard entre original y bootstrap (para obs compartidas)
      # Usar los indices unicos del bootstrap
      unique_idx <- unique(boot_idx)
      orig_labels <- labels[unique_idx]
      # Re-mapear bootstrap labels a las obs unicas (tomar la primera ocurrencia)
      first_occurrence <- match(unique_idx, boot_idx)
      boot_labels_mapped <- boot_labels[first_occurrence]

      # Jaccard = intersection / union de pares co-asignados
      n_u <- length(unique_idx)
      if (n_u < 2) {
        jaccard_scores[b] <- NA
        next
      }

      # Pares muestreados
      same_orig <- outer(orig_labels, orig_labels, "==")
      same_boot <- outer(boot_labels_mapped, boot_labels_mapped, "==")
      upper <- upper.tri(same_orig)

      a <- sum(same_orig[upper] & same_boot[upper])
      b_val <- sum(same_orig[upper] & !same_boot[upper])
      c_val <- sum(!same_orig[upper] & same_boot[upper])

      denom <- a + b_val + c_val
      jaccard_scores[b] <- if (denom > 0) a / denom else 1

    }, error = function(e) {
      jaccard_scores[b] <- NA
    })
  }

  mean(jaccard_scores, na.rm = TRUE)
}
