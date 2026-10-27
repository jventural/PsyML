
# =============================================================================
# Clustering para Aprendizaje No Supervisado
# =============================================================================

#' @noRd
.unsupervised_clustering <- function(data_processed,
                                      data_original = NULL,
                                      algorithms,
                                      k, k_range,
                                      distance,
                                      hclust_method,
                                      dbscan_eps, dbscan_minPts,
                                      gmm_type,
                                      lca_nclass = NULL, lca_nrep = 10,
                                      exclude_cols = NULL,
                                      seed,
                                      verbose) {

  if (verbose) {
    .print_section(4, "Clustering")
  }

  result <- list()
  result$results <- list()

  n_obs <- nrow(data_processed)
  data_mat <- as.matrix(data_processed)

  # =========================================================================
  # 4.1 Determinacion del k optimo
  # =========================================================================
  if (is.null(k)) {
    if (verbose) .print_subsection(4, 1, "Determinacion del k Optimo")

    optimal_k_info <- .find_optimal_k(data_mat, k_range, distance, seed, verbose)
    k <- optimal_k_info$optimal_k
    result$optimal_k <- k
    result$k_analysis <- optimal_k_info

    if (verbose) {
      cat("\n  >>> k optimo seleccionado:", k, "\n")
    }
  } else {
    result$optimal_k <- k
    result$k_analysis <- NULL
    if (verbose) {
      .print_subsection(4, 1, "k Definido por Usuario")
      cat("  k =", k, "\n")
    }
  }

  # =========================================================================
  # 4.2 K-Means
  # =========================================================================
  if ("kmeans" %in% algorithms) {
    if (verbose) .print_subsection(4, 2, "K-Means Clustering")

    tryCatch({
      if (!is.null(seed)) set.seed(seed)
      km_fit <- kmeans(data_mat, centers = k, nstart = 25, iter.max = 100)

      result$results$kmeans <- list(
        labels = km_fit$cluster,
        model = km_fit,
        centers = km_fit$centers,
        within_ss = km_fit$tot.withinss,
        between_ss = km_fit$betweenss,
        size = km_fit$size
      )

      if (verbose) {
        cat("  Clusters:", k, "\n")
        cat("  Within-cluster SS:", round(km_fit$tot.withinss, 2), "\n")
        cat("  Between-cluster SS:", round(km_fit$betweenss, 2), "\n")
        cat("  Ratio (between/total):", round(km_fit$betweenss / km_fit$totss * 100, 1), "%\n")
        cat("  Tamanio de clusters:", paste(km_fit$size, collapse = ", "), "\n")
      }
    }, error = function(e) {
      if (verbose) cat("  [ERROR] K-Means fallo:", conditionMessage(e), "\n")
    })
  }

  # =========================================================================
  # 4.3 Clustering Jerarquico
  # =========================================================================
  if ("hierarchical" %in% algorithms) {
    if (verbose) .print_subsection(4, 3, "Clustering Jerarquico")

    tryCatch({
      if (distance == "gower") {
        if (requireNamespace("cluster", quietly = TRUE)) {
          dist_mat <- cluster::daisy(data_processed, metric = "gower")
        } else {
          dist_mat <- dist(data_mat, method = "euclidean")
          if (verbose) cat("  [!] Paquete 'cluster' no disponible. Usando euclidean.\n")
        }
      } else {
        dist_mat <- dist(data_mat, method = distance)
      }

      hc_fit <- hclust(dist_mat, method = hclust_method)
      hc_labels <- cutree(hc_fit, k = k)

      result$results$hierarchical <- list(
        labels = hc_labels,
        model = hc_fit,
        dist_matrix = dist_mat,
        method = hclust_method,
        size = as.integer(table(hc_labels))
      )

      if (verbose) {
        cat("  Metodo de enlace:", hclust_method, "\n")
        cat("  Distancia:", distance, "\n")
        cat("  Clusters:", k, "\n")
        cat("  Tamanio de clusters:", paste(table(hc_labels), collapse = ", "), "\n")

        # Altura de fusion
        heights <- rev(hc_fit$height)
        cat("  Alturas de fusion (top 5):\n")
        for (i in seq_len(min(5, length(heights)))) {
          cat(sprintf("    Paso %d: %.4f\n", i, heights[i]))
        }
      }
    }, error = function(e) {
      if (verbose) cat("  [ERROR] Clustering jerarquico fallo:", conditionMessage(e), "\n")
    })
  }

  # =========================================================================
  # 4.4 DBSCAN
  # =========================================================================
  if ("dbscan" %in% algorithms) {
    if (verbose) .print_subsection(4, 4, "DBSCAN (Density-Based Clustering)")

    if (!requireNamespace("dbscan", quietly = TRUE)) {
      if (verbose) cat("  [!] Package 'dbscan' not installed. Install package 'dbscan' to use this option.\n")
    } else {
      tryCatch({
        # Auto-calcular eps si no se especifica
        if (is.null(dbscan_eps)) {
          knn_dists <- dbscan::kNNdist(data_mat, k = dbscan_minPts)
          # kNNdist puede devolver vector o matriz
          if (is.matrix(knn_dists)) {
            sorted_dists <- sort(knn_dists[, ncol(knn_dists)])
          } else {
            sorted_dists <- sort(knn_dists)
          }
          # Metodo del codo: punto de mayor curvatura
          n_pts <- length(sorted_dists)
          # Usar el percentil 90 como heuristica
          dbscan_eps <- sorted_dists[round(n_pts * 0.90)]
          if (verbose) {
            cat("  eps auto-calculado:", round(dbscan_eps, 4), "\n")
          }
        }

        db_fit <- dbscan::dbscan(data_mat, eps = dbscan_eps, minPts = dbscan_minPts)

        db_labels <- db_fit$cluster
        n_clusters_db <- max(db_labels)
        n_noise <- sum(db_labels == 0)

        result$results$dbscan <- list(
          labels = db_labels,
          model = db_fit,
          eps = dbscan_eps,
          minPts = dbscan_minPts,
          n_clusters = n_clusters_db,
          n_noise = n_noise,
          size = as.integer(table(db_labels[db_labels > 0]))
        )

        if (verbose) {
          cat("  eps:", round(dbscan_eps, 4), "\n")
          cat("  minPts:", dbscan_minPts, "\n")
          cat("  Clusters encontrados:", n_clusters_db, "\n")
          cat("  Puntos ruido:", n_noise, "(", round(n_noise / n_obs * 100, 1), "%)\n")
          if (n_clusters_db > 0) {
            cat("  Tamanio de clusters:", paste(table(db_labels[db_labels > 0]), collapse = ", "), "\n")
          }
          if (n_clusters_db == 0) {
            cat("  [!] DBSCAN no encontro clusters. Pruebe reducir eps o minPts.\n")
          }
        }
      }, error = function(e) {
        if (verbose) cat("  [ERROR] DBSCAN fallo:", conditionMessage(e), "\n")
      })
    }
  }

  # =========================================================================
  # 4.5 GMM (Gaussian Mixture Models)
  # =========================================================================
  if ("gmm" %in% algorithms) {
    if (verbose) .print_subsection(4, 5, "GMM (Gaussian Mixture Models)")

    if (!requireNamespace("mclust", quietly = TRUE)) {
      if (verbose) cat("  [!] Paquete 'mclust' no instalado; se omite GMM.\n")
    } else {
      # mclust::Mclust() rebuilds its call as mclustBIC(...) and evaluates it
      # in the calling frame, so mclustBIC must be visible there. A local
      # binding is enough; the package is not attached.
      mclustBIC <- mclust::mclustBIC
      tryCatch({
        if (!is.null(seed)) set.seed(seed)
        gmm_fit <- mclust::Mclust(data_mat, G = k, modelNames = gmm_type,
                                  verbose = FALSE)

        # Fallback: si el tipo de modelo solicitado no converge, dejar que
        # mclust seleccione la mejor estructura de covarianza para ese k.
        if (is.null(gmm_fit)) {
          gmm_fit <- mclust::Mclust(data_mat, G = k, verbose = FALSE)
        }

        if (!is.null(gmm_fit)) {
          gmm_labels <- gmm_fit$classification
          gmm_probs <- gmm_fit$z  # Probabilidades de pertenencia

          result$results$gmm <- list(
            labels = gmm_labels,
            model = gmm_fit,
            probabilities = gmm_probs,
            bic = gmm_fit$bic,
            model_name = gmm_fit$modelName,
            size = as.integer(table(gmm_labels)),
            uncertainty = gmm_fit$uncertainty
          )

          if (verbose) {
            cat("  Tipo de modelo:", gmm_fit$modelName, "\n")
            cat("  Clusters:", k, "\n")
            cat("  BIC:", round(gmm_fit$bic, 2), "\n")
            cat("  Tamanio de clusters:", paste(table(gmm_labels), collapse = ", "), "\n")
            cat("  Incertidumbre media:", round(mean(gmm_fit$uncertainty), 4), "\n")
            cat("  Incertidumbre max:", round(max(gmm_fit$uncertainty), 4), "\n")
          }
        } else {
          if (verbose) cat("  [!] GMM no convergio. Pruebe otro tipo de modelo.\n")
        }
      }, error = function(e) {
        if (verbose) cat("  [ERROR] GMM fallo:", conditionMessage(e), "\n")
      })
    }
  }

  # =========================================================================
  # 4.6 LCA (Latent Class Analysis)
  # =========================================================================
  if ("lca" %in% algorithms) {
    if (verbose) .print_subsection(4, 6, "LCA (Latent Class Analysis)")

    if (!requireNamespace("poLCA", quietly = TRUE)) {
      if (verbose) cat("  [!] Package 'poLCA' not installed. Install package 'poLCA' to use this option.\n")
    } else {
      tryCatch({
        # LCA requiere datos categoricos (enteros positivos 1, 2, ..., R)
        # Usar data_original para preservar la naturaleza categorica
        if (!is.null(data_original)) {
          if (!is.null(exclude_cols)) {
            lca_cols <- names(data_original)[!names(data_original) %in% exclude_cols]
          } else {
            lca_cols <- names(data_original)
          }
          data_lca_raw <- data_original[, lca_cols, drop = FALSE]
        } else {
          data_lca_raw <- data_processed
        }

        # Convertir a enteros positivos (poLCA requiere 1, 2, ..., R)
        data_lca <- as.data.frame(lapply(data_lca_raw, function(x) {
          if (is.factor(x) || is.character(x)) {
            as.integer(as.factor(x))
          } else if (is.numeric(x)) {
            # Si ya son enteros positivos, mantener; sino discretizar
            vals <- unique(na.omit(x))
            if (all(vals == floor(vals)) && min(vals) >= 0 && length(vals) <= 20) {
              as.integer(as.factor(x))
            } else {
              # Datos continuos: no aptos para LCA
              NULL
            }
          } else {
            NULL
          }
        }))

        # Remover columnas NULL (variables continuas no aptas)
        null_cols <- sapply(data_lca, is.null)
        if (any(null_cols)) {
          removed_lca <- names(data_lca)[null_cols]
          data_lca <- data_lca[, !null_cols, drop = FALSE]
          if (verbose) cat("  [!] Variables continuas excluidas de LCA:", paste(removed_lca, collapse = ", "), "\n")
        }

        if (ncol(data_lca) < 2) {
          if (verbose) cat("  [!] Insuficientes variables categoricas para LCA (minimo 2).\n")
        } else {
          # Remover filas con NA
          complete_idx <- complete.cases(data_lca)
          if (sum(!complete_idx) > 0 && verbose) {
            cat("  [!] Removiendo", sum(!complete_idx), "filas con NA para LCA.\n")
          }
          data_lca <- data_lca[complete_idx, , drop = FALSE]

          # Formula: todas las variables como indicadores
          f <- as.formula(paste("cbind(",
            paste(colnames(data_lca), collapse = ", "),
            ") ~ 1"))

          # Seleccion de numero de clases:
          # Si lca_nclass esta definido, usarlo directamente
          # Si no, buscar optimo por BIC (independiente del k de clustering)
          if (is.null(lca_nclass)) {
            if (verbose) cat("  Buscando numero optimo de clases por BIC...\n")

            lca_k_range <- k_range[k_range >= 2]
            lca_bic_values <- numeric(length(lca_k_range))
            lca_aic_values <- numeric(length(lca_k_range))
            lca_models <- list()

            for (ki_idx in seq_along(lca_k_range)) {
              ki <- lca_k_range[ki_idx]
              tryCatch({
                if (!is.null(seed)) set.seed(seed)
                lca_temp <- poLCA::poLCA(f, data = data_lca, nclass = ki,
                                          nrep = lca_nrep, verbose = FALSE)
                lca_bic_values[ki_idx] <- lca_temp$bic
                lca_aic_values[ki_idx] <- lca_temp$aic
                lca_models[[ki_idx]] <- lca_temp
              }, error = function(e) {
                lca_bic_values[ki_idx] <- Inf
                lca_aic_values[ki_idx] <- Inf
              })
            }

            best_lca_idx <- which.min(lca_bic_values)
            k_use <- lca_k_range[best_lca_idx]
            lca_fit <- lca_models[[best_lca_idx]]

            if (verbose) {
              cat("  BIC por numero de clases:\n")
              for (ki_idx in seq_along(lca_k_range)) {
                indicator <- if (lca_k_range[ki_idx] == k_use) " <<< optimo" else ""
                cat(sprintf("    k=%d: BIC=%.2f, AIC=%.2f%s\n",
                    lca_k_range[ki_idx], lca_bic_values[ki_idx],
                    lca_aic_values[ki_idx], indicator))
              }
            }
          } else {
            k_use <- lca_nclass
            if (!is.null(seed)) set.seed(seed)
            lca_fit <- poLCA::poLCA(f, data = data_lca, nclass = k_use,
                                     nrep = lca_nrep, verbose = FALSE)
          }

          # Probabilidades posteriores
          post_probs <- lca_fit$posterior

          # Calcular R2 entropy
          n_obs_lca <- nrow(post_probs)
          k_lca <- ncol(post_probs)
          E_sum <- -sum(post_probs * log(post_probs + 1e-10))
          r2_entropy <- 1 - E_sum / (n_obs_lca * log(k_lca))

          result$results$lca <- list(
            labels              = lca_fit$predclass,
            model               = lca_fit,
            probabilities       = post_probs,
            item_response_probs = lca_fit$probs,
            bic                 = lca_fit$bic,
            aic                 = lca_fit$aic,
            entropy             = r2_entropy,
            size                = as.integer(table(lca_fit$predclass)),
            uncertainty         = apply(post_probs, 1, function(p) 1 - max(p)),
            lca_variables       = colnames(data_lca),
            complete_idx        = which(complete_idx)
          )

          if (verbose) {
            cat("  Clases:", k_use, "\n")
            cat("  Variables LCA:", ncol(data_lca), "\n")
            cat("  Observaciones:", nrow(data_lca), "\n")
            cat("  BIC:", round(lca_fit$bic, 2), "\n")
            cat("  AIC:", round(lca_fit$aic, 2), "\n")
            cat("  Entropy (R2):", round(r2_entropy, 4), "\n")
            cat("  Tamanio de clases:", paste(table(lca_fit$predclass), collapse = ", "), "\n")
            cat("  Incertidumbre media:", round(mean(1 - apply(post_probs, 1, max)), 4), "\n")

            # Interpretacion entropy
            if (r2_entropy > 0.8) {
              cat("  Interpretacion: Separacion de clases EXCELENTE.\n")
            } else if (r2_entropy > 0.6) {
              cat("  Interpretacion: Separacion de clases BUENA.\n")
            } else if (r2_entropy > 0.4) {
              cat("  Interpretacion: Separacion de clases MODERADA.\n")
            } else {
              cat("  Interpretacion: Separacion de clases DEBIL.\n")
            }
          }
        }
      }, error = function(e) {
        if (verbose) cat("  [ERROR] LCA fallo:", conditionMessage(e), "\n")
      })
    }
  }

  return(result)
}


# =============================================================================
# Funcion auxiliar: buscar k optimo
# =============================================================================

#' @noRd
.find_optimal_k <- function(data_mat, k_range, distance, seed, verbose) {

  n_obs <- nrow(data_mat)
  k_range <- k_range[k_range < n_obs]

  # ------ Silhouette ------
  sil_scores <- numeric(length(k_range))
  names(sil_scores) <- k_range

  if (requireNamespace("cluster", quietly = TRUE)) {
    dist_mat <- dist(data_mat, method = if (distance == "gower") "euclidean" else distance)

    for (i in seq_along(k_range)) {
      ki <- k_range[i]
      if (!is.null(seed)) set.seed(seed)
      km <- kmeans(data_mat, centers = ki, nstart = 10, iter.max = 50)
      sil <- cluster::silhouette(km$cluster, dist_mat)
      sil_scores[i] <- mean(sil[, 3])
    }

    best_k_sil <- k_range[which.max(sil_scores)]
  } else {
    best_k_sil <- k_range[1]
    if (verbose) cat("  [!] Paquete 'cluster' no disponible para silhouette.\n")
  }

  # ------ Elbow (Within-SS) ------
  wss <- numeric(length(k_range))
  for (i in seq_along(k_range)) {
    ki <- k_range[i]
    if (!is.null(seed)) set.seed(seed)
    km <- kmeans(data_mat, centers = ki, nstart = 10, iter.max = 50)
    wss[i] <- km$tot.withinss
  }
  names(wss) <- k_range

  # Metodo del codo: mayor caida relativa
  if (length(wss) >= 3) {
    diffs <- -diff(wss)
    diffs2 <- -diff(diffs)
    best_k_elbow <- k_range[which.max(diffs2) + 1]
  } else {
    best_k_elbow <- k_range[1]
  }

  # ------ Gap Statistic ------
  best_k_gap <- NA
  gap_values <- NULL
  if (requireNamespace("cluster", quietly = TRUE)) {
    tryCatch({
      if (!is.null(seed)) set.seed(seed)
      gap_stat <- cluster::clusGap(data_mat, FUN = kmeans, nstart = 10,
                                    K.max = max(k_range), B = 50)
      gap_values <- gap_stat$Tab[k_range, "gap"]
      se_values <- gap_stat$Tab[k_range, "SE.sim"]

      # Criterio Tibshirani: primer k donde gap(k) >= gap(k+1) - SE(k+1)
      for (i in seq_len(length(k_range) - 1)) {
        if (gap_values[i] >= gap_values[i + 1] - se_values[i + 1]) {
          best_k_gap <- k_range[i]
          break
        }
      }
      if (is.na(best_k_gap)) best_k_gap <- k_range[which.max(gap_values)]
    }, error = function(e) {
      if (verbose) cat("  [!] Gap statistic fallo:", conditionMessage(e), "\n")
    })
  }

  # ------ Consenso: voto mayoritario ------
  candidates <- c(silhouette = best_k_sil, elbow = best_k_elbow)
  if (!is.na(best_k_gap)) candidates <- c(candidates, gap = best_k_gap)

  # El k mas votado, desempate por silhouette
  k_table <- table(candidates)
  max_votes <- max(k_table)
  top_ks <- as.integer(names(k_table[k_table == max_votes]))

  if (length(top_ks) == 1) {
    optimal_k <- top_ks
  } else {
    # Desempate: el que tenga mejor silhouette
    sil_at_top <- sil_scores[as.character(top_ks)]
    optimal_k <- top_ks[which.max(sil_at_top)]
  }

  if (verbose) {
    cat("  Metodo Silhouette -> k =", best_k_sil,
        "(score =", round(max(sil_scores), 4), ")\n")
    cat("  Metodo Elbow -> k =", best_k_elbow, "\n")
    if (!is.na(best_k_gap)) {
      cat("  Metodo Gap -> k =", best_k_gap, "\n")
    }

    cat("\n  Silhouette scores por k:\n")
    for (i in seq_along(k_range)) {
      indicator <- if (k_range[i] == optimal_k) " <<< optimo" else ""
      bar_len <- max(0, round(sil_scores[i] * 30))
      bar <- paste(rep("|", bar_len), collapse = "")
      cat(sprintf("    k=%d: %.4f %s%s\n", k_range[i], sil_scores[i], bar, indicator))
    }
  }

  list(
    optimal_k = optimal_k,
    silhouette_scores = sil_scores,
    wss = wss,
    gap_values = gap_values,
    best_k_silhouette = best_k_sil,
    best_k_elbow = best_k_elbow,
    best_k_gap = best_k_gap,
    candidates = candidates
  )
}
