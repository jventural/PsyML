
# =============================================================================
# Visualizaciones para Aprendizaje No Supervisado
# =============================================================================

#' @noRd
.unsupervised_plots <- function(data_processed, resultado, methods, verbose) {

  if (verbose) {
    .print_section(7, "Generacion de Visualizaciones")
  }

  figures <- list()
  fig_num <- 0

  # Colores para clusters
  cluster_colors <- c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00",
                       "#FFFF33", "#A65628", "#F781BF", "#999999", "#66C2A5",
                       "#FC8D62", "#8DA0CB")

  # =========================================================================
  # PLOTS DE REDUCCION DE DIMENSIONALIDAD
  # =========================================================================

  # --- Scree Plot (PCA) ---
  if (!is.null(resultado$reduction$pca)) {
    tryCatch({
      pca_res <- resultado$reduction$pca
      n_show <- min(15, length(pca_res$variance_explained))

      df_scree <- data.frame(
        PC = seq_len(n_show),
        Varianza = pca_res$variance_explained[seq_len(n_show)] * 100,
        Acumulada = pca_res$cumulative_variance[seq_len(n_show)] * 100
      )

      fig_num <- fig_num + 1
      p <- ggplot2::ggplot(df_scree) +
        ggplot2::geom_col(ggplot2::aes(x = PC, y = Varianza),
                          fill = "#377EB8", alpha = 0.7) +
        ggplot2::geom_line(ggplot2::aes(x = PC, y = Acumulada),
                           color = "#E41A1C", linewidth = 1) +
        ggplot2::geom_point(ggplot2::aes(x = PC, y = Acumulada),
                            color = "#E41A1C", size = 2) +
        ggplot2::geom_hline(yintercept = resultado$params$pca_threshold %||% 95,
                            linetype = "dashed", color = "gray50") +
        ggplot2::scale_x_continuous(breaks = seq_len(n_show)) +
        ggplot2::labs(title = "Scree Plot - PCA",
                      x = "Componente Principal",
                      y = "Varianza Explicada (%)") +
        ggplot2::theme_minimal(base_size = 12)

      figures[[paste0("Fig", fig_num, "_PCA_Scree")]] <- p
      if (verbose) cat("  Figura", fig_num, ": Scree Plot PCA\n")
    }, error = function(e) {
      if (verbose) cat("  [!] Error en Scree Plot:", conditionMessage(e), "\n")
    })

    # --- Biplot PCA ---
    tryCatch({
      pca_res <- resultado$reduction$pca
      scores_df <- as.data.frame(pca_res$scores_2d)
      colnames(scores_df) <- c("PC1", "PC2")

      # Agregar labels de cluster si existen
      if (!is.null(resultado$clustering)) {
        best_alg <- resultado$evaluation$best_algorithm$algorithm
        if (!is.null(best_alg) && best_alg %in% names(resultado$clustering$results)) {
          scores_df$Cluster <- factor(resultado$clustering$results[[best_alg]]$labels)
        }
      }

      fig_num <- fig_num + 1
      var1 <- round(pca_res$variance_explained[1] * 100, 1)
      var2 <- round(pca_res$variance_explained[2] * 100, 1)

      if ("Cluster" %in% names(scores_df)) {
        p <- ggplot2::ggplot(scores_df, ggplot2::aes(x = PC1, y = PC2, color = Cluster)) +
          ggplot2::geom_point(alpha = 0.6, size = 2) +
          ggplot2::scale_color_manual(values = cluster_colors) +
          ggplot2::stat_ellipse(level = 0.95, linetype = "dashed")
      } else {
        p <- ggplot2::ggplot(scores_df, ggplot2::aes(x = PC1, y = PC2)) +
          ggplot2::geom_point(alpha = 0.6, size = 2, color = "#377EB8")
      }

      p <- p +
        ggplot2::labs(title = "Biplot PCA",
                      x = paste0("PC1 (", var1, "%)"),
                      y = paste0("PC2 (", var2, "%)")) +
        ggplot2::theme_minimal(base_size = 12)

      figures[[paste0("Fig", fig_num, "_PCA_Biplot")]] <- p
      if (verbose) cat("  Figura", fig_num, ": Biplot PCA\n")
    }, error = function(e) {
      if (verbose) cat("  [!] Error en Biplot:", conditionMessage(e), "\n")
    })

    # --- Loadings Plot ---
    tryCatch({
      pca_res <- resultado$reduction$pca
      loadings <- pca_res$loadings[, 1:min(2, ncol(pca_res$loadings)), drop = FALSE]
      df_load <- data.frame(
        Variable = rownames(loadings),
        PC1 = loadings[, 1],
        stringsAsFactors = FALSE
      )
      df_load <- df_load[order(abs(df_load$PC1), decreasing = TRUE), ]
      df_load <- df_load[seq_len(min(15, nrow(df_load))), ]
      df_load$Variable <- factor(df_load$Variable, levels = rev(df_load$Variable))

      fig_num <- fig_num + 1
      p <- ggplot2::ggplot(df_load, ggplot2::aes(x = PC1, y = Variable)) +
        ggplot2::geom_col(fill = ifelse(df_load$PC1 > 0, "#377EB8", "#E41A1C"),
                          alpha = 0.8) +
        ggplot2::geom_vline(xintercept = 0, color = "gray40") +
        ggplot2::labs(title = "PCA Loadings - PC1",
                      x = "Loading", y = NULL) +
        ggplot2::theme_minimal(base_size = 12)

      figures[[paste0("Fig", fig_num, "_PCA_Loadings")]] <- p
      if (verbose) cat("  Figura", fig_num, ": PCA Loadings\n")
    }, error = function(e) {
      if (verbose) cat("  [!] Error en Loadings Plot:", conditionMessage(e), "\n")
    })
  }

  # --- t-SNE Plot ---
  if (!is.null(resultado$reduction$tsne)) {
    tryCatch({
      tsne_df <- as.data.frame(resultado$reduction$tsne$coords)

      if (!is.null(resultado$clustering)) {
        best_alg <- resultado$evaluation$best_algorithm$algorithm
        if (!is.null(best_alg) && best_alg %in% names(resultado$clustering$results)) {
          tsne_df$Cluster <- factor(resultado$clustering$results[[best_alg]]$labels)
        }
      }

      fig_num <- fig_num + 1
      if ("Cluster" %in% names(tsne_df)) {
        p <- ggplot2::ggplot(tsne_df, ggplot2::aes(x = tSNE1, y = tSNE2, color = Cluster)) +
          ggplot2::geom_point(alpha = 0.6, size = 2) +
          ggplot2::scale_color_manual(values = cluster_colors)
      } else {
        p <- ggplot2::ggplot(tsne_df, ggplot2::aes(x = tSNE1, y = tSNE2)) +
          ggplot2::geom_point(alpha = 0.6, size = 2, color = "#377EB8")
      }

      p <- p +
        ggplot2::labs(title = "t-SNE Projection",
                      x = "t-SNE 1", y = "t-SNE 2") +
        ggplot2::theme_minimal(base_size = 12)

      figures[[paste0("Fig", fig_num, "_tSNE")]] <- p
      if (verbose) cat("  Figura", fig_num, ": t-SNE Projection\n")
    }, error = function(e) {
      if (verbose) cat("  [!] Error en t-SNE Plot:", conditionMessage(e), "\n")
    })
  }

  # --- UMAP Plot ---
  if (!is.null(resultado$reduction$umap)) {
    tryCatch({
      umap_df <- as.data.frame(resultado$reduction$umap$coords)

      if (!is.null(resultado$clustering)) {
        best_alg <- resultado$evaluation$best_algorithm$algorithm
        if (!is.null(best_alg) && best_alg %in% names(resultado$clustering$results)) {
          umap_df$Cluster <- factor(resultado$clustering$results[[best_alg]]$labels)
        }
      }

      fig_num <- fig_num + 1
      if ("Cluster" %in% names(umap_df)) {
        p <- ggplot2::ggplot(umap_df, ggplot2::aes(x = UMAP1, y = UMAP2, color = Cluster)) +
          ggplot2::geom_point(alpha = 0.6, size = 2) +
          ggplot2::scale_color_manual(values = cluster_colors)
      } else {
        p <- ggplot2::ggplot(umap_df, ggplot2::aes(x = UMAP1, y = UMAP2)) +
          ggplot2::geom_point(alpha = 0.6, size = 2, color = "#377EB8")
      }

      p <- p +
        ggplot2::labs(title = "UMAP Projection",
                      x = "UMAP 1", y = "UMAP 2") +
        ggplot2::theme_minimal(base_size = 12)

      figures[[paste0("Fig", fig_num, "_UMAP")]] <- p
      if (verbose) cat("  Figura", fig_num, ": UMAP Projection\n")
    }, error = function(e) {
      if (verbose) cat("  [!] Error en UMAP Plot:", conditionMessage(e), "\n")
    })
  }

  # =========================================================================
  # PLOTS DE CLUSTERING
  # =========================================================================

  if (!is.null(resultado$clustering)) {

    # --- Optimal k (Elbow + Silhouette) ---
    if (!is.null(resultado$clustering$k_analysis)) {
      tryCatch({
        k_info <- resultado$clustering$k_analysis

        # Elbow Plot
        fig_num <- fig_num + 1
        df_elbow <- data.frame(k = as.integer(names(k_info$wss)), WSS = k_info$wss)
        p <- ggplot2::ggplot(df_elbow, ggplot2::aes(x = k, y = WSS)) +
          ggplot2::geom_line(color = "#377EB8", linewidth = 1) +
          ggplot2::geom_point(color = "#377EB8", size = 3) +
          ggplot2::geom_vline(xintercept = resultado$clustering$optimal_k,
                              linetype = "dashed", color = "#E41A1C") +
          ggplot2::annotate("text", x = resultado$clustering$optimal_k,
                            y = max(df_elbow$WSS) * 0.95,
                            label = paste("k =", resultado$clustering$optimal_k),
                            color = "#E41A1C", hjust = -0.2) +
          ggplot2::scale_x_continuous(breaks = df_elbow$k) +
          ggplot2::labs(title = "Metodo del Codo (Elbow)",
                        x = "Numero de Clusters (k)",
                        y = "Within-Cluster Sum of Squares") +
          ggplot2::theme_minimal(base_size = 12)

        figures[[paste0("Fig", fig_num, "_Elbow")]] <- p
        if (verbose) cat("  Figura", fig_num, ": Elbow Plot\n")

        # Silhouette por k
        fig_num <- fig_num + 1
        df_sil <- data.frame(
          k = as.integer(names(k_info$silhouette_scores)),
          Silhouette = k_info$silhouette_scores
        )
        p <- ggplot2::ggplot(df_sil, ggplot2::aes(x = k, y = Silhouette)) +
          ggplot2::geom_line(color = "#4DAF4A", linewidth = 1) +
          ggplot2::geom_point(color = "#4DAF4A", size = 3) +
          ggplot2::geom_vline(xintercept = resultado$clustering$optimal_k,
                              linetype = "dashed", color = "#E41A1C") +
          ggplot2::scale_x_continuous(breaks = df_sil$k) +
          ggplot2::labs(title = "Silhouette Score por k",
                        x = "Numero de Clusters (k)",
                        y = "Silhouette Score Promedio") +
          ggplot2::theme_minimal(base_size = 12)

        figures[[paste0("Fig", fig_num, "_Silhouette_k")]] <- p
        if (verbose) cat("  Figura", fig_num, ": Silhouette por k\n")
      }, error = function(e) {
        if (verbose) cat("  [!] Error en plots de k optimo:", conditionMessage(e), "\n")
      })
    }

    # --- Silhouette Plot del mejor modelo ---
    if (!is.null(resultado$evaluation$best_algorithm) &&
        requireNamespace("cluster", quietly = TRUE)) {
      tryCatch({
        best_alg <- resultado$evaluation$best_algorithm$algorithm
        best_labels <- resultado$clustering$results[[best_alg]]$labels
        valid_idx <- best_labels > 0

        if (sum(valid_idx) > 1 && length(unique(best_labels[valid_idx])) >= 2) {
          data_valid <- as.matrix(data_processed)[valid_idx, , drop = FALSE]
          sil_obj <- cluster::silhouette(best_labels[valid_idx], dist(data_valid))

          fig_num <- fig_num + 1
          sil_df <- data.frame(
            obs = seq_len(sum(valid_idx)),
            cluster = factor(sil_obj[, 1]),
            sil_width = sil_obj[, 3]
          )
          sil_df <- sil_df[order(sil_df$cluster, -sil_df$sil_width), ]
          sil_df$order <- seq_len(nrow(sil_df))

          p <- ggplot2::ggplot(sil_df, ggplot2::aes(x = order, y = sil_width, fill = cluster)) +
            ggplot2::geom_col(width = 1) +
            ggplot2::geom_hline(yintercept = mean(sil_df$sil_width),
                                linetype = "dashed", color = "red") +
            ggplot2::scale_fill_manual(values = cluster_colors) +
            ggplot2::coord_flip() +
            ggplot2::labs(title = paste("Silhouette Plot -", best_alg),
                          x = "Observaciones", y = "Silhouette Width",
                          fill = "Cluster") +
            ggplot2::theme_minimal(base_size = 12) +
            ggplot2::theme(axis.text.y = ggplot2::element_blank())

          figures[[paste0("Fig", fig_num, "_Silhouette_Plot")]] <- p
          if (verbose) cat("  Figura", fig_num, ": Silhouette Plot\n")
        }
      }, error = function(e) {
        if (verbose) cat("  [!] Error en Silhouette Plot:", conditionMessage(e), "\n")
      })
    }

    # --- Dendrograma (si hay hierarchical) ---
    if (!is.null(resultado$clustering$results$hierarchical)) {
      tryCatch({
        hc_model <- resultado$clustering$results$hierarchical$model
        k_val <- resultado$clustering$optimal_k

        fig_num <- fig_num + 1

        # Convertir a dendrogram y colorear
        dend <- as.dendrogram(hc_model)

        # Plot base con ggplot usando ggdendro si disponible
        if (requireNamespace("ggdendro", quietly = TRUE)) {
          dend_data <- ggdendro::dendro_data(hc_model)
          p <- ggplot2::ggplot(ggdendro::segment(dend_data)) +
            ggplot2::geom_segment(ggplot2::aes(x = x, y = y, xend = xend, yend = yend),
                                  color = "#377EB8", alpha = 0.7) +
            ggplot2::geom_hline(yintercept = hc_model$height[length(hc_model$height) - k_val + 1],
                                linetype = "dashed", color = "#E41A1C") +
            ggplot2::labs(title = paste("Dendrograma -", resultado$clustering$results$hierarchical$method),
                          x = NULL, y = "Altura") +
            ggplot2::theme_minimal(base_size = 12) +
            ggplot2::theme(axis.text.x = ggplot2::element_blank())
        } else {
          # Fallback: recordar que se puede usar plot()
          p <- ggplot2::ggplot() +
            ggplot2::annotate("text", x = 0.5, y = 0.5,
                              label = "Use plot(resultado$clustering$results$hierarchical$model)\npara ver el dendrograma",
                              size = 4) +
            ggplot2::theme_void()
        }

        figures[[paste0("Fig", fig_num, "_Dendrograma")]] <- p
        if (verbose) cat("  Figura", fig_num, ": Dendrograma\n")
      }, error = function(e) {
        if (verbose) cat("  [!] Error en Dendrograma:", conditionMessage(e), "\n")
      })
    }

    # --- Cluster Comparison (metricas por algoritmo) ---
    if (!is.null(resultado$evaluation$comparison) && nrow(resultado$evaluation$comparison) > 1) {
      tryCatch({
        comp <- resultado$evaluation$comparison

        fig_num <- fig_num + 1
        # Reshape para faceted bar chart
        comp_long <- data.frame(
          algorithm = rep(comp$algorithm, 3),
          metric = c(rep("Silhouette", nrow(comp)),
                     rep("Calinski-Harabasz\n(normalizado)", nrow(comp)),
                     rep("Davies-Bouldin\n(invertido)", nrow(comp))),
          value = c(comp$silhouette,
                    comp$calinski_harabasz / max(comp$calinski_harabasz),
                    1 / comp$davies_bouldin / max(1 / comp$davies_bouldin)),
          stringsAsFactors = FALSE
        )

        p <- ggplot2::ggplot(comp_long, ggplot2::aes(x = algorithm, y = value, fill = algorithm)) +
          ggplot2::geom_col(alpha = 0.8, show.legend = FALSE) +
          ggplot2::facet_wrap(~metric, scales = "free_y") +
          ggplot2::scale_fill_manual(values = cluster_colors) +
          ggplot2::labs(title = "Comparacion de Algoritmos de Clustering",
                        x = NULL, y = "Score") +
          ggplot2::theme_minimal(base_size = 12) +
          ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

        figures[[paste0("Fig", fig_num, "_Cluster_Comparison")]] <- p
        if (verbose) cat("  Figura", fig_num, ": Comparacion de Algoritmos\n")
      }, error = function(e) {
        if (verbose) cat("  [!] Error en Cluster Comparison:", conditionMessage(e), "\n")
      })
    }

    # --- LCA Profile Plot ---
    if (!is.null(resultado$clustering$results$lca)) {
      tryCatch({
        lca_res <- resultado$clustering$results$lca
        probs_list <- lca_res$item_response_probs
        n_classes <- length(lca_res$size)

        # Construir data frame de probabilidades de respuesta por clase
        # Para datos binarios: P(item=max_category | class)
        prof_rows <- list()
        for (item_name in names(probs_list)) {
          prob_matrix <- probs_list[[item_name]]
          # Tomar la probabilidad de la categoria mas alta (ultima columna)
          for (cl in seq_len(n_classes)) {
            prof_rows[[length(prof_rows) + 1]] <- data.frame(
              Item = item_name,
              Class = paste0("Clase ", cl),
              Probability = prob_matrix[cl, ncol(prob_matrix)],
              stringsAsFactors = FALSE
            )
          }
        }
        prof_df <- do.call(rbind, prof_rows)
        prof_df$Item <- factor(prof_df$Item, levels = names(probs_list))

        # Proporciones de clase para leyenda
        class_props <- round(lca_res$size / sum(lca_res$size) * 100, 1)
        class_labels <- paste0("Clase ", seq_len(n_classes), " (", class_props, "%)")
        prof_df$Class <- factor(prof_df$Class,
                                levels = paste0("Clase ", seq_len(n_classes)),
                                labels = class_labels)

        fig_num <- fig_num + 1
        p <- ggplot2::ggplot(prof_df, ggplot2::aes(x = Item, y = Probability,
                                                     color = Class, group = Class)) +
          ggplot2::geom_line(linewidth = 0.8) +
          ggplot2::geom_point(ggplot2::aes(shape = Class), size = 2.5) +
          ggplot2::scale_color_manual(values = cluster_colors[seq_len(n_classes)]) +
          ggplot2::scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
          ggplot2::labs(title = "LCA - Perfil de Clases Latentes",
                        x = NULL, y = "Probabilidad de Respuesta",
                        color = "Clase", shape = "Clase") +
          ggplot2::theme_minimal(base_size = 12) +
          ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                         legend.position = "bottom")

        figures[[paste0("Fig", fig_num, "_LCA_Profiles")]] <- p
        if (verbose) cat("  Figura", fig_num, ": LCA Perfil de Clases\n")
      }, error = function(e) {
        if (verbose) cat("  [!] Error en LCA Profile Plot:", conditionMessage(e), "\n")
      })
    }

    # --- Cluster Sizes ---
    if (!is.null(resultado$evaluation$best_algorithm)) {
      tryCatch({
        best_alg <- resultado$evaluation$best_algorithm$algorithm
        best_labels <- resultado$clustering$results[[best_alg]]$labels
        valid_labels <- best_labels[best_labels > 0]

        fig_num <- fig_num + 1
        df_size <- as.data.frame(table(Cluster = factor(valid_labels)))
        names(df_size)[2] <- "Count"

        p <- ggplot2::ggplot(df_size, ggplot2::aes(x = Cluster, y = Count, fill = Cluster)) +
          ggplot2::geom_col(alpha = 0.8, show.legend = FALSE) +
          ggplot2::geom_text(ggplot2::aes(label = Count), vjust = -0.5) +
          ggplot2::scale_fill_manual(values = cluster_colors) +
          ggplot2::labs(title = paste("Tamanio de Clusters -", best_alg),
                        x = "Cluster", y = "Observaciones") +
          ggplot2::theme_minimal(base_size = 12)

        figures[[paste0("Fig", fig_num, "_Cluster_Sizes")]] <- p
        if (verbose) cat("  Figura", fig_num, ": Tamanio de Clusters\n")
      }, error = function(e) {
        if (verbose) cat("  [!] Error en Cluster Sizes:", conditionMessage(e), "\n")
      })
    }
  }

  # =========================================================================
  # PLOTS DE ANOMALIAS
  # =========================================================================

  if (!is.null(resultado$anomalies) && length(resultado$anomalies$results) > 0) {

    # Score distribution para cada metodo
    for (method_name in names(resultado$anomalies$results)) {
      tryCatch({
        anom <- resultado$anomalies$results[[method_name]]

        fig_num <- fig_num + 1
        df_anom <- data.frame(
          score = anom$scores,
          anomaly = factor(ifelse(anom$is_anomaly, "Anomalia", "Normal"),
                           levels = c("Normal", "Anomalia"))
        )

        p <- ggplot2::ggplot(df_anom, ggplot2::aes(x = score, fill = anomaly)) +
          ggplot2::geom_histogram(bins = 50, alpha = 0.7, position = "identity") +
          ggplot2::geom_vline(xintercept = anom$threshold,
                              linetype = "dashed", color = "#E41A1C") +
          ggplot2::scale_fill_manual(values = c("Normal" = "#377EB8", "Anomalia" = "#E41A1C")) +
          ggplot2::labs(title = paste("Distribucion de Scores -", method_name),
                        x = "Anomaly Score", y = "Frecuencia", fill = NULL) +
          ggplot2::theme_minimal(base_size = 12)

        figures[[paste0("Fig", fig_num, "_Anomaly_", method_name)]] <- p
        if (verbose) cat("  Figura", fig_num, ": Anomaly Scores -", method_name, "\n")
      }, error = function(e) {
        if (verbose) cat("  [!] Error en plot anomalia", method_name, ":", conditionMessage(e), "\n")
      })
    }

    # Anomalias en espacio 2D (PCA o t-SNE)
    if (!is.null(resultado$reduction$pca) || !is.null(resultado$reduction$tsne)) {
      tryCatch({
        # Preferir PCA
        if (!is.null(resultado$reduction$pca)) {
          coords <- resultado$reduction$pca$scores_2d
          x_lab <- "PC1"
          y_lab <- "PC2"
          title_suffix <- "(PCA)"
        } else {
          coords <- resultado$reduction$tsne$coords[, 1:2]
          x_lab <- "t-SNE 1"
          y_lab <- "t-SNE 2"
          title_suffix <- "(t-SNE)"
        }

        # Usar consenso si disponible, sino primer metodo
        if (!is.null(resultado$anomalies$consensus)) {
          is_anom <- resultado$anomalies$consensus$is_anomaly
        } else {
          first_method <- names(resultado$anomalies$results)[1]
          is_anom <- resultado$anomalies$results[[first_method]]$is_anomaly
        }

        fig_num <- fig_num + 1
        df_2d <- data.frame(
          x = coords[, 1],
          y = coords[, 2],
          anomaly = factor(ifelse(is_anom, "Anomalia", "Normal"),
                           levels = c("Normal", "Anomalia"))
        )

        p <- ggplot2::ggplot(df_2d, ggplot2::aes(x = x, y = y, color = anomaly, size = anomaly)) +
          ggplot2::geom_point(alpha = 0.6) +
          ggplot2::scale_color_manual(values = c("Normal" = "#377EB8", "Anomalia" = "#E41A1C")) +
          ggplot2::scale_size_manual(values = c("Normal" = 1.5, "Anomalia" = 3)) +
          ggplot2::labs(title = paste("Anomalias en Espacio 2D", title_suffix),
                        x = x_lab, y = y_lab, color = NULL, size = NULL) +
          ggplot2::theme_minimal(base_size = 12)

        figures[[paste0("Fig", fig_num, "_Anomaly_2D")]] <- p
        if (verbose) cat("  Figura", fig_num, ": Anomalias en 2D\n")
      }, error = function(e) {
        if (verbose) cat("  [!] Error en plot anomalias 2D:", conditionMessage(e), "\n")
      })
    }
  }

  if (verbose) {
    cat("\n  Total de figuras generadas:", length(figures), "\n")
  }

  return(figures)
}


#' Save All Figures of an Unsupervised Analysis
#'
#' @description
#' Saves every `ggplot2` figure stored in an `unsupervisedml` object as a PNG
#' file in `output_dir`. Nothing is written unless a directory is supplied.
#'
#' @param result Object of class \code{unsupervisedml}.
#' @param output_dir Directory where the PNG files are written (required; for
#'   example `tempdir()`). It is created if it does not exist.
#' @param width Width in inches (default: 10).
#' @param height Height in inches (default: 7).
#' @param dpi Resolution in dots per inch (default: 300).
#' @param verbose Logical. If `TRUE`, the output directory and the number of
#'   saved files are reported with [message()].
#'
#' @return Invisibly, a character vector with the paths of the saved files.
#'
#' @examples
#' \donttest{
#' set.seed(1)
#' X <- data.frame(a = c(rnorm(40), rnorm(40, 3)), b = c(rnorm(40), rnorm(40, 3)))
#' res <- unsupervised_ml(X, methods = "clustering", clustering_algorithms = "kmeans",
#'                        k = 2, run_eda = FALSE,
#'                        verbose = FALSE)
#' out <- file.path(tempdir(), "psyml_unsup_figs")
#' files <- save_unsupervised_plots(res, output_dir = out, dpi = 72)
#' unlink(out, recursive = TRUE)
#' }
#' @family unsupervised learning
#' @export
save_unsupervised_plots <- function(result, output_dir,
                                     width = 10, height = 7, dpi = 300,
                                     verbose = TRUE) {
  if (!inherits(result, "unsupervisedml")) {
    stop("'result' must be an object of class 'unsupervisedml'.", call. = FALSE)
  }
  if (missing(output_dir) || is.null(output_dir)) {
    stop("Please provide 'output_dir' (for example, output_dir = tempdir()).",
         call. = FALSE)
  }

  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

  saved <- character(0)
  for (fig_name in names(result$figures)) {
    fig <- result$figures[[fig_name]]
    if (inherits(fig, "gg") || inherits(fig, "ggplot")) {
      file_path <- file.path(output_dir, paste0(fig_name, ".png"))
      ggplot2::ggsave(filename = file_path, plot = fig,
                       width = width, height = height, dpi = dpi)
      saved <- c(saved, file_path)
    }
  }

  if (verbose) {
    message("Figures saved in: ", output_dir)
    message("Total: ", length(saved), " files")
  }
  invisible(saved)
}
