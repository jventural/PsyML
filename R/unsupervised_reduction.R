
# =============================================================================
# Reduccion de Dimensionalidad para Aprendizaje No Supervisado
# =============================================================================

#' @noRd
.unsupervised_reduction <- function(data_processed,
                                     reduction_methods,
                                     n_components,
                                     pca_threshold,
                                     tsne_perplexity,
                                     umap_n_neighbors,
                                     umap_min_dist,
                                     seed,
                                     verbose) {

  if (verbose) {
    .print_section(3, "Reduccion de Dimensionalidad")
  }

  result <- list()

  # =========================================================================
  # 3.1 PCA
  # =========================================================================
  if ("pca" %in% reduction_methods) {
    if (verbose) .print_subsection(3, 1, "Analisis de Componentes Principales (PCA)")

    tryCatch({
      pca_fit <- prcomp(data_processed, center = FALSE, scale. = FALSE)

      # Varianza explicada
      var_explained <- pca_fit$sdev^2 / sum(pca_fit$sdev^2)
      cum_var <- cumsum(var_explained)

      # Numero de componentes para threshold
      n_comp_threshold <- which(cum_var >= pca_threshold)[1]
      if (is.na(n_comp_threshold)) n_comp_threshold <- length(var_explained)

      # Loadings
      loadings <- pca_fit$rotation

      # Scores
      scores <- pca_fit$x

      result$pca <- list(
        fit = pca_fit,
        variance_explained = var_explained,
        cumulative_variance = cum_var,
        n_components = n_comp_threshold,
        loadings = loadings,
        scores = scores,
        scores_2d = scores[, 1:min(2, ncol(scores)), drop = FALSE]
      )

      if (verbose) {
        cat("  Componentes totales:", length(var_explained), "\n")
        cat("  Componentes para ", pca_threshold * 100, "% varianza: ",
            n_comp_threshold, "\n", sep = "")
        cat("\n  Varianza explicada por componente:\n")
        n_show <- min(10, length(var_explained))
        for (i in seq_len(n_show)) {
          bar_len <- round(var_explained[i] * 40)
          bar <- paste(rep("|", bar_len), collapse = "")
          cat(sprintf("    PC%d: %6.2f%% (acum: %6.2f%%) %s\n",
                      i, var_explained[i] * 100, cum_var[i] * 100, bar))
        }

        # Top loadings para PC1 y PC2
        cat("\n  Top 5 loadings PC1:\n")
        pc1_order <- order(abs(loadings[, 1]), decreasing = TRUE)
        for (i in pc1_order[1:min(5, length(pc1_order))]) {
          cat(sprintf("    %s: %.4f\n", rownames(loadings)[i], loadings[i, 1]))
        }

        if (ncol(loadings) >= 2) {
          cat("\n  Top 5 loadings PC2:\n")
          pc2_order <- order(abs(loadings[, 2]), decreasing = TRUE)
          for (i in pc2_order[1:min(5, length(pc2_order))]) {
            cat(sprintf("    %s: %.4f\n", rownames(loadings)[i], loadings[i, 2]))
          }
        }
      }
    }, error = function(e) {
      if (verbose) cat("  [ERROR] PCA fallo:", conditionMessage(e), "\n")
    })
  }

  # =========================================================================
  # 3.2 t-SNE
  # =========================================================================
  if ("tsne" %in% reduction_methods) {
    if (verbose) .print_subsection(3, 2, "t-SNE (t-distributed Stochastic Neighbor Embedding)")

    if (!requireNamespace("Rtsne", quietly = TRUE)) {
      if (verbose) cat("  [!] Package 'Rtsne' not installed. Install package 'Rtsne' to use this option.\n")
    } else {
      tryCatch({
        # Ajustar perplexity si n es pequenio
        n_obs <- nrow(data_processed)
        effective_perplexity <- min(tsne_perplexity, floor((n_obs - 1) / 3))
        if (effective_perplexity < 2) effective_perplexity <- 2

        if (effective_perplexity != tsne_perplexity && verbose) {
          cat("  [!] Perplexity ajustada a", effective_perplexity,
              "(n =", n_obs, ")\n")
        }

        # Remover duplicados exactos
        dup_idx <- duplicated(data_processed)
        data_tsne <- data_processed
        if (any(dup_idx)) {
          # Agregar ruido minimo a duplicados
          noise <- matrix(rnorm(sum(dup_idx) * ncol(data_tsne), sd = 1e-10),
                          ncol = ncol(data_tsne))
          data_tsne[dup_idx, ] <- data_tsne[dup_idx, ] + noise
          if (verbose) cat("  [!] Se agrego ruido a", sum(dup_idx),
                           "filas duplicadas.\n")
        }

        if (!is.null(seed)) set.seed(seed)
        tsne_fit <- Rtsne::Rtsne(
          as.matrix(data_tsne),
          dims = n_components,
          perplexity = effective_perplexity,
          verbose = FALSE,
          max_iter = 1000,
          check_duplicates = FALSE
        )

        tsne_coords <- tsne_fit$Y
        colnames(tsne_coords) <- paste0("tSNE", seq_len(n_components))

        result$tsne <- list(
          coords = tsne_coords,
          perplexity = effective_perplexity,
          iterations = 1000,
          kl_divergence = tsne_fit$itercosts[length(tsne_fit$itercosts)]
        )

        if (verbose) {
          cat("  Dimensiones de salida:", n_components, "\n")
          cat("  Perplexity:", effective_perplexity, "\n")
          cat("  KL divergence final:", round(result$tsne$kl_divergence, 4), "\n")
        }
      }, error = function(e) {
        if (verbose) cat("  [ERROR] t-SNE fallo:", conditionMessage(e), "\n")
      })
    }
  }

  # =========================================================================
  # 3.3 UMAP
  # =========================================================================
  if ("umap" %in% reduction_methods) {
    if (verbose) .print_subsection(3, 3, "UMAP (Uniform Manifold Approximation and Projection)")

    if (!requireNamespace("umap", quietly = TRUE)) {
      if (verbose) cat("  [!] Package 'umap' not installed. Install package 'umap' to use this option.\n")
    } else {
      tryCatch({
        n_obs <- nrow(data_processed)
        effective_neighbors <- min(umap_n_neighbors, n_obs - 1)

        umap_config <- umap::umap.defaults
        umap_config$n_components <- n_components
        umap_config$n_neighbors <- effective_neighbors
        umap_config$min_dist <- umap_min_dist
        umap_config$random_state <- seed

        if (!is.null(seed)) set.seed(seed)
        umap_fit <- umap::umap(as.matrix(data_processed), config = umap_config)

        umap_coords <- umap_fit$layout
        colnames(umap_coords) <- paste0("UMAP", seq_len(n_components))

        result$umap <- list(
          fit = umap_fit,
          coords = umap_coords,
          n_neighbors = effective_neighbors,
          min_dist = umap_min_dist
        )

        if (verbose) {
          cat("  Dimensiones de salida:", n_components, "\n")
          cat("  n_neighbors:", effective_neighbors, "\n")
          cat("  min_dist:", umap_min_dist, "\n")
        }
      }, error = function(e) {
        if (verbose) cat("  [ERROR] UMAP fallo:", conditionMessage(e), "\n")
      })
    }
  }

  return(result)
}
