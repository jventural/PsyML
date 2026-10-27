
# =============================================================================
# Funciones auxiliares para Aprendizaje No Supervisado
# =============================================================================

#' Validate the Inputs of unsupervised_ml
#' @noRd
.validate_unsupervised_inputs <- function(data, methods, exclude_cols) {

  # Validar data

  if (!is.data.frame(data) && !is.matrix(data)) {
    stop("'data' must be a data.frame or a matrix.")
  }

  if (nrow(data) < 10) {
    stop("At least 10 observations are required.")
  }

  if (ncol(data) < 2) {
    stop("At least 2 variables are required.")
  }

  # Validar methods
  valid_methods <- c("clustering", "reduction", "anomaly")
  invalid <- setdiff(methods, valid_methods)
  if (length(invalid) > 0) {
    stop("Invalid methods: ", paste(invalid, collapse = ", "),
         "\nOptions: ", paste(valid_methods, collapse = ", "))
  }

  # Validar exclude_cols
  if (!is.null(exclude_cols)) {
    missing_cols <- setdiff(exclude_cols, names(data))
    if (length(missing_cols) > 0) {
      warning("Columns not found (they will be ignored): ",
              paste(missing_cols, collapse = ", "))
    }
  }

  # Verificar que quedan variables numericas
  if (!is.null(exclude_cols)) {
    remaining <- names(data)[!names(data) %in% exclude_cols]
  } else {
    remaining <- names(data)
  }

  n_numeric <- sum(sapply(data[remaining], is.numeric))
  if (n_numeric < 2) {
    stop("At least 2 numeric variables are required after excluding columns.")
  }

  invisible(TRUE)
}


#' Cluster Profiles
#'
#' Computes the mean of every numeric variable within each cluster found by
#' [unsupervised_ml()], together with the overall (global) mean. Noise points
#' (label 0, as produced by DBSCAN) are excluded.
#'
#' @param result An object of class `unsupervisedml`, as returned by
#'   [unsupervised_ml()].
#' @param algorithm Name of the clustering algorithm whose labels are used
#'   (e.g. `"kmeans"`). If NULL (default), the best algorithm of the
#'   evaluation (or the first one available) is used.
#' @param original_data Optional data frame with the same rows as the data
#'   used in the analysis, used to profile variables on their original
#'   scale or variables not used in the clustering. If NULL (default), the
#'   preprocessed data before normalization are used.
#' @param verbose Logical. If TRUE (default), the cluster sizes and means are
#'   printed to the console.
#'
#' @return Invisibly, a data frame with one row per cluster plus a final row
#'   `"Global"`: the column `Cluster` holds the cluster label and the
#'   remaining columns hold the mean of each numeric variable.
#'
#' @examples
#' set.seed(1)
#' X <- data.frame(a = c(rnorm(40), rnorm(40, 3)),
#'                 b = c(rnorm(40), rnorm(40, 3)),
#'                 c = rnorm(80))
#' res <- unsupervised_ml(X, methods = "clustering",
#'                        clustering_algorithms = c("kmeans", "hierarchical"),
#'                        k = 2, run_eda = FALSE, seed = 1, verbose = FALSE)
#' prof <- cluster_profile(res, algorithm = "kmeans", verbose = FALSE)
#' prof
#'
#' @family unsupervised learning
#' @export
cluster_profile <- function(result, algorithm = NULL, original_data = NULL,
                            verbose = TRUE) {

  if (!inherits(result, "unsupervisedml")) {
    stop("The object must be of class 'unsupervisedml'.")
  }

  if (is.null(result$clustering)) {
    stop("No clustering results are available.")
  }

  # Seleccionar algoritmo
  if (is.null(algorithm)) {
    if (!is.null(result$evaluation$best_algorithm)) {
      algorithm <- result$evaluation$best_algorithm$algorithm
    } else {
      algorithm <- names(result$clustering$results)[1]
    }
  }

  if (!algorithm %in% names(result$clustering$results)) {
    stop("Algorithm '", algorithm, "' not found. Available: ",
         paste(names(result$clustering$results), collapse = ", "))
  }

  labels <- result$clustering$results[[algorithm]]$labels

  # Datos a perfilar
  if (!is.null(original_data)) {
    data_to_use <- original_data
  } else {
    data_to_use <- result$preprocessed$data_before_norm
  }

  # Solo variables numericas
  numeric_cols <- names(data_to_use)[sapply(data_to_use, is.numeric)]
  data_num <- data_to_use[, numeric_cols, drop = FALSE]

  # Filtrar ruido (DBSCAN)
  valid_idx <- labels > 0
  data_num <- data_num[valid_idx, , drop = FALSE]
  labels_valid <- labels[valid_idx]

  # Medias por cluster
  profiles <- aggregate(. ~ Cluster, data = cbind(Cluster = labels_valid, data_num), mean)

  # Agregar fila global
  global_means <- c(Cluster = 0, colMeans(data_num, na.rm = TRUE))
  profiles <- rbind(profiles, global_means)
  profiles$Cluster[nrow(profiles)] <- "Global"

  # Tamanios
  sizes <- c(table(labels_valid), Global = sum(valid_idx))

  if (verbose) {
    cat("\n")
    cat(.line("="), "\n")
    cat(" Perfil de Clusters -", algorithm, "\n")
    cat(.line("="), "\n\n")

    cat("Tamanio de clusters:\n")
    for (i in seq_along(sizes)) {
      cat(sprintf("  Cluster %s: %d (%.1f%%)\n",
                  names(sizes)[i], sizes[i],
                  sizes[i] / sum(valid_idx) * 100))
    }
    cat("\n")

    cat("Medias por cluster:\n")
    print(profiles, row.names = FALSE, digits = 3)
  }

  invisible(profiles)
}


#' Assign New Observations to Clusters
#'
#' Assigns new observations to the clusters of an `unsupervisedml` object.
#' The new data are normalized with the parameters of the original analysis
#' and assigned to the nearest centroid (k-means and hierarchical), with the
#' fitted mixture model (GMM, requires 'mclust') or with the posterior class
#' probabilities (LCA, requires 'poLCA').
#'
#' @param object An object of class `unsupervisedml`, as returned by
#'   [unsupervised_ml()].
#' @param newdata A data frame with the new observations; it must contain the
#'   variables used in the analysis (`names(object$preprocessed$data_clean)`),
#'   on their original scale.
#' @param algorithm Algorithm used for the assignment: `"kmeans"`,
#'   `"hierarchical"`, `"gmm"` or `"lca"`. If NULL (default), the best
#'   algorithm of the evaluation (or the first one available) is used.
#' @param ... Further arguments (not used).
#'
#' @return An integer vector with the cluster label of each row of
#'   `newdata`.
#'
#' @examples
#' set.seed(1)
#' X <- data.frame(a = c(rnorm(40), rnorm(40, 3)),
#'                 b = c(rnorm(40), rnorm(40, 3)),
#'                 c = rnorm(80))
#' res <- unsupervised_ml(X, methods = "clustering",
#'                        clustering_algorithms = c("kmeans", "hierarchical"),
#'                        k = 2, run_eda = FALSE, seed = 1, verbose = FALSE)
#' new_obs <- data.frame(a = c(0, 3), b = c(0, 3), c = c(0, 0))
#' predict(res, new_obs, algorithm = "kmeans")
#'
#' @family unsupervised learning
#' @export
predict.unsupervisedml <- function(object, newdata, algorithm = NULL, ...) {

  if (is.null(object$clustering)) {
    stop("No clustering results are available.")
  }

  if (is.null(algorithm)) {
    if (!is.null(object$evaluation$best_algorithm)) {
      algorithm <- object$evaluation$best_algorithm$algorithm
    } else {
      algorithm <- names(object$clustering$results)[1]
    }
  }

  # Preprocesar newdata
  prep_params <- object$preprocessed

  # Seleccionar columnas usadas
  used_cols <- names(prep_params$data_clean)
  if (!all(used_cols %in% names(newdata))) {
    missing <- setdiff(used_cols, names(newdata))
    stop("Variables missing in newdata: ", paste(missing, collapse = ", "))
  }

  newdata_clean <- newdata[, used_cols, drop = FALSE]

  # Normalizar
  if (!is.null(prep_params$norm_params)) {
    for (v in names(prep_params$norm_params)) {
      if (v %in% names(newdata_clean)) {
        params <- prep_params$norm_params[[v]]
        if (params$method == "zscore") {
          newdata_clean[[v]] <- (newdata_clean[[v]] - params$center) / params$scale
        } else if (params$method == "minmax") {
          range_val <- params$max - params$min
          if (range_val > 0) {
            newdata_clean[[v]] <- (newdata_clean[[v]] - params$min) / range_val
          }
        }
      }
    }
  }

  new_mat <- as.matrix(newdata_clean)

  # Predecir segun algoritmo
  if (algorithm == "kmeans") {
    centers <- object$clustering$results$kmeans$centers
    # Asignar al centroide mas cercano
    dists <- apply(new_mat, 1, function(x) {
      apply(centers, 1, function(c) sum((x - c)^2))
    })
    if (is.matrix(dists)) {
      labels <- apply(dists, 2, which.min)
    } else {
      labels <- which.min(dists)
    }

  } else if (algorithm == "gmm") {
    if (!is.null(object$clustering$results$gmm$model)) {
      if (!requireNamespace("mclust", quietly = TRUE)) {
        stop("Package 'mclust' is required to predict GMM clusters.", call. = FALSE)
      }
      pred <- predict(object$clustering$results$gmm$model, newdata = new_mat)
      labels <- pred$classification
    } else {
      stop("No GMM model is available for prediction.")
    }

  } else if (algorithm == "hierarchical") {
    # Asignar al centroide mas cercano del clustering original
    orig_labels <- object$clustering$results$hierarchical$labels
    orig_data <- as.matrix(object$preprocessed$data_clean)
    centers <- aggregate(orig_data, by = list(Cluster = orig_labels), FUN = mean)
    centers_mat <- as.matrix(centers[, -1])

    dists <- apply(new_mat, 1, function(x) {
      apply(centers_mat, 1, function(c) sum((x - c)^2))
    })
    if (is.matrix(dists)) {
      labels <- centers$Cluster[apply(dists, 2, which.min)]
    } else {
      labels <- centers$Cluster[which.min(dists)]
    }

  } else if (algorithm == "lca") {
    if (!requireNamespace("poLCA", quietly = TRUE)) {
      stop("Package 'poLCA' is required for LCA prediction.")
    }
    lca_model <- object$clustering$results$lca$model
    lca_vars <- object$clustering$results$lca$lca_variables

    if (!all(lca_vars %in% names(newdata))) {
      missing <- setdiff(lca_vars, names(newdata))
      stop("LCA variables missing in newdata: ", paste(missing, collapse = ", "))
    }

    newdata_lca <- as.data.frame(lapply(newdata[, lca_vars, drop = FALSE], function(x) {
      if (is.factor(x) || is.character(x)) {
        as.integer(as.factor(x))
      } else {
        as.integer(as.factor(x))
      }
    }))

    post <- poLCA::poLCA.posterior(lca_model, y = newdata_lca)
    labels <- apply(post, 1, which.max)

  } else {
    stop("Prediction is not supported for algorithm: ", algorithm)
  }

  return(labels)
}
