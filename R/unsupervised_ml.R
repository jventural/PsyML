
# =============================================================================
# unsupervised_ml: Funcion Principal para Aprendizaje No Supervisado
# =============================================================================

#' Automated Unsupervised Learning Pipeline
#'
#' @description
#' Runs a complete unsupervised learning pipeline: exploratory data analysis,
#' preprocessing, dimensionality reduction, clustering, cluster evaluation,
#' anomaly detection and visualization.
#'
#' @details
#' Only numeric columns are analysed. Several algorithms rely on suggested
#' packages and are skipped (with a note in the verbose output) when they are
#' not installed: 'dbscan' (DBSCAN and LOF), 'mclust' (GMM), 'poLCA' (LCA),
#' 'Rtsne' (t-SNE), 'umap' (UMAP), 'isotree' (isolation forest), 'cluster'
#' (silhouette, gap statistic and Gower distance), 'ggdendro' (dendrogram)
#' and 'VIM' (kNN imputation).
#'
#' @param data A data frame with the data.
#' @param methods Character vector with the analyses to run: any of
#'   `"clustering"`, `"reduction"` and `"anomaly"` (default: all three).
#' @param clustering_algorithms Clustering algorithms: any of `"kmeans"`,
#'   `"hierarchical"`, `"dbscan"`, `"gmm"` and `"lca"` (default: all except
#'   `"lca"`). LCA (latent class analysis) requires categorical or binary
#'   data.
#' @param k Number of clusters. If NULL (default), the optimal number is
#'   searched automatically.
#' @param k_range Range of k values evaluated when `k = NULL` (default:
#'   `2:10`).
#' @param distance Distance metric: `"euclidean"` (default), `"manhattan"`
#'   or `"gower"`.
#' @param hclust_method Linkage method for hierarchical clustering:
#'   `"ward.D2"` (default), `"complete"`, `"average"` or `"single"`.
#' @param dbscan_eps Epsilon radius for DBSCAN. If NULL (default), it is
#'   computed automatically.
#' @param dbscan_minPts Minimum number of points for DBSCAN (default: 5).
#' @param gmm_type Covariance model for GMM, passed to 'mclust' (default:
#'   `"VVV"`).
#' @param lca_nclass Number of latent classes for LCA. If NULL (default), the
#'   number of classes is searched automatically.
#' @param lca_nrep Number of LCA repetitions to avoid local maxima (default:
#'   10).
#' @param reduction_methods Dimensionality reduction methods: any of `"pca"`,
#'   `"tsne"` and `"umap"` (default: all three).
#' @param n_components Number of target dimensions for t-SNE and UMAP
#'   (default: 2).
#' @param pca_threshold Cumulative proportion of variance retained by PCA
#'   (default: 0.95).
#' @param tsne_perplexity Perplexity for t-SNE (default: 30).
#' @param umap_n_neighbors Number of neighbours for UMAP (default: 15).
#' @param umap_min_dist Minimum distance for UMAP (default: 0.1).
#' @param anomaly_methods Anomaly detection methods: any of
#'   `"isolation_forest"`, `"lof"` and `"mahalanobis"` (default: all three).
#' @param contamination Expected proportion of anomalies (default: 0.05).
#' @param lof_k Number of neighbours for LOF (default: 20).
#' @param exclude_cols Character vector of columns to exclude from the
#'   analysis (IDs, text, etc.).
#' @param impute Logical; impute missing values (default: TRUE).
#' @param impute_method Imputation method: `"median"` (default), `"knn"` or
#'   `"mean"`.
#' @param normalize Logical; normalize the numeric variables (default: TRUE).
#' @param normalize_method Normalization method: `"zscore"` (default) or
#'   `"minmax"`.
#' @param treat_outliers Logical; winsorize outliers before the analysis
#'   (default: FALSE).
#' @param outlier_percentile Percentile used for winsorization (default:
#'   0.05).
#' @param remove_high_cor Logical; remove highly correlated variables
#'   (default: TRUE).
#' @param cor_threshold Correlation threshold used by `remove_high_cor`
#'   (default: 0.90).
#' @param run_eda Logical; run the exploratory data analysis (default: TRUE).
#' @param seed Optional integer seed. If NULL (default) the random number
#'   generator state is not modified.
#' @param verbose Logical; print progress to the console (default: TRUE).
#'
#' @return An object of class `unsupervisedml` (a list) with the components:
#' \describe{
#'   \item{call}{The matched call of the internal pipeline.}
#'   \item{params}{List with the main analysis parameters.}
#'   \item{eda}{Exploratory analysis: structure, missing values, descriptive
#'     statistics, outliers, correlations and normality (only if
#'     `run_eda = TRUE`).}
#'   \item{preprocessed}{List with the preprocessed data (`data_clean`), the
#'     data before normalization (`data_before_norm`), the normalization
#'     parameters (`norm_params`), the removed columns (`removed_cols`) and the
#'     number of variables before and after preprocessing.}
#'   \item{reduction}{Dimensionality reduction results (`pca`, `tsne`,
#'     `umap`).}
#'   \item{clustering}{Clustering results: `optimal_k`, `k_analysis` and
#'     `results`, a list with one element per algorithm containing the cluster
#'     `labels` and the fitted model.}
#'   \item{evaluation}{Cluster evaluation: `comparison` (data frame with the
#'     silhouette, Calinski-Harabasz and Davies-Bouldin indices of each
#'     algorithm) and `best_algorithm`.}
#'   \item{anomalies}{Anomaly detection results per method (`results`) and
#'     the majority `consensus`.}
#'   \item{figures}{Named list of 'ggplot2' plots.}
#'   \item{elapsed_time}{Execution time in seconds.}
#'   \item{data_original}{The input data.}
#'   \item{verbose_text, verbose_lines}{Captured console output (only if
#'     `verbose = TRUE`).}
#' }
#'
#' @examples
#' # Two well separated groups
#' set.seed(1)
#' X <- data.frame(a = c(rnorm(40), rnorm(40, 3)),
#'                 b = c(rnorm(40), rnorm(40, 3)),
#'                 c = rnorm(80))
#' res <- unsupervised_ml(X,
#'                        clustering_algorithms = c("kmeans", "hierarchical"),
#'                        k_range = 2:4, reduction_methods = "pca",
#'                        anomaly_methods = "mahalanobis",
#'                        run_eda = FALSE, seed = 1, verbose = FALSE)
#' res$evaluation$comparison
#'
#' @family unsupervised learning
#' @export
unsupervised_ml <- function(data,
                            methods = c("clustering", "reduction", "anomaly"),
                            # Clustering
                            clustering_algorithms = c("kmeans", "hierarchical", "dbscan", "gmm"),
                            k = NULL,
                            k_range = 2:10,
                            distance = c("euclidean", "manhattan", "gower"),
                            hclust_method = c("ward.D2", "complete", "average", "single"),
                            dbscan_eps = NULL,
                            dbscan_minPts = 5,
                            gmm_type = "VVV",
                            lca_nclass = NULL,
                            lca_nrep = 10,
                            # Reduction
                            reduction_methods = c("pca", "tsne", "umap"),
                            n_components = 2,
                            pca_threshold = 0.95,
                            tsne_perplexity = 30,
                            umap_n_neighbors = 15,
                            umap_min_dist = 0.1,
                            # Anomaly
                            anomaly_methods = c("isolation_forest", "lof", "mahalanobis"),
                            contamination = 0.05,
                            lof_k = 20,
                            # Preprocessing
                            exclude_cols = NULL,
                            impute = TRUE,
                            impute_method = c("median", "knn", "mean"),
                            normalize = TRUE,
                            normalize_method = c("zscore", "minmax"),
                            treat_outliers = FALSE,
                            outlier_percentile = 0.05,
                            remove_high_cor = TRUE,
                            cor_threshold = 0.90,
                            # General
                            run_eda = TRUE,
                            seed = NULL,
                            verbose = TRUE) {

  # =========================================================================
  # CAPTURA INTERNA DEL VERBOSE
  # =========================================================================
  if (verbose) {
    temp_file <- tempfile(fileext = ".txt")
    sink(temp_file, split = TRUE)

    tryCatch({
      resultado <- .unsupervised_ml_internal(
        data = data, methods = methods,
        clustering_algorithms = clustering_algorithms,
        k = k, k_range = k_range, distance = distance,
        hclust_method = hclust_method,
        dbscan_eps = dbscan_eps, dbscan_minPts = dbscan_minPts,
        gmm_type = gmm_type,
        lca_nclass = lca_nclass, lca_nrep = lca_nrep,
        reduction_methods = reduction_methods,
        n_components = n_components, pca_threshold = pca_threshold,
        tsne_perplexity = tsne_perplexity,
        umap_n_neighbors = umap_n_neighbors, umap_min_dist = umap_min_dist,
        anomaly_methods = anomaly_methods,
        contamination = contamination, lof_k = lof_k,
        exclude_cols = exclude_cols,
        impute = impute, impute_method = impute_method,
        normalize = normalize, normalize_method = normalize_method,
        treat_outliers = treat_outliers, outlier_percentile = outlier_percentile,
        remove_high_cor = remove_high_cor, cor_threshold = cor_threshold,
        run_eda = run_eda, seed = seed, verbose = TRUE
      )
    }, finally = {
      sink()
    })

    if (file.exists(temp_file)) {
      verbose_output <- readLines(temp_file, warn = FALSE)
      unlink(temp_file)
    } else {
      verbose_output <- character(0)
    }

    resultado$verbose_text <- paste(verbose_output, collapse = "\n")
    resultado$verbose_lines <- verbose_output

  } else {
    resultado <- .unsupervised_ml_internal(
      data = data, methods = methods,
      clustering_algorithms = clustering_algorithms,
      k = k, k_range = k_range, distance = distance,
      hclust_method = hclust_method,
      dbscan_eps = dbscan_eps, dbscan_minPts = dbscan_minPts,
      gmm_type = gmm_type,
      lca_nclass = lca_nclass, lca_nrep = lca_nrep,
      reduction_methods = reduction_methods,
      n_components = n_components, pca_threshold = pca_threshold,
      tsne_perplexity = tsne_perplexity,
      umap_n_neighbors = umap_n_neighbors, umap_min_dist = umap_min_dist,
      anomaly_methods = anomaly_methods,
      contamination = contamination, lof_k = lof_k,
      exclude_cols = exclude_cols,
      impute = impute, impute_method = impute_method,
      normalize = normalize, normalize_method = normalize_method,
      treat_outliers = treat_outliers, outlier_percentile = outlier_percentile,
      remove_high_cor = remove_high_cor, cor_threshold = cor_threshold,
      run_eda = run_eda, seed = seed, verbose = FALSE
    )
  }

  return(resultado)
}


# =============================================================================
# FUNCION INTERNA PRINCIPAL
# =============================================================================

# Runs the internal pipeline with package-version and convergence warnings
# from the modelling back-ends suppressed locally (instead of changing
# options(warn)).
#' @noRd
.unsupervised_ml_internal <- function(...) {
  suppressWarnings(.unsupervised_ml_internal_impl(...))
}

#' @noRd
.unsupervised_ml_internal_impl <- function(data,
                                       methods,
                                       clustering_algorithms,
                                       k, k_range, distance, hclust_method,
                                       dbscan_eps, dbscan_minPts, gmm_type,
                                       lca_nclass, lca_nrep,
                                       reduction_methods,
                                       n_components, pca_threshold,
                                       tsne_perplexity,
                                       umap_n_neighbors, umap_min_dist,
                                       anomaly_methods,
                                       contamination, lof_k,
                                       exclude_cols,
                                       impute, impute_method,
                                       normalize, normalize_method,
                                       treat_outliers, outlier_percentile,
                                       remove_high_cor, cor_threshold,
                                       run_eda, seed, verbose) {


  if (!is.null(seed)) set.seed(seed)
  distance <- match.arg(distance, c("euclidean", "manhattan", "gower"))
  hclust_method <- match.arg(hclust_method, c("ward.D2", "complete", "average", "single"))
  impute_method <- match.arg(impute_method, c("median", "knn", "mean"))
  normalize_method <- match.arg(normalize_method, c("zscore", "minmax"))
  start_time <- Sys.time()

  # Validar inputs
  .validate_unsupervised_inputs(data, methods, exclude_cols)

  resultado <- list()
  resultado$call <- match.call()
  resultado$params <- list(
    methods = methods,
    clustering_algorithms = clustering_algorithms,
    k = k, k_range = k_range, distance = distance,
    hclust_method = hclust_method,
    reduction_methods = reduction_methods,
    n_components = n_components,
    anomaly_methods = anomaly_methods,
    contamination = contamination,
    seed = seed
  )

  if (verbose) {
    .msg_header("unsupervised_ml - Aprendizaje No Supervisado Automatizado")
    cat("Observaciones:", nrow(data), "\n")
    cat("Variables totales:", ncol(data), "\n")
    if (!is.null(exclude_cols)) {
      cat("Variables excluidas:", paste(exclude_cols, collapse = ", "), "\n")
    }
    cat("Analisis solicitados:", paste(methods, collapse = ", "), "\n")
    cat("Semilla:", seed, "\n")
  }

  # =========================================================================
  # SECCION 1: EDA
  # =========================================================================
  if (run_eda) {
    eda_result <- .unsupervised_eda(data, exclude_cols, verbose)
    resultado$eda <- eda_result
  }

  # =========================================================================
  # SECCION 2: PREPROCESAMIENTO
  # =========================================================================
  prep_result <- .unsupervised_preprocess(
    data = data, exclude_cols = exclude_cols,
    impute = impute, impute_method = impute_method,
    normalize = normalize, normalize_method = normalize_method,
    treat_outliers = treat_outliers, outlier_percentile = outlier_percentile,
    remove_high_cor = remove_high_cor, cor_threshold = cor_threshold,
    verbose = verbose
  )
  resultado$preprocessed <- prep_result
  data_processed <- prep_result$data_clean

  # =========================================================================
  # SECCION 3: REDUCCION DE DIMENSIONALIDAD
  # =========================================================================
  if ("reduction" %in% methods) {
    reduction_result <- .unsupervised_reduction(
      data_processed = data_processed,
      reduction_methods = reduction_methods,
      n_components = n_components,
      pca_threshold = pca_threshold,
      tsne_perplexity = tsne_perplexity,
      umap_n_neighbors = umap_n_neighbors,
      umap_min_dist = umap_min_dist,
      seed = seed,
      verbose = verbose
    )
    resultado$reduction <- reduction_result
  }

  # =========================================================================
  # SECCION 4: CLUSTERING
  # =========================================================================
  if ("clustering" %in% methods) {
    clustering_result <- .unsupervised_clustering(
      data_processed = data_processed,
      data_original = data,
      algorithms = clustering_algorithms,
      k = k, k_range = k_range,
      distance = distance,
      hclust_method = hclust_method,
      dbscan_eps = dbscan_eps, dbscan_minPts = dbscan_minPts,
      gmm_type = gmm_type,
      lca_nclass = lca_nclass, lca_nrep = lca_nrep,
      exclude_cols = exclude_cols,
      seed = seed,
      verbose = verbose
    )
    resultado$clustering <- clustering_result
  }

  # =========================================================================
  # SECCION 5: EVALUACION DE CLUSTERS
  # =========================================================================
  if ("clustering" %in% methods) {
    eval_result <- .unsupervised_evaluate(
      data_processed = data_processed,
      clustering_result = resultado$clustering,
      verbose = verbose
    )
    resultado$evaluation <- eval_result
  }

  # =========================================================================
  # SECCION 6: DETECCION DE ANOMALIAS
  # =========================================================================
  if ("anomaly" %in% methods) {
    anomaly_result <- .unsupervised_anomaly(
      data_processed = data_processed,
      anomaly_methods = anomaly_methods,
      contamination = contamination,
      lof_k = lof_k,
      seed = seed,
      verbose = verbose
    )
    resultado$anomalies <- anomaly_result
  }

  # =========================================================================
  # SECCION 7: VISUALIZACION
  # =========================================================================
  figures <- .unsupervised_plots(
    data_processed = data_processed,
    resultado = resultado,
    methods = methods,
    verbose = verbose
  )
  resultado$figures <- figures

  # =========================================================================
  # TIEMPO TOTAL
  # =========================================================================
  elapsed <- round(difftime(Sys.time(), start_time, units = "secs"), 1)
  resultado$elapsed_time <- elapsed

  if (verbose) {
    cat("\n")
    cat(.line("="), "\n")
    cat("ANALISIS COMPLETADO en ", elapsed, " segundos\n", sep = "")
    cat(.line("="), "\n")
  }

  # Datos originales para referencia
  resultado$data_original <- data

  class(resultado) <- "unsupervisedml"
  return(resultado)
}


# =============================================================================
# METODOS S3
# =============================================================================

#' Print an unsupervisedml Object
#'
#' Prints a short overview of the analysis: optimal k, clustering algorithms
#' and best algorithm, dimensionality reduction and anomaly counts.
#'
#' @param x An object of class `unsupervisedml`, as returned by
#'   [unsupervised_ml()].
#' @param ... Further arguments (not used).
#' @return Invisibly returns `x`.
#' @examples
#' set.seed(1)
#' X <- data.frame(a = c(rnorm(40), rnorm(40, 3)),
#'                 b = c(rnorm(40), rnorm(40, 3)),
#'                 c = rnorm(80))
#' res <- unsupervised_ml(X,
#'                        clustering_algorithms = c("kmeans", "hierarchical"),
#'                        k_range = 2:4, reduction_methods = "pca",
#'                        anomaly_methods = "mahalanobis",
#'                        run_eda = FALSE, seed = 1, verbose = FALSE)
#' print(res)
#' @family unsupervised learning
#' @export
print.unsupervisedml <- function(x, ...) {
  cat("\n")
  cat(.line("="), "\n")
  cat(" unsupervised_ml - Resultados\n")
  cat(.line("="), "\n\n")

  cat("Observaciones:", nrow(x$data_original), "\n")
  cat("Variables procesadas:", ncol(x$preprocessed$data_clean), "\n")
  cat("Tiempo:", x$elapsed_time, "segundos\n\n")

  # Clustering
  if (!is.null(x$clustering)) {
    cat("--- Clustering ---\n")
    cat("k optimo:", x$clustering$optimal_k, "\n")
    cat("Algoritmos:", paste(names(x$clustering$results), collapse = ", "), "\n")

    if (!is.null(x$evaluation)) {
      best <- x$evaluation$best_algorithm
      cat("Mejor algoritmo:", best$algorithm,
          "(Silhouette =", round(best$silhouette, 3), ")\n")
    }

    # Info especifica LCA
    if (!is.null(x$clustering$results$lca)) {
      lca <- x$clustering$results$lca
      cat("LCA: ", length(lca$size), " clases (BIC=", round(lca$bic, 1),
          ", Entropy=", round(lca$entropy, 3), ")\n", sep = "")
    }
    cat("\n")
  }

  # Reduccion
  if (!is.null(x$reduction)) {
    cat("--- Reduccion de Dimensionalidad ---\n")
    if (!is.null(x$reduction$pca)) {
      n_comp <- x$reduction$pca$n_components
      var_exp <- round(x$reduction$pca$cumulative_variance[n_comp] * 100, 1)
      cat("PCA:", n_comp, "componentes (", var_exp, "% varianza)\n")
    }
    if (!is.null(x$reduction$tsne)) cat("t-SNE: completado\n")
    if (!is.null(x$reduction$umap)) cat("UMAP: completado\n")
    cat("\n")
  }

  # Anomalias
  if (!is.null(x$anomalies)) {
    cat("--- Deteccion de Anomalias ---\n")
    for (method_name in names(x$anomalies$results)) {
      n_anom <- sum(x$anomalies$results[[method_name]]$is_anomaly)
      cat(method_name, ":", n_anom, "anomalias detectadas\n")
    }
    if (!is.null(x$anomalies$consensus)) {
      n_cons <- sum(x$anomalies$consensus$is_anomaly)
      cat("Consenso:", n_cons, "anomalias (detectadas por mayoria)\n")
    }
    cat("\n")
  }

  cat("Figuras generadas:", length(x$figures), "\n")
  invisible(x)
}


#' Summarize an unsupervisedml Object
#'
#' Prints a detailed summary: data dimensions, comparison of the clustering
#' algorithms, best algorithm and PCA explained variance.
#'
#' @param object An object of class `unsupervisedml`, as returned by
#'   [unsupervised_ml()].
#' @param ... Further arguments (not used).
#' @return Invisibly returns `object`.
#' @examples
#' set.seed(1)
#' X <- data.frame(a = c(rnorm(40), rnorm(40, 3)),
#'                 b = c(rnorm(40), rnorm(40, 3)),
#'                 c = rnorm(80))
#' res <- unsupervised_ml(X,
#'                        clustering_algorithms = c("kmeans", "hierarchical"),
#'                        k_range = 2:4, reduction_methods = "pca",
#'                        anomaly_methods = "mahalanobis",
#'                        run_eda = FALSE, seed = 1, verbose = FALSE)
#' summary(res)
#' @family unsupervised learning
#' @export
summary.unsupervisedml <- function(object, ...) {
  x <- object

  cat("\n")
  cat(.line("="), "\n")
  cat(" unsupervised_ml - Resumen Detallado\n")
  cat(.line("="), "\n\n")

  # Datos
  cat("DATOS\n")
  cat(.line("-", 40), "\n")
  cat("  Observaciones:", nrow(x$data_original), "\n")
  cat("  Variables originales:", ncol(x$data_original), "\n")
  cat("  Variables procesadas:", ncol(x$preprocessed$data_clean), "\n")
  if (!is.null(x$preprocessed$removed_cols) && length(x$preprocessed$removed_cols) > 0) {
    cat("  Variables removidas:", paste(x$preprocessed$removed_cols, collapse = ", "), "\n")
  }
  cat("\n")

  # Evaluacion de clusters
  if (!is.null(x$evaluation)) {
    cat("COMPARACION DE ALGORITMOS DE CLUSTERING\n")
    cat(.line("-", 40), "\n")
    comp <- x$evaluation$comparison
    if (!is.null(comp) && nrow(comp) > 0) {
      print(comp, row.names = FALSE)
    }
    cat("\n")
    best <- x$evaluation$best_algorithm
    cat("  >> Mejor algoritmo:", best$algorithm, "\n")
    cat("     Silhouette:", round(best$silhouette, 4), "\n")
    cat("     Calinski-Harabasz:", round(best$calinski_harabasz, 2), "\n")
    cat("     Davies-Bouldin:", round(best$davies_bouldin, 4), "\n")
    cat("\n")
  }

  # PCA loadings
  if (!is.null(x$reduction$pca)) {
    cat("PCA - VARIANZA EXPLICADA\n")
    cat(.line("-", 40), "\n")
    pca_res <- x$reduction$pca
    n_show <- min(10, length(pca_res$variance_explained))
    for (i in seq_len(n_show)) {
      cat(sprintf("  PC%d: %.1f%% (acum: %.1f%%)\n",
                  i,
                  pca_res$variance_explained[i] * 100,
                  pca_res$cumulative_variance[i] * 100))
    }
    cat("\n")
  }

  invisible(x)
}


#' Plot an unsupervisedml Object
#'
#' Displays the figures stored in an `unsupervisedml` object.
#'
#' @param x An object of class `unsupervisedml`, as returned by
#'   [unsupervised_ml()].
#' @param which Figures to display: a numeric vector of indices, a character
#'   vector of figure names (see `names(x$figures)`), or `"all"`. If NULL
#'   (default), the catalogue of available figures is listed with
#'   [message()].
#' @param ... Further arguments (not used).
#' @return Invisibly returns `x`.
#' @examples
#' set.seed(1)
#' X <- data.frame(a = c(rnorm(40), rnorm(40, 3)),
#'                 b = c(rnorm(40), rnorm(40, 3)),
#'                 c = rnorm(80))
#' res <- unsupervised_ml(X,
#'                        clustering_algorithms = c("kmeans", "hierarchical"),
#'                        k_range = 2:4, reduction_methods = "pca",
#'                        anomaly_methods = "mahalanobis",
#'                        run_eda = FALSE, seed = 1, verbose = FALSE)
#' plot(res)
#' plot(res, which = 1)
#' @family unsupervised learning
#' @export
plot.unsupervisedml <- function(x, which = NULL, ...) {
  if (is.null(x$figures) || length(x$figures) == 0) {
    message("No figures available.")
    return(invisible(x))
  }

  if (is.null(which)) {
    # Mostrar catalogo
    message("Available figures:")
    for (i in seq_along(x$figures)) {
      message(sprintf("  [%d] %s", i, names(x$figures)[i]))
    }
    message("Use plot(x, which = 1) to show one figure, ",
            "or plot(x, which = \"all\") to show all of them.")
  } else if (identical(which, "all")) {
    for (fig in x$figures) {
      if (inherits(fig, "gg") || inherits(fig, "ggplot")) {
        print(fig)
      }
    }
  } else if (is.numeric(which)) {
    for (i in which) {
      if (i >= 1 && i <= length(x$figures)) {
        fig <- x$figures[[i]]
        if (inherits(fig, "gg") || inherits(fig, "ggplot")) {
          print(fig)
        }
      } else {
        message("Index ", i, " out of range (1-", length(x$figures), ")")
      }
    }
  } else if (is.character(which)) {
    for (nm in which) {
      fig <- x$figures[[nm]]
      if (inherits(fig, "gg") || inherits(fig, "ggplot")) {
        print(fig)
      } else {
        message("Figure '", nm, "' not found. Available: ",
                paste(names(x$figures), collapse = ", "))
      }
    }
  }

  invisible(x)
}
