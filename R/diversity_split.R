#' Diversity-Based Train/Test Split for True Generalization
#'
#' Creates train/test splits where the test set is genuinely DIFFERENT from
#' the training set, not just a random subset of the same distribution.
#' This enables a fairer evaluation of model generalization.
#'
#' @param data Data frame to split.
#' @param test_prop Proportion for test set (default 0.20).
#' @param method Method for creating diverse split:
#'   \itemize{
#'     \item \code{"cluster"}: Assigns entire clusters to test (most different)
#'     \item \code{"dissimilarity"}: Selects most dissimilar observations for test
#'     \item \code{"boundary"}: Selects observations at distribution boundaries
#'     \item \code{"hybrid"}: Combines clustering with boundary selection
#'   }
#' @param n_clusters Number of clusters for the cluster-based methods. If NULL
#'   (default) it is set to \code{round(sqrt(n / 2))}, bounded between 3 and 10.
#' @param exclude_cols Character vector of columns to exclude from the distance
#'   calculations (e.g., ID columns).
#' @param target Name of the target variable. It is excluded from the distance
#'   calculations.
#' @param stratify Logical (default TRUE). Reserved for keeping class
#'   proportions of \code{target}; it is passed to the cluster-based methods but
#'   class proportions are not currently enforced.
#' @param diversity_strength Number between 0 and 1 (default 0.5) controlling
#'   how aggressively diversity is maximized. Higher values give a more
#'   different test set, but may be too extreme.
#' @param seed Optional integer seed. If NULL (default) the random number
#'   generator state is not modified.
#' @param verbose Logical (default TRUE). If TRUE, prints a summary of the split.
#'
#' @return A list of class \code{c("diversity_split", "list")} containing:
#'   \itemize{
#'     \item train: Training data frame.
#'     \item test: Test data frame.
#'     \item train_idx: Sorted row indices of the training observations.
#'     \item test_idx: Sorted row indices of the test observations.
#'     \item diversity_metrics: List with \code{mean_distance_ratio} (mean
#'       distance of test cases to the training centroid divided by the mean
#'       within-training distance), \code{ks_statistic} (average two-sample
#'       Kolmogorov-Smirnov statistic across variables), \code{centroid_distance}
#'       (Euclidean distance between train and test centroids),
#'       \code{random_baseline} (centroid distance of one random split of the
#'       same size) and \code{diversity_ratio} (\code{centroid_distance} divided
#'       by \code{random_baseline}). Higher values mean more different sets.
#'     \item cluster_info: Data frame with cluster sizes and test membership
#'       (methods \code{"cluster"} and \code{"hybrid"}), or NULL.
#'     \item settings: List with \code{n_total}, \code{n_train}, \code{n_test},
#'       \code{method}, \code{n_clusters}, \code{diversity_strength} and \code{seed}.
#'   }
#'   If fewer than two usable numeric columns are available, a warning is
#'   issued and a plain list with \code{train}, \code{test}, \code{train_idx}
#'   and \code{test_idx} from a random split is returned.
#'
#' @details
#' The key insight is that random sampling from a homogeneous distribution
#' creates train and test sets that are essentially "twins" - they share
#' the same patterns, making test performance an optimistic estimate of
#' true generalization.
#'
#' This function creates genuinely different partitions by:
#' \itemize{
#'   \item \strong{Cluster method}: Groups similar observations, then assigns
#'     entire clusters to test. The test set contains "types" of observations
#'     not well-represented in training.
#'   \item \strong{Dissimilarity method}: Selects observations that are most
#'     different from the data centroid, ensuring test covers edge cases.
#'   \item \strong{Boundary method}: Selects observations at the boundaries
#'     of the multivariate distribution (extreme combinations of variables).
#'   \item \strong{Hybrid method}: Combines clustering with dissimilarity
#'     selection within clusters.
#' }
#'
#' @examples
#' set.seed(1)
#' survey_data <- data.frame(
#'   x1 = rnorm(120), x2 = rnorm(120), x3 = rnorm(120),
#'   depression = factor(sample(c("yes", "no"), 120, replace = TRUE))
#' )
#'
#' # Default (hybrid) method
#' result <- diversity_split(survey_data, target = "depression",
#'                           test_prop = 0.20, seed = 1)
#'
#' # Cluster-based split (entire clusters go to test)
#' result <- diversity_split(
#'   data = survey_data,
#'   target = "depression",
#'   method = "cluster",
#'   n_clusters = 5,
#'   test_prop = 0.20,
#'   seed = 1,
#'   verbose = FALSE
#' )
#'
#' # Check how different train and test are
#' result$diversity_metrics
#'
#' @family study design
#' @export
diversity_split <- function(data,
                            test_prop = 0.20,
                            method = "hybrid",
                            n_clusters = NULL,
                            exclude_cols = NULL,
                            target = NULL,
                            stratify = TRUE,
                            diversity_strength = 0.5,
                            seed = NULL,
                            verbose = TRUE) {

  if (!is.null(seed)) set.seed(seed)

  # ---- Validations ----

if (!is.data.frame(data)) {
    stop("'data' must be a data frame")
  }

  n_total <- nrow(data)

  if (n_total < 20) {
    stop("Need at least 20 observations for diversity split")
  }

  if (test_prop <= 0 || test_prop >= 1) {
    stop("'test_prop' must be between 0 and 1")
  }

  if (!method %in% c("cluster", "dissimilarity", "boundary", "hybrid")) {
    stop("'method' must be one of: cluster, dissimilarity, boundary, hybrid")
  }

  if (diversity_strength < 0 || diversity_strength > 1) {
    stop("'diversity_strength' must be between 0 and 1")
  }

  n_test <- floor(n_total * test_prop)

  # ---- Prepare data for distance calculations ----
  # Identify columns to use
  cols_to_exclude <- c(exclude_cols)
  if (!is.null(target)) {
    cols_to_exclude <- c(cols_to_exclude, target)
  }

  # Get numeric columns for distance calculation
  data_for_dist <- data[, setdiff(names(data), cols_to_exclude), drop = FALSE]

  # Convert factors to numeric (dummy coding simplified)
  data_numeric <- data_for_dist
  for (col in names(data_numeric)) {
    if (is.factor(data_numeric[[col]]) || is.character(data_numeric[[col]])) {
      data_numeric[[col]] <- as.numeric(as.factor(data_numeric[[col]]))
    }
  }

  # Remove columns with zero variance
  variances <- sapply(data_numeric, var, na.rm = TRUE)
  data_numeric <- data_numeric[, variances > 0, drop = FALSE]

  if (ncol(data_numeric) < 2) {
    warning("Too few numeric columns. Falling back to random split.")
    test_idx <- sample(1:n_total, n_test)
    train_idx <- setdiff(1:n_total, test_idx)
    return(list(
      train = data[train_idx, ],
      test = data[test_idx, ],
      train_idx = train_idx,
      test_idx = test_idx
    ))
  }

  # Handle missing values
  # Row positions (not row names, which may be non-numeric)
  complete_rows <- complete.cases(data_numeric)
  valid_rows <- which(complete_rows)
  data_numeric <- data_numeric[complete_rows, , drop = FALSE]

  if (length(valid_rows) < n_total * 0.5) {
    warning("Many missing values. Using available complete cases.")
  }

  # Scale data for distance calculations
  data_scaled <- scale(data_numeric)

  # ---- Auto-select number of clusters ----
  if (is.null(n_clusters)) {
    # Heuristic: sqrt(n/2) clusters, minimum 3, maximum 10
    n_clusters <- max(3, min(10, round(sqrt(n_total / 2))))
  }

  # ---- Apply selected method ----
  if (method == "cluster") {
    result <- .cluster_split(data_scaled, valid_rows, n_test, n_clusters,
                             target, data, stratify, diversity_strength)
  } else if (method == "dissimilarity") {
    result <- .dissimilarity_split(data_scaled, valid_rows, n_test,
                                   diversity_strength)
  } else if (method == "boundary") {
    result <- .boundary_split(data_scaled, valid_rows, n_test,
                              diversity_strength)
  } else if (method == "hybrid") {
    result <- .hybrid_split(data_scaled, valid_rows, n_test, n_clusters,
                            target, data, stratify, diversity_strength)
  }

  test_idx <- result$test_idx
  train_idx <- setdiff(1:n_total, test_idx)

  # ---- Calculate diversity metrics ----
  diversity_metrics <- .calculate_diversity_metrics(
    data_scaled, train_idx, test_idx, valid_rows
  )

  # ---- Build output ----
  output <- list(
    train = data[train_idx, , drop = FALSE],
    test = data[test_idx, , drop = FALSE],
    train_idx = sort(train_idx),
    test_idx = sort(test_idx),
    diversity_metrics = diversity_metrics,
    cluster_info = result$cluster_info,
    settings = list(
      n_total = n_total,
      n_train = length(train_idx),
      n_test = length(test_idx),
      method = method,
      n_clusters = n_clusters,
      diversity_strength = diversity_strength,
      seed = seed
    )
  )

  class(output) <- c("diversity_split", "list")

  # ---- Verbose output ----
  if (verbose) {
    cat("\n")
    cat("===== Diversity-Based Train/Test Split =====\n")
    cat("\n")
    cat("Method:", method, "\n")
    cat("Total observations:", n_total, "\n")
    cat("Training set:", length(train_idx),
        sprintf("(%.1f%%)", length(train_idx) / n_total * 100), "\n")
    cat("Test set:", length(test_idx),
        sprintf("(%.1f%%)", length(test_idx) / n_total * 100), "\n")
    cat("\n")
    cat("Diversity Metrics (higher = more different):\n")
    cat(sprintf("  Mean distance ratio:    %.3f\n",
                diversity_metrics$mean_distance_ratio))
    cat(sprintf("  KS statistic (avg):     %.3f\n",
                diversity_metrics$ks_statistic))
    cat(sprintf("  Centroid distance:      %.3f\n",
                diversity_metrics$centroid_distance))
    cat("\n")

    # Compare with random split baseline
    if (!is.null(diversity_metrics$random_baseline)) {
      cat("Comparison with random split:\n")
      cat(sprintf("  This split is %.1fx more diverse\n",
                  diversity_metrics$diversity_ratio))
    }
    cat("\n")
  }

  return(output)
}


#' Cluster-based split: Assign entire clusters to test
#' @noRd
.cluster_split <- function(data_scaled, valid_rows, n_test, n_clusters,
                           target, original_data, stratify, diversity_strength) {

  # Perform clustering
  km <- kmeans(data_scaled, centers = n_clusters, nstart = 25, iter.max = 100)
  clusters <- km$cluster

  # Calculate cluster sizes
  cluster_sizes <- table(clusters)
  cluster_order <- order(cluster_sizes)  # Smallest to largest

  # Calculate cluster "uniqueness" (average distance from global centroid)
  global_centroid <- colMeans(data_scaled)
  cluster_uniqueness <- sapply(1:n_clusters, function(k) {
    cluster_data <- data_scaled[clusters == k, , drop = FALSE]
    cluster_centroid <- colMeans(cluster_data)
    sqrt(sum((cluster_centroid - global_centroid)^2))
  })

  # Combine size and uniqueness for selection
  # Prefer smaller, more unique clusters for test
  selection_score <- rank(cluster_sizes) * (1 - diversity_strength) +
    rank(-cluster_uniqueness) * diversity_strength

  cluster_priority <- order(selection_score)

  # Select clusters for test until we have enough observations
  test_idx <- c()
  clusters_in_test <- c()

  for (k in cluster_priority) {
    cluster_rows <- valid_rows[clusters == k]

    if (length(test_idx) + length(cluster_rows) <= n_test * 1.5) {
      test_idx <- c(test_idx, cluster_rows)
      clusters_in_test <- c(clusters_in_test, k)
    }

    if (length(test_idx) >= n_test) break
  }

  # If we have too many, sample down
  if (length(test_idx) > n_test) {
    test_idx <- sample(test_idx, n_test)
  }

  # If we don't have enough, add from remaining clusters
  if (length(test_idx) < n_test) {
    remaining <- setdiff(valid_rows, test_idx)
    additional <- sample(remaining, n_test - length(test_idx))
    test_idx <- c(test_idx, additional)
  }

  cluster_info <- data.frame(
    cluster = 1:n_clusters,
    size = as.numeric(cluster_sizes),
    uniqueness = cluster_uniqueness,
    in_test = 1:n_clusters %in% clusters_in_test
  )

  list(test_idx = test_idx, cluster_info = cluster_info)
}


#' Dissimilarity-based split: Select most dissimilar observations
#' @noRd
.dissimilarity_split <- function(data_scaled, valid_rows, n_test,
                                 diversity_strength) {

  # Calculate distance from global centroid for each observation
  global_centroid <- colMeans(data_scaled)
  distances_to_centroid <- apply(data_scaled, 1, function(x) {
    sqrt(sum((x - global_centroid)^2))
  })

  # Also calculate local density (average distance to k nearest neighbors)
  # Low density = isolated observation = more unique
  k_neighbors <- min(10, nrow(data_scaled) - 1)

  dist_matrix <- as.matrix(dist(data_scaled))
  local_density <- apply(dist_matrix, 1, function(d) {
    mean(sort(d)[2:(k_neighbors + 1)])  # Exclude self (distance 0)
  })

  # Combine: prefer far from centroid AND isolated
  combined_score <- rank(distances_to_centroid) * diversity_strength +
    rank(local_density) * (1 - diversity_strength)

  # Select top scoring observations for test
  # But not all from extreme - use probability sampling
  probs <- combined_score / sum(combined_score)
  probs <- probs^(1 + diversity_strength)  # Sharpen distribution
  probs <- probs / sum(probs)

  selected_idx <- sample(1:length(valid_rows), n_test, prob = probs, replace = FALSE)
  test_idx <- valid_rows[selected_idx]

  list(test_idx = test_idx, cluster_info = NULL)
}


#' Boundary-based split: Select observations at distribution boundaries
#' @noRd
.boundary_split <- function(data_scaled, valid_rows, n_test,
                            diversity_strength) {

  # Use PCA to find main axes of variation
  pca <- prcomp(data_scaled, center = TRUE, scale. = FALSE)
  n_components <- min(5, ncol(data_scaled))
  pca_scores <- pca$x[, 1:n_components, drop = FALSE]

  # For each PC, identify observations at the extremes
  boundary_score <- rep(0, nrow(pca_scores))

  for (pc in 1:n_components) {
    scores <- pca_scores[, pc]
    # Distance from median (captures both tails)
    boundary_score <- boundary_score +
      abs(scores - median(scores)) * pca$sdev[pc]
  }

  # Also add Mahalanobis distance for multivariate outliers
  tryCatch({
    mah_dist <- mahalanobis(data_scaled,
                            colMeans(data_scaled),
                            cov(data_scaled))
    boundary_score <- boundary_score + scale(mah_dist)[, 1]
  }, error = function(e) {
    # If covariance is singular, skip Mahalanobis
  })

  # Sample with probability proportional to boundary score
  probs <- boundary_score - min(boundary_score) + 0.1
  probs <- probs^(1 + diversity_strength)
  probs <- probs / sum(probs)

  selected_idx <- sample(1:length(valid_rows), n_test, prob = probs, replace = FALSE)
  test_idx <- valid_rows[selected_idx]

  list(test_idx = test_idx, cluster_info = NULL)
}


#' Hybrid split: Clustering + dissimilarity within clusters
#' @noRd
.hybrid_split <- function(data_scaled, valid_rows, n_test, n_clusters,
                          target, original_data, stratify, diversity_strength) {

  # Step 1: Cluster the data
km <- kmeans(data_scaled, centers = n_clusters, nstart = 25, iter.max = 100)
  clusters <- km$cluster

  # Step 2: Within each cluster, identify the most "boundary" observations
  test_idx <- c()
  samples_per_cluster <- ceiling(n_test / n_clusters)

  for (k in 1:n_clusters) {
    cluster_mask <- clusters == k
    cluster_data <- data_scaled[cluster_mask, , drop = FALSE]
    cluster_rows <- valid_rows[cluster_mask]

    if (length(cluster_rows) == 0) next

    n_from_cluster <- min(samples_per_cluster, length(cluster_rows))

    if (n_from_cluster <= 2) {
      # Too small, take all
      selected <- cluster_rows
    } else {
      # Calculate distance from cluster centroid
      cluster_centroid <- km$centers[k, ]
      dist_to_center <- apply(cluster_data, 1, function(x) {
        sqrt(sum((x - cluster_centroid)^2))
      })

      # Select mix of central and boundary observations based on diversity_strength
      # High diversity = more boundary, low diversity = more representative
      n_boundary <- round(n_from_cluster * diversity_strength)
      n_central <- n_from_cluster - n_boundary

      order_by_dist <- order(dist_to_center, decreasing = TRUE)

      if (n_boundary > 0) {
        boundary_idx <- order_by_dist[1:min(n_boundary, length(order_by_dist))]
      } else {
        boundary_idx <- c()
      }

      if (n_central > 0) {
        central_candidates <- setdiff(1:length(cluster_rows), boundary_idx)
        if (length(central_candidates) > 0) {
          central_idx <- sample(central_candidates,
                                min(n_central, length(central_candidates)))
        } else {
          central_idx <- c()
        }
      } else {
        central_idx <- c()
      }

      selected <- cluster_rows[c(boundary_idx, central_idx)]
    }

    test_idx <- c(test_idx, selected)
  }

  # Adjust to exact size
  if (length(test_idx) > n_test) {
    test_idx <- sample(test_idx, n_test)
  } else if (length(test_idx) < n_test) {
    remaining <- setdiff(valid_rows, test_idx)
    if (length(remaining) > 0) {
      additional <- sample(remaining, min(n_test - length(test_idx), length(remaining)))
      test_idx <- c(test_idx, additional)
    }
  }

  # Cluster info
  cluster_info <- data.frame(
    cluster = 1:n_clusters,
    size = as.numeric(table(clusters)),
    samples_in_test = sapply(1:n_clusters, function(k) {
      sum(test_idx %in% valid_rows[clusters == k])
    })
  )

  list(test_idx = test_idx, cluster_info = cluster_info)
}


#' Calculate diversity metrics between train and test
#' @noRd
.calculate_diversity_metrics <- function(data_scaled, train_idx, test_idx, valid_rows) {

  # Map indices to scaled data rows
  train_mask <- valid_rows %in% train_idx
  test_mask <- valid_rows %in% test_idx

  train_data <- data_scaled[train_mask, , drop = FALSE]
  test_data <- data_scaled[test_mask, , drop = FALSE]

  if (nrow(train_data) == 0 || nrow(test_data) == 0) {
    return(list(
      mean_distance_ratio = NA,
      ks_statistic = NA,
      centroid_distance = NA
    ))
  }

  # 1. Centroid distance
  train_centroid <- colMeans(train_data)
  test_centroid <- colMeans(test_data)
  centroid_distance <- sqrt(sum((train_centroid - test_centroid)^2))

  # 2. Average KS statistic across variables
  ks_stats <- sapply(1:ncol(data_scaled), function(j) {
    tryCatch({
      ks.test(train_data[, j], test_data[, j])$statistic
    }, error = function(e) NA)
  })
  ks_statistic <- mean(ks_stats, na.rm = TRUE)

  # 3. Mean distance ratio
  # Compare: avg distance test-to-train vs avg distance within-train
  # Higher = test is more different from train

  # Sample for computational efficiency
  n_sample <- min(100, nrow(train_data), nrow(test_data))
  train_sample <- train_data[sample(nrow(train_data), n_sample), , drop = FALSE]
  test_sample <- test_data[sample(nrow(test_data), min(n_sample, nrow(test_data))), , drop = FALSE]

  # Distance from test to train centroid
  dist_test_to_train <- mean(apply(test_sample, 1, function(x) {
    sqrt(sum((x - train_centroid)^2))
  }))

  # Distance from train to train centroid (within-train spread)
  dist_within_train <- mean(apply(train_sample, 1, function(x) {
    sqrt(sum((x - train_centroid)^2))
  }))

  mean_distance_ratio <- dist_test_to_train / (dist_within_train + 0.001)

  # 4. Calculate baseline from random split for comparison
  random_test_idx <- sample(valid_rows, length(test_idx))
  random_test_mask <- valid_rows %in% random_test_idx
  random_train_mask <- !random_test_mask

  random_train_centroid <- colMeans(data_scaled[random_train_mask, , drop = FALSE])
  random_test_centroid <- colMeans(data_scaled[random_test_mask, , drop = FALSE])
  random_centroid_distance <- sqrt(sum((random_train_centroid - random_test_centroid)^2))

  diversity_ratio <- centroid_distance / (random_centroid_distance + 0.001)

  list(
    mean_distance_ratio = mean_distance_ratio,
    ks_statistic = ks_statistic,
    centroid_distance = centroid_distance,
    random_baseline = random_centroid_distance,
    diversity_ratio = diversity_ratio
  )
}


#' Print a Diversity-Based Split
#'
#' Prints the sizes of the training and test sets and the diversity metrics.
#'
#' @param x A \code{diversity_split} object returned by [diversity_split()].
#' @param ... Additional arguments (ignored).
#' @return Invisibly returns \code{x}.
#' @examples
#' res <- diversity_split(iris, target = "Species", method = "dissimilarity",
#'                        seed = 1, verbose = FALSE)
#' print(res)
#' @export
print.diversity_split <- function(x, ...) {
  cat("\n")
  cat("===== Diversity-Based Train/Test Split =====\n")
  cat("\n")
  cat("Method:", x$settings$method, "\n")
  cat("Total observations:", x$settings$n_total, "\n")
  cat("Training set:", x$settings$n_train,
      sprintf("(%.1f%%)", x$settings$n_train / x$settings$n_total * 100), "\n")
  cat("Test set:", x$settings$n_test,
      sprintf("(%.1f%%)", x$settings$n_test / x$settings$n_total * 100), "\n")
  cat("\n")
  cat("Diversity Metrics:\n")
  cat(sprintf("  Mean distance ratio: %.3f\n", x$diversity_metrics$mean_distance_ratio))
  cat(sprintf("  KS statistic (avg):  %.3f\n", x$diversity_metrics$ks_statistic))
  cat(sprintf("  Centroid distance:   %.3f\n", x$diversity_metrics$centroid_distance))
  if (!is.null(x$diversity_metrics$diversity_ratio)) {
    cat(sprintf("  vs Random split:     %.1fx more diverse\n",
                x$diversity_metrics$diversity_ratio))
  }
  cat("\n")
  invisible(x)
}
