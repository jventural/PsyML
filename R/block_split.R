#' Block-Based Train/Test Split for Enhanced Generalization
#'
#' Splits data into train and test sets by sampling from sequential blocks,
#' ensuring the test set represents different segments of the data distribution.
#' This approach helps evaluate model generalization across different "contexts"
#' or time periods, reducing the risk of overfitting to a specific data segment.
#'
#' @param data Data frame to split.
#' @param order_col Column name (character) or index used to order the data
#'   (e.g., date, response order, ID). If NULL, uses row order.
#' @param num_blocks Number of sequential blocks to divide the data into (default 5).
#' @param test_prop Proportion of data to use for test set (default 0.20).
#'   Alternatively, use \code{test_n} to specify exact number.
#' @param test_n Exact number of observations for test set. Overrides \code{test_prop}.
#' @param stratify Column name for stratified sampling within blocks (e.g., outcome variable).
#'   Ensures proportional class representation in both train and test sets.
#' @param balance_blocks Logical. If TRUE, samples equal amounts from each block.
#'   If FALSE, samples proportionally to block size (default TRUE).
#' @param seed Optional integer seed. If NULL (default) the random number
#'   generator state is not modified.
#'
#' @return A list of class \code{c("block_split", "list")} containing:
#'   \itemize{
#'     \item train: Training data frame (rows of \code{data} in their original order).
#'     \item test: Test data frame (rows of \code{data} in their original order).
#'     \item train_idx: Sorted row indices of the training observations in \code{data}.
#'     \item test_idx: Sorted row indices of the test observations in \code{data}.
#'     \item block_info: Data frame with one row per block and columns
#'       \code{block}, \code{start_row}, \code{end_row} (positions after ordering),
#'       \code{block_size} and \code{test_sampled} (test cases drawn from the block).
#'     \item settings: List with the configuration used and the realized sizes
#'       (\code{n_total}, \code{n_train}, \code{n_test}, \code{num_blocks},
#'       \code{test_prop_actual}, \code{order_col}, \code{stratify},
#'       \code{balance_blocks}, \code{seed}).
#'   }
#'
#' @details
#' The function addresses the concern that traditional random splits may not
#' adequately test generalization, since train and test come from the same
#' homogeneous distribution. By sampling from different sequential blocks,
#' the test set captures variability across different "periods" or segments.
#'
#' \strong{When to use:}
#' \itemize{
#'   \item Data collected over time (surveys, longitudinal studies)
#'   \item Data with natural ordering (response sequence, ID order)
#'   \item When concerned about temporal or contextual generalization
#' }
#'
#' \strong{Stratification:}
#' When \code{stratify} is specified, the function ensures that within each block,
#' samples are drawn proportionally from each class. This prevents class imbalance
#' issues in train or test sets.
#'
#' @examples
#' # Simulated survey data collected over 100 days
#' set.seed(1)
#' survey_data <- data.frame(
#'   response_date = as.Date("2025-01-01") + sample(0:99, 150, replace = TRUE),
#'   x1 = rnorm(150),
#'   outcome = factor(sample(c("yes", "no"), 150, replace = TRUE))
#' )
#'
#' # Basic usage with row order
#' result <- block_split(survey_data, num_blocks = 5, test_prop = 0.20, seed = 42)
#' nrow(result$train)
#' nrow(result$test)
#'
#' # With date ordering and stratification
#' result <- block_split(
#'   data = survey_data,
#'   order_col = "response_date",
#'   num_blocks = 5,
#'   test_prop = 0.20,
#'   stratify = "outcome",
#'   seed = 42
#' )
#' result
#'
#' # View block information
#' result$block_info
#'
#' @references
#' Ventura-Leon, J. (2026). Block-based sampling for machine learning validation.
#'
#' @family study design
#' @export
block_split <- function(data,
                        order_col = NULL,
                        num_blocks = 5,
                        test_prop = 0.20,
                        test_n = NULL,
                        stratify = NULL,
                        balance_blocks = TRUE,
                        seed = NULL) {

  if (!is.null(seed)) set.seed(seed)


  # ---- Validations ----
  if (!is.data.frame(data)) {
    stop("'data' must be a data frame")
  }

  n_total <- nrow(data)
  data_in <- data

  if (n_total < num_blocks * 2) {
    stop("Not enough observations. Need at least ", num_blocks * 2,
         " rows for ", num_blocks, " blocks")
  }

  if (!is.null(order_col)) {
    if (is.character(order_col) && !order_col %in% names(data)) {
      stop("Column '", order_col, "' not found in data")
    }
    if (is.numeric(order_col) && (order_col < 1 || order_col > ncol(data))) {
      stop("Column index ", order_col, " out of range")
    }
  }

  if (!is.null(stratify)) {
    if (!stratify %in% names(data)) {
      stop("Stratification column '", stratify, "' not found in data")
    }
  }

  if (num_blocks < 2) {
    stop("'num_blocks' must be at least 2")
  }

  if (test_prop <= 0 || test_prop >= 1) {
    stop("'test_prop' must be between 0 and 1 (exclusive)")
  }


  # ---- Determine test set size ----
  if (!is.null(test_n)) {
    n_test_total <- test_n
    if (n_test_total >= n_total) {
      stop("'test_n' must be less than total observations (", n_total, ")")
    }
  } else {
    n_test_total <- floor(n_total * test_prop)
  }

  if (n_test_total < num_blocks) {
    stop("Test set size (", n_test_total, ") must be >= num_blocks (", num_blocks, ")")
  }


  # ---- Order data ----
  # Keep track of original indices
  data$.original_idx <- seq_len(n_total)

  if (!is.null(order_col)) {
    col_name <- if (is.numeric(order_col)) names(data)[order_col] else order_col
    data <- data[order(data[[col_name]]), ]
  }

  # New indices after ordering
  data$.row_idx <- seq_len(n_total)


  # ---- Create blocks ----
  block_size <- n_total / num_blocks
  data$.block <- ceiling(data$.row_idx / block_size)
  data$.block <- pmin(data$.block, num_blocks)  # Ensure max is num_blocks


  # ---- Calculate samples per block ----
  if (balance_blocks) {
    # Equal samples from each block
    base_per_block <- floor(n_test_total / num_blocks)
    remainder <- n_test_total - (base_per_block * num_blocks)
    samples_per_block <- rep(base_per_block, num_blocks)
    # Distribute remainder to first blocks
    if (remainder > 0) {
      samples_per_block[1:remainder] <- samples_per_block[1:remainder] + 1
    }
  } else {
    # Proportional to block size
    block_sizes <- table(data$.block)
    samples_per_block <- round(as.numeric(block_sizes) / n_total * n_test_total)
    # Adjust to match exact total
    diff <- n_test_total - sum(samples_per_block)
    if (diff != 0) {
      idx <- which.max(block_sizes)
      samples_per_block[idx] <- samples_per_block[idx] + diff
    }
  }


  # ---- Sample from each block ----
  test_indices <- c()
  block_info <- data.frame(
    block = 1:num_blocks,
    start_row = numeric(num_blocks),
    end_row = numeric(num_blocks),
    block_size = numeric(num_blocks),
    test_sampled = numeric(num_blocks)
  )

  for (i in 1:num_blocks) {
    block_data <- data[data$.block == i, ]
    block_rows <- block_data$.original_idx
    n_block <- length(block_rows)
    n_sample <- min(samples_per_block[i], n_block)

    # Store block info
    block_info$start_row[i] <- min(block_data$.row_idx)
    block_info$end_row[i] <- max(block_data$.row_idx)
    block_info$block_size[i] <- n_block
    block_info$test_sampled[i] <- n_sample

    if (n_sample == 0) next

    if (!is.null(stratify)) {
      # Stratified sampling within block
      strat_col <- block_data[[stratify]]
      classes <- unique(strat_col)
      class_props <- table(strat_col) / length(strat_col)

      sampled_from_block <- c()
      for (cls in classes) {
        cls_indices <- block_rows[strat_col == cls]
        n_cls_sample <- max(1, round(n_sample * class_props[as.character(cls)]))
        n_cls_sample <- min(n_cls_sample, length(cls_indices))

        if (length(cls_indices) > 0 && n_cls_sample > 0) {
          sampled_from_block <- c(sampled_from_block,
                                   sample(cls_indices, n_cls_sample))
        }
      }
      test_indices <- c(test_indices, sampled_from_block)

    } else {
      # Simple random sampling within block
      sampled <- sample(block_rows, n_sample)
      test_indices <- c(test_indices, sampled)
    }
  }

  # Remove duplicates (can happen with stratification rounding)
  test_indices <- unique(test_indices)


  # ---- Create train and test sets ----
  # test_indices are row numbers of the original (unordered) data, so the
  # subsets are taken from the untouched input (works with any row names)
  train_indices <- setdiff(seq_len(n_total), test_indices)


  # ---- Build result ----
  result <- list(
    train = data_in[sort(train_indices), , drop = FALSE],
    test = data_in[sort(test_indices), , drop = FALSE],
    train_idx = sort(train_indices),
    test_idx = sort(test_indices),
    block_info = block_info,
    settings = list(
      n_total = n_total,
      n_train = length(train_indices),
      n_test = length(test_indices),
      num_blocks = num_blocks,
      test_prop_actual = length(test_indices) / n_total,
      order_col = order_col,
      stratify = stratify,
      balance_blocks = balance_blocks,
      seed = seed
    )
  )

  class(result) <- c("block_split", "list")
  return(result)
}


#' Print a Block-Based Split
#'
#' Prints the sizes of the training and test sets and the distribution of
#' test cases across blocks.
#'
#' @param x A \code{block_split} object returned by [block_split()].
#' @param ... Additional arguments (ignored).
#' @return Invisibly returns \code{x}.
#' @examples
#' res <- block_split(iris, num_blocks = 3, test_prop = 0.2, seed = 1)
#' print(res)
#' @export
print.block_split <- function(x, ...) {
  cat("\n")
  cat("===== Block-Based Train/Test Split =====\n")
  cat("\n")
  cat("Total observations:", x$settings$n_total, "\n")
  cat("Training set:      ", x$settings$n_train,
      sprintf("(%.1f%%)", x$settings$n_train / x$settings$n_total * 100), "\n")
  cat("Test set:          ", x$settings$n_test,
      sprintf("(%.1f%%)", x$settings$n_test / x$settings$n_total * 100), "\n")
  cat("Number of blocks:  ", x$settings$num_blocks, "\n")
  cat("\n")
  cat("Block distribution:\n")
  print(x$block_info, row.names = FALSE)
  cat("\n")
  if (!is.null(x$settings$stratify)) {
    cat("Stratified by:", x$settings$stratify, "\n")
  }
  if (!is.null(x$settings$order_col)) {
    cat("Ordered by:", x$settings$order_col, "\n")
  }
  cat("Seed:", x$settings$seed, "\n")
  cat("\n")
  invisible(x)
}
