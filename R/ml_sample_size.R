#' Sample Size Estimation for Machine Learning via Monte Carlo Simulation
#'
#' Estimates the minimum sample size needed to achieve a target performance
#' in machine learning models using Monte Carlo simulation and learning curves.
#' It plays a role similar to G*Power, but for machine learning.
#'
#' @param data Data frame with real data. If NULL (default), a synthetic
#'   population is simulated using \code{sim_params}.
#' @param formula Model formula (e.g., \code{y ~ .}). Required when \code{data}
#'   is supplied.
#' @param task Task type: \code{"classification"} (default) or \code{"regression"}.
#' @param model Model(s) to use: \code{"rf"} (random forest, default),
#'   \code{"xgboost"}, \code{"svm"} or \code{"glm"}. Can be a vector to compare
#'   several models.
#' @param metric Evaluation metric (yardstick naming convention). If NULL
#'   (default), \code{"roc_auc"} is used for classification and \code{"rsq"} for
#'   regression. Classification: \code{"roc_auc"}, \code{"accuracy"},
#'   \code{"f_meas"}, \code{"mcc"}, \code{"kap"}, \code{"bal_accuracy"}.
#'   Regression: \code{"rsq"}, \code{"rmse"}, \code{"mae"}. The aliases
#'   \code{auc}, \code{f1}, \code{kappa}, \code{balanced_accuracy} and \code{r2}
#'   are accepted.
#' @param n_grid Numeric vector with the sample sizes to evaluate (default
#'   \code{c(50, 100, 150, 200, 300, 400, 600, 800, 1000, 1500, 2000)}). Values
#'   larger than the available training pool are dropped.
#' @param reps Number of Monte Carlo repetitions per sample size (default 50).
#'   Recommendations: R = 50 for exploratory analysis (SE about 0.057), R = 100
#'   for standard use (SE about 0.040), R >= 200 for publication (SE about 0.028).
#' @param target Target metric value (e.g., 0.80 for AUC >= 0.80). If NULL
#'   (default), no sample size is recommended and only the learning curve is
#'   estimated.
#' @param target_dir Direction of the target: \code{"higher_better"} or
#'   \code{"lower_better"}. If not supplied, it is set automatically
#'   (\code{"lower_better"} for \code{rmse} and \code{mae}, \code{"higher_better"}
#'   otherwise).
#' @param prob_min Minimum proportion of repetitions that must meet the target
#'   (default 0.80). This is the probability-of-success criterion p_k.
#' @param sd_max Optional maximum SD of the metric (stability criterion). NULL
#'   (default) disables the criterion.
#' @param n_test Size of the fixed test set (default 5000). If it is at least
#'   50% of the available data, it is reduced to 20% of the data with a warning.
#' @param n_outer_splits Number of independent test partitions used to assess
#'   the robustness of n* (default 1). It is set to 5 automatically when the
#'   data have fewer than 1000 rows and it is left at 1.
#' @param bootstrap_ci Logical (default FALSE). If TRUE and \code{target} is
#'   given, a bootstrap 95% CI for n* is computed (single partition only).
#' @param n_boot Number of bootstrap resamples (default 1000).
#' @param sampling Sampling type used to draw training samples in
#'   classification: \code{"stratified"} (default), \code{"none"}, \code{"up"}
#'   or \code{"down"}.
#' @param positive Label of the positive class (default \code{"1"}).
#' @param threshold Classification threshold (default 0.5).
#' @param sim_params List with simulation parameters, used when \code{data} is
#'   NULL:
#'   \itemize{
#'     \item N_pop: Population size to simulate (default 30000).
#'     \item p: Number of predictors (default 20).
#'     \item prevalence: Positive class prevalence for binary outcomes (default 0.20).
#'     \item signal: Signal strength (default 2.5). It determines the maximum
#'       achievable AUC: signal = 1.5 gives AUC about 0.70-0.75, signal = 2.5
#'       about 0.85-0.90 and signal = 3.5 about 0.92-0.95.
#'     \item noise_sd: Noise SD for regression (default 1.0).
#'     \item n_classes: Number of classes for classification (default 2).
#'       With 3 or more classes a multinomial logit (softmax) simulation is used.
#'     \item class_probs: Numeric vector of class probabilities for multiclass
#'       outcomes (default: equal probabilities 1/K). Ignored for binary outcomes.
#'   }
#' @param model_params List with model hyperparameters:
#'   \itemize{
#'     \item num.trees: Number of trees for random forest (default 500).
#'     \item mtry: Number of variables considered at each split.
#'     \item min.node.size: Minimum node size (default 5).
#'     \item max.depth: Maximum depth (default 0, no limit).
#'   }
#' @param seed Optional integer seed. If NULL (default) the random number
#'   generator state is not modified.
#' @param verbose Logical (default TRUE). If TRUE, progress and results are
#'   reported with \code{message()}.
#'
#' @return An object of class \code{c("ml_sample_size", "list")} with:
#'   \itemize{
#'     \item raw: Data frame with one row per repetition (\code{model}, \code{n},
#'       \code{rep}, \code{metric_value} and, if \code{target} is given,
#'       \code{meets_target}).
#'     \item summary: Data frame by model and sample size with \code{mean},
#'       \code{sd}, \code{se}, \code{p_meet} (proportion of repetitions meeting
#'       the target) and \code{se_p_meet} (its binomial standard error).
#'     \item recommend_n: Recommended sample size n* (a number, or a named list
#'       with one value per model); NA if no evaluated size meets the criteria.
#'       With several partitions it is the median n* across partitions.
#'     \item recommend_n_ci: Bootstrap 95% CI for n* (list with \code{ci_lower},
#'       \code{ci_upper}, \code{ci_median}, \code{n_valid}), or NULL.
#'     \item curve_fit: Fitted power-law \code{nls} model (or a list of them per
#'       model), NULL if the fit failed.
#'     \item curve_diagnostics: Fit diagnostics (\code{pseudo_r2},
#'       \code{rmse_fit}, \code{residuals}, \code{fit_quality}).
#'     \item outer_splits_results: Summary of n* across partitions
#'       (\code{median}, \code{mean}, \code{sd}, \code{cv}, \code{range},
#'       \code{n_valid}, \code{all_values}) when \code{n_outer_splits > 1},
#'       otherwise NULL.
#'     \item settings: List with the configuration used.
#'   }
#'
#' @details
#' The function implements the methodology described in Ventura-Leon (2026) for
#' sample size estimation in machine learning. Key features:
#'
#' \strong{Decision criteria:} n* is the smallest n where:
#' (1) mean performance >= target, (2) P(metric >= target) >= prob_min,
#' (3) SD <= sd_max (optional).
#'
#' \strong{SE for p_k:} The standard error of the success proportion is
#' SE(p_k) = sqrt(p_k * (1 - p_k) / R). Use R >= 100 for reliable estimates.
#'
#' \strong{Multiple partitions:} When N < 1000, using n_outer_splits > 1 is
#' recommended to assess variability in n* estimates.
#'
#' The power-law learning curve is
#' \eqn{metric(n) = a - b n^{-c}} (or \eqn{a + b n^{-c}} when lower values are
#' better), where \eqn{a} is the asymptote. See [estimate_n()] and
#' [estimate_n_plateau()] to invert the fitted curve, and
#' [plot_learning_curve()] and [plot_distribution()] to display the results.
#'
#' @examples
#' # Simulated population (classification), small settings for speed
#' result <- ml_sample_size(
#'   task = "classification",
#'   model = "glm",
#'   metric = "roc_auc",
#'   n_grid = c(50, 100, 200, 400),
#'   reps = 5,
#'   target = 0.75,
#'   n_test = 500,
#'   sim_params = list(N_pop = 2000, p = 5, prevalence = 0.30),
#'   seed = 1,
#'   verbose = FALSE
#' )
#' result
#'
#' # View recommendation
#' result$recommend_n
#'
#' # Plot learning curve
#' plot_learning_curve(result)
#'
#' # Small real dataset (N < 1000): several partitions and stability check
#' set.seed(2)
#' df <- data.frame(x1 = rnorm(400), x2 = rnorm(400))
#' df$y <- factor(ifelse(df$x1 + rnorm(400) > 0, "1", "0"))
#' res_real <- ml_sample_size(
#'   data = df,
#'   formula = y ~ .,
#'   task = "classification",
#'   model = "glm",
#'   n_grid = c(40, 80),
#'   reps = 3,
#'   target = 0.75,
#'   n_test = 100,
#'   n_outer_splits = 2,
#'   seed = 1,
#'   verbose = FALSE
#' )
#' res_real$outer_splits_results
#'
#' @references
#' Ventura-Leon, J. (2026). Sample Size Estimation for Machine Learning via
#' Monte Carlo Simulation: Learning Curves and Power Laws.
#'
#' @family study design
#' @export
#' @importFrom stats sd uniroot plogis rbinom rnorm nls coef predict quantile median
ml_sample_size <- function(data = NULL,
                           formula = NULL,
                           task = c("classification", "regression"),
                           model = c("rf"),
                           metric = NULL,
                           n_grid = c(50, 100, 150, 200, 300, 400, 600, 800, 1000, 1500, 2000),
                           reps = 50,
                           target = NULL,
                           target_dir = c("higher_better", "lower_better"),
                           prob_min = 0.80,
                           sd_max = NULL,
                           n_test = 5000,
                           n_outer_splits = 1,
                           bootstrap_ci = FALSE,
                           n_boot = 1000,
                           sampling = c("stratified", "none", "up", "down"),
                           positive = "1",
                           threshold = 0.5,
                           sim_params = list(),
                           model_params = list(),
                           seed = NULL,
                           verbose = TRUE) {

  if (!is.null(seed)) set.seed(seed)
  task <- match.arg(task)
  sampling <- match.arg(sampling)
  target_dir <- match.arg(target_dir)

  # Validate models
  valid_models <- c("rf", "xgboost", "svm", "glm")
  invalid_models <- setdiff(model, valid_models)
  if (length(invalid_models) > 0) {
    stop("Invalid models: ", paste(invalid_models, collapse = ", "),
         "\nOptions: ", paste(valid_models, collapse = ", "))
  }

  # Check dependencies for each model
  for (m in model) {
    .check_dependencies(m)
  }

  # Default metric (ROC-AUC para clasificacion, R2 para regresion)
  if (is.null(metric)) {
    metric <- if (task == "classification") "roc_auc" else "rsq"
  }

  # Normalize metric name (accept aliases: auc, f1, kappa, balanced_accuracy, r2)
  metric <- .normalize_metric(metric)

  # Validate metric
  valid_metrics_class <- c("roc_auc", "accuracy", "f_meas", "kap", "mcc", "bal_accuracy")
  valid_metrics_reg <- c("rmse", "mae", "rsq")

  if (task == "classification" && !metric %in% valid_metrics_class) {
    stop("For classification, metric must be: ", paste(valid_metrics_class, collapse = ", "),
         "\n  (aliases accepted: auc, f1, kappa, balanced_accuracy)")
  }
  if (task == "regression" && !metric %in% valid_metrics_reg) {
    stop("For regression, metric must be: ", paste(valid_metrics_reg, collapse = ", "),
         "\n  (aliases accepted: r2)")
  }

  # Auto-detect target direction
  if (is.null(target_dir) || missing(target_dir)) {
    target_dir <- if (metric %in% c("rmse", "mae")) "lower_better" else "higher_better"
  }

  # Get or simulate data
  if (is.null(data)) {
    if (task == "regression" && !is.null(sim_params$prevalence)) {
      warning("'prevalence' is ignored for regression. It only applies to classification.")
    }
    if (verbose) message("Simulating population data...")
    pop_data <- .simulate_population(
      task = task,
      N_pop = sim_params$N_pop %||% 30000,
      p = sim_params$p %||% 20,
      prevalence = sim_params$prevalence %||% 0.20,
      signal = sim_params$signal %||% 2.5,
      noise_sd = sim_params$noise_sd %||% 1.0,
      n_classes = sim_params$n_classes %||% 2L,
      class_probs = sim_params$class_probs,
      seed = seed
    )
  } else {
    if (is.null(formula)) {
      stop("Must specify 'formula' when using real data")
    }
    pop_data <- .prepare_real_data(data, formula, task, positive)
  }

  # Build mc_info for multiclass support
  mc_info <- NULL
  if (task == "classification") {
    class_levels <- levels(pop_data$y)
    n_classes <- length(class_levels)
    mc_info <- list(
      n_classes = n_classes,
      class_levels = class_levels,
      is_multiclass = n_classes >= 3L
    )

    if (mc_info$is_multiclass) {
      if (verbose) {
        message("\nMulticlass detected: ", n_classes, " classes (",
                paste(class_levels, collapse = ", "), ")")
        message("  Note: 'positive' and 'threshold' are ignored for multiclass.")
      }
    }
  }

  # Validate n_test
  if (n_test >= nrow(pop_data) * 0.5) {
    n_test <- floor(nrow(pop_data) * 0.2)
    warning("n_test adjusted to ", n_test, " (20% of data)")
  }

  # Warn about small samples
  N_total <- nrow(pop_data)
  if (N_total < 1000 && n_outer_splits == 1) {
    n_outer_splits <- 5
    if (verbose) {
      message("\nNote: N < 1000. Automatically set n_outer_splits = 5 for robustness.")
    }
  }

  # SE guidance based on reps
  if (verbose) {
    se_approx <- round(sqrt(0.80 * 0.20 / reps), 3)
    message("\nMonte Carlo settings:")
    message("  - Repetitions (R): ", reps)
    message("  - Approx SE(p_k) at p=0.80: ", se_approx)
    if (reps < 50) {
      message("  - Warning: R < 50 may yield unstable estimates. Consider R >= 100.")
    } else if (reps >= 200) {
      message("  - High precision mode (R >= 200)")
    }
  }

  # ============================================================================
  # MULTIPLE PARTITIONS (n_outer_splits > 1)
  # ============================================================================
  if (n_outer_splits > 1) {
    if (verbose) {
      message("\n=== ROBUSTNESS PROTOCOL ===")
      message("Running ", n_outer_splits, " independent test partitions...")
    }

    outer_results <- vector("list", n_outer_splits)
    all_n_star <- list()

    for (s in seq_len(n_outer_splits)) {
      if (verbose) message("\nPartition ", s, "/", n_outer_splits, ":")

      if (!is.null(seed)) set.seed(seed + s * 1000)

      partition_result <- .run_single_partition(
        pop_data = pop_data,
        n_test = n_test,
        n_grid = n_grid,
        reps = reps,
        task = task,
        model = model,
        metric = metric,
        target = target,
        target_dir = target_dir,
        prob_min = prob_min,
        sd_max = sd_max,
        sampling = sampling,
        positive = positive,
        threshold = threshold,
        model_params = model_params,
        mc_info = mc_info,
        seed = if (is.null(seed)) NULL else seed + s * 1000,
        verbose = FALSE,
        partition_id = s
      )

      outer_results[[s]] <- partition_result

      for (m in model) {
        if (is.null(all_n_star[[m]])) all_n_star[[m]] <- numeric(0)
        all_n_star[[m]] <- c(all_n_star[[m]], partition_result$recommend_n[[m]])
      }

      if (verbose) {
        for (m in model) {
          rec <- partition_result$recommend_n[[m]]
          message("  ", toupper(m), ": n* = ", if (!is.na(rec)) rec else "NA")
        }
      }
    }

    # Aggregate results across partitions
    recommend_n <- list()
    outer_splits_summary <- list()

    for (m in model) {
      n_star_vec <- all_n_star[[m]]
      valid_n_star <- n_star_vec[!is.na(n_star_vec)]

      if (length(valid_n_star) > 0) {
        med_n <- median(valid_n_star)
        mean_n <- mean(valid_n_star)
        sd_n <- sd(valid_n_star)
        cv_n <- if (mean_n > 0) sd_n / mean_n else NA
        range_n <- range(valid_n_star)

        recommend_n[[m]] <- med_n

        outer_splits_summary[[m]] <- list(
          median = med_n,
          mean = mean_n,
          sd = sd_n,
          cv = cv_n,
          range = range_n,
          n_valid = length(valid_n_star),
          all_values = n_star_vec
        )

        # Warn if high variability
        if (!is.na(cv_n) && cv_n > 0.50 && verbose) {
          message("\nWarning: High variability in n* for ", toupper(m),
                  " (CV = ", round(cv_n * 100, 1), "%). ",
                  "Range [", range_n[1], ", ", range_n[2], "] exceeds 50% of median.")
        }
      } else {
        recommend_n[[m]] <- NA_integer_
        outer_splits_summary[[m]] <- list(
          median = NA, mean = NA, sd = NA, cv = NA,
          range = c(NA, NA), n_valid = 0, all_values = n_star_vec
        )
      }
    }

    # Combine raw data from all partitions
    raw <- do.call(rbind, lapply(outer_results, function(x) x$raw))

    # Create combined summary
    summary_df <- aggregate(metric_value ~ model + n, data = raw, FUN = function(x) {
      c(mean = mean(x, na.rm = TRUE),
        sd = sd(x, na.rm = TRUE),
        se = sd(x, na.rm = TRUE) / sqrt(sum(!is.na(x))))
    })
    summary_df <- do.call(data.frame, summary_df)
    names(summary_df) <- c("model", "n", "mean", "sd", "se")

    if (!is.null(target)) {
      raw$meets_target <- if (target_dir == "higher_better") {
        raw$metric_value >= target
      } else {
        raw$metric_value <= target
      }
      p_target <- aggregate(meets_target ~ model + n, data = raw, FUN = mean)
      names(p_target) <- c("model", "n", "p_meet")
      summary_df <- merge(summary_df, p_target, by = c("model", "n"))
      summary_df$se_p_meet <- sqrt(summary_df$p_meet * (1 - summary_df$p_meet) /
                                     (reps * n_outer_splits))
    } else {
      summary_df$p_meet <- NA_real_
      summary_df$se_p_meet <- NA_real_
    }

    # Simplify if single model
    if (length(model) == 1) {
      recommend_n <- recommend_n[[1]]
      outer_splits_summary <- outer_splits_summary[[1]]
    }

    # Fit power law on combined data
    curve_fit <- list()
    curve_diagnostics <- list()

    for (current_model in model) {
      model_summary <- summary_df[summary_df$model == current_model, ]
      temp_summary <- model_summary[, c("n", "mean", "sd", "se")]

      fit <- tryCatch({
        # The 3-parameter power law needs at least 3 sample sizes (nls may not
        # terminate with fewer points)
        if (nrow(temp_summary) < 3) NULL else .fit_power_law(temp_summary, target_dir)
      }, error = function(e) NULL)

      curve_fit[current_model] <- list(fit)  # keeps NULL fits
      curve_diagnostics[[current_model]] <- .compute_fit_diagnostics(fit, temp_summary)
    }

    if (length(model) == 1) {
      curve_fit <- curve_fit[[1]]
      curve_diagnostics <- curve_diagnostics[[1]]
    }

    # Verbose output
    if (verbose) {
      message("\n=== RESULTS (Multiple Partitions) ===")
      message("Metric: ", metric)
      message("Target: ", if (!is.null(target)) target else "Not specified")
      message("Partitions: ", n_outer_splits)

      if (length(model) == 1) {
        oss <- outer_splits_summary
        message("\nModel: ", model)
        message("  Median n*: ", if (!is.na(oss$median)) oss$median else "Not determined")
        message("  Range: [", oss$range[1], ", ", oss$range[2], "]")
        message("  CV: ", if (!is.na(oss$cv)) paste0(round(oss$cv * 100, 1), "%") else "NA")
        message("  Valid estimates: ", oss$n_valid, "/", n_outer_splits)
      } else {
        message("\n--- Results by model ---")
        for (m in model) {
          oss <- outer_splits_summary[[m]]
          message("  ", toupper(m), ":")
          message("    Median n*: ", if (!is.na(oss$median)) oss$median else "NA")
          message("    Range: [", oss$range[1], ", ", oss$range[2], "]")
          message("    CV: ", if (!is.na(oss$cv)) paste0(round(oss$cv * 100, 1), "%") else "NA")
        }
      }
    }

    result <- list(
      raw = raw,
      summary = summary_df,
      recommend_n = recommend_n,
      recommend_n_ci = NULL,
      curve_fit = curve_fit,
      curve_diagnostics = curve_diagnostics,
      outer_splits_results = outer_splits_summary,
      settings = list(
        task = task,
        models = model,
        metric = metric,
        target = target,
        target_dir = target_dir,
        prob_min = prob_min,
        sd_max = sd_max,
        sampling = sampling,
        reps = reps,
        n_test = n_test,
        n_outer_splits = n_outer_splits,
        bootstrap_ci = bootstrap_ci,
        n_boot = n_boot,
        n_classes = if (!is.null(mc_info)) mc_info$n_classes else NULL,
        class_levels = if (!is.null(mc_info)) mc_info$class_levels else NULL
      )
    )

    class(result) <- c("ml_sample_size", "list")
    return(result)
  }

  # ============================================================================
  # SINGLE PARTITION (original flow, enhanced)
  # ============================================================================

  # Split: fixed test set
  test_idx <- sample(seq_len(nrow(pop_data)), size = n_test)
  test_data <- pop_data[test_idx, ]
  pool_data <- pop_data[-test_idx, ]

  # Filter n_grid exceeding pool_data
  n_grid <- n_grid[n_grid <= nrow(pool_data)]
  if (length(n_grid) == 0) {
    stop("All n_grid values exceed available data size")
  }

  # Model hyperparameters
  mp <- list(
    num.trees = model_params$num.trees %||% 500,
    mtry = model_params$mtry,
    min.node.size = model_params$min.node.size %||% 5,
    max.depth = model_params$max.depth %||% 0
  )

  # Monte Carlo simulation
  if (verbose) {
    message("\nStarting Monte Carlo simulation...")
    message("  - Models: ", paste(model, collapse = ", "))
    message("  - Sample sizes: ", paste(n_grid, collapse = ", "))
    message("  - Repetitions per size: ", reps)
    message("  - Total iterations: ", length(model) * length(n_grid) * reps)
  }

  raw <- vector("list", length(model) * length(n_grid) * reps)
  k <- 1
  total_iter <- length(model) * length(n_grid) * reps

  for (current_model in model) {
    if (verbose && length(model) > 1) {
      message("\n  Evaluating model: ", toupper(current_model))
    }

    for (n in n_grid) {
      for (r in seq_len(reps)) {
        train_data <- .sample_train(pool_data, n = n, task = task,
                                    sampling = sampling, positive = positive,
                                    mc_info = mc_info)

        preds <- .fit_predict(
          train_data = train_data,
          test_data = test_data,
          task = task,
          model = current_model,
          model_params = mp,
          mc_info = mc_info,
          seed = if (is.null(seed)) NULL else seed + 10000 + k
        )

        y_true <- test_data$y
        val <- .compute_metric(
          y_true = y_true,
          y_pred = preds,
          task = task,
          metric = metric,
          threshold = threshold,
          positive = positive,
          mc_info = mc_info
        )

        raw[[k]] <- data.frame(model = current_model, n = n, rep = r, metric_value = val)
        k <- k + 1

        if (verbose && k %% 100 == 0) {
          message("  Progress: ", round(k / total_iter * 100, 1), "%")
        }
      }
    }
  }

  raw <- do.call(rbind, raw)

  # Summary by model and n
  summary_df <- aggregate(metric_value ~ model + n, data = raw, FUN = function(x) {
    c(mean = mean(x, na.rm = TRUE),
      sd = sd(x, na.rm = TRUE),
      se = sd(x, na.rm = TRUE) / sqrt(sum(!is.na(x))))
  })
  summary_df <- do.call(data.frame, summary_df)
  names(summary_df) <- c("model", "n", "mean", "sd", "se")

  # Target compliance with SE for p_meet
  if (!is.null(target)) {
    raw$meets_target <- if (target_dir == "higher_better") {
      raw$metric_value >= target
    } else {
      raw$metric_value <= target
    }

    p_target <- aggregate(meets_target ~ model + n, data = raw, FUN = mean)
    names(p_target) <- c("model", "n", "p_meet")
    summary_df <- merge(summary_df, p_target, by = c("model", "n"))

    # SE for success proportion (binomial)
    summary_df$se_p_meet <- sqrt(summary_df$p_meet * (1 - summary_df$p_meet) / reps)
  } else {
    summary_df$p_meet <- NA_real_
    summary_df$se_p_meet <- NA_real_
  }

  # Recommendation by model
  recommend_n <- list()

  for (current_model in model) {
    model_summary <- summary_df[summary_df$model == current_model, ]
    model_summary <- model_summary[order(model_summary$n), ]

    rec_n <- NA_integer_
    if (!is.null(target)) {
      ok <- rep(TRUE, nrow(model_summary))

      if (target_dir == "higher_better") {
        ok <- ok & model_summary$mean >= target
      } else {
        ok <- ok & model_summary$mean <= target
      }

      ok <- ok & model_summary$p_meet >= prob_min

      if (!is.null(sd_max)) {
        ok <- ok & model_summary$sd <= sd_max
      }

      if (any(ok, na.rm = TRUE)) {
        rec_n <- model_summary$n[which(ok)[1]]
      }
    }
    recommend_n[[current_model]] <- rec_n
  }

  if (length(model) == 1) {
    recommend_n <- recommend_n[[1]]
  }

  # Bootstrap CI for n* (if requested)
  recommend_n_ci <- NULL
  if (bootstrap_ci && !is.null(target)) {
    if (verbose) message("\nComputing bootstrap CI for n* (B = ", n_boot, ")...")

    if (length(model) == 1) {
      model_raw <- raw[raw$model == model, ]
      boot_result <- .bootstrap_n_star(
        raw_data = model_raw,
        target = target,
        target_dir = target_dir,
        prob_min = prob_min,
        sd_max = sd_max,
        n_boot = n_boot,
        seed = seed
      )
      recommend_n_ci <- list(
        ci_lower = boot_result$ci_lower,
        ci_upper = boot_result$ci_upper,
        ci_median = boot_result$ci_median,
        n_valid = boot_result$n_valid
      )
    } else {
      recommend_n_ci <- list()
      for (m in model) {
        model_raw <- raw[raw$model == m, ]
        boot_result <- .bootstrap_n_star(
          raw_data = model_raw,
          target = target,
          target_dir = target_dir,
          prob_min = prob_min,
          sd_max = sd_max,
          n_boot = n_boot,
          seed = seed
        )
        recommend_n_ci[[m]] <- list(
          ci_lower = boot_result$ci_lower,
          ci_upper = boot_result$ci_upper,
          ci_median = boot_result$ci_median,
          n_valid = boot_result$n_valid
        )
      }
    }
  }

  # Power law fit with diagnostics
  curve_fit <- list()
  curve_diagnostics <- list()

  for (current_model in model) {
    model_summary <- summary_df[summary_df$model == current_model, ]
    temp_summary <- model_summary[, c("n", "mean", "sd", "se")]

    fit <- tryCatch({
      # The 3-parameter power law needs at least 3 sample sizes (nls may not
      # terminate with fewer points)
      if (nrow(temp_summary) < 3) NULL else .fit_power_law(temp_summary, target_dir)
    }, error = function(e) {
      if (verbose) message("Note: Could not fit curve for ", current_model)
      NULL
    })

    curve_fit[current_model] <- list(fit)  # keeps NULL fits
    curve_diagnostics[[current_model]] <- .compute_fit_diagnostics(fit, temp_summary)
  }

  if (length(model) == 1) {
    curve_fit <- curve_fit[[1]]
    curve_diagnostics <- curve_diagnostics[[1]]
  }

  # Verbose output
  if (verbose) {
    message("\n=== RESULTS ===")
    message("Metric: ", metric)
    message("Target: ", if (!is.null(target)) target else "Not specified")

    if (length(model) == 1) {
      message("Model: ", model)
      message("Recommended n*: ", if (!is.na(recommend_n)) recommend_n else "Not determined")

      if (!is.null(curve_fit)) {
        co <- coef(curve_fit)
        message("Estimated asymptote: ", round(co["a"], 4))
        message("Fit quality: ", curve_diagnostics$fit_quality,
                " (pseudo-R2 = ", round(curve_diagnostics$pseudo_r2, 3), ")")
      }

      if (!is.null(recommend_n_ci)) {
        message("Bootstrap 95% CI: [", recommend_n_ci$ci_lower, ", ",
                recommend_n_ci$ci_upper, "]")
      }
    } else {
      message("\n--- Results by model ---")
      for (current_model in model) {
        rec <- recommend_n[[current_model]]
        msg_n <- if (!is.na(rec)) rec else "Not determined"

        asym_msg <- ""
        diag_msg <- ""
        if (!is.null(curve_fit[[current_model]])) {
          co <- coef(curve_fit[[current_model]])
          asym_msg <- paste0(" (asymptote: ", round(co["a"], 4), ")")
          diag <- curve_diagnostics[[current_model]]
          diag_msg <- paste0(" [", diag$fit_quality, ", R2=",
                             round(diag$pseudo_r2, 2), "]")
        }

        message("  ", toupper(current_model), ": n* = ", msg_n, asym_msg, diag_msg)

        if (!is.null(recommend_n_ci) && !is.null(recommend_n_ci[[current_model]])) {
          ci <- recommend_n_ci[[current_model]]
          message("    Bootstrap 95% CI: [", ci$ci_lower, ", ", ci$ci_upper, "]")
        }
      }

      valid_recs <- unlist(recommend_n)
      valid_recs <- valid_recs[!is.na(valid_recs)]
      if (length(valid_recs) > 0) {
        best_model <- names(which.min(valid_recs))
        message("\n  -> Best model (lowest n*): ", toupper(best_model),
                " with n* = ", min(valid_recs))
      }
    }
  }

  # Result
  result <- list(
    raw = raw,
    summary = summary_df,
    recommend_n = recommend_n,
    recommend_n_ci = recommend_n_ci,
    curve_fit = curve_fit,
    curve_diagnostics = curve_diagnostics,
    outer_splits_results = NULL,
    settings = list(
      task = task,
      models = model,
      metric = metric,
      target = target,
      target_dir = target_dir,
      prob_min = prob_min,
      sd_max = sd_max,
      sampling = sampling,
      reps = reps,
      n_test = n_test,
      n_outer_splits = n_outer_splits,
      bootstrap_ci = bootstrap_ci,
      n_boot = n_boot,
      n_classes = if (!is.null(mc_info)) mc_info$n_classes else NULL,
      class_levels = if (!is.null(mc_info)) mc_info$class_levels else NULL
    )
  )

  class(result) <- c("ml_sample_size", "list")
  return(result)
}


#' Print ML Sample Size Results
#'
#' Prints the settings, the recommended sample size n* (with its bootstrap CI
#' and curve-fit quality when available) and the summary by sample size of an
#' [ml_sample_size()] result.
#'
#' @param x An object of class \code{ml_sample_size}.
#' @param ... Additional arguments (ignored).
#' @return Invisibly returns \code{x}.
#' @examples
#' res <- ml_sample_size(model = "glm", n_grid = c(50, 100, 200), reps = 3,
#'                       target = 0.75, n_test = 300,
#'                       sim_params = list(N_pop = 1500, p = 5),
#'                       seed = 1, verbose = FALSE)
#' print(res)
#' @export
print.ml_sample_size <- function(x, ...) {
  cat("\n=== Sample Size Estimation for ML ===\n\n")
  cat("Task:", x$settings$task, "\n")
  cat("Model(s):", paste(x$settings$models, collapse = ", "), "\n")
  cat("Metric:", x$settings$metric, "\n")
  cat("Target:", if (!is.null(x$settings$target)) x$settings$target else "Not specified", "\n")
  cat("Repetitions (R):", x$settings$reps, "\n")

  # SE guidance
  se_approx <- round(sqrt(0.80 * 0.20 / x$settings$reps), 3)
  cat("Approx SE(p_k):", se_approx, "\n")

  if (x$settings$n_outer_splits > 1) {
    cat("Partitions:", x$settings$n_outer_splits, "\n")
  }

  cat("\n--- Results ---\n")

  # Multiple partitions results
  if (!is.null(x$outer_splits_results)) {
    if (is.list(x$outer_splits_results) && !is.null(x$outer_splits_results$median)) {
      # Single model
      oss <- x$outer_splits_results
      cat("Median n*:", if (!is.na(oss$median)) oss$median else "Not determined", "\n")
      cat("Range: [", oss$range[1], ", ", oss$range[2], "]\n")
      cat("CV:", if (!is.na(oss$cv)) paste0(round(oss$cv * 100, 1), "%") else "NA", "\n")
    } else {
      # Multiple models
      for (m in names(x$outer_splits_results)) {
        oss <- x$outer_splits_results[[m]]
        cat("  ", toupper(m), ":\n")
        cat("    Median n*:", if (!is.na(oss$median)) oss$median else "NA", "\n")
        cat("    Range: [", oss$range[1], ", ", oss$range[2], "]\n")
        cat("    CV:", if (!is.na(oss$cv)) paste0(round(oss$cv * 100, 1), "%") else "NA", "\n")
      }
    }
  } else {
    # Single partition results
    if (is.list(x$recommend_n)) {
      for (m in names(x$recommend_n)) {
        rec <- x$recommend_n[[m]]
        cat("  ", toupper(m), ": n* =", if (!is.na(rec)) rec else "Not determined")

        # Show CI if available
        if (!is.null(x$recommend_n_ci) && !is.null(x$recommend_n_ci[[m]])) {
          ci <- x$recommend_n_ci[[m]]
          cat(" [95% CI:", ci$ci_lower, "-", ci$ci_upper, "]")
        }

        # Show diagnostics if available
        if (!is.null(x$curve_diagnostics) && !is.null(x$curve_diagnostics[[m]])) {
          diag <- x$curve_diagnostics[[m]]
          cat(" (fit:", diag$fit_quality, ")")
        }
        cat("\n")
      }
    } else {
      cat("Recommended n*:", if (!is.na(x$recommend_n)) x$recommend_n else "Not determined", "\n")

      # Show CI if available
      if (!is.null(x$recommend_n_ci)) {
        cat("Bootstrap 95% CI: [", x$recommend_n_ci$ci_lower, ", ",
            x$recommend_n_ci$ci_upper, "]\n")
      }

      # Show curve fit info
      if (!is.null(x$curve_fit) && !is.list(x$curve_fit)) {
        co <- coef(x$curve_fit)
        cat("Estimated asymptote:", round(co["a"], 4), "\n")
      }

      # Show diagnostics
      if (!is.null(x$curve_diagnostics) && !is.null(x$curve_diagnostics$pseudo_r2)) {
        cat("Fit quality:", x$curve_diagnostics$fit_quality,
            "(pseudo-R2 =", round(x$curve_diagnostics$pseudo_r2, 3), ")\n")
      }
    }
  }

  cat("\n--- Summary by sample size ---\n")
  # Show relevant columns sorted by model and n
  cols_to_show <- c("model", "n", "mean", "sd", "p_meet", "se_p_meet")
  cols_available <- intersect(cols_to_show, names(x$summary))
  summary_sorted <- x$summary[order(x$summary$model, x$summary$n), cols_available]
  print(summary_sorted, row.names = FALSE, digits = 3)

  invisible(x)
}


#' Estimate N for a Target Metric Using the Curve Fit
#'
#' Uses the fitted power-law model to estimate the sample size required
#' to achieve a specified target metric value.
#'
#' @param object An object of class \code{ml_sample_size} returned by
#'   [ml_sample_size()].
#' @param target_metric Target metric value.
#' @param model For multi-model results, the model to use. If NULL (default),
#'   the first model is used.
#' @param warn_extrapolation Logical (default TRUE). Warn if the fit is poor or
#'   the estimate extrapolates beyond the observed sample sizes.
#'
#' @return A single number: the estimated sample size (rounded up to an
#'   integer value), or \code{Inf} if the target is beyond the estimated
#'   asymptote.
#'
#' @details
#' This function inverts the power-law equation to find the required n.
#' Warnings are issued when:
#' \itemize{
#'   \item The target exceeds the estimated asymptote.
#'   \item The required n exceeds 2 times the largest observed sample size.
#'   \item The curve fit quality is poor (pseudo-R2 < 0.80).
#' }
#'
#' @examples
#' res <- ml_sample_size(model = "glm", n_grid = c(50, 100, 200, 400), reps = 5,
#'                       n_test = 500, sim_params = list(N_pop = 2000, p = 5),
#'                       seed = 1, verbose = FALSE)
#' a <- coef(res$curve_fit)[["a"]]
#' estimate_n(res, target_metric = a - 0.02)
#'
#' @family study design
#' @export
estimate_n <- function(object, target_metric, model = NULL, warn_extrapolation = TRUE) {
  if (!inherits(object, "ml_sample_size")) {
    stop("object must be of class 'ml_sample_size'")
  }

  # Handle multi-model case
  curve_fit <- object$curve_fit
  curve_diag <- object$curve_diagnostics

  if (is.list(curve_fit) && !inherits(curve_fit, "nls")) {
    if (is.null(model)) {
      model <- names(curve_fit)[1]
      message("Using model: ", model)
    }
    curve_fit <- curve_fit[[model]]
    curve_diag <- object$curve_diagnostics[[model]]
  }

  if (is.null(curve_fit)) {
    stop("No curve fit available")
  }

  # Check fit quality
  if (warn_extrapolation && !is.null(curve_diag)) {
    if (!is.na(curve_diag$pseudo_r2) && curve_diag$pseudo_r2 < 0.80) {
      warning("Curve fit quality is poor (pseudo-R2 = ", round(curve_diag$pseudo_r2, 3),
              "). Extrapolation may be unreliable.")
    }
  }

  co <- coef(curve_fit)
  a <- co["a"]
  b <- co["b"]
  c <- co["c"]

  if (object$settings$target_dir == "higher_better") {
    if (target_metric >= a) {
      warning("Target (", target_metric, ") exceeds estimated asymptote (",
              round(a, 4), "). Target may be unachievable.")
      return(Inf)
    }
    n_needed <- (b / (a - target_metric))^(1 / c)
  } else {
    if (target_metric <= a) {
      warning("Target (", target_metric, ") is below estimated asymptote (",
              round(a, 4), "). Target may be unachievable.")
      return(Inf)
    }
    n_needed <- (b / (target_metric - a))^(1 / c)
  }

  n_result <- ceiling(n_needed)

  # Warn about extrapolation beyond observed data
  if (warn_extrapolation) {
    max_n_observed <- max(object$summary$n)
    if (n_result > 2 * max_n_observed) {
      warning("Estimated n (", n_result, ") exceeds 2x the largest observed sample size (",
              max_n_observed, "). Extrapolation may be unreliable. ",
              "Consider expanding n_grid to include larger sample sizes.")
    }
  }

  n_result
}


#' Estimate N to Reach the Plateau (Near Maximum Performance)
#'
#' Computes the sample size at which performance approaches the asymptote
#' within a specified distance (delta).
#'
#' @param object An object of class \code{ml_sample_size} returned by
#'   [ml_sample_size()].
#' @param delta Distance to the asymptote that defines the plateau (default 0.01).
#' @param relative Logical (default FALSE). If TRUE, \code{delta} is interpreted
#'   as a proportion of the target metric (e.g., delta = 0.01 means 1% of the
#'   target). Requires a result computed with a \code{target}.
#' @param model For multi-model results, the model to use. If NULL (default),
#'   the first model is used.
#' @param warn_extrapolation Logical (default TRUE). Warn if the fit is poor or
#'   the estimate extrapolates beyond the observed sample sizes.
#'
#' @return A single number: the estimated sample size for the plateau (rounded
#'   up to an integer value).
#'
#' @details
#' The plateau is defined operationally as n_plateau = (b / delta)^(1 / c).
#'
#' When relative = TRUE and target = 0.80, delta = 0.01 becomes 0.008 (1% of
#' 0.80), yielding a more stringent plateau definition.
#'
#' @examples
#' result <- ml_sample_size(model = "glm", n_grid = c(50, 100, 200, 400),
#'                          reps = 5, target = 0.75, n_test = 500,
#'                          sim_params = list(N_pop = 2000, p = 5),
#'                          seed = 1, verbose = FALSE)
#'
#' # Absolute delta (default)
#' estimate_n_plateau(result, delta = 0.01, warn_extrapolation = FALSE)
#'
#' # Relative delta (1% of target)
#' estimate_n_plateau(result, delta = 0.01, relative = TRUE,
#'                    warn_extrapolation = FALSE)
#'
#' @family study design
#' @export
estimate_n_plateau <- function(object, delta = 0.01, relative = FALSE,
                                model = NULL, warn_extrapolation = TRUE) {
  if (!inherits(object, "ml_sample_size")) {
    stop("object must be of class 'ml_sample_size'")
  }

  # Handle multi-model case
  curve_fit <- object$curve_fit
  curve_diag <- object$curve_diagnostics

  if (is.list(curve_fit) && !inherits(curve_fit, "nls")) {
    if (is.null(model)) {
      model <- names(curve_fit)[1]
      message("Using model: ", model)
    }
    curve_fit <- curve_fit[[model]]
    curve_diag <- object$curve_diagnostics[[model]]
  }

  if (is.null(curve_fit)) {
    stop("No curve fit available")
  }

  # Check fit quality
  if (warn_extrapolation && !is.null(curve_diag)) {
    if (!is.na(curve_diag$pseudo_r2) && curve_diag$pseudo_r2 < 0.80) {
      warning("Curve fit quality is poor (pseudo-R2 = ", round(curve_diag$pseudo_r2, 3),
              "). Extrapolation may be unreliable.")
    }
  }

  # Convert relative delta if needed
  if (relative && !is.null(object$settings$target)) {
    delta_original <- delta
    delta <- delta * object$settings$target
    message("Using relative delta: ", delta_original, " x ", object$settings$target,
            " = ", round(delta, 4))
  }

  co <- coef(curve_fit)
  b <- co["b"]
  c <- co["c"]

  n_plateau <- ceiling((b / delta)^(1 / c))

  # Warn about extrapolation
  if (warn_extrapolation) {
    max_n_observed <- max(object$summary$n)
    if (n_plateau > 2 * max_n_observed) {
      warning("Estimated n_plateau (", n_plateau, ") exceeds 2x the largest observed sample size (",
              max_n_observed, "). Extrapolation may be unreliable.")
    }
  }

  n_plateau
}


# Operador %||% para valores por defecto
`%||%` <- function(x, y) if (is.null(x)) y else x
