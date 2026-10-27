# =============================================================================
# Internal helper functions for ml_sample_size
# =============================================================================

#' Normalize a Metric Name to the Yardstick Convention
#'
#' Accepts both simple names (auc, f1, kappa, balanced_accuracy, r2) and
#' yardstick names (roc_auc, f_meas, kap, bal_accuracy, rsq).
#' Returns the canonical yardstick name.
#' @noRd
.normalize_metric <- function(metric) {
  aliases <- c(
    "auc"               = "roc_auc",
    "f1"                = "f_meas",
    "kappa"             = "kap",
    "balanced_accuracy" = "bal_accuracy",
    "r2"                = "rsq"
  )
  if (metric %in% names(aliases)) aliases[[metric]] else metric
}


#' Compute Fit Diagnostics for the Power Law Model
#' @noRd
.compute_fit_diagnostics <- function(fit, df_summary) {
  if (is.null(fit)) {
    return(list(
      pseudo_r2 = NA_real_,
      rmse_fit = NA_real_,
      residuals = NULL,
      fit_quality = "failed"
    ))
  }

  # Predicted values
  predicted <- predict(fit, newdata = df_summary)
  observed <- df_summary$mean

  # Residuals
  residuals <- observed - predicted

  # RMSE of fit
  rmse_fit <- sqrt(mean(residuals^2))


  # Pseudo R-squared
  ss_res <- sum(residuals^2)
  ss_tot <- sum((observed - mean(observed))^2)
  pseudo_r2 <- ifelse(ss_tot > 0, 1 - ss_res / ss_tot, NA_real_)

  # Classify fit quality
  fit_quality <- if (is.na(pseudo_r2)) {
    "failed"
  } else if (pseudo_r2 >= 0.95) {
    "good"
  } else if (pseudo_r2 >= 0.80) {
    "acceptable"
  } else {
    "poor"
  }

  list(
    pseudo_r2 = pseudo_r2,
    rmse_fit = rmse_fit,
    residuals = residuals,
    fit_quality = fit_quality
  )
}


#' Bootstrap Confidence Interval for n*
#' @noRd
.bootstrap_n_star <- function(raw_data, target, target_dir, prob_min, sd_max,
                               n_boot = 1000, conf_level = 0.95, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)

  n_values <- sort(unique(raw_data$n))
  boot_n_star <- numeric(n_boot)

  for (b in seq_len(n_boot)) {
    # Resample within each n
    boot_summary <- do.call(rbind, lapply(n_values, function(n_k) {
      subset_data <- raw_data[raw_data$n == n_k, ]
      boot_idx <- sample(nrow(subset_data), replace = TRUE)
      boot_sample <- subset_data[boot_idx, ]

      mean_val <- mean(boot_sample$metric_value, na.rm = TRUE)
      sd_val <- sd(boot_sample$metric_value, na.rm = TRUE)

      if (target_dir == "higher_better") {
        p_meet <- mean(boot_sample$metric_value >= target, na.rm = TRUE)
      } else {
        p_meet <- mean(boot_sample$metric_value <= target, na.rm = TRUE)
      }

      data.frame(n = n_k, mean = mean_val, sd = sd_val, p_meet = p_meet)
    }))

    # Find n* for this bootstrap sample
    ok <- rep(TRUE, nrow(boot_summary))

    if (target_dir == "higher_better") {
      ok <- ok & boot_summary$mean >= target
    } else {
      ok <- ok & boot_summary$mean <= target
    }

    ok <- ok & boot_summary$p_meet >= prob_min

    if (!is.null(sd_max)) {
      ok <- ok & boot_summary$sd <= sd_max
    }

    if (any(ok, na.rm = TRUE)) {
      boot_n_star[b] <- boot_summary$n[which(ok)[1]]
    } else {
      boot_n_star[b] <- NA_real_
    }
  }

  # Compute CI
  alpha <- 1 - conf_level
  valid_boot <- boot_n_star[!is.na(boot_n_star)]

  if (length(valid_boot) < n_boot * 0.5) {
    warning("Less than 50% of bootstrap samples yielded valid n*. CI may be unreliable.")
  }

  if (length(valid_boot) > 0) {
    ci_lower <- quantile(valid_boot, probs = alpha / 2, na.rm = TRUE)
    ci_upper <- quantile(valid_boot, probs = 1 - alpha / 2, na.rm = TRUE)
    ci_median <- median(valid_boot, na.rm = TRUE)
  } else {
    ci_lower <- NA_real_
    ci_upper <- NA_real_
    ci_median <- NA_real_
  }

  list(
    ci_lower = as.numeric(ci_lower),
    ci_upper = as.numeric(ci_upper),
    ci_median = as.numeric(ci_median),
    boot_distribution = boot_n_star,
    n_valid = sum(!is.na(boot_n_star)),
    n_boot = n_boot
  )
}


#' Run a Single Partition Analysis
#' @noRd
.run_single_partition <- function(pop_data, n_test, n_grid, reps, task, model,
                                   metric, target, target_dir, prob_min, sd_max,
                                   sampling, positive, threshold, model_params,
                                   mc_info = NULL,
                                   seed, verbose, partition_id = 1) {

  # Split: fixed test set

  test_idx <- sample(seq_len(nrow(pop_data)), size = n_test)
  test_data <- pop_data[test_idx, ]
  pool_data <- pop_data[-test_idx, ]

  # Filter n_grid that exceed pool_data
  n_grid_valid <- n_grid[n_grid <= nrow(pool_data)]
  if (length(n_grid_valid) == 0) {
    stop("All n_grid values exceed available data size")
  }

  # Hyperparameters
  mp <- list(
    num.trees = model_params$num.trees %||% 500,
    mtry = model_params$mtry,
    min.node.size = model_params$min.node.size %||% 5,
    max.depth = model_params$max.depth %||% 0
  )

  # Monte Carlo simulation
  raw <- vector("list", length(model) * length(n_grid_valid) * reps)
  k <- 1

  for (current_model in model) {
    for (n in n_grid_valid) {
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
          seed = if (is.null(seed)) NULL else seed + 10000 * partition_id + k
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

        raw[[k]] <- data.frame(model = current_model, n = n, rep = r,
                               metric_value = val, partition = partition_id)
        k <- k + 1
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

  # Target compliance
  if (!is.null(target)) {
    raw$meets_target <- if (target_dir == "higher_better") {
      raw$metric_value >= target
    } else {
      raw$metric_value <= target
    }

    p_target <- aggregate(meets_target ~ model + n, data = raw, FUN = mean)
    names(p_target) <- c("model", "n", "p_meet")
    summary_df <- merge(summary_df, p_target, by = c("model", "n"))

    # Add SE for p_meet (binomial SE)
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

  list(
    raw = raw,
    summary = summary_df,
    recommend_n = recommend_n,
    n_grid_used = n_grid_valid
  )
}


#' Check Installed Dependencies
#' @noRd
.check_dependencies <- function(model) {
  # ranger siempre requerido para RF
  if (model == "rf" && !requireNamespace("ranger", quietly = TRUE)) {
    stop("Package 'ranger' is required for random forest. Install package 'ranger' to use this option.")
  }

  if (model == "xgboost" && !requireNamespace("xgboost", quietly = TRUE)) {
    stop("Package 'xgboost' is required. Install package 'xgboost' to use this option.")
  }

  if (model == "svm" && !requireNamespace("e1071", quietly = TRUE)) {
    stop("Package 'e1071' is required for SVM. Install package 'e1071' to use this option.")
  }

  # pROC para AUC
  if (!requireNamespace("pROC", quietly = TRUE)) {
    stop("Package 'pROC' is required. Install package 'pROC' to use this option.")
  }
}


#' Simulate a Synthetic Population
#' @noRd
.simulate_population <- function(task, N_pop = 30000, p = 20,
                                  prevalence = 0.20, signal = 2.5,
                                  noise_sd = 1.0, n_classes = 2L,
                                  class_probs = NULL, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)

  X <- matrix(rnorm(N_pop * p), nrow = N_pop, ncol = p)
  colnames(X) <- paste0("x", 1:p)

  # Coeficientes con senal distribuida
  # signal = 2.5 genera AUC teorico ~0.85-0.90
  # signal = 1.5 genera AUC teorico ~0.70-0.75
  # signal = 3.5 genera AUC teorico ~0.92-0.95
  beta <- rep(signal / sqrt(p), p)
  lin <- as.vector(X %*% beta)

  if (task == "classification") {
    K <- as.integer(n_classes)

    if (K >= 3L) {
      # --- Multiclass: multinomial logit (softmax) ---
      if (is.null(class_probs)) class_probs <- rep(1 / K, K)
      class_probs <- class_probs / sum(class_probs)
      class_labels <- paste0("class", seq_len(K))

      # K-1 linear predictors (class 1 = reference)
      directions <- matrix(rnorm(p * (K - 1)), nrow = p, ncol = K - 1)
      directions <- apply(directions, 2, function(d) d / sqrt(sum(d^2)))
      eta <- X %*% (directions * signal / sqrt(p))  # N x (K-1)

      # Calibrate intercepts to approximate desired class_probs
      b0 <- log(class_probs[-1] / class_probs[1])

      # Softmax probabilities
      log_probs <- cbind(0, sweep(eta, 2, b0, "+"))  # N x K
      log_probs <- log_probs - apply(log_probs, 1, max)  # numerical stability
      probs <- exp(log_probs)
      probs <- probs / rowSums(probs)

      # Sample from multinomial
      y <- apply(probs, 1, function(pr) sample(class_labels, 1, prob = pr))
      dat <- data.frame(y = factor(y, levels = class_labels), X)

    } else {
      # --- Binary (original code) ---
      f <- function(b0) mean(plogis(b0 + lin)) - prevalence
      b0 <- uniroot(f, interval = c(-20, 20))$root
      prob <- plogis(b0 + lin)
      y <- rbinom(N_pop, 1, prob)
      dat <- data.frame(y = factor(y, levels = c(0, 1)), X)
    }
  } else {
    # y continuo con ruido controlable
    y <- lin + rnorm(N_pop, mean = 0, sd = noise_sd)
    dat <- data.frame(y = y, X)
  }

  dat
}


#' Prepare Real Data
#' @noRd
.prepare_real_data <- function(data, formula, task, positive) {
  # Extraer variables de la formula
  mf <- model.frame(formula, data = data, na.action = na.omit)
  y <- model.response(mf)
  X <- model.matrix(formula, data = mf)[, -1, drop = FALSE]  # sin intercepto

  if (task == "classification") {
    if (!is.factor(y)) {
      y <- factor(y)
    }

    if (nlevels(y) >= 3L) {
      # Multiclass: conservar niveles originales, NO recodificar
      # (positive y threshold se ignoran para multiclase)
    } else {
      # Binary: recodificar a 0/1
      if (!positive %in% levels(y)) {
        levels_str <- paste(levels(y), collapse = ", ")
        stop("'positive' (", positive, ") is not a valid level. Levels: ", levels_str)
      }
      y <- factor(ifelse(y == positive, "1", "0"), levels = c("0", "1"))
    }
  }

  data.frame(y = y, X)
}


#' Sample Training Data
#' @noRd
.sample_train <- function(pool_data, n, task,
                          sampling = "stratified",
                          positive = "1",
                          mc_info = NULL,
                          seed = NULL) {
  if (!is.null(seed)) set.seed(seed)

  if (task != "classification" || sampling == "none") {
    idx <- sample(seq_len(nrow(pool_data)), size = n, replace = FALSE)
    return(pool_data[idx, , drop = FALSE])
  }

  # --- Multiclass sampling ---
  if (!is.null(mc_info) && mc_info$is_multiclass) {
    y <- pool_data$y
    K <- mc_info$n_classes
    class_levels <- mc_info$class_levels

    if (sampling == "stratified") {
      # Proportional sampling per class
      idx <- integer(0)
      remaining <- n
      for (i in seq_len(K)) {
        cl_idx <- which(y == class_levels[i])
        prop_cl <- length(cl_idx) / length(y)
        if (i < K) {
          n_cl <- max(1L, round(n * prop_cl))
          n_cl <- min(n_cl, length(cl_idx), remaining)
        } else {
          n_cl <- min(remaining, length(cl_idx))
        }
        if (n_cl > 0) {
          idx <- c(idx, sample(cl_idx, n_cl, replace = FALSE))
        }
        remaining <- remaining - n_cl
      }
      return(pool_data[idx, , drop = FALSE])
    }

    if (sampling == "down") {
      # Equal size per class: min(n/K, smallest class)
      n_per <- floor(n / K)
      idx <- integer(0)
      for (cl in class_levels) {
        cl_idx <- which(y == cl)
        n_cl <- min(n_per, length(cl_idx))
        idx <- c(idx, sample(cl_idx, n_cl, replace = FALSE))
      }
      return(pool_data[idx, , drop = FALSE])
    }

    if (sampling == "up") {
      # Oversample each class to n/K with replacement
      n_per <- floor(n / K)
      idx <- integer(0)
      for (cl in class_levels) {
        cl_idx <- which(y == cl)
        idx <- c(idx, sample(cl_idx, n_per, replace = TRUE))
      }
      return(pool_data[idx, , drop = FALSE])
    }

    stop("Unrecognized sampling: ", sampling)
  }

  # --- Binary sampling (original code) ---
  y <- pool_data$y
  pos_idx <- which(y == positive)
  neg_idx <- which(y != positive)

  if (sampling == "stratified") {
    # Mantener proporcion de la pool
    prop_pos <- length(pos_idx) / length(y)
    n_pos <- max(1, round(n * prop_pos))
    n_neg <- n - n_pos

    # Ajustar si no hay suficientes casos
    if (n_pos > length(pos_idx)) {
      n_pos <- length(pos_idx)
      n_neg <- min(n - n_pos, length(neg_idx))
    }
    if (n_neg > length(neg_idx)) {
      n_neg <- length(neg_idx)
      n_pos <- min(n - n_neg, length(pos_idx))
    }

    idx <- c(sample(pos_idx, n_pos, replace = FALSE),
             sample(neg_idx, n_neg, replace = FALSE))
    return(pool_data[idx, , drop = FALSE])
  }

  if (sampling == "down") {
    # Balancear reduciendo la clase mayoritaria
    n_half <- floor(n / 2)
    n_pos <- min(n_half, length(pos_idx))
    n_neg <- min(n - n_pos, length(neg_idx))
    idx <- c(sample(pos_idx, n_pos, replace = FALSE),
             sample(neg_idx, n_neg, replace = FALSE))
    return(pool_data[idx, , drop = FALSE])
  }

  if (sampling == "up") {
    # Balancear aumentando minoritaria con reemplazo
    n_half <- floor(n / 2)
    n_pos <- n_half
    n_neg <- n - n_pos
    idx <- c(sample(pos_idx, n_pos, replace = TRUE),
             sample(neg_idx, n_neg, replace = TRUE))
    return(pool_data[idx, , drop = FALSE])
  }

  stop("Unrecognized sampling: ", sampling)
}


#' Fit a Model and Predict
#' @noRd
.fit_predict <- function(train_data, test_data, task, model,
                         model_params, mc_info = NULL, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)

  if (model == "rf") {
    return(.fit_predict_rf(train_data, test_data, task, model_params, mc_info))
  }

  if (model == "xgboost") {
    return(.fit_predict_xgb(train_data, test_data, task, model_params, mc_info))
  }

  if (model == "svm") {
    return(.fit_predict_svm(train_data, test_data, task, model_params, mc_info))
  }

  if (model == "glm") {
    return(.fit_predict_glm(train_data, test_data, task, mc_info))
  }

  stop("Unsupported model: ", model)
}


#' Fit a Random Forest and Predict
#' @noRd
.fit_predict_rf <- function(train_data, test_data, task, model_params,
                            mc_info = NULL) {
  mtry <- model_params$mtry
  if (is.null(mtry)) {
    mtry <- floor(sqrt(ncol(train_data) - 1))
  }

  if (task == "classification") {
    fit <- ranger::ranger(
      y ~ .,
      data = train_data,
      probability = TRUE,
      num.trees = model_params$num.trees,
      mtry = mtry,
      min.node.size = model_params$min.node.size,
      max.depth = if (model_params$max.depth == 0) NULL else model_params$max.depth
    )

    if (!is.null(mc_info) && mc_info$is_multiclass) {
      # Multiclass: return full N x K probability matrix
      pred <- predict(fit, data = test_data)$predictions[, mc_info$class_levels, drop = FALSE]
    } else {
      # Binary: return vector of P(class=1)
      pred <- predict(fit, data = test_data)$predictions[, "1"]
    }
  } else {
    fit <- ranger::ranger(
      y ~ .,
      data = train_data,
      num.trees = model_params$num.trees,
      mtry = mtry,
      min.node.size = model_params$min.node.size,
      max.depth = if (model_params$max.depth == 0) NULL else model_params$max.depth
    )
    pred <- predict(fit, data = test_data)$predictions
  }

  pred
}


#' Fit XGBoost and Predict
#' @noRd
.fit_predict_xgb <- function(train_data, test_data, task, model_params,
                             mc_info = NULL) {
  # Preparar matrices
  X_train <- as.matrix(train_data[, -1, drop = FALSE])
  X_test <- as.matrix(test_data[, -1, drop = FALSE])

  if (task == "classification") {

    if (!is.null(mc_info) && mc_info$is_multiclass) {
      # Multiclass: multi:softprob
      K <- mc_info$n_classes
      y_train <- as.integer(train_data$y) - 1L  # 0-indexed
      dtrain <- xgboost::xgb.DMatrix(data = X_train, label = y_train)
      dtest <- xgboost::xgb.DMatrix(data = X_test)

      params <- list(
        objective = "multi:softprob",
        num_class = K,
        eval_metric = "mlogloss",
        max_depth = if (model_params$max.depth == 0) 6 else model_params$max.depth,
        eta = 0.1
      )

      fit <- xgboost::xgb.train(
        params = params,
        data = dtrain,
        nrounds = model_params$num.trees %/% 10,
        verbose = 0
      )

      raw_pred <- predict(fit, dtest)
      pred <- matrix(raw_pred, ncol = K, byrow = TRUE)
      colnames(pred) <- mc_info$class_levels

    } else {
      # Binary (original code)
      y_train <- as.numeric(as.character(train_data$y))
      dtrain <- xgboost::xgb.DMatrix(data = X_train, label = y_train)
      dtest <- xgboost::xgb.DMatrix(data = X_test)

      params <- list(
        objective = "binary:logistic",
        eval_metric = "auc",
        max_depth = if (model_params$max.depth == 0) 6 else model_params$max.depth,
        eta = 0.1
      )

      fit <- xgboost::xgb.train(
        params = params,
        data = dtrain,
        nrounds = model_params$num.trees %/% 10,
        verbose = 0
      )

      pred <- predict(fit, dtest)
    }
  } else {
    y_train <- train_data$y
    dtrain <- xgboost::xgb.DMatrix(data = X_train, label = y_train)
    dtest <- xgboost::xgb.DMatrix(data = X_test)

    params <- list(
      objective = "reg:squarederror",
      max_depth = if (model_params$max.depth == 0) 6 else model_params$max.depth,
      eta = 0.1
    )

    fit <- xgboost::xgb.train(
      params = params,
      data = dtrain,
      nrounds = model_params$num.trees %/% 10,
      verbose = 0
    )

    pred <- predict(fit, dtest)
  }

  pred
}


#' Fit an SVM and Predict
#' @noRd
.fit_predict_svm <- function(train_data, test_data, task, model_params,
                             mc_info = NULL) {
  if (task == "classification") {
    fit <- e1071::svm(y ~ ., data = train_data, probability = TRUE, kernel = "radial")
    pred_obj <- predict(fit, test_data, probability = TRUE)
    prob_mat <- attr(pred_obj, "probabilities")

    if (!is.null(mc_info) && mc_info$is_multiclass) {
      # Multiclass: return full N x K probability matrix with consistent column order
      pred <- prob_mat[, mc_info$class_levels, drop = FALSE]
    } else {
      # Binary: return vector of P(class=1)
      pred <- prob_mat[, "1"]
    }
  } else {
    fit <- e1071::svm(y ~ ., data = train_data, kernel = "radial")
    pred <- predict(fit, test_data)
  }

  pred
}


#' Fit a GLM and Predict
#' @noRd
.fit_predict_glm <- function(train_data, test_data, task, mc_info = NULL) {
  if (task == "classification") {

    if (!is.null(mc_info) && mc_info$is_multiclass) {
      # Multiclass: use nnet::multinom
      if (!requireNamespace("nnet", quietly = TRUE)) {
        stop("Package 'nnet' is required for multiclass GLM. Install package 'nnet' to use this option.")
      }
      fit <- nnet::multinom(y ~ ., data = train_data, trace = FALSE)
      pred <- predict(fit, newdata = test_data, type = "probs")
      # If only 1 observation, predict returns a named vector instead of matrix
      if (is.null(dim(pred))) {
        pred <- matrix(pred, nrow = 1, dimnames = list(NULL, names(pred)))
      }
      # Ensure column order matches class_levels
      pred <- pred[, mc_info$class_levels, drop = FALSE]
    } else {
      # Binary (original code)
      fit <- glm(y ~ ., data = train_data, family = binomial(link = "logit"))
      pred <- predict(fit, newdata = test_data, type = "response")
    }
  } else {
    fit <- lm(y ~ ., data = train_data)
    pred <- predict(fit, newdata = test_data)
  }

  pred
}


#' Compute an Evaluation Metric
#'
#' Uses canonical metric names (yardstick convention):
#'   roc_auc, accuracy, f_meas, kap, mcc, bal_accuracy (classification)
#'   rmse, mae, rsq (regression)
#' @noRd
.compute_metric <- function(y_true, y_pred, task, metric,
                            threshold = 0.5, positive = "1",
                            mc_info = NULL) {

  if (task == "classification") {

    # --- Multiclass path ---
    if (!is.null(mc_info) && mc_info$is_multiclass) {
      # y_pred is an N x K probability matrix
      class_levels <- mc_info$class_levels
      K <- mc_info$n_classes
      y_true <- factor(y_true, levels = class_levels)

      if (metric == "roc_auc") {
        # Hand-Till multiclass AUC via pROC
        return(as.numeric(pROC::multiclass.roc(y_true, y_pred, quiet = TRUE)$auc))
      }

      # Predicted class = argmax of probability matrix
      y_hat <- class_levels[apply(y_pred, 1, which.max)]
      y_hat <- factor(y_hat, levels = class_levels)

      if (metric == "accuracy") {
        return(mean(y_hat == y_true))
      }

      if (metric == "f_meas") {
        # Macro-F1: average of per-class F1 (one-vs-rest)
        f1_per_class <- vapply(class_levels, function(cl) {
          tp <- sum(y_hat == cl & y_true == cl)
          fp <- sum(y_hat == cl & y_true != cl)
          fn <- sum(y_hat != cl & y_true == cl)
          prec <- if (tp + fp == 0) 0 else tp / (tp + fp)
          rec  <- if (tp + fn == 0) 0 else tp / (tp + fn)
          if (prec + rec == 0) 0 else 2 * prec * rec / (prec + rec)
        }, numeric(1))
        return(mean(f1_per_class))
      }

      # Confusion matrix for kap, mcc, bal_accuracy
      if (metric %in% c("kap", "mcc", "bal_accuracy")) {
        N <- length(y_true)
        # Build K x K confusion matrix: C[i,j] = count(true=i, pred=j)
        C <- table(True = y_true, Pred = y_hat)
        # Ensure full K x K even if some classes absent in predictions
        full_C <- matrix(0L, nrow = K, ncol = K,
                         dimnames = list(class_levels, class_levels))
        for (r in rownames(C)) {
          for (cc in colnames(C)) {
            full_C[r, cc] <- C[r, cc]
          }
        }
        C <- full_C
      }

      if (metric == "kap") {
        # Cohen's Kappa: (accuracy - p_e) / (1 - p_e)
        N <- sum(C)
        acc <- sum(diag(C)) / N
        # Expected accuracy under independence
        row_sums <- rowSums(C)
        col_sums <- colSums(C)
        p_e <- sum(row_sums * col_sums) / (N^2)
        if (p_e == 1) return(NA_real_)
        return((acc - p_e) / (1 - p_e))
      }

      if (metric == "mcc") {
        # Generalized MCC for K x K confusion matrix (Gorodkin, 2004)
        N <- sum(C)
        c_correct <- sum(diag(C))  # trace
        s <- N  # total
        p_k <- colSums(C)  # column sums (predicted)
        t_k <- rowSums(C)  # row sums (true)
        denom <- sqrt(s^2 - sum(p_k^2)) * sqrt(s^2 - sum(t_k^2))
        if (denom == 0) return(NA_real_)
        return((c_correct * s - sum(p_k * t_k)) / denom)
      }

      if (metric == "bal_accuracy") {
        # Mean per-class recall (sensitivity)
        recall_per_class <- vapply(class_levels, function(cl) {
          tp <- sum(y_hat == cl & y_true == cl)
          fn <- sum(y_hat != cl & y_true == cl)
          if (tp + fn == 0) return(0)
          tp / (tp + fn)
        }, numeric(1))
        return(mean(recall_per_class))
      }

      stop("Unsupported classification metric: ", metric)
    }

    # --- Binary path ---
    y_true <- factor(y_true, levels = c("0", "1"))

    if (metric == "roc_auc") {
      return(as.numeric(pROC::auc(pROC::roc(y_true, y_pred, quiet = TRUE))))
    }

    # Para otras metricas, binarizar predicciones
    y_hat <- ifelse(y_pred >= threshold, "1", "0")
    y_hat <- factor(y_hat, levels = c("0", "1"))

    tp <- as.numeric(sum(y_hat == "1" & y_true == "1"))
    tn <- as.numeric(sum(y_hat == "0" & y_true == "0"))
    fp <- as.numeric(sum(y_hat == "1" & y_true == "0"))
    fn <- as.numeric(sum(y_hat == "0" & y_true == "1"))

    if (metric == "accuracy") {
      return((tp + tn) / (tp + tn + fp + fn))
    }

    if (metric == "f_meas") {
      prec <- if (tp + fp == 0) NA else tp / (tp + fp)
      rec <- if (tp + fn == 0) NA else tp / (tp + fn)
      f1 <- if (is.na(prec) || is.na(rec) || (prec + rec) == 0) NA else 2 * prec * rec / (prec + rec)
      return(f1)
    }

    if (metric == "kap") {
      # Cohen's Kappa (binary)
      N <- tp + tn + fp + fn
      acc <- (tp + tn) / N
      p_e <- ((tp + fp) * (tp + fn) + (tn + fn) * (tn + fp)) / (N^2)
      if (p_e == 1) return(NA_real_)
      return((acc - p_e) / (1 - p_e))
    }

    if (metric == "mcc") {
      # Matthews Correlation Coefficient (binary)
      denom <- sqrt((tp + fp) * (tp + fn) * (tn + fp) * (tn + fn))
      if (denom == 0) return(NA_real_)
      return((tp * tn - fp * fn) / denom)
    }

    if (metric == "bal_accuracy") {
      # (Sensitivity + Specificity) / 2
      sens <- if (tp + fn == 0) 0 else tp / (tp + fn)
      spec <- if (tn + fp == 0) 0 else tn / (tn + fp)
      return((sens + spec) / 2)
    }

    stop("Unsupported classification metric: ", metric)
  }

  # Regresion
  err <- y_true - y_pred

  if (metric == "rmse") return(sqrt(mean(err^2)))
  if (metric == "mae") return(mean(abs(err)))
  if (metric == "rsq") {
    ss_res <- sum(err^2)
    ss_tot <- sum((y_true - mean(y_true))^2)
    return(1 - ss_res / ss_tot)
  }

  stop("Unsupported regression metric: ", metric)
}


#' Fit a Power Law Curve
#' @noRd
.fit_power_law <- function(df_summary, target_dir) {
  df <- df_summary[order(df_summary$n), ]

  # Usar regresion lineal en log-log para estimar valores iniciales
  # log(mean - a) = log(b) - c * log(n)  para higher_better
  # log(mean - a) = log(b) - c * log(n)  para lower_better (con signo ajustado)

  if (target_dir == "higher_better") {
    # AUC(n) = a - b * n^(-c)  (crece hacia asintota a)
    # Estimar asintota como max observado + pequeno margen basado en la tendencia
    max_mean <- max(df$mean)
    min_mean <- min(df$mean)
    rango <- max_mean - min_mean

    # Asintota realista: max + 5-10% del rango observado
    a0 <- max_mean + rango * 0.05
    # Limitar asintota para AUC a 1.0
    if (a0 > 1) a0 <- min(max_mean * 1.02, 0.99)

    b0 <- a0 - min_mean
    c0 <- 0.5

    fit <- tryCatch({
      nls(
        mean ~ a - b * n^(-c),
        data = df,
        start = list(a = a0, b = b0, c = c0),
        algorithm = "port",
        lower = c(max_mean, 0.001, 0.01),
        upper = c(min(a0 * 1.1, 1), rango * 2, 3),
        control = list(maxiter = 200, warnOnly = TRUE)
      )
    }, error = function(e) {
      # Fallback: ajuste mas simple
      nls(
        mean ~ a - b * n^(-c),
        data = df,
        start = list(a = a0, b = b0, c = c0),
        control = list(maxiter = 200, warnOnly = TRUE)
      )
    })

  } else {
    # RMSE(n) = a + b * n^(-c)  (decrece hacia asintota a)
    max_mean <- max(df$mean)
    min_mean <- min(df$mean)
    rango <- max_mean - min_mean

    # Asintota realista: min - 5% del rango
    a0 <- max(min_mean - rango * 0.05, 0)

    b0 <- max_mean - a0
    c0 <- 0.5

    fit <- tryCatch({
      nls(
        mean ~ a + b * n^(-c),
        data = df,
        start = list(a = a0, b = b0, c = c0),
        algorithm = "port",
        lower = c(0, 0.001, 0.01),
        upper = c(min_mean, rango * 2, 3),
        control = list(maxiter = 200, warnOnly = TRUE)
      )
    }, error = function(e) {
      # Fallback
      nls(
        mean ~ a + b * n^(-c),
        data = df,
        start = list(a = a0, b = b0, c = c0),
        control = list(maxiter = 200, warnOnly = TRUE)
      )
    })
  }

  fit
}
