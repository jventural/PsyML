# =============================================================================
# Interpretability functions (variable importance and SHAP) for PsyML
# =============================================================================

#' Model Interpretability Analysis
#'
#' @description
#' Computes variable importance and Shapley (SHAP) values for a fitted
#' workflow returned by [evaluate_model()].
#'
#' @param evaluation_result Result of [evaluate_model()]; it must contain the
#'   element `final_fit` (a fitted `workflow`).
#' @param train_data Training data frame (used as background data for SHAP).
#' @param test_data Test data frame (used to compute importance and SHAP).
#' @param target Name of the outcome variable (character).
#' @param n_shap Number of test observations used to compute SHAP values.
#' @param verbose Logical. If `TRUE`, progress is printed to the console.
#'
#' @return A list of class `supervisedml_interpret` with elements
#'   `importance` (data frame returned by [get_importance()]) and `shap`
#'   (list returned by [calculate_shap()], or `NULL` if SHAP values could not
#'   be computed).
#'
#' @examples
#' \donttest{
#' set.seed(1)
#' n <- 200
#' df <- data.frame(x1 = rnorm(n), x2 = rnorm(n), x3 = rnorm(n))
#' df$y <- factor(ifelse(df$x1 + 0.5 * df$x2 + rnorm(n) > 0, "yes", "no"))
#' idx <- sample(n, 150)
#' rec <- recipes::recipe(y ~ ., data = df[idx, ])
#' wf <- workflows::workflow() |>
#'   workflows::add_recipe(rec) |>
#'   workflows::add_model(parsnip::logistic_reg() |> parsnip::set_engine("glm"))
#' fit <- parsnip::fit(wf, data = df[idx, ])
#' res <- interpret_model(list(final_fit = fit), df[idx, ], df[-idx, ], "y",
#'                        n_shap = 20, verbose = FALSE)
#' res$importance
#' }
#' @family supervised learning
#' @export
interpret_model <- function(evaluation_result,
                            train_data,
                            test_data,
                            target,
                            n_shap = 100,
                            verbose = TRUE) {

  if (verbose) {
    .print_section(6, "Interpretabilidad del Modelo")
  }

  results <- list()
  final_fit <- evaluation_result$final_fit

  # 6.1 Variable importance
  if (verbose) .print_subsection(6, 1, "Importancia de Variables")
  importance <- get_importance(final_fit, data = test_data, target = target,
                               verbose = verbose)
  results$importance <- importance
  if (verbose) .print_reference("importance")

  # 6.2 SHAP values
  if (verbose) .print_subsection(6, 2, "Valores SHAP")

  shap_result <- tryCatch({
    calculate_shap(
      final_fit = final_fit,
      train_data = train_data,
      test_data = test_data,
      target = target,
      n_samples = n_shap,
      verbose = verbose
    )
  }, error = function(e) {
    if (verbose) {
      cat("    [!] No se pudo calcular SHAP:", conditionMessage(e), "\n")
    }
    NULL
  })

  results$shap <- shap_result
  if (verbose) .print_reference("shap")

  class(results) <- c("supervisedml_interpret", "list")
  return(results)
}


#' Variable Importance of a Fitted Workflow
#'
#' @description
#' Extracts variable importance from a fitted `workflow`. With
#' `method = "auto"`, a model-specific measure is used when available:
#' impurity importance for `ranger` and `rpart`, gain for `xgboost`, absolute
#' coefficients for `glmnet`, and absolute z/t statistics for `glm`/`lm`. For
#' other engines (for example `nnet` or `kernlab`), or with
#' `method = "permutation"`, permutation importance is computed on `data`:
#' the drop in performance (AUC for binary outcomes, accuracy for multiclass
#' outcomes, RMSE for numeric outcomes) after shuffling each predictor.
#'
#' @param final_fit A fitted `workflow` (for example `result$final_fit` from
#'   [supervised_ml()]).
#' @param data Optional data frame with the predictors and the outcome, used
#'   for permutation importance. If `NULL`, only model-specific importance
#'   can be returned.
#' @param target Name of the outcome variable in `data`.
#' @param method One of `"auto"` (model-specific if available, otherwise
#'   permutation), `"native"`, or `"permutation"`.
#' @param nsim Number of permutations per predictor.
#' @param verbose Logical. If `TRUE`, the top 10 variables are printed.
#'
#' @return A data frame with columns `Variable`, `Importance`, and `Rank`,
#'   sorted by decreasing importance. The attribute `"method"` records the
#'   importance measure used. An empty data frame is returned when importance
#'   cannot be computed.
#'
#' @examples
#' set.seed(1)
#' df <- data.frame(x1 = rnorm(100), x2 = rnorm(100))
#' df$y <- 2 * df$x1 + rnorm(100)
#' wf <- workflows::workflow() |>
#'   workflows::add_formula(y ~ .) |>
#'   workflows::add_model(parsnip::linear_reg() |> parsnip::set_engine("lm"))
#' fit <- parsnip::fit(wf, data = df)
#' get_importance(fit, verbose = FALSE)
#' get_importance(fit, data = df, target = "y", method = "permutation",
#'                verbose = FALSE)
#' @family supervised learning
#' @export
get_importance <- function(final_fit, data = NULL, target = NULL,
                           method = c("auto", "native", "permutation"),
                           nsim = 5, verbose = TRUE) {

  method <- match.arg(method)
  empty <- data.frame(Variable = character(), Importance = numeric(),
                      Rank = integer())

  imp_df <- NULL
  used <- NA_character_
  if (method %in% c("auto", "native")) {
    imp_df <- tryCatch(.native_importance(final_fit), error = function(e) NULL)
    if (!is.null(imp_df)) used <- attr(imp_df, "method")
  }
  if (is.null(imp_df) && method %in% c("auto", "permutation") &&
      !is.null(data) && !is.null(target)) {
    imp_df <- tryCatch(.permutation_importance(final_fit, data, target, nsim),
                       error = function(e) {
                         if (verbose) {
                           cat("    [!] No se pudo extraer importancia:",
                               conditionMessage(e), "\n")
                         }
                         NULL
                       })
    if (!is.null(imp_df)) used <- "permutation"
  }

  if (is.null(imp_df) || nrow(imp_df) == 0) {
    if (verbose) cat("    [!] No se pudo extraer importancia de variables\n")
    return(empty)
  }

  imp_df <- imp_df[is.finite(imp_df$Importance), , drop = FALSE]
  imp_df <- imp_df[order(-imp_df$Importance), , drop = FALSE]
  rownames(imp_df) <- NULL
  imp_df$Rank <- seq_len(nrow(imp_df))
  attr(imp_df, "method") <- used

  if (verbose && nrow(imp_df) > 0) {
    cat("    Top 10 variables mas importantes (", used, "):\n\n", sep = "")
    top10 <- utils::head(imp_df, 10)
    for (i in seq_len(nrow(top10))) {
      cat("    ", i, ". ", top10$Variable[i], " (",
          round(top10$Importance[i], 3), ")\n", sep = "")
    }
  }

  imp_df
}


# Model-specific importance from the engine fit of a workflow.
# Returns NULL when the engine has no built-in measure.
#' @noRd
.native_importance <- function(final_fit) {
  pfit <- if (inherits(final_fit, "workflow")) {
    workflows::extract_fit_parsnip(final_fit)
  } else {
    final_fit
  }
  eng <- if (inherits(pfit, "model_fit")) pfit$fit else pfit

  out <- NULL
  if (inherits(eng, "ranger")) {
    vi <- eng$variable.importance
    if (!is.null(vi) && length(vi) > 0) {
      out <- data.frame(Variable = names(vi), Importance = as.numeric(vi))
      attr(out, "method") <- "impurity"
    }
  } else if (inherits(eng, "rpart")) {
    vi <- eng$variable.importance
    if (!is.null(vi) && length(vi) > 0) {
      out <- data.frame(Variable = names(vi), Importance = as.numeric(vi))
      attr(out, "method") <- "impurity"
    }
  } else if (inherits(eng, "xgb.Booster")) {
    tab <- xgboost::xgb.importance(model = eng)
    if (nrow(tab) > 0) {
      out <- data.frame(Variable = tab$Feature, Importance = tab$Gain)
      attr(out, "method") <- "gain"
    }
  } else if (inherits(eng, "glmnet") && inherits(pfit, "model_fit")) {
    td <- as.data.frame(parsnip::tidy(pfit))
    td <- td[td$term != "(Intercept)", , drop = FALSE]
    if (nrow(td) > 0) {
      agg <- stats::aggregate(abs(td$estimate), by = list(Variable = td$term),
                              FUN = mean)
      out <- data.frame(Variable = agg$Variable, Importance = agg$x)
      attr(out, "method") <- "abs_coefficient"
    }
  } else if (inherits(eng, "lm")) {
    co <- summary(eng)$coefficients
    co <- co[rownames(co) != "(Intercept)", , drop = FALSE]
    if (nrow(co) > 0) {
      out <- data.frame(Variable = rownames(co), Importance = abs(co[, 3]))
      attr(out, "method") <- "abs_statistic"
    }
  }
  out
}


# Permutation importance on the original predictors of a fitted workflow.
#' @noRd
.permutation_importance <- function(final_fit, data, target, nsim = 5) {
  data <- as.data.frame(data)
  predictors <- setdiff(names(data), target)
  y <- data[[target]]
  is_reg <- is.numeric(y)
  binary <- !is_reg && nlevels(factor(y)) == 2

  score <- function(d) {
    if (is_reg) {
      p <- stats::predict(final_fit, new_data = d)$.pred
      -sqrt(mean((y - p)^2, na.rm = TRUE))
    } else if (binary) {
      pr <- stats::predict(final_fit, new_data = d, type = "prob")
      lv <- levels(factor(y))
      as.numeric(pROC::auc(pROC::roc(factor(y, levels = lv), pr[[2]],
                                     levels = lv, direction = "<",
                                     quiet = TRUE)))
    } else {
      pc <- stats::predict(final_fit, new_data = d, type = "class")$.pred_class
      mean(as.character(pc) == as.character(y), na.rm = TRUE)
    }
  }

  base <- score(data)
  imp <- vapply(predictors, function(v) {
    drops <- vapply(seq_len(nsim), function(s) {
      d <- data
      d[[v]] <- d[[v]][sample.int(nrow(d))]
      base - score(d)
    }, numeric(1))
    mean(drops)
  }, numeric(1))
  data.frame(Variable = predictors, Importance = as.numeric(imp))
}


#' SHAP Values of a Fitted Workflow
#'
#' @description
#' Computes Kernel SHAP values with [kernelshap::kernelshap()] for a sample
#' of test observations, using a sample of the training data as background.
#' For binary classification the explained output is the probability of the
#' second class level; for multiclass outcomes the mean absolute SHAP value
#' across classes is returned. Predictors removed by the preprocessing recipe
#' are excluded from the output.
#'
#' @param final_fit A fitted `workflow`.
#' @param train_data Training data frame (background data).
#' @param test_data Test data frame (observations to explain).
#' @param target Name of the outcome variable.
#' @param n_samples Number of test observations to explain. In classification
#'   the sample is stratified by class.
#' @param bg_n Number of background observations sampled from `train_data`.
#' @param verbose Logical. If `TRUE`, progress is printed to the console.
#'
#' @return `NULL` if the package \pkg{kernelshap} is not installed or the
#'   computation fails; otherwise a list with elements `shap_values` (data
#'   frame of SHAP values, one column per predictor), `importance` (data frame
#'   with columns `Variable` and `Mean_Abs_SHAP`), `test_sample` (explained
#'   observations), `removed_vars` (predictors dropped by the recipe),
#'   `collinear_vars` (predictors with VIF > 5), and `sample_design`.
#'
#' @examples
#' \donttest{
#' if (requireNamespace("kernelshap", quietly = TRUE)) {
#'   set.seed(1)
#'   df <- data.frame(x1 = rnorm(120), x2 = rnorm(120), x3 = rnorm(120))
#'   df$y <- df$x1 - df$x2 + rnorm(120)
#'   wf <- workflows::workflow() |>
#'     workflows::add_formula(y ~ .) |>
#'     workflows::add_model(parsnip::linear_reg() |> parsnip::set_engine("lm"))
#'   fit <- parsnip::fit(wf, data = df[1:100, ])
#'   sh <- calculate_shap(fit, df[1:100, ], df[101:120, ], "y",
#'                        n_samples = 20, bg_n = 30, verbose = FALSE)
#'   sh$importance
#' }
#' }
#' @family supervised learning
#' @export
calculate_shap <- function(final_fit,
                           train_data,
                           test_data,
                           target,
                           n_samples = 100,
                           bg_n = 50,
                           verbose = TRUE) {

  if (!requireNamespace("kernelshap", quietly = TRUE)) {
    if (verbose) cat("    [!] Paquete kernelshap no instalado\n")
    return(NULL)
  }

  predictors <- setdiff(names(train_data), target)

  # Variables actually used by the model: the recipe can drop predictors
  # (high correlation, VIF, zero variance). They are excluded from the output.
  used_predictors <- .shap_used_predictors(final_fit, train_data, predictors)
  removed_vars <- setdiff(predictors, used_predictors)

  # Sample of observations to explain (stratified by class in classification)
  target_test <- test_data[[target]]
  n_test <- min(n_samples, nrow(test_data))
  if (!is.numeric(target_test)) {
    idx <- .shap_stratified_index(target_test, n_test)
    sample_design <- "estratificado (mismo numero de casos por clase)"
  } else {
    idx <- sample(nrow(test_data), n_test)
    sample_design <- "aleatorio"
  }
  test_sample <- test_data[idx, ]
  n_test <- length(idx)

  if (verbose) {
    cat("    Calculando SHAP para", n_test, "observaciones (muestreo",
        sample_design, ")...\n")
    if (!is.numeric(target_test)) {
      tab <- table(test_sample[[target]])
      cat("    Casos por clase:", paste(names(tab), tab, sep = " = ", collapse = ", "), "\n")
    }
  }

  target_vec <- train_data[[target]]
  is_regression <- is.numeric(target_vec)
  n_classes <- if (is_regression) 0 else length(levels(factor(target_vec)))
  is_multiclass <- !is_regression && n_classes >= 3

  X_shap <- as.data.frame(test_sample[, predictors, drop = FALSE])
  bg_idx <- sample(nrow(train_data), min(bg_n, nrow(train_data)))
  bg_X <- as.data.frame(train_data[bg_idx, predictors, drop = FALSE])

  pred_fun <- if (is_regression) {
    function(model, newdata) {
      as.numeric(stats::predict(model, new_data = as.data.frame(newdata))$.pred)
    }
  } else if (is_multiclass) {
    function(model, newdata) {
      as.matrix(stats::predict(model, new_data = as.data.frame(newdata),
                               type = "prob"))
    }
  } else {
    function(model, newdata) {
      stats::predict(model, new_data = as.data.frame(newdata),
                     type = "prob")[[2]]
    }
  }

  if (verbose && is_multiclass) {
    cat("    Calculando SHAP por clase (", n_classes, " clases)...\n", sep = "")
  }

  shap_values <- tryCatch({
    ks <- kernelshap::kernelshap(final_fit, X = X_shap, bg_X = bg_X,
                                 pred_fun = pred_fun, verbose = FALSE)
    S <- ks$S
    if (is.list(S)) {
      # Multiclass: mean |SHAP| across classes
      S <- Reduce("+", lapply(S, abs)) / length(S)
    }
    as.data.frame(S)
  }, error = function(e) {
    if (verbose) cat("    Error en kernelshap:", conditionMessage(e), "\n")
    NULL
  })

  if (is.null(shap_values)) {
    return(NULL)
  }

  keep <- intersect(colnames(shap_values), used_predictors)
  shap_values <- shap_values[, keep, drop = FALSE]

  shap_importance <- data.frame(
    Variable = colnames(shap_values),
    Mean_Abs_SHAP = colMeans(abs(shap_values))
  )
  shap_importance <- shap_importance[order(-shap_importance$Mean_Abs_SHAP), ]
  rownames(shap_importance) <- NULL

  # With high VIF the split of SHAP among nearly redundant predictors is
  # arbitrary and should not be interpreted variable by variable.
  collinear_vars <- .shap_collinear(train_data, keep, threshold = 5)

  if (verbose) {
    cat("\n    Importancia SHAP (top 10):\n\n")
    top10 <- utils::head(shap_importance, 10)
    for (i in seq_len(nrow(top10))) {
      cat("    ", i, ". ", top10$Variable[i], " (",
          round(top10$Mean_Abs_SHAP[i], 4), ")\n", sep = "")
    }
    if (length(removed_vars) > 0) {
      cat("\n    [i] Variables eliminadas en el preprocesamiento (el modelo no las usa,\n",
          "        se excluyen del SHAP):", paste(removed_vars, collapse = ", "), "\n")
    }
    if (nrow(collinear_vars) > 0) {
      cat("\n    [!] Colinealidad entre predictores usados (VIF > 5):\n")
      for (i in seq_len(nrow(collinear_vars))) {
        cat("        -", collinear_vars$Variable[i], "(VIF =",
            round(collinear_vars$VIF[i], 1), ")\n")
      }
      cat("        El reparto del SHAP entre estas variables depende de cual elige\n",
          "       el modelo y NO debe interpretarse variable por variable. Para saber\n",
          "       cuanto discrimina cada una por si sola, use una medida univariada\n",
          "       (p. ej., AUC de cada predictor).\n")
    }
  }

  test_sample_clean <- as.data.frame(test_sample[, keep, drop = FALSE])
  rownames(test_sample_clean) <- NULL
  rownames(shap_values) <- NULL

  list(
    shap_values = shap_values,
    importance = shap_importance,
    test_sample = test_sample_clean,
    removed_vars = removed_vars,
    collinear_vars = collinear_vars,
    sample_design = sample_design
  )
}



# Predictores que el workflow ajustado realmente usa: los que sobreviven a la
# receta preparada. Una variable original cuenta como usada si aparece tal
# cual o como prefijo de una columna dummy (var_nivel). Si no se puede
# determinar, se devuelven todos (comportamiento anterior).
.shap_used_predictors <- function(final_fit, train_data, predictors) {
  tryCatch({
    rec <- workflows::extract_recipe(final_fit, estimated = TRUE)
    baked <- names(recipes::bake(rec, new_data = utils::head(train_data, 5)))
    used <- predictors[vapply(predictors, function(p) {
      p %in% baked || any(startsWith(baked, paste0(p, "_")))
    }, logical(1))]
    if (length(used) == 0) predictors else used
  }, error = function(e) predictors)
}


# Indices de una muestra con el mismo numero de casos por clase (hasta donde
# alcance cada clase); lo que falte se completa al azar con el resto.
.shap_stratified_index <- function(y, n) {
  y <- factor(y)
  por_clase <- floor(n / nlevels(y))
  idx <- unlist(lapply(levels(y), function(k) {
    pool <- which(y == k)
    if (length(pool) <= por_clase) pool else sample(pool, por_clase)
  }), use.names = FALSE)
  faltan <- n - length(idx)
  if (faltan > 0) {
    resto <- setdiff(seq_along(y), idx)
    idx <- c(idx, if (length(resto) <= faltan) resto else sample(resto, faltan))
  }
  idx
}


# VIF de las variables numericas usadas: diagonal de la inversa de la matriz
# de correlaciones. Devuelve las que superan el umbral.
.shap_collinear <- function(data, vars, threshold = 5) {
  vacio <- data.frame(Variable = character(), VIF = numeric())
  num <- vars[vapply(vars, function(v) is.numeric(data[[v]]), logical(1))]
  if (length(num) < 2) return(vacio)
  tryCatch({
    R <- stats::cor(data[, num, drop = FALSE], use = "pairwise.complete.obs")
    # si R es singular (variables exactamente redundantes), pseudoinversa por
    # SVD, sin depender de MASS
    inv <- tryCatch(solve(R), error = function(e) {
      s <- svd(R)
      d <- ifelse(s$d > max(dim(R)) * max(s$d) * .Machine$double.eps, 1 / s$d, 0)
      s$v %*% (d * t(s$u))
    })
    vif <- diag(inv)
    out <- data.frame(Variable = num, VIF = as.numeric(vif))
    out <- out[out$VIF > threshold, , drop = FALSE]
    out[order(-out$VIF), , drop = FALSE]
  }, error = function(e) vacio)
}


#' @title Plot Variable Importance
#'
#' @description
#' Horizontal bar chart of the most important variables.
#'
#' @param interpret_result A list with an `importance` element (data frame
#'   with columns `Variable` and `Importance`, sorted by decreasing
#'   importance), such as the result of [interpret_model()].
#' @param top_n Maximum number of variables shown (default: 15).
#'
#' @return A `ggplot` object, or NULL (with a message) when no importance
#'   data are available.
#'
#' @examples
#' set.seed(1)
#' df <- data.frame(x1 = rnorm(100), x2 = rnorm(100), x3 = rnorm(100))
#' df$y <- 2 * df$x1 - df$x2 + rnorm(100)
#' wf <- workflows::workflow() |>
#'   workflows::add_formula(y ~ .) |>
#'   workflows::add_model(parsnip::linear_reg() |> parsnip::set_engine("lm"))
#' fit <- parsnip::fit(wf, data = df)
#' interp <- list(importance = get_importance(fit, verbose = FALSE))
#' plot_importance(interp)
#'
#' @family supervised learning
#' @export
plot_importance <- function(interpret_result, top_n = 15) {

  importance <- interpret_result$importance

  if (is.null(importance) || nrow(importance) == 0) {
    message("No importance data available")
    return(NULL)
  }

  top_vars <- utils::head(importance, top_n)
  top_vars$Variable <- factor(top_vars$Variable,
                               levels = rev(top_vars$Variable))

  ggplot2::ggplot(top_vars, ggplot2::aes(x = Importance, y = Variable)) +
    ggplot2::geom_col(fill = "#3498db", alpha = 0.8) +
    ggplot2::labs(
      title = "Importancia de Variables",
      x = "Importancia",
      y = NULL
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold")
    )
}


#' @title Plot a SHAP Summary (Beeswarm)
#'
#' @description
#' Beeswarm plot of the SHAP values of the most important variables, colored
#' by the feature value, drawn with \pkg{shapviz} when it is installed.
#' Otherwise a boxplot of the SHAP values per variable is drawn.
#'
#' @param interpret_result A list with a `shap` element as returned by
#'   [calculate_shap()], such as the result of [interpret_model()] or the
#'   `interpretation` element of a [supervised_ml()] result.
#' @param top_n Maximum number of variables shown (default: 15).
#'
#' @return A `ggplot` object, or NULL (with a message) when no SHAP values
#'   are available.
#'
#' @examples
#' if (requireNamespace("kernelshap", quietly = TRUE)) {
#'   set.seed(1)
#'   df <- data.frame(x1 = rnorm(120), x2 = rnorm(120), x3 = rnorm(120))
#'   df$y <- df$x1 - df$x2 + rnorm(120)
#'   wf <- workflows::workflow() |>
#'     workflows::add_formula(y ~ .) |>
#'     workflows::add_model(parsnip::linear_reg() |> parsnip::set_engine("lm"))
#'   fit <- parsnip::fit(wf, data = df[1:100, ])
#'   sh <- calculate_shap(fit, df[1:100, ], df[101:120, ], "y",
#'                        n_samples = 20, bg_n = 30, verbose = FALSE)
#'   interp <- list(shap = sh)
#'   plot_shap_summary(interp)
#' }
#'
#' @family supervised learning
#' @export
plot_shap_summary <- function(interpret_result, top_n = 15) {

  if (is.null(interpret_result$shap) ||
      is.null(interpret_result$shap$shap_values)) {
    message("No SHAP values available")
    return(NULL)
  }

  shap_values <- interpret_result$shap$shap_values
  test_sample <- interpret_result$shap$test_sample

  # Usar shapviz para beeswarm plot
  if (requireNamespace("shapviz", quietly = TRUE)) {
    X_data <- as.data.frame(test_sample)
    sv <- shapviz::shapviz(as.matrix(shap_values), X = X_data)
    p <- shapviz::sv_importance(sv, kind = "beeswarm", show_numbers = TRUE,
                                max_display = top_n,
                                viridis_args = list(option = "D")) +
      ggplot2::theme_bw() +
      ggplot2::labs(title = "SHAP Values (Impact on Model Output)",
                    x = "", y = "SHAP value", color = "Feature Value")
    return(p)
  }

  # Fallback si shapviz no esta instalado: boxplot manual
  importance <- interpret_result$shap$importance
  top_vars <- utils::head(importance$Variable, top_n)
  shap_df <- as.data.frame(shap_values[, top_vars, drop = FALSE])
  shap_long <- tidyr::pivot_longer(
    shap_df,
    cols = dplyr::everything(),
    names_to = "Variable",
    values_to = "SHAP"
  )
  shap_long$Variable <- factor(shap_long$Variable, levels = rev(top_vars))

  ggplot2::ggplot(shap_long, ggplot2::aes(x = SHAP, y = Variable)) +
    ggplot2::geom_boxplot(fill = "#e74c3c", alpha = 0.6, outlier.size = 0.5) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
    ggplot2::labs(
      title = "Distribucion de Valores SHAP",
      x = "Valor SHAP",
      y = NULL
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold")
    )
}


#' @title Plot SHAP Feature Importance
#'
#' @description
#' Bar chart of the mean absolute SHAP value of the most important
#' variables, drawn with \pkg{shapviz} when it is installed (otherwise with
#' \pkg{ggplot2}).
#'
#' @param interpret_result A list with a `shap` element as returned by
#'   [calculate_shap()], such as the result of [interpret_model()] or the
#'   `interpretation` element of a [supervised_ml()] result.
#' @param top_n Maximum number of variables shown (default: 15).
#'
#' @return A `ggplot` object, or NULL (with a message) when no SHAP values
#'   are available.
#'
#' @examples
#' if (requireNamespace("kernelshap", quietly = TRUE)) {
#'   set.seed(1)
#'   df <- data.frame(x1 = rnorm(120), x2 = rnorm(120), x3 = rnorm(120))
#'   df$y <- df$x1 - df$x2 + rnorm(120)
#'   wf <- workflows::workflow() |>
#'     workflows::add_formula(y ~ .) |>
#'     workflows::add_model(parsnip::linear_reg() |> parsnip::set_engine("lm"))
#'   fit <- parsnip::fit(wf, data = df[1:100, ])
#'   sh <- calculate_shap(fit, df[1:100, ], df[101:120, ], "y",
#'                        n_samples = 20, bg_n = 30, verbose = FALSE)
#'   interp <- list(shap = sh)
#'   plot_shap_bar(interp)
#' }
#'
#' @family supervised learning
#' @export
plot_shap_bar <- function(interpret_result, top_n = 15) {

  if (is.null(interpret_result$shap) ||
      is.null(interpret_result$shap$shap_values)) {
    message("No SHAP values available")
    return(NULL)
  }

  shap_values <- interpret_result$shap$shap_values
  test_sample <- interpret_result$shap$test_sample

  # Usar shapviz para bar plot
  if (requireNamespace("shapviz", quietly = TRUE)) {
    X_data <- as.data.frame(test_sample)
    sv <- shapviz::shapviz(as.matrix(shap_values), X = X_data)
    p <- shapviz::sv_importance(sv, kind = "bar", max_display = top_n) +
      ggplot2::theme_bw() +
      ggplot2::labs(title = "SHAP Feature Importance",
                    x = "", y = "mean(|SHAP value|)")
    return(p)
  }

  # Fallback: barras manuales con mean(|SHAP|)
  importance <- interpret_result$shap$importance
  top_vars <- utils::head(importance, top_n)
  top_vars$Variable <- factor(top_vars$Variable,
                               levels = rev(top_vars$Variable))

  ggplot2::ggplot(top_vars,
                  ggplot2::aes(x = Mean_Abs_SHAP, y = Variable)) +
    ggplot2::geom_col(fill = "#33D1FF", color = "gray32", alpha = 0.8) +
    ggplot2::labs(
      title = "SHAP Feature Importance",
      x = "mean(|SHAP value|)",
      y = NULL
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold")
    )
}


#' @title Plot SHAP Dependence for One Variable
#'
#' @description
#' Scatter plot of the SHAP values of one variable against its observed
#' values in the explained sample, with a loess smoother.
#'
#' @param interpret_result A list with a `shap` element as returned by
#'   [calculate_shap()], such as the result of [interpret_model()] or the
#'   `interpretation` element of a [supervised_ml()] result.
#' @param variable Character string with the name of the variable to plot.
#' @param test_data Not used; the explained sample stored in
#'   `interpret_result$shap$test_sample` is plotted. Kept for backward
#'   compatibility.
#'
#' @return A `ggplot` object, or NULL (with a message) when no SHAP values
#'   are available or `variable` is not among them.
#'
#' @examples
#' if (requireNamespace("kernelshap", quietly = TRUE)) {
#'   set.seed(1)
#'   df <- data.frame(x1 = rnorm(120), x2 = rnorm(120), x3 = rnorm(120))
#'   df$y <- df$x1 - df$x2 + rnorm(120)
#'   wf <- workflows::workflow() |>
#'     workflows::add_formula(y ~ .) |>
#'     workflows::add_model(parsnip::linear_reg() |> parsnip::set_engine("lm"))
#'   fit <- parsnip::fit(wf, data = df[1:100, ])
#'   sh <- calculate_shap(fit, df[1:100, ], df[101:120, ], "y",
#'                        n_samples = 20, bg_n = 30, verbose = FALSE)
#'   interp <- list(shap = sh)
#'   plot_shap_dependence(interp, variable = "x1", test_data = df[101:120, ])
#' }
#'
#' @family supervised learning
#' @export
plot_shap_dependence <- function(interpret_result, variable, test_data) {

  if (is.null(interpret_result$shap)) {
    message("No SHAP values available")
    return(NULL)
  }

  shap_values <- interpret_result$shap$shap_values
  test_sample <- interpret_result$shap$test_sample

  if (!variable %in% colnames(shap_values)) {
    message("Variable not found in the SHAP values")
    return(NULL)
  }

  plot_df <- data.frame(
    Feature_Value = test_sample[[variable]],
    SHAP_Value = shap_values[, variable]
  )

  ggplot2::ggplot(plot_df, ggplot2::aes(x = Feature_Value, y = SHAP_Value)) +
    ggplot2::geom_point(alpha = 0.6, color = "#9b59b6") +
    ggplot2::geom_smooth(method = "loess", se = FALSE, color = "#2c3e50") +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    ggplot2::labs(
      title = paste("Dependencia SHAP:", variable),
      x = variable,
      y = "Valor SHAP"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold")
    )
}
