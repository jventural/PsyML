# =============================================================================
# causal_ml: Inferencia Causal Automatizada para PsyML
# =============================================================================

#' Automated Causal Inference Pipeline
#'
#' @description
#' Complete causal inference pipeline for observational data. It includes a
#' causal exploratory analysis, propensity score methods (matching, inverse
#' probability weighting and augmented IPW), Double Machine Learning, causal
#' forests, meta-learners (S, T and X) and, optionally, TMLE and Python
#' methods (EconML through 'reticulate').
#'
#' @details
#' Propensity score methods and meta-learners are only run for binary
#' treatments. Several methods rely on suggested packages that are used only
#' when installed: 'MatchIt' (matching), 'cobalt' (love plot), 'grf' (causal
#' forest), 'DoubleML', 'mlr3' and 'mlr3learners' (DML), 'tmle' and
#' 'SuperLearner' (TMLE) and 'reticulate' (Python methods). When one of them is
#' missing, the corresponding method is skipped with a warning.
#'
#' @param data A data frame with the data.
#' @param treatment Character string with the name of the treatment variable.
#' @param outcome Character string with the name of the outcome variable.
#' @param covariates Character vector with the names of the covariates. If
#'   NULL (default), all remaining columns are used.
#' @param methods Character vector with the methods to run: any of
#'   `"propensity"`, `"dml"`, `"causal_forest"` and `"meta_learners"`
#'   (default: all four).
#' @param treatment_type Treatment type: `"auto"` (default, detected from the
#'   data), `"binary"` or `"continuous"`.
#' @param ps_model Model used to estimate the propensity score: `"logistic"`
#'   (default), `"rf"` or `"xgboost"`.
#' @param ps_methods Propensity score methods to use: any of `"matching"`,
#'   `"ipw"` and `"doubly_robust"` (default: all three).
#' @param matching_ratio Number of controls matched to each treated unit
#'   (default: 1).
#' @param caliper Matching caliper in standard deviations of the propensity
#'   score (default: 0.2).
#' @param dml_model Base learner for Double Machine Learning: `"rf"`
#'   (default), `"xgboost"` or `"lasso"`.
#' @param dml_n_folds Number of cross-fitting folds in DML (default: 5).
#' @param cf_num_trees Number of trees of the causal forest (default: 2000).
#' @param cf_honesty Logical; use honest splitting in the causal forest
#'   (default: TRUE).
#' @param meta_learners Meta-learners to run: any of `"S"`, `"T"` and `"X"`
#'   (default: all three).
#' @param meta_base_model Base model of the meta-learners: `"rf"` (default) or
#'   `"xgboost"`.
#' @param run_tmle Logical; run TMLE (default: FALSE).
#' @param use_python Logical; run Python methods through 'reticulate'
#'   (default: FALSE).
#' @param python_methods Python methods to run: any of `"econml_dml"` and
#'   `"econml_forest"`.
#' @param exclude_cols Character vector of columns to exclude from the
#'   analysis (default: NULL).
#' @param impute Logical; impute missing values (default: TRUE).
#' @param impute_method Imputation method: `"median"` (default), `"knn"` or
#'   `"mean"`.
#' @param normalize Logical; normalize the covariates (default: TRUE).
#' @param normalize_method Normalization method: `"zscore"` (default) or
#'   `"minmax"`.
#' @param run_eda Logical; run the causal exploratory analysis (default:
#'   TRUE).
#' @param confidence_level Confidence level of the intervals (default: 0.95).
#' @param seed Optional integer seed. If NULL (default) the random number
#'   generator state is not modified.
#' @param verbose Logical; print progress to the console (default: TRUE).
#'
#' @return An object of class `causalml` (a list) with the components:
#' \describe{
#'   \item{call}{The matched call of the internal pipeline.}
#'   \item{params}{List of analysis parameters (treatment, outcome,
#'     covariates, methods, treatment_type, confidence_level, seed).}
#'   \item{eda}{Causal exploratory analysis: distributions, pre-adjustment
#'     balance and potential confounders (only if `run_eda = TRUE`).}
#'   \item{preprocessed}{List with the preprocessed data and the final
#'     covariates.}
#'   \item{assumptions}{Assumption checks: overlap, positivity and balance
#'     after weighting.}
#'   \item{propensity}{Propensity score results (scores, matching, IPW and
#'     AIPW estimates with their confidence intervals).}
#'   \item{advanced}{DML, causal forest (with CATE and variable importance)
#'     and TMLE results, when requested.}
#'   \item{meta_learners}{S-, T- and X-learner results with individual CATE
#'     estimates and the ATE.}
#'   \item{python}{EconML results (only if `use_python = TRUE`).}
#'   \item{summary_table}{Data frame comparing the methods, with columns
#'     `method`, `ate` (average treatment effect), `se`, `ci_lower`,
#'     `ci_upper` and `p_value`.}
#'   \item{evalue}{List with the E-value of the sensitivity analysis
#'     (`evalue_point`, `evalue_ci`).}
#'   \item{figures}{Named list of 'ggplot2' plots.}
#'   \item{figures_catalog}{Data frame cataloguing the figures.}
#'   \item{elapsed_time}{Execution time in seconds.}
#'   \item{data_original}{The input data.}
#'   \item{verbose_text, verbose_lines}{Captured console output (only if
#'     `verbose = TRUE`).}
#' }
#'
#' @examples
#' # Simulated confounded data: x1 drives both treatment and outcome
#' set.seed(1)
#' n <- 300
#' df <- data.frame(x1 = rnorm(n), x2 = rnorm(n), x3 = rnorm(n))
#' df$treat <- rbinom(n, 1, plogis(df$x1))
#' df$y <- 1.5 * df$treat + df$x1 + 0.5 * df$x2 + rnorm(n)
#'
#' res <- causal_ml(df, treatment = "treat", outcome = "y",
#'                  methods = c("propensity", "meta_learners"),
#'                  ps_methods = c("ipw", "doubly_robust"),
#'                  meta_learners = "T", run_eda = FALSE,
#'                  seed = 1, verbose = FALSE)
#' res$summary_table
#'
#' @family causal inference
#' @export
causal_ml <- function(data,
                      treatment,
                      outcome,
                      covariates = NULL,
                      methods = c("propensity", "dml", "causal_forest", "meta_learners"),
                      treatment_type = c("auto", "binary", "continuous"),
                      ps_model = c("logistic", "rf", "xgboost"),
                      ps_methods = c("matching", "ipw", "doubly_robust"),
                      matching_ratio = 1,
                      caliper = 0.2,
                      dml_model = c("rf", "xgboost", "lasso"),
                      dml_n_folds = 5,
                      cf_num_trees = 2000,
                      cf_honesty = TRUE,
                      meta_learners = c("S", "T", "X"),
                      meta_base_model = c("rf", "xgboost"),
                      run_tmle = FALSE,
                      use_python = FALSE,
                      python_methods = c("econml_dml", "econml_forest"),
                      exclude_cols = NULL,
                      impute = TRUE,
                      impute_method = c("median", "knn", "mean"),
                      normalize = TRUE,
                      normalize_method = c("zscore", "minmax"),
                      run_eda = TRUE,
                      confidence_level = 0.95,
                      seed = NULL,
                      verbose = TRUE) {

  # Captura de verbose
  if (verbose) {
    temp_file <- tempfile(fileext = ".txt")
    sink(temp_file, split = TRUE)

    tryCatch({
      resultado <- .causal_ml_internal(
        data = data, treatment = treatment, outcome = outcome,
        covariates = covariates, methods = methods,
        treatment_type = treatment_type, ps_model = ps_model,
        ps_methods = ps_methods, matching_ratio = matching_ratio,
        caliper = caliper, dml_model = dml_model,
        dml_n_folds = dml_n_folds, cf_num_trees = cf_num_trees,
        cf_honesty = cf_honesty, meta_learners = meta_learners,
        meta_base_model = meta_base_model, run_tmle = run_tmle,
        use_python = use_python, python_methods = python_methods,
        exclude_cols = exclude_cols, impute = impute,
        impute_method = impute_method, normalize = normalize,
        normalize_method = normalize_method, run_eda = run_eda,
        confidence_level = confidence_level, seed = seed, verbose = TRUE
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
    resultado <- .causal_ml_internal(
      data = data, treatment = treatment, outcome = outcome,
      covariates = covariates, methods = methods,
      treatment_type = treatment_type, ps_model = ps_model,
      ps_methods = ps_methods, matching_ratio = matching_ratio,
      caliper = caliper, dml_model = dml_model,
      dml_n_folds = dml_n_folds, cf_num_trees = cf_num_trees,
      cf_honesty = cf_honesty, meta_learners = meta_learners,
      meta_base_model = meta_base_model, run_tmle = run_tmle,
      use_python = use_python, python_methods = python_methods,
      exclude_cols = exclude_cols, impute = impute,
      impute_method = impute_method, normalize = normalize,
      normalize_method = normalize_method, run_eda = run_eda,
      confidence_level = confidence_level, seed = seed, verbose = FALSE
    )
  }

  return(resultado)
}


# Runs the internal pipeline with package-version and convergence warnings
# from the modelling back-ends suppressed locally (instead of changing
# options(warn)).
#' @noRd
.causal_ml_internal <- function(...) {
  suppressWarnings(.causal_ml_internal_impl(...))
}

#' @noRd
#' @title Internal Implementation of causal_ml
#' @noRd
.causal_ml_internal_impl <- function(data, treatment, outcome, covariates,
                                methods, treatment_type, ps_model,
                                ps_methods, matching_ratio, caliper,
                                dml_model, dml_n_folds, cf_num_trees,
                                cf_honesty, meta_learners, meta_base_model,
                                run_tmle, use_python, python_methods,
                                exclude_cols, impute, impute_method,
                                normalize, normalize_method, run_eda,
                                confidence_level, seed, verbose) {


  if (!is.null(seed)) set.seed(seed)
  start_time <- Sys.time()

  # Match args
  treatment_type <- match.arg(treatment_type, c("auto", "binary", "continuous"))
  ps_model <- match.arg(ps_model, c("logistic", "rf", "xgboost"))
  dml_model <- match.arg(dml_model, c("rf", "xgboost", "lasso"))
  meta_base_model <- match.arg(meta_base_model, c("rf", "xgboost"))
  impute_method <- match.arg(impute_method, c("median", "knn", "mean"))
  normalize_method <- match.arg(normalize_method, c("zscore", "minmax"))

  # Validar inputs
  .validate_causal_inputs(data, treatment, outcome, covariates)

  # Determinar covariables
  if (is.null(covariates)) {
    covariates <- setdiff(names(data), c(treatment, outcome))
  }

  # Detectar tipo de tratamiento
  if (treatment_type == "auto") {
    treatment_type <- .detect_treatment_type(data[[treatment]])
  }

  if (verbose) {
    .msg_header("causal_ml - Inferencia Causal Automatizada")
    cat("Observaciones:", nrow(data), "\n")
    cat("Tratamiento:", treatment, "(", treatment_type, ")\n")
    cat("Resultado:", outcome, "\n")
    cat("Covariables:", length(covariates), "\n")
    cat("Metodos:", paste(methods, collapse = ", "), "\n")
    cat("Nivel de confianza:", confidence_level * 100, "%\n")
  }

  resultado <- list()
  resultado$call <- match.call()
  resultado$params <- list(
    treatment = treatment,
    outcome = outcome,
    covariates = covariates,
    methods = methods,
    treatment_type = treatment_type,
    confidence_level = confidence_level,
    seed = seed
  )

  all_summary_rows <- list()

  # =========================================================================
  # 1. EDA Causal
  # =========================================================================
  if (run_eda) {
    resultado$eda <- .causal_eda(data, treatment, outcome, covariates,
                                  treatment_type, verbose)
  }

  # =========================================================================
  # 2. Preprocesamiento
  # =========================================================================
  preprocess_result <- .causal_preprocess(
    data, treatment, outcome, covariates, treatment_type,
    exclude_cols, impute, impute_method, normalize, normalize_method, verbose
  )
  resultado$preprocessed <- preprocess_result
  covariates <- preprocess_result$covariates
  data_proc <- preprocess_result$data

  # =========================================================================
  # 3. Supuestos (primera pasada, sin PS aun)
  # =========================================================================
  # Se ejecutara completo despues de estimar PS

  # =========================================================================
  # 4. Propensity Score Methods
  # =========================================================================
  ps_result_for_assumptions <- NULL

  if ("propensity" %in% methods && treatment_type == "binary") {
    prop_result <- .causal_propensity(
      data_proc, treatment, outcome, covariates,
      ps_model, ps_methods, matching_ratio, caliper,
      confidence_level, verbose
    )
    resultado$propensity <- prop_result
    all_summary_rows <- c(all_summary_rows, prop_result$summary_rows)

    # Guardar PS para assumptions
    ps_result_for_assumptions <- list(ps = prop_result$ps)
  }

  # =========================================================================
  # 3. Verificacion de Supuestos (ahora con PS)
  # =========================================================================
  resultado$assumptions <- .causal_assumptions(
    data_proc, treatment, outcome, covariates,
    treatment_type, ps_result_for_assumptions,
    confidence_level, verbose
  )

  # =========================================================================
  # 5. Metodos Avanzados (DML, Causal Forest, TMLE)
  # =========================================================================
  advanced_methods <- intersect(methods, c("dml", "causal_forest"))

  if (length(advanced_methods) > 0 || run_tmle) {
    adv_result <- .causal_advanced(
      data_proc, treatment, outcome, covariates,
      advanced_methods, dml_model, dml_n_folds,
      cf_num_trees, cf_honesty, run_tmle,
      confidence_level, seed, verbose
    )
    resultado$advanced <- adv_result
    all_summary_rows <- c(all_summary_rows, adv_result$summary_rows)
  }

  # =========================================================================
  # 6. Meta-Learners
  # =========================================================================
  if ("meta_learners" %in% methods && treatment_type == "binary") {
    ml_result <- .causal_metalearners(
      data_proc, treatment, outcome, covariates,
      meta_learners, meta_base_model, confidence_level, seed, verbose
    )
    resultado$meta_learners <- ml_result
    all_summary_rows <- c(all_summary_rows, ml_result$summary_rows)
  }

  # =========================================================================
  # 7. Python Methods (opcional)
  # =========================================================================
  if (use_python) {
    py_result <- .causal_python(
      data_proc, treatment, outcome, covariates,
      python_methods, confidence_level, seed, verbose
    )
    resultado$python <- py_result
    all_summary_rows <- c(all_summary_rows, py_result$summary_rows)
  }

  # =========================================================================
  # Summary Table
  # =========================================================================
  if (length(all_summary_rows) > 0) {
    resultado$summary_table <- do.call(rbind, all_summary_rows)
    rownames(resultado$summary_table) <- NULL
  } else {
    resultado$summary_table <- data.frame(
      method = character(), ate = numeric(), se = numeric(),
      ci_lower = numeric(), ci_upper = numeric(), p_value = numeric()
    )
  }

  # =========================================================================
  # E-value (con el ATE mas robusto)
  # =========================================================================
  if (nrow(resultado$summary_table) > 0) {
    # Usar AIPW si disponible, sino el primero
    best_row <- if ("AIPW (Doubly Robust)" %in% resultado$summary_table$method) {
      resultado$summary_table[resultado$summary_table$method == "AIPW (Doubly Robust)", ]
    } else {
      resultado$summary_table[1, ]
    }

    resultado$evalue <- .calc_evalue(best_row$ate, best_row$se)

    if (verbose) {
      .print_subsection(3, 3, "E-value (Calculado)")
      cat("  Basado en:", best_row$method, "\n")
      cat("  E-value (punto):", round(resultado$evalue$evalue_point, 2), "\n")
      if (!is.na(resultado$evalue$evalue_ci)) {
        cat("  E-value (IC):", round(resultado$evalue$evalue_ci, 2), "\n")
      }
      cat("  Interpretacion: un confusor no medido tendria que estar asociado\n")
      cat("  con tratamiento Y resultado por un factor de al menos\n")
      cat("  ", round(resultado$evalue$evalue_point, 2),
          " para explicar el efecto observado.\n", sep = "")
    }
  }

  # =========================================================================
  # 8. Figuras
  # =========================================================================
  fig_result <- .causal_plots(resultado, verbose)
  resultado$figures <- fig_result$plots
  resultado$figures_catalog <- fig_result$figures_catalog

  # =========================================================================
  # Resumen Final
  # =========================================================================
  elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  resultado$elapsed_time <- elapsed
  resultado$data_original <- data

  if (verbose) {
    .print_section(9, "Resumen de Resultados")

    if (nrow(resultado$summary_table) > 0) {
      cat("\n  Tabla comparativa de efectos:\n\n")
      st <- resultado$summary_table
      cat(sprintf("  %-25s %8s %8s %10s %10s %10s\n",
                  "Metodo", "ATE", "SE", "IC Inf", "IC Sup", "p-valor"))
      cat("  ", paste(rep("-", 75), collapse = ""), "\n", sep = "")
      for (i in seq_len(nrow(st))) {
        sig <- if (st$p_value[i] < 0.001) "***" else
          if (st$p_value[i] < 0.01) "**" else
            if (st$p_value[i] < 0.05) "*" else ""
        cat(sprintf("  %-25s %8.4f %8.4f %10.4f %10.4f %10s %s\n",
                    st$method[i], st$ate[i], st$se[i],
                    st$ci_lower[i], st$ci_upper[i],
                    format(st$p_value[i], digits = 4), sig))
      }
      cat("\n  Significancia: *** p<0.001, ** p<0.01, * p<0.05\n")
    }

    cat("\n  Figuras generadas:", length(resultado$figures), "\n")
    cat("  Tiempo total:", round(elapsed, 1), "segundos\n")
  }

  class(resultado) <- c("causalml", "list")
  return(resultado)
}


# =============================================================================
# S3 Methods
# =============================================================================

#' Print a causalml Object
#'
#' Prints the configuration of the analysis, the average treatment effect
#' estimated by each method and the E-value.
#'
#' @param x An object of class `causalml`, as returned by [causal_ml()].
#' @param ... Further arguments (not used).
#' @return Invisibly returns `x`.
#' @examples
#' set.seed(1)
#' n <- 300
#' df <- data.frame(x1 = rnorm(n), x2 = rnorm(n), x3 = rnorm(n))
#' df$treat <- rbinom(n, 1, plogis(df$x1))
#' df$y <- 1.5 * df$treat + df$x1 + 0.5 * df$x2 + rnorm(n)
#' res <- causal_ml(df, treatment = "treat", outcome = "y",
#'                  methods = c("propensity", "meta_learners"),
#'                  ps_methods = c("ipw", "doubly_robust"),
#'                  meta_learners = "T", run_eda = FALSE,
#'                  seed = 1, verbose = FALSE)
#' print(res)
#' @family causal inference
#' @export
print.causalml <- function(x, ...) {
  cat("\n")
  cat(.line("="), "\n")
  cat("  causal_ml - Resultados de Inferencia Causal\n")
  cat(.line("="), "\n\n")

  cat("CONFIGURACION:\n")
  cat("  Tratamiento:", x$params$treatment, "(", x$params$treatment_type, ")\n")
  cat("  Resultado:", x$params$outcome, "\n")
  cat("  Covariables:", length(x$params$covariates), "\n")
  cat("  Metodos:", paste(x$params$methods, collapse = ", "), "\n")

  if (!is.null(x$summary_table) && nrow(x$summary_table) > 0) {
    cat("\nRESULTADOS (ATE):\n")
    st <- x$summary_table
    for (i in seq_len(nrow(st))) {
      sig <- if (st$p_value[i] < 0.05) "*" else ""
      cat(sprintf("  %-25s ATE = %7.4f  IC[%7.4f, %7.4f]  p = %s %s\n",
                  st$method[i], st$ate[i], st$ci_lower[i], st$ci_upper[i],
                  format(st$p_value[i], digits = 3), sig))
    }
  }

  if (!is.null(x$evalue)) {
    cat("\nSENSIBILIDAD:\n")
    cat("  E-value:", round(x$evalue$evalue_point, 2), "\n")
  }

  cat("\n  Figuras:", length(x$figures), "| Tiempo:", round(x$elapsed_time, 1), "s\n")

  invisible(x)
}


#' Summarize a causalml Object
#'
#' Prints the comparison table of the methods and the sensitivity analysis
#' (E-value).
#'
#' @param object An object of class `causalml`, as returned by [causal_ml()].
#' @param ... Further arguments (not used).
#' @return Invisibly returns `object`.
#' @examples
#' set.seed(1)
#' n <- 300
#' df <- data.frame(x1 = rnorm(n), x2 = rnorm(n), x3 = rnorm(n))
#' df$treat <- rbinom(n, 1, plogis(df$x1))
#' df$y <- 1.5 * df$treat + df$x1 + 0.5 * df$x2 + rnorm(n)
#' res <- causal_ml(df, treatment = "treat", outcome = "y",
#'                  methods = c("propensity", "meta_learners"),
#'                  ps_methods = c("ipw", "doubly_robust"),
#'                  meta_learners = "T", run_eda = FALSE,
#'                  seed = 1, verbose = FALSE)
#' summary(res)
#' @family causal inference
#' @export
summary.causalml <- function(object, ...) {
  cat("\n=== Resumen causal_ml ===\n\n")
  cat("Tratamiento:", object$params$treatment, "\n")
  cat("Resultado:", object$params$outcome, "\n")
  cat("Tipo:", object$params$treatment_type, "\n\n")

  if (!is.null(object$summary_table) && nrow(object$summary_table) > 0) {
    cat("--- Comparacion de Metodos ---\n")
    print(object$summary_table)
  }

  if (!is.null(object$evalue)) {
    cat("\n--- Analisis de Sensibilidad ---\n")
    cat("E-value (punto):", round(object$evalue$evalue_point, 2), "\n")
    if (!is.na(object$evalue$evalue_ci)) {
      cat("E-value (IC):", round(object$evalue$evalue_ci, 2), "\n")
    }
  }

  invisible(object)
}


#' Plot a causalml Object
#'
#' Returns the figures stored in a `causalml` object.
#'
#' @param x An object of class `causalml`, as returned by [causal_ml()].
#' @param type Plot type: `"all"` (default) or one of `"treatment"`,
#'   `"outcome"`, `"ps"`, `"overlap"`, `"love"`, `"cate"`,
#'   `"heterogeneity"`, `"importance"`, `"ate_forest"`, `"meta"`,
#'   `"evalue"` and `"balance"`. The internal name of a figure (see
#'   `names(x$figures)`) is also accepted.
#' @param ... Further arguments (not used).
#' @return For `type = "all"`, the named list of 'ggplot2' objects in
#'   `x$figures`; otherwise the requested 'ggplot2' object (printed when
#'   called at top level). If no figure is available or the type is not
#'   found, a message is issued and `NULL` is returned invisibly.
#' @examples
#' set.seed(1)
#' n <- 300
#' df <- data.frame(x1 = rnorm(n), x2 = rnorm(n), x3 = rnorm(n))
#' df$treat <- rbinom(n, 1, plogis(df$x1))
#' df$y <- 1.5 * df$treat + df$x1 + 0.5 * df$x2 + rnorm(n)
#' res <- causal_ml(df, treatment = "treat", outcome = "y",
#'                  methods = c("propensity", "meta_learners"),
#'                  ps_methods = c("ipw", "doubly_robust"),
#'                  meta_learners = "T", run_eda = FALSE,
#'                  seed = 1, verbose = FALSE)
#' plot(res, type = "ate_forest")
#' @family causal inference
#' @export
plot.causalml <- function(x, type = "all", ...) {

  if (is.null(x$figures) || length(x$figures) == 0) {
    message("No figures available")
    return(invisible(NULL))
  }

  if (type == "all") {
    return(x$figures)
  }

  plot_map <- list(
    treatment = "treatment_distribution",
    outcome = "outcome_by_treatment",
    ps = "ps_distribution",
    overlap = "ps_overlap",
    love = "balance_love_plot",
    cate = "cate_distribution",
    heterogeneity = "cate_heterogeneity",
    importance = "variable_importance_cf",
    ate_forest = "ate_forest_plot",
    meta = "meta_learner_cate",
    evalue = "sensitivity_evalue",
    balance = "matching_balance"
  )

  plot_name <- plot_map[[type]]
  if (is.null(plot_name)) plot_name <- type

  if (plot_name %in% names(x$figures)) {
    return(x$figures[[plot_name]])
  }

  available <- names(x$figures)
  message("Type '", type, "' not found. Available: ",
          paste(available, collapse = ", "))
  invisible(NULL)
}


#' Predict Conditional Average Treatment Effects from a causalml Object
#'
#' Returns conditional average treatment effect (CATE) estimates, either for
#' the original data or for new observations.
#'
#' @param object An object of class `causalml`, as returned by [causal_ml()].
#' @param new_data A data frame with new observations containing the
#'   covariates used in the analysis. If NULL (default), the CATE estimates
#'   of the original data are returned (from the causal forest or, if not
#'   available, the X-, T- or S-learner).
#' @param method Method used for prediction on new data: `"best"` (default;
#'   causal forest if available, otherwise T-learner), `"causal_forest"` or
#'   `"T"`.
#' @param ... Further arguments (not used).
#' @return A numeric vector of CATE estimates, one per observation.
#' @examples
#' set.seed(1)
#' n <- 300
#' df <- data.frame(x1 = rnorm(n), x2 = rnorm(n), x3 = rnorm(n))
#' df$treat <- rbinom(n, 1, plogis(df$x1))
#' df$y <- 1.5 * df$treat + df$x1 + 0.5 * df$x2 + rnorm(n)
#' res <- causal_ml(df, treatment = "treat", outcome = "y",
#'                  methods = c("propensity", "meta_learners"),
#'                  ps_methods = c("ipw", "doubly_robust"),
#'                  meta_learners = "T", run_eda = FALSE,
#'                  seed = 1, verbose = FALSE)
#' head(predict(res))
#' # New data must be on the same (normalized) scale as the covariates
#' predict(res, new_data = res$preprocessed$data[1:5, ], method = "T")
#' @family causal inference
#' @export
predict.causalml <- function(object, new_data = NULL, method = "best", ...) {

  if (is.null(new_data)) {
    # Devolver CATE del dataset original
    cate <- .get_best_cate(object)
    if (is.null(cate)) {
      stop("No CATE estimates are available")
    }
    return(cate)
  }

  # Predecir CATE para nuevos datos
  # Causal Forest (grf)
  if (!is.null(object$advanced$causal_forest$model) &&
      (method == "best" || method == "causal_forest")) {
    cf_model <- object$advanced$causal_forest$model
    X_new <- .prepare_covariate_matrix(new_data, object$params$covariates)
    return(stats::predict(cf_model, X_new)$predictions)
  }

  # T-learner
  if (!is.null(object$meta_learners$T) &&
      (method == "best" || method == "T")) {
    X_new <- new_data[, object$params$covariates, drop = FALSE]
    mu_1 <- stats::predict(object$meta_learners$T$model_1, data = X_new)$predictions
    mu_0 <- stats::predict(object$meta_learners$T$model_0, data = X_new)$predictions
    return(mu_1 - mu_0)
  }

  stop("No model is available to predict CATE for new data. ",
       "Methods that support prediction: causal_forest, T-learner")
}
