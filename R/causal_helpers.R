# =============================================================================
# Funciones auxiliares para causal_ml
# =============================================================================

# --- Validacion de inputs ---

#' @noRd
.validate_causal_inputs <- function(data, treatment, outcome, covariates) {
  if (!is.data.frame(data)) {
    stop("'data' must be a data.frame")
  }
  if (!treatment %in% names(data)) {
    stop("Treatment variable '", treatment, "' not found in the data")
  }
  if (!outcome %in% names(data)) {
    stop("Outcome variable '", outcome, "' not found in the data")
  }
  if (!is.null(covariates)) {
    missing_covs <- setdiff(covariates, names(data))
    if (length(missing_covs) > 0) {
      stop("Covariates not found: ", paste(missing_covs, collapse = ", "))
    }
  }
}

#' @noRd
.detect_treatment_type <- function(x) {
  if (is.factor(x) || is.character(x) || is.logical(x)) {
    return("binary")
  }
  n_unique <- length(unique(stats::na.omit(x)))
  if (n_unique == 2) {
    return("binary")
  }
  return("continuous")
}

#' @noRd
.ensure_binary_treatment <- function(x) {
  if (is.factor(x)) {
    x <- as.integer(x) - 1L
  } else if (is.character(x)) {
    x <- as.integer(as.factor(x)) - 1L
  } else if (is.logical(x)) {
    x <- as.integer(x)
  }
  vals <- sort(unique(stats::na.omit(x)))
  if (length(vals) != 2) {
    stop("A binary treatment requires exactly 2 unique values; found: ",
         length(vals))
  }
  if (!all(vals == c(0, 1))) {
    x <- ifelse(x == vals[2], 1L, 0L)
  }
  as.integer(x)
}

# --- Calculo de SMD ---

#' @title Standardized Mean Difference
#' @noRd
.calc_smd <- function(x, treatment) {
  idx1 <- treatment == 1
  idx0 <- treatment == 0
  x1 <- x[idx1]
  x0 <- x[idx0]
  m1 <- mean(x1, na.rm = TRUE)
  m0 <- mean(x0, na.rm = TRUE)
  s1 <- stats::sd(x1, na.rm = TRUE)
  s0 <- stats::sd(x0, na.rm = TRUE)
  pooled_sd <- sqrt((s1^2 + s0^2) / 2)
  if (pooled_sd < 1e-10) return(0)
  (m1 - m0) / pooled_sd
}

#' @title SMD for All Covariates
#' @noRd
.calc_all_smd <- function(data, treatment_vec, covariates) {
  smd_vals <- sapply(covariates, function(v) {
    x <- data[[v]]
    if (is.numeric(x)) {
      .calc_smd(x, treatment_vec)
    } else {
      # Para categoricas, calcular SMD de la proporcion
      props <- tapply(as.integer(as.factor(x)), treatment_vec, function(z) {
        mean(z == max(z, na.rm = TRUE), na.rm = TRUE)
      })
      if (length(props) < 2) return(0)
      (props[["1"]] - props[["0"]]) / sqrt((props[["1"]] * (1 - props[["1"]]) +
                                              props[["0"]] * (1 - props[["0"]])) / 2 + 1e-10)
    }
  })
  data.frame(
    variable = covariates,
    smd = abs(as.numeric(smd_vals)),
    stringsAsFactors = FALSE
  )
}

# --- Utilidades de Propensity Score ---

#' @noRd
.estimate_ps <- function(data, treatment, covariates, method = "logistic") {
  df <- data[, c(treatment, covariates), drop = FALSE]
  df[[treatment]] <- as.factor(df[[treatment]])

  if (method == "logistic") {
    fml <- stats::as.formula(paste(treatment, "~ ."))
    fit <- stats::glm(fml, data = df, family = stats::binomial(link = "logit"))
    ps <- stats::predict(fit, type = "response")
    return(list(ps = as.numeric(ps), model = fit))

  } else if (method == "rf") {
    if (!requireNamespace("ranger", quietly = TRUE)) {
      stop("Package 'ranger' is required for a random forest propensity score model")
    }
    df[[treatment]] <- as.factor(df[[treatment]])
    fml <- stats::as.formula(paste(treatment, "~ ."))
    fit <- ranger::ranger(fml, data = df, probability = TRUE, num.trees = 500)
    ps <- stats::predict(fit, data = df)$predictions[, "1"]
    return(list(ps = as.numeric(ps), model = fit))

  } else if (method == "xgboost") {
    if (!requireNamespace("xgboost", quietly = TRUE)) {
      stop("Package 'xgboost' is required for an XGBoost propensity score model")
    }
    X <- stats::model.matrix(~ . - 1, data = df[, covariates, drop = FALSE])
    y <- as.numeric(as.character(df[[treatment]]))
    dtrain <- xgboost::xgb.DMatrix(data = X, label = y)
    fit <- xgboost::xgb.train(
      params = list(objective = "binary:logistic", eval_metric = "logloss",
                    max_depth = 4, eta = 0.1),
      data = dtrain, nrounds = 100, verbose = 0
    )
    ps <- stats::predict(fit, X)
    return(list(ps = as.numeric(ps), model = fit))
  }

  stop("Unsupported propensity score method: ", method)
}

# --- Utilidades de IC ---

#' @noRd
.calc_ci <- function(estimate, se, confidence_level = 0.95) {
  z <- stats::qnorm(1 - (1 - confidence_level) / 2)
  list(
    ci_lower = estimate - z * se,
    ci_upper = estimate + z * se,
    p_value = 2 * stats::pnorm(-abs(estimate / se))
  )
}

# --- E-value ---

#' @title E-value for Sensitivity Analysis
#' @noRd
.calc_evalue <- function(estimate, se = NULL, outcome_rare = FALSE) {
  # E-value: minima fuerza de confusor no medido que explicaria el efecto
  # VanderWeele & Ding (2017)
  rr <- exp(abs(estimate))
  if (outcome_rare) rr <- abs(estimate)  # OR ~ RR si outcome raro
  if (rr < 1) rr <- 1 / rr

  evalue_point <- rr + sqrt(rr * (rr - 1))

  evalue_ci <- NA
  if (!is.null(se) && !is.na(se) && se > 0) {
    ci_bound <- exp(abs(estimate) - 1.96 * se)
    if (ci_bound > 1) {
      evalue_ci <- ci_bound + sqrt(ci_bound * (ci_bound - 1))
    } else {
      evalue_ci <- 1
    }
  }

  list(evalue_point = evalue_point, evalue_ci = evalue_ci)
}

# --- Formato para summary table ---

#' @noRd
.create_summary_row <- function(method, ate, se, ci_lower, ci_upper, p_value) {
  data.frame(
    method = method,
    ate = round(ate, 4),
    se = round(se, 4),
    ci_lower = round(ci_lower, 4),
    ci_upper = round(ci_upper, 4),
    p_value = round(p_value, 6),
    stringsAsFactors = FALSE
  )
}

# --- Verificar paquete opcional ---

#' @noRd
.check_package <- function(pkg, purpose = "") {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    msg <- paste0("Package '", pkg, "' is not installed.")
    if (nchar(purpose) > 0) {
      msg <- paste0(msg, " It is required for: ", purpose, ".")
    }
    msg <- paste0(msg, " Please install it to use this option.")
    warning(msg, call. = FALSE)
    return(FALSE)
  }
  return(TRUE)
}

# --- Referencias causales ---

#' @noRd
.print_causal_reference <- function(ref_key) {
  references <- list(
    # Propensity Score
    ps = "Rosenbaum, P. R., & Rubin, D. B. (1983). The central role of the propensity score in observational studies for causal effects. Biometrika, 70(1), 41-55.",
    matching = "Stuart, E. A. (2010). Matching methods for causal inference: A review and a look forward. Statistical Science, 25(1), 1-21.",
    ipw = "Hernan, M. A., & Robins, J. M. (2020). Causal inference: What if. Chapman & Hall/CRC.",
    aipw = "Robins, J. M., Rotnitzky, A., & Zhao, L. P. (1994). Estimation of regression coefficients when some regressors are not always observed. JASA, 89(427), 846-866.",

    # Double ML
    dml = "Chernozhukov, V., Chetverikov, D., Demirer, M., Duflo, E., Hansen, C., Newey, W., & Robins, J. (2018). Double/debiased machine learning for treatment and structural parameters. The Econometrics Journal, 21(1), C1-C68.",

    # Causal Forest
    cf = "Wager, S., & Athey, S. (2018). Estimation and inference of heterogeneous treatment effects using random forests. JASA, 113(523), 1228-1242.",
    grf = "Athey, S., Tibshirani, J., & Wager, S. (2019). Generalized random forests. The Annals of Statistics, 47(2), 1148-1178.",

    # Meta-learners
    meta = "Kunzel, S. R., Sekhon, J. S., Bickel, P. J., & Yu, B. (2019). Metalearners for estimating heterogeneous treatment effects using machine learning. PNAS, 116(10), 4156-4165.",

    # TMLE
    tmle = "van der Laan, M. J., & Rose, S. (2011). Targeted learning: Causal inference for observational and experimental data. Springer.",

    # E-value
    evalue = "VanderWeele, T. J., & Ding, P. (2017). Sensitivity analysis in observational research: Introducing the E-value. Annals of Internal Medicine, 167(4), 268-274.",

    # SMD
    smd = "Austin, P. C. (2011). An introduction to propensity score methods for reducing the effects of confounding in observational studies. Multivariate Behavioral Research, 46(3), 399-424.",

    # Overlap
    overlap = "Petersen, M. L., Porter, K. E., Gruber, S., Wang, Y., & van der Laan, M. J. (2012). Diagnosing and responding to violations in the positivity assumption. Statistical Methods in Medical Research, 21(1), 31-54.",

    # EconML
    econml = "Battocchi, K., Dillon, E., Hei, M., Lewis, G., Raz, P., Reitzner, D., ... & Syrgkanis, V. (2019). EconML: A Python package for ML-based heterogeneous treatment effects estimation."
  )

  if (ref_key %in% names(references)) {
    cat("\n    Referencia: ", references[[ref_key]], "\n", sep = "")
  }
}

# --- Utilidad para preparar matriz de covariables ---

#' @noRd
.prepare_covariate_matrix <- function(data, covariates) {
  df <- data[, covariates, drop = FALSE]
  # Convertir factores a dummies
  num_cols <- sapply(df, is.numeric)
  if (all(num_cols)) return(as.matrix(df))

  stats::model.matrix(~ . - 1, data = df)
}
