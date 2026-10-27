# =============================================================================
# Automatic feature engineering for PsyML
# =============================================================================

#' @title Automatic Feature Engineering
#'
#' @description
#' Applies generic transformations that may improve model performance
#' without requiring domain knowledge: (1) suspicious zeros in numeric
#' variables whose non-zero minimum is far from zero are converted to NA;
#' (2) strictly positive variables with skewness > 2 get an additional
#' `log1p()` version (suffix `_log`); (3) variables with more than 5% (and
#' less than 90%) missing values get a missing indicator (suffix
#' `_missing`); (4) pairwise products (suffix `_x_`) are created among the
#' three numeric variables most correlated with the target.
#'
#' @param data Data frame with the data.
#' @param target Name of the target variable.
#' @param verbose Logical. Print progress (default: TRUE).
#'
#' @return A list with `data` (the modified data frame), `new_features`
#'   (names of the created variables), `transformations` (list with elements
#'   `zeros_to_na`, `log_transform`, `missing_indicators` and/or
#'   `interactions` listing the variables involved), `n_original` (number of
#'   original predictors) and `n_new` (number of new variables).
#' @examples
#' set.seed(1)
#' n <- 100
#' df <- data.frame(x1 = rnorm(n), x2 = rnorm(n), x3 = rexp(n)^3)
#' df$x2[sample(n, 10)] <- NA
#' df$y <- df$x1 + rnorm(n)
#' fe <- auto_feature_engineering(df, target = "y", verbose = FALSE)
#' fe$new_features
#' @family supervised learning
#' @export
auto_feature_engineering <- function(data, target, verbose = TRUE) {

  if (verbose) {
    .print_subsection(2, 0, "Feature Engineering Automatico")
    cat("    Se aplican transformaciones genericas para mejorar el modelo.\n")
    cat("    Estas transformaciones no requieren conocimiento del dominio.\n\n")
  }

  original_vars <- setdiff(names(data), target)
  num_vars <- names(data)[sapply(data, is.numeric)]
  num_vars <- setdiff(num_vars, target)
  new_features <- character()
  transformations <- list()

  # -------------------------------------------------------------------------
  # 1. Detectar ceros sospechosos -> convertir a NA
  # -------------------------------------------------------------------------
  # Variables donde 0 no es un valor valido (ej: glucose, blood pressure)
  # Heuristica: si una variable tiene ceros, pero su media sin ceros es >> 0
  # y el minimo sin ceros es >> 0, entonces los ceros son NAs disfrazados.

  zero_converted <- character()

  for (v in num_vars) {
    x <- data[[v]]
    n_zeros <- sum(x == 0, na.rm = TRUE)
    n_total <- sum(!is.na(x))

    if (n_zeros == 0 || n_total < 10) next

    pct_zeros <- n_zeros / n_total
    # Solo si hay entre 1% y 50% de ceros (no si todo es ceros o casi nada)
    if (pct_zeros < 0.01 || pct_zeros > 0.50) next

    # Minimo sin ceros
    non_zero <- x[x != 0 & !is.na(x)]
    if (length(non_zero) < 5) next

    min_nz <- min(non_zero)
    mean_nz <- mean(non_zero)

    # Si el minimo sin ceros es muy lejano a 0, los ceros son sospechosos
    # Ej: glucose minimo sin ceros = 44, media = 121 -> 0 es imposible
    # Requiere min_nz >= 5 para evitar falsos positivos en conteos (0,1,2...)
    if (min_nz >= 5 && (min_nz / mean_nz) > 0.05) {
      data[[v]][data[[v]] == 0] <- NA
      zero_converted <- c(zero_converted, v)
    }
  }

  if (length(zero_converted) > 0) {
    transformations$zeros_to_na <- zero_converted
    if (verbose) {
      cat("    [+] CEROS SOSPECHOSOS CONVERTIDOS A NA:\n")
      cat("        Variables donde 0 no es un valor valido:\n")
      for (v in zero_converted) {
        cat("          -", v, "\n")
      }
      cat("        (Seran imputados en el preprocesamiento)\n\n")
    }
  }

  # -------------------------------------------------------------------------
  # 2. Log-transform para variables muy sesgadas (skewness > 2)
  # -------------------------------------------------------------------------
  log_transformed <- character()

  for (v in num_vars) {
    x <- data[[v]]
    x_clean <- x[!is.na(x)]
    if (length(x_clean) < 10) next

    # Solo variables estrictamente positivas
    if (any(x_clean <= 0)) next

    # Calcular skewness (formula de Fisher)
    n <- length(x_clean)
    m <- mean(x_clean)
    s <- sd(x_clean)
    if (s == 0) next
    skew <- (n / ((n - 1) * (n - 2))) * sum(((x_clean - m) / s)^3)

    if (abs(skew) > 2) {
      new_name <- paste0(v, "_log")
      data[[new_name]] <- log1p(data[[v]])
      log_transformed <- c(log_transformed, v)
      new_features <- c(new_features, new_name)
    }
  }

  if (length(log_transformed) > 0) {
    transformations$log_transform <- log_transformed
    if (verbose) {
      cat("    [+] LOG-TRANSFORM (variables con sesgo > 2):\n")
      for (v in log_transformed) {
        cat("          -", v, "-->", paste0(v, "_log"), "\n")
      }
      cat("        (Se mantiene la variable original + la transformada)\n\n")
    }
  }

  # -------------------------------------------------------------------------
  # 3. Indicador de missing (para variables con > 5% NAs)
  # -------------------------------------------------------------------------
  missing_indicators <- character()

  # Solo variables originales (no las recien creadas por log-transform)
  for (v in setdiff(original_vars, target)) {
    x <- data[[v]]
    pct_na <- sum(is.na(x)) / length(x)

    if (pct_na > 0.05 && pct_na < 0.90) {
      new_name <- paste0(v, "_missing")
      data[[new_name]] <- as.integer(is.na(data[[v]]))
      missing_indicators <- c(missing_indicators, v)
      new_features <- c(new_features, new_name)
    }
  }

  if (length(missing_indicators) > 0) {
    transformations$missing_indicators <- missing_indicators
    if (verbose) {
      cat("    [+] INDICADORES DE MISSING (variables con > 5% NAs):\n")
      for (v in missing_indicators) {
        new_name <- paste0(v, "_missing")
        cat("          -", new_name, "(1 = dato faltante, 0 = dato presente)\n")
      }
      cat("        (A veces 'que falte un dato' es informativo por si mismo)\n\n")
    }
  }

  # -------------------------------------------------------------------------
  # 4. Interacciones entre top variables numericas
  # -------------------------------------------------------------------------
  # Crear interacciones solo entre las variables mas correlacionadas con target
  interactions_created <- character()

  if (length(num_vars) >= 2) {
    # Calcular correlacion con target (si target es numerico o binario)
    target_vec <- data[[target]]
    if (is.factor(target_vec)) {
      target_num <- as.numeric(target_vec) - 1
    } else {
      target_num <- target_vec
    }

    cors <- sapply(num_vars, function(v) {
      x <- data[[v]]
      complete <- !is.na(x) & !is.na(target_num)
      if (sum(complete) < 10) return(0)
      abs(cor(x[complete], target_num[complete]))
    })

    # Top 3 variables mas correlacionadas
    top_vars <- names(sort(cors, decreasing = TRUE))[1:min(3, length(cors))]

    if (length(top_vars) >= 2) {
      # Crear interacciones entre pares del top 3
      pairs <- combn(top_vars, 2, simplify = FALSE)
      for (pair in pairs) {
        v1 <- pair[1]
        v2 <- pair[2]
        new_name <- paste0(v1, "_x_", v2)
        x1 <- data[[v1]]
        x2 <- data[[v2]]
        # Solo crear si ambas tienen suficientes no-NA
        complete <- !is.na(x1) & !is.na(x2)
        if (sum(complete) > nrow(data) * 0.5) {
          data[[new_name]] <- x1 * x2
          interactions_created <- c(interactions_created, new_name)
          new_features <- c(new_features, new_name)
        }
      }
    }
  }

  if (length(interactions_created) > 0) {
    transformations$interactions <- interactions_created
    if (verbose) {
      cat("    [+] INTERACCIONES (entre variables mas correlacionadas con target):\n")
      for (v in interactions_created) {
        cat("          -", v, "\n")
      }
      cat("        (Producto entre variables, captura efectos combinados)\n\n")
    }
  }

  # -------------------------------------------------------------------------
  # Resumen
  # -------------------------------------------------------------------------
  total_new <- length(new_features)
  total_original <- length(original_vars)

  if (verbose) {
    if (total_new > 0) {
      cat("    Resumen: ", total_new, " variables nuevas creadas (",
          total_original, " originales + ", total_new, " nuevas = ",
          total_original + total_new, " total)\n\n", sep = "")
    } else {
      cat("    No se detectaron transformaciones aplicables\n\n")
    }

    # Sugerencia para el usuario
    cat("    NOTA: Estas son transformaciones genericas automaticas.\n")
    cat("    Para mejorar mas, considere crear variables especificas\n")
    cat("    de su dominio usando el argumento custom_features:\n\n")
    cat("      resultado <- supervised_ml(data, target = \"mi_target\",\n")
    cat("        custom_features = list(\n")
    cat("          familia_total = \"SibSp + Parch\",\n")
    cat("          bmi_cat = \"ifelse(mass > 30, 1, 0)\"\n")
    cat("        ))\n\n")
    .print_reference("feature_engineering")
  }

  list(
    data = data,
    new_features = new_features,
    transformations = transformations,
    n_original = total_original,
    n_new = total_new
  )
}


# =============================================================================
# Domain feature engineering (custom_features)
# =============================================================================

#' @title Domain Feature Engineering
#'
#' @description
#' Lets the user create new variables from R expressions. Each expression
#' is evaluated using the columns of the data frame. Expressions that fail,
#' or that do not return one value per row, are skipped and reported in
#' `failed`.
#'
#' @param data Data frame with the data.
#' @param custom_features Named list: each name is the new variable and each
#'   value an R expression given as a string (e.g. `"SibSp + Parch"`).
#' @param verbose Logical. Print progress (default: TRUE).
#'
#' @return A list with `data` (the data frame with the new variables),
#'   `created` (names of the variables created) and `failed` (names of the
#'   expressions that could not be evaluated).
#' @examples
#' res <- apply_custom_features(mtcars,
#'   custom_features = list(power_weight = "hp / wt",
#'                          heavy = "ifelse(wt > 3, 1, 0)"),
#'   verbose = FALSE)
#' head(res$data[, c("power_weight", "heavy")])
#' res$created
#' @family supervised learning
#' @export
apply_custom_features <- function(data, custom_features, verbose = TRUE) {

  if (verbose) {
    .print_subsection(2, 0, "Feature Engineering de Dominio")
    cat("    Variables definidas por el usuario basadas en conocimiento del area.\n\n")
  }

  created <- character()
  failed <- character()

  for (feat_name in names(custom_features)) {
    expr_str <- custom_features[[feat_name]]

    ok <- tryCatch({
      result <- eval(parse(text = expr_str), envir = data)

      if (length(result) != nrow(data)) {
        stop("The result has ", length(result),
             " elements but the data have ", nrow(data), " rows")
      }

      data[[feat_name]] <- result
      created <- c(created, feat_name)

      if (verbose) {
        cat("    [+]", feat_name, "=", expr_str, "\n")
      }
      TRUE
    }, error = function(e) {
      if (verbose) {
        cat("    [!] ERROR en", feat_name, "=", expr_str, "\n")
        cat("        ", conditionMessage(e), "\n")
      }
      FALSE
    })
    if (!isTRUE(ok)) failed <- c(failed, feat_name)
  }

  if (verbose) {
    cat("\n")
    if (length(created) > 0) {
      cat("    Variables creadas:", length(created), "\n")
    }
    if (length(failed) > 0) {
      cat("    Variables fallidas:", length(failed),
          "(revise las expresiones)\n")
    }
    cat("\n")
  }

  list(
    data = data,
    created = created,
    failed = failed
  )
}
