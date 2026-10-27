# =============================================================================
# Funciones de Tuning de Hiperparametros para PsyML
# =============================================================================

#' @title Tune the Hyperparameters of the Best Model
#'
#' @description
#' Tunes the hyperparameters of the best model selected by [train_models()]
#' with cross-validation on the training folds, and returns the finalized
#' workflow with the best combination. Tunable models are `"rf"`,
#' `"xgboost"`, `"svm"`, `"nnet"`, `"glmnet"` and `"tree"`; for other models
#' (e.g. `"glm"`) tuning is skipped and the untuned workflow is returned.
#'
#' @param modeling_result Result of [train_models()] (or the `modeling`
#'   element of a [supervised_ml()] result). It must contain `best_model`,
#'   `task`, `recipe` and `cv_folds`.
#' @param method Search method: `"random"` (random search, default),
#'   `"grid"` (grid search), `"bayes"` (Bayesian optimization) or `"racing"`
#'   (racing with ANOVA; requires \pkg{finetune}, otherwise random search is
#'   used with a warning).
#' @param grid_size Number of hyperparameter combinations evaluated with the
#'   grid, random and racing methods (default: 20).
#' @param iter Maximum number of iterations of Bayesian optimization
#'   (default: 30).
#' @param select_metric Metric used to select the best combination. If NULL
#'   (default) ROC-AUC is used for classification and R-squared for
#'   regression (also when `"rank_mean"` is given).
#' @param seed Optional integer seed. If NULL (default) the random number
#'   generator state is not modified.
#' @param verbose Logical. Print the progress and the interpretation of the
#'   best hyperparameters (default: TRUE).
#'
#' @return A list (class `supervisedml_tuning` when tuning was performed)
#'   with `original_model` (name of the model), `tuned` (logical),
#'   `best_params` (named list with the best hyperparameter values, NULL if not
#'   tuned), `final_workflow` (finalized, unfitted tidymodels workflow) and,
#'   when tuned, `tune_results` (the `tune_results` object), `task` and
#'   `method`.
#'
#' @examples
#' \donttest{
#' set.seed(1)
#' n <- 200
#' df <- data.frame(x1 = rnorm(n), x2 = rnorm(n), x3 = rnorm(n))
#' df$y <- factor(ifelse(df$x1 + 0.5 * df$x2 + rnorm(n) > 0, "yes", "no"))
#' res <- supervised_ml(df, target = "y", models = "rf", cv_folds = 3,
#'                      tune_best = FALSE, run_shap = FALSE, n_cores = 1,
#'                      verbose = FALSE)
#' tuned <- tune_best_model(res$modeling, method = "random", grid_size = 3,
#'                          select_metric = "roc_auc", seed = 1,
#'                          verbose = FALSE)
#' tuned$best_params
#' }
#'
#' @family supervised learning
#' @export
tune_best_model <- function(modeling_result,
                            method = "random",
                            grid_size = 20,
                            iter = 30,
                            select_metric = NULL,
                            seed = NULL,
                            verbose = TRUE) {

  if (!is.null(seed)) set.seed(seed)

  best_model <- modeling_result$best_model
  task <- modeling_result$task
  recipe <- modeling_result$recipe
  cv_folds <- modeling_result$cv_folds

  if (verbose) {
    .print_section(4, "Tuning de Hiperparametros")
  }

  results <- list()
  results$original_model <- best_model

  # 4.1 Identificar Mejor Modelo
  if (verbose) .print_subsection(4, 1, "Identificar Mejor Modelo")

  if (!best_model %in% c("rf", "xgboost", "svm", "nnet", "glmnet", "tree")) {
    if (verbose) {
      cat("    Mejor modelo:", .get_model_label(best_model), "\n")
      cat("    [!] Este modelo no tiene hiperparametros tuneables\n")
      cat("    Omitiendo tuning...\n")
    }

    results$tuned <- FALSE
    results$best_params <- NULL
    results$final_workflow <- .create_final_workflow(best_model, task, recipe)
    return(results)
  }

  # Validar metodo

  method <- match.arg(method, c("grid", "random", "bayes", "racing"))

  method_labels <- c(
    grid = "Grid Search (busqueda exhaustiva en cuadricula)",
    random = "Random Search (busqueda aleatoria)",
    bayes = "Bayesian Optimization (optimizacion bayesiana)",
    racing = "Racing ANOVA (descarte temprano de malas combinaciones)"
  )

  if (verbose) {
    cat("    Los hiperparametros son configuraciones del modelo que afectan\n")
    cat("    como aprende de los datos. A diferencia de los parametros normales\n")
    cat("    (que el modelo aprende automaticamente), los hiperparametros deben\n")
    cat("    ser definidos antes del entrenamiento. El 'tuning' busca los mejores\n")
    cat("    valores probando diferentes combinaciones.\n\n")

    cat("    Modelo seleccionado:", .get_model_label(best_model), "\n")
    cat("    Metodo de busqueda:", method_labels[method], "\n")
    if (method %in% c("grid", "random")) {
      cat("    Combinaciones a evaluar:", grid_size, "\n")
    } else if (method == "bayes") {
      cat("    Iteraciones maximas:", iter, "\n")
    } else if (method == "racing") {
      cat("    Combinaciones iniciales:", grid_size, "\n")
    }
    cat("\n    Hiperparametros a tunear:\n")
    params_info <- .get_tunable_params_info(best_model)
    for (p in names(params_info)) {
      cat("      -", p, "\n")
      cat("        ", params_info[[p]], "\n")
    }
  }

  # 4.2 Realizar Tuning
  if (verbose) {
    .print_subsection(4, 2, "Busqueda de Hiperparametros")
    cat("    Ahora se probaran diferentes combinaciones de hiperparametros.\n")
    cat("    Cada combinacion se evalua usando validacion cruzada para estimar\n")
    cat("    su rendimiento. Al final, se selecciona la combinacion que obtuvo\n")
    cat("    el mejor resultado promedio.\n\n")
  }

  tune_result <- .perform_tuning(
    model_name = best_model,
    recipe = recipe,
    cv_folds = cv_folds,
    task = task,
    method = method,
    grid_size = grid_size,
    iter = iter,
    select_metric = select_metric,
    verbose = verbose
  )

  results$tune_results <- tune_result$tune_results
  results$best_params <- tune_result$best_params
  results$final_workflow <- tune_result$workflow
  results$tuned <- TRUE

  if (verbose) {
    cat("\n    Mejores hiperparametros encontrados:\n")
    params_interpretation <- .interpret_best_params(best_model, tune_result$best_params)
    for (param in names(tune_result$best_params)) {
      cat("      -", param, ":", tune_result$best_params[[param]], "\n")
      if (param %in% names(params_interpretation)) {
        cat("        ", params_interpretation[[param]], "\n")
      }
    }
    cat("\n    Estos valores seran utilizados para entrenar el modelo final\n")
    cat("    que se evaluara en el conjunto de test (Seccion 5. Evaluacion).\n")
  }

  # 4.3 Resultados del Tuning
  if (verbose) .print_subsection(4, 3, "Resultados del Tuning")

  if (verbose) {
    cat("    El tuning evaluo multiples combinaciones de hiperparametros usando\n")
    cat("    validacion cruzada. La mejor combinacion es la que obtuvo el mejor\n")
    cat("    rendimiento promedio en todos los folds.\n\n")

    # Mostrar mejora (dinamico segun select_metric)
    metrics_tune <- tune::collect_metrics(tune_result$tune_results)

    metric_labels_tune <- c(
      roc_auc = "ROC-AUC", f_meas = "F1-Score", f2_meas = "F2-Score",
      accuracy = "Accuracy", mcc = "MCC", kap = "Cohen's Kappa",
      rmse = "RMSE", rsq = "R-squared", mae = "MAE"
    )
    maximize_metrics <- c("roc_auc", "f_meas", "f2_meas", "accuracy", "mcc", "kap", "rsq")

    metric_vals <- metrics_tune$mean[metrics_tune$.metric == select_metric]
    if (length(metric_vals) > 0) {
      best_val <- if (select_metric %in% maximize_metrics) max(metric_vals) else min(metric_vals)
      label <- metric_labels_tune[select_metric]
      cat("    Mejor", label, "en CV:", round(best_val, 4), "\n")

      interpretation <- .interpret_metric_value(select_metric, best_val)
      cat("    Interpretacion:", interpretation, "\n\n")
    }

    cat("    Siguiente paso: El modelo final sera entrenado con estos hiperparametros\n")
    cat("    optimizados y se evaluara en el conjunto de test (datos que el modelo\n")
    cat("    nunca ha visto) para medir su rendimiento real.\n")

    # Imprimir referencia segun metodo al final
    if (method == "bayes") {
      .print_reference("bayes_opt")
    } else if (method == "racing") {
      .print_reference("racing")
    } else if (method == "grid") {
      .print_reference("grid_search")
    } else {
      .print_reference("random_search")
    }
  }

  results$task <- task
  results$method <- method
  class(results) <- c("supervisedml_tuning", "list")

  return(results)
}


#' Get the Names of the Tunable Parameters
#' @noRd
.get_tunable_params <- function(model_name) {
  if (model_name == "rf") {
    return(c("mtry", "min_n"))
  } else if (model_name == "xgboost") {
    return(c("mtry", "min_n", "tree_depth", "learn_rate"))
  } else if (model_name == "svm") {
    return(c("cost", "rbf_sigma"))
  } else if (model_name == "nnet") {
    return(c("hidden_units", "penalty"))
  } else if (model_name == "glmnet") {
    return(c("penalty", "mixture"))
  } else if (model_name == "tree") {
    return(c("cost_complexity", "tree_depth", "min_n"))
  }
  return(character())
}

#' Get the Tunable Parameters with Descriptions
#' @noRd
.get_tunable_params_info <- function(model_name) {
  if (model_name == "rf") {
    return(list(
      "mtry (variables por division)" =
        "Cuantas variables considera en cada division del arbol. Mas variables = decisiones mas informadas pero puede sobreajustar.",
      "min_n (observaciones minimas)" =
        "Minimo de casos requeridos para dividir un nodo. Valores altos = modelo mas simple, valores bajos = mas detalle."
    ))
  } else if (model_name == "xgboost") {
    return(list(
      "mtry (variables por division)" =
        "Cuantas variables considera en cada division. Similar a Random Forest.",
      "min_n (observaciones minimas)" =
        "Minimo de casos para dividir. Controla la complejidad del modelo.",
      "tree_depth (profundidad del arbol)" =
        "Niveles maximos de cada arbol. Mas profundo = captura patrones complejos pero puede sobreajustar.",
      "learn_rate (tasa de aprendizaje)" =
        "Velocidad de aprendizaje. Valores pequenos = aprendizaje lento pero estable, valores grandes = rapido pero inestable."
    ))
  } else if (model_name == "svm") {
    return(list(
      "cost (costo de penalizacion)" =
        "Controla cuanto penaliza las clasificaciones incorrectas. Valores altos = menos errores en train pero riesgo de sobreajuste.",
      "rbf_sigma (ancho del kernel RBF)" =
        "Controla la influencia de cada observacion. Valores bajos = decision suave, valores altos = decision mas compleja."
    ))
  } else if (model_name == "nnet") {
    return(list(
      "hidden_units (neuronas ocultas)" =
        "Numero de neuronas en la capa oculta. Mas neuronas = mayor capacidad de capturar patrones complejos.",
      "penalty (regularizacion / weight decay)" =
        "Penalizacion sobre los pesos de la red. Valores altos = modelo mas simple, reduce sobreajuste."
    ))
  } else if (model_name == "glmnet") {
    return(list(
      "penalty (penalizacion lambda)" =
        "Fuerza de la regularizacion. Valores altos = coeficientes mas pequenos, modelo mas simple.",
      "mixture (mezcla Lasso/Ridge)" =
        "0 = Ridge (penaliza magnitud), 1 = Lasso (elimina variables), 0.5 = Elastic Net (ambos)."
    ))
  } else if (model_name == "tree") {
    return(list(
      "cost_complexity (parametro de poda)" =
        "Controla cuanto se poda el arbol. Valores altos = arbol mas simple, valores bajos = arbol mas complejo.",
      "tree_depth (profundidad maxima)" =
        "Niveles maximos del arbol. Mas profundo = captura interacciones complejas pero puede sobreajustar.",
      "min_n (observaciones minimas)" =
        "Minimo de casos requeridos para dividir un nodo. Valores altos = arbol mas simple."
    ))
  }
  return(list())
}


#' Interpret the Best Hyperparameter Values
#' @noRd
.interpret_best_params <- function(model_name, params) {
  interpretations <- list()

  # mtry: variables por division

  if ("mtry" %in% names(params)) {
    mtry_val <- params$mtry
    if (mtry_val <= 3) {
      interpretations$mtry <- paste0("Considera pocas variables (", mtry_val, ") por decision = modelo mas diverso y robusto")
    } else if (mtry_val <= 7) {
      interpretations$mtry <- paste0("Considera un numero moderado de variables (", mtry_val, ") = buen balance")
    } else {
      interpretations$mtry <- paste0("Considera muchas variables (", mtry_val, ") por decision = decisiones muy informadas")
    }
  }

  # min_n: observaciones minimas
  if ("min_n" %in% names(params)) {
    min_n_val <- params$min_n
    if (min_n_val <= 5) {
      interpretations$min_n <- paste0("Permite nodos pequenos (", min_n_val, " obs) = modelo detallado, puede captar patrones finos")
    } else if (min_n_val <= 15) {
      interpretations$min_n <- paste0("Requiere nodos medianos (", min_n_val, " obs) = buen balance entre detalle y generalizacion")
    } else {
      interpretations$min_n <- paste0("Requiere nodos grandes (", min_n_val, " obs) = modelo mas simple y generalizable")
    }
  }

  # tree_depth: profundidad del arbol (XGBoost)
  if ("tree_depth" %in% names(params)) {
    depth_val <- params$tree_depth
    if (depth_val <= 3) {
      interpretations$tree_depth <- paste0("Arboles poco profundos (", depth_val, " niveles) = modelo simple, menos riesgo de sobreajuste")
    } else if (depth_val <= 6) {
      interpretations$tree_depth <- paste0("Profundidad moderada (", depth_val, " niveles) = buen balance complejidad-generalizacion")
    } else {
      interpretations$tree_depth <- paste0("Arboles profundos (", depth_val, " niveles) = captura patrones complejos")
    }
  }

  # learn_rate: tasa de aprendizaje (XGBoost)
  if ("learn_rate" %in% names(params)) {
    lr_val <- params$learn_rate
    if (lr_val <= 0.05) {
      interpretations$learn_rate <- paste0("Aprendizaje lento (", round(lr_val, 4), ") = conservador, necesita mas arboles pero mas estable")
    } else if (lr_val <= 0.2) {
      interpretations$learn_rate <- paste0("Aprendizaje moderado (", round(lr_val, 4), ") = buen balance velocidad-estabilidad")
    } else {
      interpretations$learn_rate <- paste0("Aprendizaje rapido (", round(lr_val, 4), ") = agresivo, puede converger rapido pero riesgo de inestabilidad")
    }
  }

  # cost: costo de penalizacion (SVM)
  if ("cost" %in% names(params)) {
    cost_val <- params$cost
    if (cost_val <= 0.1) {
      interpretations$cost <- paste0("Penalizacion baja (", round(cost_val, 4), ") = margen amplio, modelo mas tolerante a errores")
    } else if (cost_val <= 10) {
      interpretations$cost <- paste0("Penalizacion moderada (", round(cost_val, 4), ") = buen balance margen-precision")
    } else {
      interpretations$cost <- paste0("Penalizacion alta (", round(cost_val, 4), ") = margen estrecho, ajuste preciso pero riesgo de sobreajuste")
    }
  }

  # rbf_sigma: ancho del kernel (SVM)
  if ("rbf_sigma" %in% names(params)) {
    sigma_val <- params$rbf_sigma
    if (sigma_val <= 0.01) {
      interpretations$rbf_sigma <- paste0("Kernel amplio (sigma=", round(sigma_val, 4), ") = decision suave, mayor generalizacion")
    } else if (sigma_val <= 0.1) {
      interpretations$rbf_sigma <- paste0("Kernel moderado (sigma=", round(sigma_val, 4), ") = buen balance suavidad-complejidad")
    } else {
      interpretations$rbf_sigma <- paste0("Kernel estrecho (sigma=", round(sigma_val, 4), ") = decision compleja, riesgo de sobreajuste")
    }
  }

  # hidden_units: neuronas ocultas (nnet)
  if ("hidden_units" %in% names(params)) {
    hu_val <- params$hidden_units
    if (hu_val <= 5) {
      interpretations$hidden_units <- paste0("Pocas neuronas (", hu_val, ") = modelo simple, captura patrones lineales o basicos")
    } else if (hu_val <= 15) {
      interpretations$hidden_units <- paste0("Neuronas moderadas (", hu_val, ") = buen balance capacidad-generalizacion")
    } else {
      interpretations$hidden_units <- paste0("Muchas neuronas (", hu_val, ") = alta capacidad, captura patrones complejos")
    }
  }

  # penalty: regularizacion (nnet)
  if ("penalty" %in% names(params)) {
    pen_val <- params$penalty
    if (pen_val <= 0.001) {
      interpretations$penalty <- paste0("Regularizacion minima (", round(pen_val, 4), ") = red libre para aprender, riesgo de sobreajuste")
    } else if (pen_val <= 0.1) {
      interpretations$penalty <- paste0("Regularizacion moderada (", round(pen_val, 4), ") = buen balance flexibilidad-control")
    } else {
      interpretations$penalty <- paste0("Regularizacion fuerte (", round(pen_val, 4), ") = pesos restringidos, modelo mas simple")
    }
  }

  # mixture: mezcla Lasso/Ridge (glmnet)
  if ("mixture" %in% names(params)) {
    mix_val <- params$mixture
    if (mix_val <= 0.1) {
      interpretations$mixture <- paste0("Casi puro Ridge (", round(mix_val, 2), ") = penaliza magnitud de coeficientes, mantiene todas las variables")
    } else if (mix_val >= 0.9) {
      interpretations$mixture <- paste0("Casi puro Lasso (", round(mix_val, 2), ") = selecciona variables, elimina las menos importantes")
    } else {
      interpretations$mixture <- paste0("Elastic Net (", round(mix_val, 2), ") = combina Ridge y Lasso, buen balance")
    }
  }

  # cost_complexity: parametro de poda (tree)
  if ("cost_complexity" %in% names(params)) {
    cp_val <- params$cost_complexity
    if (cp_val <= 0.001) {
      interpretations$cost_complexity <- paste0("Poda minima (cp=", round(cp_val, 4), ") = arbol complejo, captura detalles finos")
    } else if (cp_val <= 0.01) {
      interpretations$cost_complexity <- paste0("Poda moderada (cp=", round(cp_val, 4), ") = buen balance complejidad-simplicidad")
    } else {
      interpretations$cost_complexity <- paste0("Poda agresiva (cp=", round(cp_val, 4), ") = arbol simple, mas generalizable")
    }
  }

  return(interpretations)
}


#' Perform the Tuning
#' @noRd
.perform_tuning <- function(model_name, recipe, cv_folds, task, method, grid_size, iter, select_metric = NULL, verbose) {

  # Establecer metrica de seleccion
  if (is.null(select_metric)) {
    select_metric <- if (task == "classification") "roc_auc" else "rsq"
  }
  # rank_mean no es una metrica de yardstick; usar roc_auc/rsq para tuning
  if (select_metric == "rank_mean") {
    select_metric <- if (task == "classification") "roc_auc" else "rsq"
  }

  # Definir metricas
  if (task == "classification") {
    f2_meas <- yardstick::metric_tweak("f2_meas", yardstick::f_meas, beta = 2)
    metrics_set <- yardstick::metric_set(
      yardstick::roc_auc,
      yardstick::accuracy,
      yardstick::mcc,
      yardstick::kap,
      f2_meas
    )
  } else {
    metrics_set <- yardstick::metric_set(
      yardstick::rmse,
      yardstick::rsq
    )
  }

  # Crear especificacion con tune()
  if (model_name == "rf") {
    tune_spec <- parsnip::rand_forest(
      mtry = tune::tune(),
      min_n = tune::tune(),
      trees = 500
    ) |>
      parsnip::set_engine("ranger", importance = "impurity") |>
      parsnip::set_mode(task)

  } else if (model_name == "xgboost") {
    tune_spec <- parsnip::boost_tree(
      mtry = tune::tune(),
      min_n = tune::tune(),
      tree_depth = tune::tune(),
      learn_rate = tune::tune(),
      trees = 500
    ) |>
      parsnip::set_engine("xgboost") |>
      parsnip::set_mode(task)

  } else if (model_name == "svm") {
    tune_spec <- parsnip::svm_rbf(
      cost = tune::tune(),
      rbf_sigma = tune::tune()
    ) |>
      parsnip::set_engine("kernlab") |>
      parsnip::set_mode(task)

  } else if (model_name == "nnet") {
    tune_spec <- parsnip::mlp(
      hidden_units = tune::tune(),
      penalty = tune::tune(),
      epochs = 100
    ) |>
      parsnip::set_engine("nnet") |>
      parsnip::set_mode(task)

  } else if (model_name == "glmnet") {
    tune_spec <- if (task == "classification") {
      parsnip::logistic_reg(
        penalty = tune::tune(),
        mixture = tune::tune()
      ) |>
        parsnip::set_engine("glmnet") |>
        parsnip::set_mode("classification")
    } else {
      parsnip::linear_reg(
        penalty = tune::tune(),
        mixture = tune::tune()
      ) |>
        parsnip::set_engine("glmnet") |>
        parsnip::set_mode("regression")
    }

  } else if (model_name == "tree") {
    tune_spec <- parsnip::decision_tree(
      cost_complexity = tune::tune(),
      tree_depth = tune::tune(),
      min_n = tune::tune()
    ) |>
      parsnip::set_engine("rpart") |>
      parsnip::set_mode(task)
  }

  # Workflow
  wf <- workflows::workflow() |>
    workflows::add_recipe(recipe) |>
    workflows::add_model(tune_spec)

  # Ejecutar busqueda segun metodo
  if (method == "grid") {
    # Grid Search: cuadricula regular
    if (verbose) {
      cat("    Generando cuadricula regular de", grid_size, "puntos...\n")
    }

    # Crear grid regular
    grid <- dials::grid_regular(
      hardhat::extract_parameter_set_dials(wf),
      levels = ceiling(grid_size^(1/length(.get_tunable_params(model_name))))
    )

    if (verbose) {
      cat("    Evaluando", nrow(grid), "combinaciones...\n")
    }

    tune_results <- suppressMessages(
      tune::tune_grid(
        wf,
        resamples = cv_folds,
        grid = grid,
        metrics = metrics_set,
        control = tune::control_grid(verbose = FALSE)
      )
    )

  } else if (method == "random") {
    # Random Search: puntos aleatorios en el espacio
    if (verbose) {
      cat("    Evaluando", grid_size, "combinaciones aleatorias...\n")
    }

    tune_results <- suppressMessages(
      tune::tune_grid(
        wf,
        resamples = cv_folds,
        grid = grid_size,
        metrics = metrics_set,
        control = tune::control_grid(verbose = FALSE)
      )
    )

  } else if (method == "bayes") {
    # Bayesian Optimization
    if (verbose) {
      cat("    Iniciando optimizacion bayesiana...\n")
      cat("    Evaluaciones iniciales: 5, Iteraciones: ", iter, "\n")
    }

    tune_results <- suppressMessages(
      tune::tune_bayes(
        wf,
        resamples = cv_folds,
        initial = 5,
        iter = iter,
        metrics = metrics_set,
        control = tune::control_bayes(verbose = FALSE, no_improve = 10)
      )
    )

  } else if (method == "racing") {
    # Racing ANOVA: descarta malas combinaciones temprano
    if (verbose) {
      cat("    Iniciando Racing ANOVA con", grid_size, "candidatos iniciales...\n")
    }

    # Requiere finetune package
    if (!requireNamespace("finetune", quietly = TRUE)) {
      warning("Package 'finetune' is not available. Using random search instead.")
      tune_results <- suppressMessages(
        tune::tune_grid(
          wf,
          resamples = cv_folds,
          grid = grid_size,
          metrics = metrics_set,
          control = tune::control_grid(verbose = FALSE)
        )
      )
    } else {
      tune_results <- suppressMessages(
        finetune::tune_race_anova(
          wf,
          resamples = cv_folds,
          grid = grid_size,
          metrics = metrics_set,
          control = finetune::control_race(verbose = FALSE)
        )
      )
    }
  }

  # Seleccionar mejores parametros
  best_params <- tune::select_best(tune_results, metric = select_metric)

  # Finalizar workflow
  final_wf <- tune::finalize_workflow(wf, best_params)

  # Convertir parametros a lista limpia
  params_list <- as.list(best_params)
  params_list$.config <- NULL

  list(
    tune_results = tune_results,
    best_params = params_list,
    workflow = final_wf
  )
}


#' Create the Final Workflow Without Tuning
#' @noRd
.create_final_workflow <- function(model_name, task, recipe) {
  model_spec <- .get_model_spec(model_name, task)

  workflows::workflow() |>
    workflows::add_recipe(recipe) |>
    workflows::add_model(model_spec)
}


#' @title Plot the Hyperparameter Tuning Results
#'
#' @description
#' Plots the cross-validated performance of the evaluated hyperparameter
#' combinations with [tune::autoplot()][tune::autoplot.tune_results].
#'
#' @param tune_result Result of [tune_best_model()] (or the `tuning` element
#'   of a [supervised_ml()] result).
#' @param metric Optional character vector with the metrics to plot. If NULL
#'   (default) all computed metrics are shown.
#'
#' @return A `ggplot` object, or NULL (with a message) when no tuning was
#'   performed.
#'
#' @examples
#' \donttest{
#' set.seed(1)
#' n <- 200
#' df <- data.frame(x1 = rnorm(n), x2 = rnorm(n), x3 = rnorm(n))
#' df$y <- factor(ifelse(df$x1 + 0.5 * df$x2 + rnorm(n) > 0, "yes", "no"))
#' res <- supervised_ml(df, target = "y", models = "glm", cv_folds = 3,
#'                      tune_best = FALSE, run_shap = FALSE, n_cores = 1,
#'                      verbose = FALSE)
#' plot_tuning(res$tuning)  # NULL: no tuning was performed
#'
#' res_rf <- supervised_ml(df, target = "y", models = "rf", cv_folds = 3,
#'                         tune_grid = 3, run_shap = FALSE, n_cores = 1,
#'                         verbose = FALSE)
#' plot_tuning(res_rf$tuning, metric = "roc_auc")
#' }
#'
#' @family supervised learning
#' @export
plot_tuning <- function(tune_result, metric = NULL) {

  if (!isTRUE(tune_result$tuned) || is.null(tune_result$tune_results)) {
    message("No tuning results to plot")
    return(NULL)
  }

  tune::autoplot(tune_result$tune_results, metric = metric)
}
