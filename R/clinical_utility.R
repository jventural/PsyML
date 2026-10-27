# =============================================================================
# Decision Curve Analysis (DCA) - Utilidad Clinica para PsyML
# Solo clasificacion binaria
# =============================================================================

#' @title Clinical Utility Analysis (Decision Curve Analysis)
#'
#' @description
#' Computes the net benefit of a binary classification model across
#' probability thresholds and compares it with the "treat all" and "treat
#' none" strategies, following Vickers and Elkin (2006).
#'
#' @param predictions Data frame with one row per case containing the observed
#'   outcome (\code{target}, a factor with 2 levels) and the predicted
#'   probability of the positive class (column \code{.pred_<positive level>}).
#' @param target Name of the outcome variable.
#' @param thresholds Numeric vector of probability thresholds to evaluate
#'   (default \code{seq(0.01, 0.99, by = 0.01)}).
#' @param verbose Logical (default TRUE). Print the results to the console.
#'
#' @return A list with:
#'   \itemize{
#'     \item dca_data: Data frame with one row per threshold and columns
#'       \code{threshold}, \code{nb_model} (net benefit of the model),
#'       \code{nb_all} (treat all), \code{nb_none} (treat none, always 0),
#'       \code{tp_rate} and \code{fp_rate} (true and false positives divided
#'       by n), \code{nnt} (number needed to treat, \code{1 / nb_model}, Inf
#'       when the net benefit is not positive) and \code{model_useful}
#'       (TRUE when the model beats both reference strategies).
#'     \item useful_range: Lowest and highest threshold at which the model is
#'       useful (NA, NA if never).
#'     \item max_net_benefit: Maximum net benefit of the model.
#'     \item optimal_threshold_dca: Threshold with the maximum net benefit.
#'     \item prevalence: Observed prevalence of the positive class.
#'     \item n: Number of cases.
#'   }
#'
#' @references
#' Vickers, A. J., & Elkin, E. B. (2006). Decision curve analysis: A novel
#' method for evaluating prediction models. Medical Decision Making, 26(6),
#' 565-574.
#'
#' @examples
#' set.seed(1)
#' n <- 300
#' prob <- plogis(rnorm(n, sd = 1.5))
#' preds <- data.frame(
#'   outcome = factor(ifelse(runif(n) < prob, "yes", "no"), levels = c("no", "yes")),
#'   .pred_yes = prob
#' )
#' dca <- analyze_dca(preds, target = "outcome")
#' dca$useful_range
#' head(dca$dca_data)
#'
#' @family supervised learning
#' @export
analyze_dca <- function(predictions, target,
                        thresholds = seq(0.01, 0.99, by = 0.01),
                        verbose = TRUE) {

  # --- Validaciones ---
  if (!target %in% names(predictions)) {
    stop("Outcome variable '", target, "' not found in 'predictions'")
  }

  target_vec <- predictions[[target]]
  if (!is.factor(target_vec) || length(levels(target_vec)) != 2) {
    stop("DCA requires binary classification (outcome factor with 2 levels)")
  }

  # Detectar clase positiva y columna de probabilidades
  event_info <- .detect_event_level(target_vec)
  pos_class <- event_info$positive_class
  prob_col <- event_info$prob_col

  if (!prob_col %in% names(predictions)) {
    stop("Probability column '", prob_col, "' not found.\n",
         "DCA requires predicted probabilities.")
  }

  probs <- predictions[[prob_col]]
  truth <- as.integer(target_vec == pos_class)
  n <- length(truth)
  prevalence <- mean(truth)

  if (verbose) {
    cat("  Clase positiva: ", pos_class, "\n", sep = "")
    cat("  Prevalencia: ", round(prevalence * 100, 1), "%\n", sep = "")
    cat("  n: ", n, "\n", sep = "")
    cat("  Thresholds evaluados: ", length(thresholds), "\n\n", sep = "")
  }

  # --- Calcular Net Benefit a cada threshold ---
  dca_rows <- lapply(thresholds, function(pt) {
    pred_pos <- as.integer(probs >= pt)
    tp <- sum(pred_pos == 1 & truth == 1)
    fp <- sum(pred_pos == 1 & truth == 0)

    # Net Benefit del modelo
    odds_ratio <- pt / (1 - pt)
    nb_model <- tp / n - fp / n * odds_ratio

    # Net Benefit de tratar a todos
    nb_all <- prevalence - (1 - prevalence) * odds_ratio

    # Net Benefit de no tratar a nadie = 0

    # NNT (Number Needed to Treat) cuando nb > 0
    nnt <- if (nb_model > 0) 1 / nb_model else Inf

    data.frame(
      threshold = pt,
      nb_model = nb_model,
      nb_all = nb_all,
      nb_none = 0,
      tp_rate = tp / n,
      fp_rate = fp / n,
      nnt = nnt,
      stringsAsFactors = FALSE
    )
  })

  dca_data <- do.call(rbind, dca_rows)

  # --- Rango util: donde modelo > max(treat_all, 0) ---
  dca_data$model_useful <- dca_data$nb_model > pmax(dca_data$nb_all, 0)
  useful_idx <- which(dca_data$model_useful)

  if (length(useful_idx) > 0) {
    useful_range <- c(dca_data$threshold[min(useful_idx)],
                      dca_data$threshold[max(useful_idx)])
  } else {
    useful_range <- c(NA, NA)
  }

  # Net Benefit maximo del modelo
  max_nb <- max(dca_data$nb_model, na.rm = TRUE)
  optimal_threshold <- dca_data$threshold[which.max(dca_data$nb_model)]

  # --- Verbose ---
  if (verbose) {
    cat("  DECISION CURVE ANALYSIS:\n")
    cat("  ", paste(rep("-", 50), collapse = ""), "\n", sep = "")
    cat("  Concepto: El Net Benefit cuantifica el beneficio clinico neto\n")
    cat("  de usar el modelo para decidir a quien tratar, penalizando\n")
    cat("  los falsos positivos segun la importancia relativa del threshold.\n\n")

    # Tabla a thresholds clave
    key_pts <- c(0.05, 0.10, 0.20, 0.30, 0.50)
    key_pts <- key_pts[key_pts %in% dca_data$threshold]

    cat(sprintf("  %-12s %10s %10s %10s %8s\n",
                "Threshold", "NB Modelo", "NB Todos", "NB Nadie", "NNT"))
    cat("  ", paste(rep("-", 55), collapse = ""), "\n", sep = "")

    for (pt in key_pts) {
      row <- dca_data[dca_data$threshold == pt, ]
      nnt_str <- if (row$nnt == Inf) "Inf" else as.character(round(row$nnt))
      marker <- if (row$model_useful) " *" else ""
      cat(sprintf("  %-12.2f %10.4f %10.4f %10.4f %8s%s\n",
                  pt, row$nb_model, row$nb_all, row$nb_none, nnt_str, marker))
    }
    cat("  (* = modelo superior a tratar todos y a nadie)\n")

    cat("\n  RESUMEN:\n")
    if (!is.na(useful_range[1])) {
      cat("    Rango util del modelo: [", round(useful_range[1], 2),
          ", ", round(useful_range[2], 2), "]\n", sep = "")
      cat("    En este rango, usar el modelo es mejor que tratar a todos\n")
      cat("    o no tratar a nadie.\n")
    } else {
      cat("    El modelo no supera las estrategias triviales en ningun\n")
      cat("    threshold. Considerar recalibrar o mejorar el modelo.\n")
    }

    cat("    Net Benefit maximo: ", round(max_nb, 4),
        " (threshold = ", round(optimal_threshold, 2), ")\n", sep = "")

    # Interpretacion NNT en threshold optimo
    best_row <- dca_data[dca_data$threshold == optimal_threshold, ]
    if (best_row$nnt < Inf && best_row$nnt > 0) {
      cat("    NNT en threshold optimo: ", round(best_row$nnt),
          " (tratar ", round(best_row$nnt), " para beneficiar a 1)\n", sep = "")
    }

    cat("\n  INTERPRETACION CLINICA:\n")
    if (!is.na(useful_range[1])) {
      cat("    Si el clinico considera que el costo de un falso positivo\n")
      cat("    es entre ", round(useful_range[1] / (1 - useful_range[1]), 1),
          " y ", round(useful_range[2] / (1 - useful_range[2]), 1),
          " veces menor que perder\n", sep = "")
      cat("    un verdadero positivo, el modelo agrega valor clinico.\n")

      if (useful_range[2] - useful_range[1] > 0.3) {
        cat("    El modelo es clinicamente util en un rango amplio de\n")
        cat("    preferencias del decisor.\n")
      } else if (useful_range[2] - useful_range[1] > 0.1) {
        cat("    El modelo es clinicamente util en un rango moderado.\n")
      } else {
        cat("    El modelo es clinicamente util solo en un rango estrecho.\n")
      }
    }
  }

  # --- Resultado ---
  list(
    dca_data = dca_data,
    useful_range = useful_range,
    max_net_benefit = round(max_nb, 4),
    optimal_threshold_dca = round(optimal_threshold, 4),
    prevalence = round(prevalence, 4),
    n = n
  )
}
