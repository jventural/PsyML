# ============================================================================
# TUTORIAL COMPLETO: easyML con Student Depression Dataset
# ============================================================================
# Dataset: Student Depression (Kaggle, N = 27,901)
# Target:  Depression (0 = No, 1 = Si) -> Clasificacion binaria
# Autor:   Jose Ventura-Leon
# ============================================================================
#
# CONTENIDO:
#   PARTE A: Pipeline completo con easy_ml() [una sola funcion]
#   PARTE B: Pipeline paso a paso [funciones individuales]
#     1. Carga y Exploracion de Datos
#     2. Preprocesamiento de Datos
#     3. Modelado
#     4. Tuning de Hiperparametros
#     5. Evaluacion en Test
#     6. Interpretabilidad del Modelo
#     7. Analisis Avanzado
#     8. Generacion de Graficos
#   PARTE C: Estimacion de Tamano de Muestra con ml_sample_size()
#
# ============================================================================


# --- Configuracion inicial ---------------------------------------------------

library(easyML)
library(tidyverse)

# Definir directorio de trabajo
setwd("D:/14. LIBRERIAS/easyML_github/demo_depression")


# ============================================================================
# PREPARACION DE DATOS (comun a Parte A y B)
# ============================================================================

df <- read.csv("depression_data.csv", stringsAsFactors = FALSE)
cat("Dimensiones originales:", nrow(df), "x", ncol(df), "\n")

# Limpiar nombres de columnas
names(df) <- make.names(names(df))

# Eliminar columnas no utiles para ML
df <- df %>%
  select(-id, -City,                 # id = identificador, City = demasiadas categorias
         -Work.Pressure,             # Varianza cero (todos son estudiantes, valor = 0)
         -Job.Satisfaction) %>%      # Varianza cero (todos son estudiantes, valor = 0)
  drop_na(Depression)                # Asegurar target sin NAs

# Convertir target a factor
df$Depression <- factor(df$Depression, levels = c(0, 1), labels = c("No", "Yes"))

# Muestra estratificada de 1000 (para que sea rapido en clase)
set.seed(2024)
df_sample <- df %>%
  group_by(Depression) %>%
  slice_sample(n = 500) %>%
  ungroup()

cat("Muestra final:", nrow(df_sample), "filas x", ncol(df_sample), "columnas\n")
cat("Distribucion del target:\n")
print(table(df_sample$Depression))
cat("\n")



# ############################################################################
#
#   PARTE A: PIPELINE COMPLETO CON easy_ml()
#
# ############################################################################
# Una sola funcion que ejecuta todo el pipeline de ML automaticamente:
#   EDA -> Preprocesamiento -> Modelado -> Tuning -> Evaluacion ->
#   Interpretabilidad -> Analisis Avanzado
# ============================================================================

result <- easy_ml(
  data             = df_sample,
  target           = "Depression",
  task             = "classification",
  models           = c("rf", "xgboost", "svm", "nnet", "glm", "tree"),
  test_split       = 0.20,
  cv_folds         = 10,
  tune_best        = TRUE,
  tune_method      = "random",
  tune_grid        = 20,
  balance_classes  = FALSE,
  run_eda          = TRUE,
  run_shap         = TRUE,
  n_shap           = 100,
  check_leakage    = TRUE,
  seed             = 2024,
  verbose          = TRUE
)

# --- Resultados rapidos ---
print(result)
summary(result)

# --- Grafico panel (ROC + Importancia + Confusion + Metricas) ---
plot(result)

# --- Exportar reporte completo a TXT ---
export_verbose_txt(result, file_path = "reporte_completo.txt")

# --- Interpretacion con IA ---
explain_with_ai(result, language = "es")



# ############################################################################
#
#   PARTE B: PIPELINE PASO A PASO
#
# ############################################################################
# Las mismas operaciones que easy_ml(), pero ejecutadas una por una.
# Esto permite mayor control, inspeccion intermedia y personalizacion.
# ============================================================================


# ============================================================================
# PASO 1: CARGA Y EXPLORACION DE DATOS (EDA)
# ============================================================================
# Las funciones eda_* analizan los datos ANTES del preprocesamiento.
# Ayudan a detectar problemas (NAs, outliers, desbalance, colinealidad).

# Resumen general: dimensiones, tipos, target
eda_result <- eda_summary(df_sample, target = "Depression", task = "classification")

# Estructura de variables (tipos, NAs, valores unicos)
eda_structure(df_sample)

# Distribucion del target (clases, proporciones, desbalance)
eda_target_classification(df_sample, target = "Depression")

# Normalidad del target (solo aplica a regresion)
# eda_normality(df_sample, target = "Depression")  # No aplica: Depression es categorica

# Patron de datos faltantes
eda_missing(df_sample)

# Analisis de variables categoricas (frecuencias, V de Cramer)
eda_categorical(df_sample, target = "Depression")

# Analisis de variables numericas (medias por grupo, d de Cohen)
eda_numeric(df_sample, target = "Depression")

# Deteccion de outliers (IQR x 3)
eda_outliers(df_sample, iqr_multiplier = 3)

# Correlaciones entre numericas
eda_correlation(df_sample, target = "Depression")

# VIF (Factor de Inflacion de Varianza -> multicolinealidad)
eda_vif(df_sample, target = "Depression")

# Deteccion de posible data leakage en las variables
eda_leakage(df_sample, target = "Depression")


# ============================================================================
# PASO 2: PREPROCESAMIENTO DE DATOS
# ============================================================================
# Convierte datos crudos en datos listos para modelar:
#   - Codifica categoricas (dummies)
#   - Imputa NAs
#   - Normaliza numericas
#   - Elimina variables con alta correlacion/VIF
#   - Divide en train/test

preprocess_result <- preprocess_data(
  data              = df_sample,
  target            = "Depression",
  task              = "classification",
  test_split        = 0.20,
  feature_selection = FALSE,
  balance_classes   = FALSE,
  impute            = TRUE,
  impute_method     = "median",
  normalize         = TRUE,
  normalize_method  = "zscore",
  treat_outliers    = TRUE,
  outlier_percentile = 0.05,
  remove_high_cor   = TRUE,
  cor_threshold     = 0.90,
  remove_high_vif   = TRUE,
  vif_threshold     = 5,
  seed              = 2024,
  verbose           = TRUE
)

# Inspeccion: que contiene el resultado?
cat("Train:", nrow(preprocess_result$train_data), "filas\n")
cat("Test: ", nrow(preprocess_result$test_data), "filas\n")
cat("Task: ", preprocess_result$task, "\n")

# --- Alternativa: paso a paso con funciones prep_* ---
# Si necesitas mas control, puedes ejecutar cada paso por separado:
#
# paso1 <- prep_target(df_sample, target = "Depression", task = "classification")
# paso2 <- prep_split(paso1$data, target = "Depression", task = "classification",
#                      test_split = 0.20, seed = 2024)
# paso3 <- prep_recipe(paso2$train_data, target = "Depression",
#                       task = "classification", impute = TRUE)


# ============================================================================
# PASO 3: MODELADO (Entrenamiento con Validacion Cruzada)
# ============================================================================
# Entrena multiples modelos con CV y compara su rendimiento.
# Modelos disponibles: rf, xgboost, svm, nnet, glm, tree
#
# NOTA: Usamos rf y xgboost porque son los que soportan tuning (Paso 4).
# Si usas los 6 modelos, puede ganar glm/svm que NO tienen tuning,
# y el Paso 4 se omite (best_params = NULL).

modeling_result <- train_models(
  preprocess_result = preprocess_result,
  models            = c("rf", "xgboost"),
  cv_folds          = 10,
  seed              = 2024,
  verbose           = TRUE
)

# Ver comparacion de modelos
cat("\nModelo ganador:", modeling_result$best_model, "\n")
cat("Metrica de seleccion:", modeling_result$metric_used, "\n")

# --- Alternativa: usar los 6 modelos disponibles ---
# modeling_result <- train_models(
#   preprocess_result = preprocess_result,
#   models = c("rf", "xgboost", "svm", "nnet", "glm", "tree"),
#   cv_folds = 10, seed = 2024, verbose = TRUE)

# --- Alternativa: paso a paso con funciones individuales ---
# cv_folds   <- setup_cv(preprocess_result$train_data, ...)
# model_defs <- define_models(c("rf", "xgboost"), task = "classification")
# cv_results <- fit_models_cv(model_defs, preprocess_result$recipe, cv_folds, ...)
# comparison <- compare_models(cv_results, task = "classification")


# ============================================================================
# PASO 4: TUNING DE HIPERPARAMETROS
# ============================================================================
# Optimiza los hiperparametros del mejor modelo encontrado en el paso 3.
# Metodos: "random" (rapido), "grid" (exhaustivo), "bayes", "racing"
# NOTA: Solo funciona con RF y XGBoost (los demas no tienen hiperparametros tuneables).

tuning_result <- tune_best_model(
  modeling_result = modeling_result,
  method          = "random",
  grid_size       = 20,
  seed            = 2024,
  verbose         = TRUE
)

# Ver mejores hiperparametros
cat("\nMejores hiperparametros encontrados:\n")
print(tuning_result$best_params)

# Grafico de tuning (rendimiento vs hiperparametros)
plot_tuning(tuning_result)


# ============================================================================
# PASO 5: EVALUACION EN TEST
# ============================================================================
# Evalua el modelo tuneado en datos que NUNCA ha visto (test set).
# Genera metricas, predicciones, matriz de confusion.

evaluation_result <- evaluate_model(
  tuning_result = tuning_result,
  train_data    = preprocess_result$train_data,
  test_data     = preprocess_result$test_data,
  target        = "Depression",
  task          = "classification",
  verbose       = TRUE
)

# Metricas en test
cat("\nMetricas en test set:\n")
print(evaluation_result$metrics)

# --- Funciones de evaluacion individuales ---
# Si necesitas graficos o metricas especificas:

# Curva ROC
eval_roc(evaluation_result$predictions, target = "Depression")

# Matriz de confusion
eval_confusion(evaluation_result$predictions, target = "Depression")

# Calibracion de probabilidades
eval_calibration(evaluation_result$predictions, target = "Depression")


# ============================================================================
# PASO 6: INTERPRETABILIDAD DEL MODELO
# ============================================================================
# Importancia de variables (VIP) y valores SHAP.
# SHAP explica CUANTO y EN QUE DIRECCION cada variable afecta la prediccion.

interpret_result <- interpret_model(
  evaluation_result = evaluation_result,
  train_data        = preprocess_result$train_data,
  test_data         = preprocess_result$test_data,
  target            = "Depression",
  n_shap            = 100,
  verbose           = TRUE
)

# Grafico de importancia de variables (top 15)
plot_importance(interpret_result, top_n = 15)

# SHAP summary plot (distribucion de impacto por variable)
plot_shap_summary(interpret_result, top_n = 15)

# SHAP dependence: como una variable especifica afecta la prediccion
plot_shap_dependence(interpret_result,
                     variable = "Academic.Pressure",
                     test_data = preprocess_result$test_data)


# ============================================================================
# PASO 7: ANALISIS AVANZADO
# ============================================================================

# --- 7.1 Optimizacion de umbral ---
# Por defecto el corte es 0.50, pero el umbral optimo depende de los costos
# de falsos positivos vs falsos negativos.

threshold_result <- optimize_threshold(
  predictions = evaluation_result$predictions,
  target      = "Depression",
  method      = "youden",
  verbose     = TRUE
)
cat("Umbral optimo (Youden):", threshold_result$optimal_threshold, "\n")

# Visualizar la curva de umbrales
plot_threshold_optimization(threshold_result)

# Aplicar el umbral optimo a las predicciones
new_preds <- apply_threshold(
  predictions = evaluation_result$predictions,
  target      = "Depression",
  threshold   = threshold_result$optimal_threshold
)


# --- 7.2 Calibracion de probabilidades ---
# Las probabilidades del modelo, reflejan la probabilidad real?

calibration_result <- calibrate_probabilities(
  predictions = evaluation_result$predictions,
  target      = "Depression",
  method      = "platt",
  verbose     = TRUE
)

# Curva de calibracion (diagonal = perfectamente calibrado)
plot_calibration_curve(calibration_result)


# --- 7.3 Deteccion de data leakage ---
# Identifica si alguna variable tiene informacion que "filtra" el target.

leakage_result <- detect_data_leakage(
  model_results = list(
    test_metrics = evaluation_result$metrics,
    train_metrics = evaluation_result$train_metrics
  ),
  importance = interpret_result$importance,
  verbose = TRUE
)


# --- 7.4 Validacion cruzada anidada (Nested CV) ---
# Gold standard para evaluacion sin sesgo de seleccion de modelo.
# NOTA: Es lento (~5-10 min). Descomentar solo si se necesita.

# nested_result <- run_nested_cv(
#   data        = df_sample,
#   target      = "Depression",
#   task        = "classification",
#   recipe      = preprocess_result$recipe,
#   model_name  = "rf",
#   outer_folds = 5,
#   inner_folds = 5,
#   grid_size   = 10,
#   seed        = 2024,
#   verbose     = TRUE
# )


# ============================================================================
# PASO 8: GENERACION DE GRAFICOS
# ============================================================================

# Para la Parte B (paso a paso), necesitamos ensamblar el objeto easyml
# que contiene todos los resultados. Como usamos easy_ml() en la Parte A,
# podemos usar ese resultado directamente:

# --- Panel de 4 graficos (ROC + Importancia + Confusion + Metricas) ---
plot(result, type = "panel")

# --- Grafico individual: solo ROC ---
plot(result, type = "roc")

# --- Grafico individual: solo importancia ---
plot(result, type = "importance", top_n = 10)

# --- Grafico individual: solo confusion ---
plot(result, type = "confusion")

# --- Generar TODOS los graficos disponibles ---
all_plots <- generate_all_plots(result)

# Ver catalogo de graficos generados
print_figures_catalog(result)

# Obtener catalogo como data.frame (util para seleccionar)
catalog <- get_figures_catalog(result)
print(catalog)

# --- Guardar todos los graficos como archivos PNG ---
save_all_plots(result,
               path   = "figuras",
               prefix = "depression",
               width  = 8,
               height = 6,
               dpi    = 300)



# ############################################################################
#
#   PARTE C: ESTIMACION DE TAMANO DE MUESTRA
#
# ############################################################################
# ml_sample_size() determina cuantas observaciones necesitas para que
# tu modelo alcance un rendimiento estable. Usa simulacion Monte Carlo
# con ajuste de curva power-law.
#
# Resultado guardado en: D:/9. IPCSAP/Curso Machine learning/
# ============================================================================

# Usar la data completa (o una muestra grande) para la estimacion
set.seed(2024)
df_ss <- df %>%
  group_by(Depression) %>%
  slice_sample(n = 2500) %>%
  ungroup()

cat("Datos para sample size:", nrow(df_ss), "filas\n")

# Estimar tamano de muestra con Random Forest
# NOTA: Usamos reps >= 50 para estimaciones estables.
# Con n_grid pequenos (20-100) se ve mejor la curva de aprendizaje.
# target = 0.85 (AUC minimo aceptable para que la funcion determine n*)
ss_result <- ml_sample_size(
  data        = df_ss,
  formula     = Depression ~ .,
  task        = "classification",
  model       = "rf",
  metric      = "auc",
  positive    = "Yes",
  target      = 0.85,
  n_grid      = c(20, 40, 60, 80, 100, 150, 200, 300, 500, 750, 1000),
  reps        = 50,
  prob_min    = 0.80,
  bootstrap_ci = TRUE,
  n_boot      = 500,
  seed        = 2024,
  verbose     = TRUE
)

# --- Resultados ---
# SALIDA ESPERADA:
#   Recommended n*: 60
#   Bootstrap 95% CI: [ 60 , 80 ]
#   Interpretacion: Se necesitan al menos 60 participantes para que
#   Random Forest alcance un AUC >= 0.85 en el 80% de las repeticiones.
print(ss_result)

# Estimar N con la curva ajustada para un AUC objetivo de 0.90
# (extrapolacion basada en el modelo power-law)
estimate_n(ss_result, target_metric = 0.90)

# --- Graficos ---
# Curva de aprendizaje (N vs rendimiento)
plot_learning_curve(ss_result)

# Distribucion de metricas por tamano de muestra
plot_distribution(ss_result)

# Todos los graficos juntos
plot_all(ss_result)

# --- Guardar resultados ---
output_dir <- "."

# Guardar grafico de curva de aprendizaje
ggsave(
  filename = file.path(output_dir, "sample_size_learning_curve.png"),
  plot     = plot_learning_curve(ss_result),
  width    = 10,
  height   = 7,
  dpi      = 300
)

# Guardar grafico de distribucion
ggsave(
  filename = file.path(output_dir, "sample_size_distribution.png"),
  plot     = plot_distribution(ss_result),
  width    = 10,
  height   = 7,
  dpi      = 300
)

# Guardar el objeto completo (para reutilizar sin re-simular)
saveRDS(ss_result, file = file.path(output_dir, "sample_size_depression_rf.rds"))

cat("\nResultados guardados en:", normalizePath(output_dir), "\n")


# ============================================================================
# FIN DEL TUTORIAL
# ============================================================================
cat("\n")
cat(strrep("=", 60), "\n")
cat("  Tutorial easyML completado exitosamente\n")
cat(strrep("=", 60), "\n")
cat("\n")
cat("Archivos generados:\n")
cat("  - reporte_completo.txt\n")
cat("  - figuras/*.png\n")
cat("  - sample_size_learning_curve.png\n")
cat("  - sample_size_distribution.png\n")
cat("  - sample_size_depression_rf.rds\n")
