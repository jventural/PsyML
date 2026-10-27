# =============================================================================
# EJEMPLO REPRODUCIBLE: block_split()
# Muestreo por bloques para train/test split con mejor generalizacion
# =============================================================================

# Cargar la funcion
source("D:/14. LIBRERIAS/easyML_github/R/block_split.R")

# -----------------------------------------------------------------------------
# ESCENARIO: Encuesta de satisfaccion laboral recolectada durante 6 meses
# Queremos predecir si un empleado renunciara (turnover)
# -----------------------------------------------------------------------------

set.seed(2024)

# Simular datos de encuesta con fechas de respuesta
n <- 600
survey_data <- data.frame(
  employee_id = 1001:(1000 + n),
  fecha_respuesta = seq(as.Date("2024-01-15"),
                        as.Date("2024-06-30"),
                        length.out = n),
  edad = sample(22:60, n, replace = TRUE),
  antiguedad_anios = round(runif(n, 0.5, 15), 1),
  satisfaccion = sample(1:5, n, replace = TRUE),
  salario_miles = round(rnorm(n, mean = 45, sd = 12), 1),
  horas_extra_mes = sample(0:30, n, replace = TRUE),
  # Variable objetivo: renuncia (0 = no, 1 = si) - prevalencia ~25%
  renuncia = sample(c(0, 1), n, replace = TRUE, prob = c(0.75, 0.25))
)

cat(strrep("=", 60), "\n")
cat("DATOS SIMULADOS: Encuesta de satisfaccion laboral\n")
cat(strrep("=", 60), "\n\n")

cat("Dimensiones:", nrow(survey_data), "filas x", ncol(survey_data), "columnas\n")
cat("Periodo:", as.character(min(survey_data$fecha_respuesta)), "a",
    as.character(max(survey_data$fecha_respuesta)), "\n")
cat("Prevalencia de renuncia:", round(mean(survey_data$renuncia) * 100, 1), "%\n\n")

head(survey_data)


# -----------------------------------------------------------------------------
# CASO 1: Split tradicional aleatorio (para comparar)
# -----------------------------------------------------------------------------

cat("\n", strrep("=", 60), "\n")
cat("CASO 1: Split tradicional aleatorio\n")
cat(strrep("=", 60), "\n\n")

set.seed(123)
idx_random <- sample(1:n, size = round(n * 0.20))
test_random <- survey_data[idx_random, ]
train_random <- survey_data[-idx_random, ]

cat("Train:", nrow(train_random), "| Test:", nrow(test_random), "\n")
cat("Rango de fechas en TEST:\n")
cat("  Desde:", as.character(min(test_random$fecha_respuesta)), "\n")
cat("  Hasta:", as.character(max(test_random$fecha_respuesta)), "\n")
cat("  -> El test tiene datos de TODO el periodo (no hay separacion temporal)\n")


# -----------------------------------------------------------------------------
# CASO 2: Block split - muestreo por bloques temporales
# -----------------------------------------------------------------------------

cat("\n", strrep("=", 60), "\n")
cat("CASO 2: Block split por fecha de respuesta\n")
cat(strrep("=", 60), "\n\n")

result <- block_split(
  data = survey_data,
  order_col = "fecha_respuesta",
  num_blocks = 5,
  test_prop = 0.20,
  seed = 123
)

print(result)

cat("Rango de fechas en TEST:\n")
cat("  Desde:", as.character(min(result$test$fecha_respuesta)), "\n")
cat("  Hasta:", as.character(max(result$test$fecha_respuesta)), "\n")
cat("  -> El test tiene datos de TODOS los periodos (diversidad temporal)\n")


# -----------------------------------------------------------------------------
# CASO 3: Block split CON estratificacion por clase
# -----------------------------------------------------------------------------

cat("\n", strrep("=", 60), "\n")
cat("CASO 3: Block split con estratificacion por 'renuncia'\n")
cat(strrep("=", 60), "\n\n")

result_strat <- block_split(
  data = survey_data,
  order_col = "fecha_respuesta",
  num_blocks = 5,
  test_prop = 0.20,
  stratify = "renuncia",
  seed = 123
)

print(result_strat)

cat("Comparacion de proporciones de clase:\n\n")
cat("  Original:  ", sprintf("%.1f%% no renuncia | %.1f%% renuncia\n",
    prop.table(table(survey_data$renuncia))[1] * 100,
    prop.table(table(survey_data$renuncia))[2] * 100))
cat("  Train set: ", sprintf("%.1f%% no renuncia | %.1f%% renuncia\n",
    prop.table(table(result_strat$train$renuncia))[1] * 100,
    prop.table(table(result_strat$train$renuncia))[2] * 100))
cat("  Test set:  ", sprintf("%.1f%% no renuncia | %.1f%% renuncia\n",
    prop.table(table(result_strat$test$renuncia))[1] * 100,
    prop.table(table(result_strat$test$renuncia))[2] * 100))


# -----------------------------------------------------------------------------
# CASO 4: Verificar que el test viene de diferentes bloques temporales
# -----------------------------------------------------------------------------

cat("\n", strrep("=", 60), "\n")
cat("CASO 4: Verificacion de diversidad temporal en test set\n")
cat(strrep("=", 60), "\n\n")

# Asignar mes a cada observacion
result_strat$test$mes <- format(result_strat$test$fecha_respuesta, "%B")
result_strat$train$mes <- format(result_strat$train$fecha_respuesta, "%B")

cat("Distribucion del TEST set por mes:\n")
print(table(result_strat$test$mes))

cat("\nDistribucion del TRAIN set por mes:\n")
print(table(result_strat$train$mes))

cat("\n-> Ambos conjuntos tienen representacion de todos los meses!\n")


# -----------------------------------------------------------------------------
# CASO 5: Uso con modelo de machine learning (ejemplo conceptual)
# -----------------------------------------------------------------------------

cat("\n", strrep("=", 60), "\n")
cat("CASO 5: Ejemplo de uso con modelo predictivo\n")
cat(strrep("=", 60), "\n\n")

# Preparar datos
train_data <- result_strat$train
test_data <- result_strat$test

# Remover columnas no predictoras
train_data$employee_id <- NULL
train_data$fecha_respuesta <- NULL
train_data$mes <- NULL

test_data$employee_id <- NULL
test_data$fecha_respuesta <- NULL
test_data$mes <- NULL

cat("Datos de entrenamiento listos:", nrow(train_data), "obs\n")
cat("Datos de prueba listos:", nrow(test_data), "obs\n\n")

# Modelo simple (regresion logistica)
modelo <- glm(renuncia ~ ., data = train_data, family = binomial)

# Predicciones
pred_prob <- predict(modelo, newdata = test_data, type = "response")
pred_class <- ifelse(pred_prob > 0.5, 1, 0)

# Metricas
accuracy <- mean(pred_class == test_data$renuncia)
cat("Accuracy en test set:", round(accuracy * 100, 1), "%\n")

# Tabla de confusion
cat("\nMatriz de confusion:\n")
print(table(Predicho = pred_class, Real = test_data$renuncia))


cat("\n", strrep("=", 60), "\n")
cat("FIN DEL EJEMPLO\n")
cat(strrep("=", 60), "\n")
