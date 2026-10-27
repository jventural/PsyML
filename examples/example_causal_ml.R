# =============================================================================
# Ejemplo de uso de causal_ml()
# =============================================================================

library(easyML)

# --- 1. Datos simulados (RCT simple) ---

set.seed(42)
n <- 1000
X1 <- rnorm(n)
X2 <- rnorm(n)
X3 <- rbinom(n, 1, 0.5)
X4 <- rnorm(n)

# Tratamiento con confounding
ps_true <- plogis(0.5 * X1 - 0.3 * X2 + 0.2 * X3)
Treatment <- rbinom(n, 1, ps_true)

# Outcome con efecto heterogeneo
tau <- 2 + 0.5 * X1  # CATE depende de X1
Y <- tau * Treatment + X1 + 0.5 * X2 - 0.3 * X3 + 0.2 * X4 + rnorm(n)

sim_data <- data.frame(Y, Treatment, X1, X2, X3, X4)

# --- 2. Uso basico ---

result <- causal_ml(
  data = sim_data,
  treatment = "Treatment",
  outcome = "Y"
)

# Ver resultados
print(result)
summary(result)

# --- 3. Figuras ---

# Todas las figuras
all_figs <- plot(result)

# Figuras individuales
plot(result, type = "ate_forest")
plot(result, type = "ps")
plot(result, type = "cate")
plot(result, type = "love")

# --- 4. Prediccion CATE ---

# CATE del dataset original
cate_estimates <- predict(result)

# CATE para nuevos datos (requiere causal_forest o T-learner)
new_data <- data.frame(
  X1 = c(-1, 0, 1),
  X2 = c(0, 0, 0),
  X3 = c(0, 1, 0),
  X4 = c(0, 0, 0)
)
# cate_new <- predict(result, new_data)

# --- 5. Dataset clasico: LaLonde ---

if (requireNamespace("MatchIt", quietly = TRUE)) {
  data("lalonde", package = "MatchIt")

  result_lalonde <- causal_ml(
    data = lalonde,
    treatment = "treat",
    outcome = "re78",
    methods = c("propensity", "meta_learners"),
    ps_methods = c("matching", "ipw", "doubly_robust"),
    meta_learners = c("S", "T"),
    verbose = TRUE
  )

  print(result_lalonde)
  plot(result_lalonde, type = "ate_forest")
}

# --- 6. Solo metodos especificos ---

# Solo propensity score
result_ps <- causal_ml(
  data = sim_data,
  treatment = "Treatment",
  outcome = "Y",
  methods = "propensity",
  ps_methods = c("ipw", "doubly_robust"),
  run_eda = FALSE
)

# Solo meta-learners
result_ml <- causal_ml(
  data = sim_data,
  treatment = "Treatment",
  outcome = "Y",
  methods = "meta_learners",
  meta_learners = c("S", "T", "X"),
  run_eda = FALSE
)

# --- 7. Con paquetes avanzados (si instalados) ---

# Causal Forest (requiere grf)
if (requireNamespace("grf", quietly = TRUE)) {
  result_cf <- causal_ml(
    data = sim_data,
    treatment = "Treatment",
    outcome = "Y",
    methods = c("causal_forest", "meta_learners"),
    cf_num_trees = 2000
  )
  plot(result_cf, type = "importance")
  plot(result_cf, type = "heterogeneity")
}

# Double ML (requiere DoubleML + mlr3)
if (requireNamespace("DoubleML", quietly = TRUE)) {
  result_dml <- causal_ml(
    data = sim_data,
    treatment = "Treatment",
    outcome = "Y",
    methods = "dml",
    dml_model = "rf",
    dml_n_folds = 5
  )
}
