# =============================================================================
# EJEMPLO REPRODUCIBLE: unsupervised_ml()
# Pipeline completo de Aprendizaje No Supervisado con el dataset iris
# =============================================================================

# Cargar funciones
for (f in list.files("D:/14. LIBRERIAS/easyML_github/R", full.names = TRUE)) {
  tryCatch(source(f, local = TRUE), error = function(e) NULL)
}

# =============================================================================
# EJEMPLO 1: Analisis completo (clustering + reduccion + anomalias)
# =============================================================================

cat("\n")
cat("=================================================================\n")
cat(" EJEMPLO 1: Analisis completo con iris\n")
cat("=================================================================\n\n")

resultado <- unsupervised_ml(
  data = iris,
  exclude_cols = "Species",
  methods = c("clustering", "reduction", "anomaly"),
  # Clustering
  clustering_algorithms = c("kmeans", "hierarchical", "dbscan"),
  k_range = 2:6,
  hclust_method = "ward.D2",
  # Reduccion
  reduction_methods = c("pca", "tsne"),
  # Anomalias
  anomaly_methods = c("lof", "mahalanobis"),
  contamination = 0.05,
  # Preprocesamiento
  normalize = TRUE,
  normalize_method = "zscore",
  remove_high_cor = TRUE,
  cor_threshold = 0.90,
  # General
  seed = 2024,
  verbose = TRUE
)

# --- Ver resultados ---
print(resultado)
summary(resultado)

# --- Perfil de clusters (medias por cluster en variables originales) ---
cat("\n--- Perfil de clusters ---\n")
cluster_profile(resultado, original_data = iris[, 1:4])

# --- Ver figuras disponibles ---
plot(resultado)

# --- Mostrar una figura especifica ---
plot(resultado, which = 1)  # Scree Plot
plot(resultado, which = 2)  # Biplot PCA
plot(resultado, which = "all")  # Todas

# --- Guardar figuras en disco ---
# save_unsupervised_plots(resultado, output_dir = "figures_iris")


# =============================================================================
# EJEMPLO 2: Solo clustering con k fijo
# =============================================================================

cat("\n\n")
cat("=================================================================\n")
cat(" EJEMPLO 2: Solo clustering con k = 3\n")
cat("=================================================================\n\n")

resultado_k3 <- unsupervised_ml(
  data = iris,
  exclude_cols = "Species",
  methods = "clustering",
  clustering_algorithms = c("kmeans", "hierarchical"),
  k = 3,  # k fijo, no busca automaticamente
  run_eda = FALSE,
  verbose = TRUE
)

# Comparar clusters con las especies reales
cat("\n--- Tabla cruzada: clusters vs Species ---\n")
best_labels <- resultado_k3$clustering$results$kmeans$labels
cat("\nK-Means (k=3) vs Species:\n")
print(table(Cluster = best_labels, Species = iris$Species))


# =============================================================================
# EJEMPLO 3: Solo reduccion de dimensionalidad
# =============================================================================

cat("\n\n")
cat("=================================================================\n")
cat(" EJEMPLO 3: Solo reduccion de dimensionalidad\n")
cat("=================================================================\n\n")

resultado_pca <- unsupervised_ml(
  data = iris,
  exclude_cols = "Species",
  methods = "reduction",
  reduction_methods = c("pca", "tsne", "umap"),
  pca_threshold = 0.90,
  run_eda = FALSE,
  remove_high_cor = FALSE,  # mantener todas las variables
  verbose = TRUE
)

# Acceder a los scores PCA
cat("\nPrimeras 5 filas de scores PCA:\n")
print(head(resultado_pca$reduction$pca$scores, 5))

# Acceder a coordenadas t-SNE
cat("\nPrimeras 5 filas de t-SNE:\n")
print(head(resultado_pca$reduction$tsne$coords, 5))


# =============================================================================
# EJEMPLO 4: Solo deteccion de anomalias
# =============================================================================

cat("\n\n")
cat("=================================================================\n")
cat(" EJEMPLO 4: Deteccion de anomalias\n")
cat("=================================================================\n\n")

resultado_anom <- unsupervised_ml(
  data = iris,
  exclude_cols = "Species",
  methods = "anomaly",
  anomaly_methods = c("lof", "mahalanobis"),
  contamination = 0.05,
  lof_k = 10,
  run_eda = FALSE,
  verbose = TRUE
)

# Ver las observaciones anomalas por consenso
if (!is.null(resultado_anom$anomalies$consensus)) {
  anomaly_idx <- which(resultado_anom$anomalies$consensus$is_anomaly)
  cat("\nObservaciones anomalas por consenso:\n")
  print(iris[anomaly_idx, ])
}


# =============================================================================
# EJEMPLO 5: Prediccion de nuevos datos
# =============================================================================

cat("\n\n")
cat("=================================================================\n")
cat(" EJEMPLO 5: Asignar clusters a nuevos datos\n")
cat("=================================================================\n\n")

# Entrenar modelo
modelo <- unsupervised_ml(
  data = iris,
  exclude_cols = "Species",
  methods = "clustering",
  clustering_algorithms = "kmeans",
  k = 3,
  run_eda = FALSE,
  verbose = FALSE
)

# Predecir en observaciones nuevas (simuladas)
nuevos_datos <- data.frame(
  Sepal.Length = c(5.0, 6.5, 7.5),
  Sepal.Width  = c(3.5, 3.0, 2.8),
  Petal.Length  = c(1.4, 4.5, 6.2),
  Petal.Width   = c(0.2, 1.5, 2.0)
)

clusters_pred <- predict(modelo, nuevos_datos)
cat("Nuevos datos:\n")
print(nuevos_datos)
cat("\nClusters asignados:", clusters_pred, "\n")
cat("(Esperado: setosa=2, versicolor=1, virginica=1 aprox.)\n")


# =============================================================================
# EJEMPLO 6: Uso con GMM (si mclust esta instalado)
# =============================================================================

cat("\n\n")
cat("=================================================================\n")
cat(" EJEMPLO 6: GMM (Gaussian Mixture Models)\n")
cat("=================================================================\n\n")

if (requireNamespace("mclust", quietly = TRUE)) {
  resultado_gmm <- unsupervised_ml(
    data = iris,
    exclude_cols = "Species",
    methods = "clustering",
    clustering_algorithms = "gmm",
    k = 3,
    run_eda = FALSE,
    verbose = TRUE
  )

  # Probabilidades de pertenencia (soft clustering)
  cat("\nPrimeras 5 probabilidades de pertenencia:\n")
  print(head(resultado_gmm$clustering$results$gmm$probabilities, 5))

  # Incertidumbre
  cat("\nObservaciones con mayor incertidumbre:\n")
  unc <- resultado_gmm$clustering$results$gmm$uncertainty
  top_unc <- order(unc, decreasing = TRUE)[1:5]
  print(data.frame(
    obs = top_unc,
    uncertainty = round(unc[top_unc], 4),
    species = iris$Species[top_unc]
  ))
} else {
  cat("  Paquete 'mclust' no instalado. Saltar ejemplo.\n")
}

cat("\n=== TODOS LOS EJEMPLOS COMPLETADOS ===\n")
