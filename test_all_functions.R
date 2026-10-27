# =============================================================================
# Comprehensive Test Script for PsyML v2.0.0
# =============================================================================
# Tests all major function groups with iris and simulated data
# PsyML only supports binary classification, so we use binary datasets
# =============================================================================

library(PsyML)

# Tracking results
results <- data.frame(
  Group = character(),
  Function_Name = character(),
  Status = character(),
  Message = character(),
  stringsAsFactors = FALSE
)

add_result <- function(group, fn_name, status, msg = "") {
  results <<- rbind(results, data.frame(
    Group = group,
    Function_Name = fn_name,
    Status = status,
    Message = msg,
    stringsAsFactors = FALSE
  ))
  if (status == "SUCCESS") {
    cat(sprintf("  [OK] %s\n", fn_name))
  } else {
    cat(sprintf("  [FAIL] %s => %s\n", fn_name, msg))
  }
}

test_fn <- function(group, fn_name, expr) {
  tryCatch({
    eval(expr)
    add_result(group, fn_name, "SUCCESS")
  }, error = function(e) {
    add_result(group, fn_name, "FAILURE", conditionMessage(e))
  })
}

cat("\n")
cat("=============================================================================\n")
cat("  PsyML v2.0.0 - Comprehensive Function Test\n")
cat("=============================================================================\n")
cat("  Date:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("  R version:", R.version.string, "\n")
cat("=============================================================================\n\n")

# --- Prepare binary classification dataset from iris ---
# PsyML only supports binary classification (2 levels)
iris_bin <- iris[iris$Species != "setosa", ]
iris_bin$Species <- factor(as.character(iris_bin$Species))  # drop unused level

# --- Prepare a second binary dataset for variety ---
set.seed(42)
n_bin <- 200
bin_data <- data.frame(
  x1 = rnorm(n_bin),
  x2 = rnorm(n_bin),
  x3 = rnorm(n_bin),
  x4 = rnorm(n_bin)
)
bin_data$target <- factor(ifelse(bin_data$x1 + 0.5 * bin_data$x2 + rnorm(n_bin, sd = 0.8) > 0, "pos", "neg"))

# =====================================================================
# GROUP A: EDA Functions
# =====================================================================
cat("GROUP A: EDA Functions\n")
cat("---------------------------------------------------------------------\n")

test_fn("A-EDA", "eda_summary(iris)", quote({
  res <- eda_summary(iris, target = "Species", verbose = FALSE)
  stopifnot(is.list(res))
}))

test_fn("A-EDA", "eda_summary(iris_bin)", quote({
  res <- eda_summary(iris_bin, target = "Species", verbose = FALSE)
  stopifnot(is.list(res))
}))

test_fn("A-EDA", "eda_structure(iris)", quote({
  res <- eda_structure(iris, verbose = FALSE)
  stopifnot(is.list(res))
}))

test_fn("A-EDA", "eda_normality(iris, numeric target)", quote({
  # Use a numeric target for normality test
  res <- eda_normality(iris, target = "Sepal.Length", verbose = FALSE)
  # Returns shapiro test result
  stopifnot(!is.null(res))
}))

test_fn("A-EDA", "eda_normality(iris, factor target)", quote({
  # Factor target returns NULL silently
  res <- eda_normality(iris, target = "Species", verbose = FALSE)
  # Should return NULL for non-numeric target
}))

test_fn("A-EDA", "eda_missing(iris)", quote({
  res <- eda_missing(iris, verbose = FALSE)
  # Returns a list with by_variable, total, total_pct
  stopifnot(is.list(res))
  stopifnot(!is.null(res$by_variable))
  stopifnot(is.data.frame(res$by_variable))
  stopifnot(res$total == 0)  # iris has no missing values
}))

test_fn("A-EDA", "eda_missing(data with NAs)", quote({
  df_na <- iris
  df_na$Sepal.Length[c(1, 5, 10)] <- NA
  df_na$Petal.Width[c(3, 7)] <- NA
  res <- eda_missing(df_na, verbose = FALSE)
  stopifnot(is.list(res))
  stopifnot(res$total == 5)
}))

test_fn("A-EDA", "eda_outliers(iris)", quote({
  res <- eda_outliers(iris, verbose = FALSE)
  stopifnot(is.list(res))
}))

test_fn("A-EDA", "eda_correlation(iris)", quote({
  res <- eda_correlation(iris, target = "Species", verbose = FALSE)
  # Returns a correlation matrix (matrix, not list)
  stopifnot(is.matrix(res))
}))

test_fn("A-EDA", "eda_numeric(iris, target='Species')", quote({
  res <- eda_numeric(iris, target = "Species", verbose = FALSE)
  stopifnot(is.list(res))
}))

# Create a dataset with categorical vars for eda_categorical
test_fn("A-EDA", "eda_categorical(df_with_cats)", quote({
  df_cat <- data.frame(
    color = sample(c("red", "blue", "green"), 100, replace = TRUE),
    size = sample(c("S", "M", "L"), 100, replace = TRUE),
    target = sample(c("yes", "no"), 100, replace = TRUE),
    stringsAsFactors = TRUE
  )
  res <- eda_categorical(df_cat, target = "target", verbose = FALSE)
  stopifnot(is.list(res))
  stopifnot(length(res$variables) == 2)  # color and size (not target)
}))

test_fn("A-EDA", "eda_basic(iris)", quote({
  res <- eda_basic(iris, verbose = FALSE)
  stopifnot(is.list(res))
}))

cat("\n")

# =====================================================================
# GROUP B: Preprocessing Functions
# =====================================================================
cat("GROUP B: Preprocessing Functions\n")
cat("---------------------------------------------------------------------\n")

test_fn("B-Preproc", "preprocess_data(iris_bin, classification)", quote({
  res <- preprocess_data(iris_bin, target = "Species", task = "classification", verbose = FALSE)
  stopifnot(is.list(res))
  stopifnot(!is.null(res$train_data))
  stopifnot(!is.null(res$test_data))
}))

test_fn("B-Preproc", "preprocess_data(regression)", quote({
  set.seed(42)
  n <- 200
  reg_df <- data.frame(x1 = rnorm(n), x2 = rnorm(n))
  reg_df$y <- 2 * reg_df$x1 + rnorm(n, sd = 0.5)
  res <- preprocess_data(reg_df, target = "y", task = "regression", verbose = FALSE)
  stopifnot(is.list(res))
  stopifnot(!is.null(res$train_data))
}))

test_fn("B-Preproc", "prep_target(iris_bin)", quote({
  d <- prep_target(iris_bin, target = "Species", task = "classification", verbose = FALSE)
  stopifnot(is.data.frame(d))
  stopifnot(is.factor(d$Species))
  stopifnot(length(levels(d$Species)) == 2)
}))

test_fn("B-Preproc", "prep_split(iris_bin)", quote({
  d <- iris_bin
  d$Species <- as.factor(as.character(d$Species))
  res <- prep_split(d, target = "Species", task = "classification",
                    test_split = 0.20, seed = 2024, verbose = FALSE)
  stopifnot(is.list(res))
  stopifnot(!is.null(res$train))
  stopifnot(!is.null(res$test))
  cat("    Train:", nrow(res$train), " Test:", nrow(res$test), "\n")
}))

test_fn("B-Preproc", "prep_recipe(iris_bin)", quote({
  d <- iris_bin
  d$Species <- as.factor(as.character(d$Species))
  res <- prep_recipe(d, target = "Species", task = "classification", verbose = FALSE)
  # Should return a recipe object
  stopifnot(!is.null(res))
}))

cat("\n")

# =====================================================================
# GROUP C: Main Pipeline - Classification (Binary)
# =====================================================================
cat("GROUP C: Main Pipeline - Classification (Binary RF on iris_bin)\n")
cat("---------------------------------------------------------------------\n")

classification_result <- NULL
test_fn("C-Classif", "easy_ml(iris_bin, classification, rf)", quote({
  classification_result <<- easy_ml(
    iris_bin,
    target = "Species",
    models = c("rf"),
    cv_folds = 3,
    tune_best = FALSE,
    run_shap = FALSE,
    run_eda = FALSE,
    check_leakage = FALSE,
    optimize_threshold = FALSE,
    analyze_interactions = FALSE,
    verbose = FALSE
  )
  stopifnot(inherits(classification_result, "easyml"))
}))

test_fn("C-Classif", "print.easyml(classification)", quote({
  stopifnot(!is.null(classification_result))
  print(classification_result)
}))

test_fn("C-Classif", "summary.easyml(classification)", quote({
  stopifnot(!is.null(classification_result))
  summary(classification_result)
}))

test_fn("C-Classif", "predict.easyml(classification)", quote({
  stopifnot(!is.null(classification_result))
  preds <- predict(classification_result, iris_bin[1:10, ])
  stopifnot(nrow(preds) == 10)
}))

# Test with the simulated binary dataset too
classif_result2 <- NULL
test_fn("C-Classif", "easy_ml(bin_data, classification, rf)", quote({
  classif_result2 <<- easy_ml(
    bin_data,
    target = "target",
    models = c("rf"),
    cv_folds = 3,
    tune_best = FALSE,
    run_shap = FALSE,
    run_eda = FALSE,
    check_leakage = FALSE,
    optimize_threshold = FALSE,
    analyze_interactions = FALSE,
    verbose = FALSE
  )
  stopifnot(inherits(classif_result2, "easyml"))
}))

cat("\n")

# =====================================================================
# GROUP D: Main Pipeline - Regression
# =====================================================================
cat("GROUP D: Main Pipeline - Regression (RF)\n")
cat("---------------------------------------------------------------------\n")

regression_result <- NULL
test_fn("D-Regress", "easy_ml(regression, rf)", quote({
  set.seed(42)
  n <- 200
  reg_data <- data.frame(
    x1 = rnorm(n),
    x2 = rnorm(n),
    x3 = rnorm(n)
  )
  reg_data$y <- 2 * reg_data$x1 + 0.5 * reg_data$x2 + rnorm(n, sd = 0.5)

  regression_result <<- easy_ml(
    reg_data,
    target = "y",
    task = "regression",
    models = c("rf"),
    cv_folds = 3,
    tune_best = FALSE,
    run_shap = FALSE,
    run_eda = FALSE,
    check_leakage = FALSE,
    analyze_interactions = FALSE,
    verbose = FALSE
  )
  stopifnot(inherits(regression_result, "easyml"))
}))

test_fn("D-Regress", "print.easyml(regression)", quote({
  stopifnot(!is.null(regression_result))
  print(regression_result)
}))

test_fn("D-Regress", "summary.easyml(regression)", quote({
  stopifnot(!is.null(regression_result))
  summary(regression_result)
}))

test_fn("D-Regress", "predict.easyml(regression)", quote({
  stopifnot(!is.null(regression_result))
  new_df <- data.frame(x1 = c(1, 2), x2 = c(0, 1), x3 = c(-1, 0))
  preds <- predict(regression_result, new_df)
  stopifnot(!is.null(preds))
  stopifnot(nrow(preds) == 2)
}))

cat("\n")

# =====================================================================
# GROUP E: Block Split & Diversity Split
# =====================================================================
cat("GROUP E: Block Split & Diversity Split\n")
cat("---------------------------------------------------------------------\n")

test_fn("E-Split", "block_split(sequential data)", quote({
  set.seed(42)
  n <- 200
  seq_data <- data.frame(
    time_index = 1:n,
    x1 = rnorm(n),
    x2 = rnorm(n),
    y = sample(c("A", "B"), n, replace = TRUE)
  )
  res <- block_split(seq_data, order_col = "time_index",
                     num_blocks = 5, test_prop = 0.20, seed = 123)
  stopifnot(inherits(res, "block_split"))
  stopifnot(!is.null(res$train))
  stopifnot(!is.null(res$test))
  cat("    Train:", nrow(res$train), " Test:", nrow(res$test), "\n")
}))

test_fn("E-Split", "print.block_split()", quote({
  set.seed(42)
  n <- 200
  seq_data <- data.frame(
    time_index = 1:n,
    x1 = rnorm(n),
    x2 = rnorm(n),
    y = sample(c("A", "B"), n, replace = TRUE)
  )
  res <- block_split(seq_data, order_col = "time_index",
                     num_blocks = 5, test_prop = 0.20, seed = 123)
  print(res)
}))

test_fn("E-Split", "block_split(stratified)", quote({
  set.seed(42)
  n <- 200
  seq_data <- data.frame(
    time_index = 1:n,
    x1 = rnorm(n),
    y = factor(sample(c("A", "B"), n, replace = TRUE, prob = c(0.7, 0.3)))
  )
  res <- block_split(seq_data, order_col = "time_index",
                     num_blocks = 4, test_prop = 0.25,
                     stratify = "y", seed = 42)
  stopifnot(inherits(res, "block_split"))
  cat("    Train:", nrow(res$train), " Test:", nrow(res$test), "\n")
}))

test_fn("E-Split", "diversity_split(simulated data)", quote({
  set.seed(42)
  n <- 100
  div_data <- data.frame(
    x1 = rnorm(n),
    x2 = rnorm(n),
    x3 = rnorm(n),
    target = sample(c("A", "B"), n, replace = TRUE)
  )
  res <- diversity_split(div_data, test_prop = 0.20,
                         target = "target", seed = 123, verbose = FALSE)
  stopifnot(inherits(res, "diversity_split"))
  stopifnot(!is.null(res$train))
  stopifnot(!is.null(res$test))
  cat("    Train:", nrow(res$train), " Test:", nrow(res$test), "\n")
}))

test_fn("E-Split", "print.diversity_split()", quote({
  set.seed(42)
  n <- 100
  div_data <- data.frame(
    x1 = rnorm(n),
    x2 = rnorm(n),
    x3 = rnorm(n),
    target = sample(c("A", "B"), n, replace = TRUE)
  )
  res <- diversity_split(div_data, test_prop = 0.20,
                         target = "target", seed = 123, verbose = FALSE)
  print(res)
}))

test_fn("E-Split", "diversity_split(method='cluster')", quote({
  set.seed(42)
  n <- 100
  div_data <- data.frame(
    x1 = rnorm(n),
    x2 = rnorm(n),
    target = sample(c("A", "B"), n, replace = TRUE)
  )
  res <- diversity_split(div_data, test_prop = 0.25, method = "cluster",
                         target = "target", seed = 42, verbose = FALSE)
  stopifnot(inherits(res, "diversity_split"))
}))

cat("\n")

# =====================================================================
# GROUP F: Sample Size Estimation
# =====================================================================
cat("GROUP F: Sample Size Estimation\n")
cat("---------------------------------------------------------------------\n")

test_fn("F-SampleSize", "ml_sample_size(iris_bin, classification, formula)", quote({
  res <- ml_sample_size(
    data = iris_bin,
    formula = Species ~ .,
    task = "classification",
    model = "rf",
    positive = "versicolor",
    n_grid = c(30, 50, 70),
    reps = 3,
    n_test = 50,
    seed = 123,
    verbose = FALSE
  )
  stopifnot(inherits(res, "ml_sample_size"))
  cat("    Completed with", length(res$results), "grid points\n")
}))

test_fn("F-SampleSize", "ml_sample_size(simulated, regression)", quote({
  # Use simulated data with small population to be fast
  res <- ml_sample_size(
    data = NULL,
    task = "regression",
    model = "rf",
    n_grid = c(50, 100),
    reps = 3,
    n_test = 100,
    sim_params = list(N_pop = 500, p = 5),
    seed = 42,
    verbose = FALSE
  )
  stopifnot(inherits(res, "ml_sample_size"))
}))

test_fn("F-SampleSize", "ml_sample_size(simulated, classification)", quote({
  res <- ml_sample_size(
    data = NULL,
    task = "classification",
    model = "glm",
    n_grid = c(50),
    reps = 2,
    n_test = 50,
    sim_params = list(N_pop = 300, p = 3),
    seed = 123,
    verbose = FALSE
  )
  stopifnot(inherits(res, "ml_sample_size"))
  print(res)
}))

cat("\n")

# =====================================================================
# GROUP G: Feature Engineering
# =====================================================================
cat("GROUP G: Feature Engineering\n")
cat("---------------------------------------------------------------------\n")

test_fn("G-FeatEng", "auto_feature_engineering(iris)", quote({
  res <- auto_feature_engineering(iris, target = "Species", verbose = FALSE)
  stopifnot(is.data.frame(res) || is.list(res))
}))

test_fn("G-FeatEng", "auto_feature_engineering(regression data)", quote({
  set.seed(42)
  n <- 150
  fe_data <- data.frame(
    x1 = rnorm(n, 10, 2),
    x2 = rnorm(n, 5, 1),
    x3 = rpois(n, 3),
    y = rnorm(n)
  )
  res <- auto_feature_engineering(fe_data, target = "y", verbose = FALSE)
  stopifnot(is.data.frame(res) || is.list(res))
}))

cat("\n")

# =====================================================================
# GROUP H: Advanced Functions
# =====================================================================
cat("GROUP H: Advanced Functions\n")
cat("---------------------------------------------------------------------\n")

test_fn("H-Advanced", "detect_data_leakage(classification)", quote({
  stopifnot(!is.null(classification_result))
  imp <- classification_result$importance
  res <- detect_data_leakage(
    model_results = classification_result,
    importance = imp,
    verbose = FALSE
  )
}))

test_fn("H-Advanced", "detect_data_leakage(NULL args)", quote({
  res <- detect_data_leakage(model_results = NULL, importance = NULL, verbose = FALSE)
}))

test_fn("H-Advanced", "optimize_threshold(binary classification)", quote({
  stopifnot(!is.null(classif_result2))
  # optimize_threshold needs predictions with probability columns
  if (!is.null(classif_result2$predictions)) {
    ot <- optimize_threshold(
      predictions = classif_result2$predictions,
      target = "target",
      method = "youden",
      verbose = FALSE
    )
    stopifnot(is.list(ot))
    cat("    Optimal threshold:", ot$optimal_threshold, "\n")
  } else {
    stop("No predictions available in classif_result2")
  }
}))

test_fn("H-Advanced", "optimize_threshold(method='f1')", quote({
  stopifnot(!is.null(classif_result2))
  if (!is.null(classif_result2$predictions)) {
    ot <- optimize_threshold(
      predictions = classif_result2$predictions,
      target = "target",
      method = "f1",
      verbose = FALSE
    )
    stopifnot(is.list(ot))
    cat("    Optimal threshold (F1):", ot$optimal_threshold, "\n")
  }
}))

cat("\n")

# =====================================================================
# GROUP I: Export Functions
# =====================================================================
cat("GROUP I: Export Functions\n")
cat("---------------------------------------------------------------------\n")

# Use a temp directory for export files
tmp_dir <- tempdir()

test_fn("I-Export", "easy_ml_capture(binary classification)", quote({
  captured <- easy_ml_capture(
    iris_bin,
    target = "Species",
    models = c("rf"),
    cv_folds = 3,
    tune_best = FALSE,
    run_shap = FALSE,
    run_eda = FALSE,
    check_leakage = FALSE,
    optimize_threshold = FALSE,
    analyze_interactions = FALSE
  )
  stopifnot(inherits(captured, "easyml"))
  stopifnot(!is.null(captured$verbose_text))
  cat("    Captured", nchar(captured$verbose_text), "chars of verbose output\n")
}))

test_fn("I-Export", "export_verbose_txt(classification)", quote({
  stopifnot(!is.null(classification_result))
  txt_path <- file.path(tmp_dir, "test_report.txt")
  export_verbose_txt(classification_result, file_path = txt_path)
  stopifnot(file.exists(txt_path))
  fsize <- file.info(txt_path)$size
  cat("    TXT file size:", fsize, "bytes\n")
}))

test_fn("I-Export", "export_verbose_json(classification)", quote({
  stopifnot(!is.null(classification_result))
  json_path <- file.path(tmp_dir, "test_report.json")
  export_verbose_json(classification_result, file_path = json_path)
  stopifnot(file.exists(json_path))
  fsize <- file.info(json_path)$size
  cat("    JSON file size:", fsize, "bytes\n")
}))

test_fn("I-Export", "export_verbose_txt(regression)", quote({
  stopifnot(!is.null(regression_result))
  txt_path <- file.path(tmp_dir, "test_report_reg.txt")
  export_verbose_txt(regression_result, file_path = txt_path)
  stopifnot(file.exists(txt_path))
  fsize <- file.info(txt_path)$size
  cat("    TXT file size:", fsize, "bytes\n")
}))

test_fn("I-Export", "export_verbose_json(regression)", quote({
  stopifnot(!is.null(regression_result))
  json_path <- file.path(tmp_dir, "test_report_reg.json")
  export_verbose_json(regression_result, file_path = json_path)
  stopifnot(file.exists(json_path))
  fsize <- file.info(json_path)$size
  cat("    JSON file size:", fsize, "bytes\n")
}))

cat("\n")

# =====================================================================
# SUMMARY
# =====================================================================
cat("=============================================================================\n")
cat("  TEST SUMMARY\n")
cat("=============================================================================\n\n")

n_total <- nrow(results)
n_pass <- sum(results$Status == "SUCCESS")
n_fail <- sum(results$Status == "FAILURE")

cat(sprintf("  Total tests:  %d\n", n_total))
cat(sprintf("  Passed:       %d\n", n_pass))
cat(sprintf("  Failed:       %d\n", n_fail))
cat(sprintf("  Pass rate:    %.1f%%\n\n", 100 * n_pass / n_total))

# Print table
cat(sprintf("  %-12s %-50s %-8s\n", "Group", "Function", "Status"))
cat("  ", paste(rep("-", 73), collapse = ""), "\n", sep = "")
for (i in seq_len(nrow(results))) {
  cat(sprintf("  %-12s %-50s %-8s\n",
              results$Group[i],
              results$Function_Name[i],
              results$Status[i]))
}
cat("\n")

# Print failures detail if any
if (n_fail > 0) {
  cat("  FAILURES DETAIL:\n")
  cat("  ", paste(rep("-", 73), collapse = ""), "\n", sep = "")
  fails <- results[results$Status == "FAILURE", ]
  for (i in seq_len(nrow(fails))) {
    cat(sprintf("  [%s] %s\n    Error: %s\n\n",
                fails$Group[i], fails$Function_Name[i], fails$Message[i]))
  }
}

cat("=============================================================================\n")
cat("  Test completed at:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("=============================================================================\n")

# Exit with non-zero if any failures
if (n_fail > 0) {
  quit(status = 1, save = "no")
}
