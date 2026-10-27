make_data <- function(n = 300) {
  set.seed(42)
  x1 <- rnorm(n)
  d <- data.frame(x1 = x1, x2 = x1 + rnorm(n, sd = 0.05), x3 = rnorm(n))
  lp <- -0.5 + 1.2 * x1 + 0.8 * d$x3
  d$y <- factor(ifelse(runif(n) < plogis(lp), "Yes", "No"),
                levels = c("No", "Yes"))
  d
}

test_that("supervised_ml runs with a single model", {
  d <- make_data()
  r <- supervised_ml(d, "y", task = "classification", models = "glm",
                     cv_folds = 3, tune_best = FALSE, run_shap = FALSE,
                     n_cores = 1, seed = 1, verbose = FALSE)
  expect_s3_class(r, "supervisedml")
  expect_identical(r$best_model, "glm")
  p <- predict(r, d[1:5, ])
  expect_equal(nrow(p), 5)
})

test_that("SHAP excludes predictors removed by preprocessing", {
  skip_on_cran()
  skip_if_not_installed("kernelshap")
  d <- make_data()
  r <- supervised_ml(d, "y", task = "classification", models = "glm",
                     cv_folds = 3, tune_best = FALSE, run_shap = TRUE,
                     n_shap = 30, n_cores = 1, seed = 1, verbose = FALSE)
  s <- r$interpretation$shap
  skip_if(is.null(s))
  expect_false(any(s$removed_vars %in% s$importance$Variable))
  expect_identical(colnames(s$shap_values), colnames(s$test_sample))
})
