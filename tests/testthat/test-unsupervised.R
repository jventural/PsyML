test_that("unsupervised_ml finds two separated clusters", {
  set.seed(1)
  X <- data.frame(a = c(rnorm(50), rnorm(50, 4)), b = c(rnorm(50), rnorm(50, 4)))
  u <- unsupervised_ml(X, methods = "clustering",
                       clustering_algorithms = "kmeans", k = 2,
                       run_eda = FALSE, seed = 1, verbose = FALSE)
  expect_s3_class(u, "unsupervisedml")
  lab <- u$clustering$results$kmeans$labels
  expect_length(unique(lab), 2)
  pr <- predict(u, X[c(1, 100), ], algorithm = "kmeans")
  expect_length(pr, 2)
  expect_false(pr[1] == pr[2])
})

test_that("Gaussian mixture clustering works without attaching mclust", {
  skip_if_not_installed("mclust")
  set.seed(2)
  X <- data.frame(a = c(rnorm(50), rnorm(50, 4)), b = c(rnorm(50), rnorm(50, 4)))
  u <- unsupervised_ml(X, methods = "clustering",
                       clustering_algorithms = "gmm", k = 2,
                       run_eda = FALSE, seed = 1, verbose = FALSE)
  expect_false("package:mclust" %in% search())
  expect_length(unique(u$clustering$results$gmm$labels), 2)
})
