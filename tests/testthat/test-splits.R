test_that("block_split returns disjoint train and test sets", {
  set.seed(42)
  n <- 200
  d <- data.frame(
    id = seq_len(n),
    date = seq(as.Date("2024-01-01"), by = "day", length.out = n),
    score = rnorm(n)
  )
  s <- block_split(d, order_col = "date", num_blocks = 5, test_prop = 0.2,
                   seed = 1)
  expect_s3_class(s, "block_split")
  expect_equal(nrow(s$train) + nrow(s$test), n)
  expect_length(intersect(s$train$id, s$test$id), 0)
})

test_that("block_split with the same seed is reproducible", {
  d <- data.frame(id = 1:100, x = rnorm(100))
  a <- block_split(d, num_blocks = 4, test_prop = 0.25, seed = 7)
  b <- block_split(d, num_blocks = 4, test_prop = 0.25, seed = 7)
  expect_identical(a$test$id, b$test$id)
})

test_that("diversity_split returns disjoint train and test sets", {
  set.seed(1)
  d <- data.frame(id = 1:150, x1 = rnorm(150), x2 = rnorm(150))
  s <- diversity_split(d, test_prop = 0.2, method = "cluster",
                       exclude_cols = "id", seed = 1, verbose = FALSE)
  expect_s3_class(s, "diversity_split")
  expect_equal(nrow(s$train) + nrow(s$test), 150)
  expect_length(intersect(s$train$id, s$test$id), 0)
})

test_that("prep_split and setup_cv stratify a rare class (< 10%)", {
  d <- data.frame(y = factor(c(rep("Si", 50), rep("No", 950)), levels = c("No", "Si")),
                  x = rnorm(1000))
  n_test <- vapply(1:3, function(s) {
    sp <- prep_split(d, "y", task = "classification", seed = s, verbose = FALSE)
    sum(sp$test$y == "Si")
  }, numeric(1))
  expect_true(all(n_test == 10))
  sp <- prep_split(d, "y", task = "classification", seed = 1, verbose = FALSE)
  folds <- setup_cv(sp$train, "y", task = "classification", cv_folds = 5, seed = 1,
                    verbose = FALSE)
  per_fold <- vapply(folds$splits, function(s) sum(rsample::assessment(s)$y == "Si"),
                     numeric(1))
  expect_true(all(per_fold == 8))
})
