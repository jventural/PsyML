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
