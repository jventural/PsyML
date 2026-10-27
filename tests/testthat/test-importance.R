test_that("get_importance returns model-specific and permutation importance", {
  set.seed(1)
  df <- data.frame(x1 = rnorm(150), x2 = rnorm(150), x3 = rnorm(150))
  df$y <- 2 * df$x1 + 0.5 * df$x2 + rnorm(150)
  wf <- workflows::workflow() |>
    workflows::add_formula(y ~ .) |>
    workflows::add_model(parsnip::set_engine(parsnip::linear_reg(), "lm"))
  fit <- parsnip::fit(wf, data = df)

  nat <- get_importance(fit, verbose = FALSE)
  expect_named(nat, c("Variable", "Importance", "Rank"))
  expect_equal(nat$Variable[1], "x1")
  expect_identical(attr(nat, "method"), "abs_statistic")

  perm <- get_importance(fit, data = df, target = "y",
                         method = "permutation", nsim = 2, verbose = FALSE)
  expect_equal(perm$Variable[1], "x1")
  expect_identical(attr(perm, "method"), "permutation")
})
