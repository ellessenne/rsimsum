context("Testing relative bias")

test_that("Relative bias calculations are okay, if unbiased", {
  sims <- vapply(X = seq(500), FUN = function(i) {
    N <- 3e3
    x <- rnorm(n = N)
    e <- rnorm(n = N)
    y <- 2 * x + e
    fit <- lm(y ~ x)
    coef(fit)["x"]
  }, FUN.VALUE = numeric(1))
  sims <- data.frame(b = sims)
  s <- simsum(data = sims, estvarname = "b", true = 2)
  expect_equal(
    object = s$summ$est[s$summ$stat == "rbias"],
    expected = 0,
    tolerance = 1e-2
  )
})

test_that("Relative bias calculations are okay, if biased", {
  sims <- vapply(X = seq(500), FUN = function(i) {
    N <- 3e3
    x <- rnorm(n = N)
    e <- rnorm(n = N)
    y <- 2 * x + e
    fit <- lm(y ~ x)
    coef(fit)["x"]
  }, FUN.VALUE = numeric(1))
  sims <- data.frame(b = sims)
  s <- simsum(data = sims, estvarname = "b", true = 1)
  expect_equal(
    object = s$summ$est[s$summ$stat == "rbias"],
    expected = 1,
    tolerance = 1e-2
  )
})
