testthat::context("performance-math")

testthat::test_that("bias", {
  set.seed(20190107)
  n <- 1000

  # No bias
  df <- data.frame(
    i = seq(n),
    b = rnorm(n = n, mean = 0.50, sd = 0.001),
    se = exp(rnorm(n = n, mean = 0.1, sd = 0.1))
  )
  s <- rsimsum::simsum(data = df, estvarname = "b", se = "se", true = 0.50)
  testthat::expect_equal(object = s$summ$est[s$summ$stat == "bias"], expected = 0, tolerance = 1e-4)

  # Positive bias
  df <- data.frame(
    i = seq(n),
    b = rnorm(n = n, mean = 0.5001, sd = 0.001),
    se = exp(rnorm(n = n, mean = 0.1, sd = 0.1))
  )
  s <- rsimsum::simsum(data = df, estvarname = "b", se = "se", true = 0.50)
  testthat::expect_true(object = (s$summ$est[s$summ$stat == "bias"] > 0))

  # Negative bias
  df <- data.frame(
    i = seq(n),
    b = rnorm(n = n, mean = 0.4999, sd = 0.001),
    se = exp(rnorm(n = n, mean = 0.1, sd = 0.1))
  )
  s <- rsimsum::simsum(data = df, estvarname = "b", se = "se", true = 0.50)
  testthat::expect_true(object = (s$summ$est[s$summ$stat == "bias"] < 0))
})
