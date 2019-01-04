testthat::context("summary.simsum")

testthat::test_that("summarising a simsum object works fine and prints ok", {
  data("MIsim")
  x <- simsum(data = MIsim, estvarname = "b", true = 0.5, se = "se", methodvar = "method")
  testthat::expect_output(print(summary(x)))
  testthat::expect_output(print(summary(x, ci_level = 0.99)))
  x <- simsum(data = MIsim, estvarname = "b", true = 0.5, se = "se")
  testthat::expect_output(print(summary(x)))
  data("relhaz")
  x <- simsum(data = relhaz, estvarname = "theta", true = -0.5, se = "se", methodvar = "model", by = c("n", "baseline"))
  testthat::expect_output(print(summary(x)))
  x <- simsum(data = relhaz, estvarname = "theta", true = -0.5, se = "se", by = c("n", "baseline"))
  testthat::expect_output(print(summary(x)))
  testthat::expect_output(print(summary(x), mcse = NULL))
  testthat::expect_output(print(summary(x), mcse = TRUE))
  testthat::expect_output(print(summary(x), mcse = FALSE))
  testthat::expect_error(print(summary(x), digits = -1))
})

testthat::test_that("summary.simsum returns an object of class summary.simsum", {
  data("MIsim")
  x <- simsum(data = MIsim, estvarname = "b", true = 0.5, se = "se", methodvar = "method")
  s <- summary(x)
  testthat::expect_s3_class(object = s, class = "summary.simsum")
})

testthat::test_that("summary.simsum returns confidence intervals when mcse = TRUE", {
  data("MIsim")
  x <- simsum(data = MIsim, estvarname = "b", true = 0.5, se = "se", methodvar = "method")
  s <- summary(x)
  testthat::expect_true(object = all(c("lower", "upper") %in% names(s$summ)))
})

testthat::test_that("summary.simsum does not return confidence intervals when mcse = FALSE", {
  data("MIsim")
  x <- simsum(data = MIsim, estvarname = "b", true = 0.5, se = "se", methodvar = "method", control = list(mcse = FALSE))
  s <- summary(x)
  testthat::expect_true(object = all(!(c("lower", "upper") %in% names(s$summ))))
})

testthat::test_that("summary.simsum with wrong arguments throws an error", {
  data("MIsim")
  x <- simsum(data = MIsim, estvarname = "b", true = 0.5, se = "se", methodvar = "method", control = list(mcse = FALSE))
  testthat::expect_error(object = summary(x, ci_level = -1))
  testthat::expect_error(object = summary(x, ci_level = 2))
  testthat::expect_error(object = summary(x, ci_level = 1.50))
  testthat::expect_error(object = summary(x, stats = "42"))
  testthat::expect_error(object = summary(x, ci_level = "0.05"))
})
