context("summary.simsum")

test_that("summarising a simsum object works fine and prints ok", {
  data("MIsim")
  x <- simsum(data = MIsim, estvarname = "b", true = 0.5, se = "se", methodvar = "method", mcse = TRUE)
  print(summary(x))
  print(summary(x, ci_level = 0.99))
  x <- simsum(data = MIsim, estvarname = "b", true = 0.5, se = "se")
  print(summary(x))
  data("relhaz")
  x <- simsum(data = relhaz, estvarname = "theta", true = -0.5, se = "se", methodvar = "model", by = c("n", "baseline"))
  print(summary(x))
  print(summary(x), sstat = c("bias", "bccover"))
  x <- simsum(data = relhaz, estvarname = "theta", true = -0.5, se = "se", by = c("n", "baseline"))
  print(summary(x), sstat = c("bias", "bccover"))
})

test_that("summary.simsum returns an object of class summary.simsum", {
  data("MIsim")
  x <- simsum(data = MIsim, estvarname = "b", true = 0.5, se = "se", methodvar = "method", mcse = TRUE)
  s <- summary(x)
  expect_s3_class(object = s, class = "summary.simsum")
})

test_that("summary.simsum returns confidence intervals when mcse = TRUE", {
  data("MIsim")
  x <- simsum(data = MIsim, estvarname = "b", true = 0.5, se = "se", methodvar = "method", mcse = TRUE)
  s <- summary(x)
  expect_true(object = all(c("lower", "upper") %in% names(s$summ)))
})

test_that("summary.simsum does not return confidence intervals when mcse = FALSE", {
  data("MIsim")
  x <- simsum(data = MIsim, estvarname = "b", true = 0.5, se = "se", methodvar = "method", mcse = FALSE)
  s <- summary(x)
  expect_true(object = all(!(c("lower", "upper") %in% names(s$summ))))
})

test_that("summary.simsum with wrong arguments throws an error", {
  data("MIsim")
  x <- simsum(data = MIsim, estvarname = "b", true = 0.5, se = "se", methodvar = "method", mcse = FALSE)
  expect_error(object = summary(x, ci_level = -1))
  expect_error(object = summary(x, ci_level = 2))
  expect_error(object = summary(x, ci_level = "0.05"))
})
