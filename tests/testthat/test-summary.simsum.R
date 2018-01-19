context("summary.simsum")

test_that("summarising a simsum object works fine and prints ok", {
  data("MIsim")
  x <- simsum(data = MIsim, estvarname = "b", true = 0.5, se = "se", methodvar = "method", mcse = TRUE)
  summary(x)
  summary(x, ci_level = 0.99)
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
