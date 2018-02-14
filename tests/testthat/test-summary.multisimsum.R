context("summary.multisimsum")

test_that("summarising a simsum object works fine and prints ok", {
  data("frailty")
  x <- multisimsum(data = frailty, par = "par", true = c(trt = -0.50, fv = 0.75), estvarname = "b", se = "se", methodvar = "model", by = "fv_dist")
  expect_output(print(summary(x)))
  expect_output(print(summary(x, ci_level = 0.99)))
  x <- multisimsum(data = frailty, par = "par", true = c(trt = -0.50, fv = 0.75), estvarname = "b", se = "se", methodvar = "model")
  expect_output(print(summary(x)))
  data("frailty")
  x <- multisimsum(data = frailty, par = "par", true = c(trt = -0.50, fv = 0.75), estvarname = "b", se = "se", by = "fv_dist")
  expect_output(print(summary(x)))
  data("frailty")
  x <- multisimsum(data = frailty, par = "par", true = c(trt = -0.50, fv = 0.75), estvarname = "b", se = "se")
  expect_output(print(summary(x)))
  expect_output(print(summary(x), sstat = c("bias", "bccover")))
})

test_that("summary.multisimsum returns an object of class summary.multisimsum", {
  data("frailty")
  x <- multisimsum(data = frailty, par = "par", true = c(trt = -0.50, fv = 0.75), estvarname = "b", se = "se", methodvar = "model", by = "fv_dist")
  s <- summary(x)
  expect_s3_class(object = s, class = "summary.multisimsum")
})

test_that("summary.multisimsum returns confidence intervals when mcse = TRUE", {
  data("frailty")
  x <- multisimsum(data = frailty, par = "par", true = c(trt = -0.50, fv = 0.75), estvarname = "b", se = "se", methodvar = "model", by = "fv_dist", mcse = TRUE)
  s <- summary(x)
  expect_true(object = all(c("lower", "upper") %in% names(s$summ)))
})

test_that("summary.multisimsum does not return confidence intervals when mcse = FALSE", {
  data("frailty")
  x <- multisimsum(data = frailty, par = "par", true = c(trt = -0.50, fv = 0.75), estvarname = "b", se = "se", methodvar = "model", by = "fv_dist", mcse = FALSE)
  s <- summary(x)
  expect_true(object = all(!(c("lower", "upper") %in% names(s$summ))))
})

test_that("summary.multisimsum with wrong arguments throws an error", {
  data("frailty")
  x <- multisimsum(data = frailty, par = "par", true = c(trt = -0.50, fv = 0.75), estvarname = "b", se = "se", methodvar = "model", by = "fv_dist")
  expect_error(object = summary(x, ci_level = -1))
  expect_error(object = summary(x, ci_level = 2))
  expect_error(object = summary(x, ci_level = "0.05"))
})
