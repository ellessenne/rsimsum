context("is.multisimsum")

test_that("is.simsum works as expected", {
  data(frailty)
  x <- multisimsum(data = frailty, par = "par", true = c(trt = -0.50, fv = 0.75), estvarname = "b", se = "se", methodvar = "model", by = "fv_dist")
  expect_true(object = is.multisimsum(x))
  expect_false(object = is.multisimsum(frailty))
  expect_false(object = is.simsum(x))
})
