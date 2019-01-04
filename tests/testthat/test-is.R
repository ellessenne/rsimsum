testthat::context("is.simsum")

testthat::test_that("is.simsum works as expected", {
  data("MIsim")
  x <- simsum(data = MIsim, estvarname = "b", true = 0.5, se = "se", methodvar = "method")
  testthat::expect_true(object = is.simsum(x))
  testthat::expect_false(object = is.simsum(MIsim))
  testthat::expect_false(object = is.multisimsum(x))
})

testthat::context("is.multisimsum")

testthat::test_that("is.simsum works as expected", {
  data(frailty)
  x <- multisimsum(data = frailty, par = "par", true = c(trt = -0.50, fv = 0.75), estvarname = "b", se = "se", methodvar = "model", by = "fv_dist")
  testthat::expect_true(object = is.multisimsum(x))
  testthat::expect_false(object = is.multisimsum(frailty))
  testthat::expect_false(object = is.simsum(x))
})
