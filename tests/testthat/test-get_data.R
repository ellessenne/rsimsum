testthat::context("get_data")

testthat::test_that("get_data returns a data.frame", {
  data("MIsim", package = "rsimsum")
  x <- simsum(data = MIsim, estvarname = "b", true = 0.5, se = "se", methodvar = "method")
  testthat::expect_s3_class(object = get_data(x), class = "data.frame")
  xs <- summary(x)
  testthat::expect_s3_class(object = get_data(xs), class = "data.frame")
  data("frailty", package = "rsimsum")
  ms <- multisimsum(data = frailty, par = "par", true = c(trt = -0.50, fv = 0.75), estvarname = "b", se = "se", methodvar = "model", by = "fv_dist")
  testthat::expect_s3_class(object = get_data(ms), class = "data.frame")
  sms <- summary(ms)
  testthat::expect_s3_class(object = get_data(sms), class = "data.frame")
})
