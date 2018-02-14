context("miss")

test_that("miss prints ok", {
  data("frailty", package = "rsimsum")
  m <- miss(data = frailty, estvarname = "b", se = "se", par = "par", methodvar = "model", by = "fv_dist")
  expect_output(print(m))
  expect_output(print(m, fmt = "%.2f"))
})

test_that("miss returns an object of class miss", {
  data("frailty", package = "rsimsum")
  m <- miss(data = frailty, estvarname = "b", se = "se", par = "par", methodvar = "model", by = "fv_dist")
  expect_s3_class(m, class = "miss")
})

test_that("missdata slot of a miss object is a data.frame", {
  data("frailty", package = "rsimsum")
  m <- miss(data = frailty, estvarname = "b", se = "se", par = "par", methodvar = "model", by = "fv_dist")
  expect_s3_class(m$missdata, class = "data.frame")
})

test_that("it is ok not to pass some arguments", {
  data("frailty", package = "rsimsum")
  m <- miss(data = frailty, estvarname = "b", se = "se")
  expect_s3_class(m, class = "miss")
})

test_that("not passing data, estvarname, se throws an error", {
  data("frailty", package = "rsimsum")
  expect_error(miss(estvarname = "b", se = "se"))
  expect_error(miss(data = frailty, se = "se"))
  expect_error(miss(data = frailty, estvarname = "b"))
})

test_that("specifying one of par, method, by works fine", {
  data("frailty", package = "rsimsum")
  expect_output(print(miss(data = frailty, estvarname = "b", se = "se", par = "par")))
  expect_output(print(miss(data = frailty, estvarname = "b", se = "se", methodvar = "model")))
  expect_output(print(miss(data = frailty, estvarname = "b", se = "se", by = "fv_dist")))
})
