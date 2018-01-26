context("multisimsum")

test_that("multisimsum prints ok", {
  data("frailty")
  print(multisimsum(data = frailty, par = "par", true = c(trt = -0.50, fv = 0.75), estvarname = "b", se = "se", methodvar = "model", by = "fv_dist"))
  print(multisimsum(data = frailty, par = "par", true = c(trt = -0.50, fv = 0.75), estvarname = "b", se = "se", by = "fv_dist"))
  print(multisimsum(data = frailty, par = "par", true = c(trt = -0.50, fv = 0.75), estvarname = "b", se = "se", methodvar = "model"))
  print(multisimsum(data = frailty, par = "par", true = c(trt = -0.50, fv = 0.75), estvarname = "b", se = "se"))
  print(multisimsum(data = frailty, par = "par", true = c(trt = -0.50, fv = 0.75), estvarname = "b", se = "se", methodvar = "model", by = "fv_dist", mcse = FALSE))
})

test_that("multisimsum returns an object of class multisimsum", {
  data("frailty")
  ms <- multisimsum(data = frailty, par = "par", true = c(trt = -0.50, fv = 0.75), estvarname = "b", se = "se", methodvar = "model", by = "fv_dist")
  expect_s3_class(ms, "multisimsum")
})

test_that("summ slot of a multisimsum object is a data.frame", {
  data("frailty")
  ms <- multisimsum(data = frailty, par = "par", true = c(trt = -0.50, fv = 0.75), estvarname = "b", se = "se", methodvar = "model", by = "fv_dist")
  expect_s3_class(ms$summ, "data.frame")
})

test_that("not passing estvarname throws an error", {
  expect_error({
    data("frailty")
    ms <- multisimsum(data = frailty, par = "par", true = c(trt = -0.50, fv = 0.75), se = "se", methodvar = "model", by = "fv_dist")
  }, 'argument "estvarname" is missing, with no default')
})

test_that("not passing true throws an error", {
  expect_error({
    data("frailty")
    ms <- multisimsum(data = frailty, par = "par", estvarname = "b", se = "se", methodvar = "model", by = "fv_dist")
  }, 'argument "true" is missing, with no default')
})

test_that("not passing se throws an error", {
  expect_error({
    data("frailty")
    ms <- multisimsum(data = frailty, par = "par", true = c(trt = -0.50, fv = 0.75), estvarname = "b", methodvar = "model", by = "fv_dist")
  }, 'argument "se" is missing, with no default')
})

test_that("specifying ref and not methodvar throws a warning", {
  expect_warning({
    data("frailty")
    ms <- multisimsum(data = frailty, par = "par", true = c(trt = -0.50, fv = 0.75), estvarname = "b", se = "se", ref = "Cox, Gamma", by = "fv_dist")
  }, "`ref` is specified while `methodvar` is not; `ref` will be ignored")
})

test_that("specifying methodvar and not ref shows a message", {
  expect_message({
    data("frailty")
    ms <- multisimsum(data = frailty, par = "par", true = c(trt = -0.50, fv = 0.75), estvarname = "b", se = "se", methodvar = "model", by = "fv_dist")
  }, "`ref` was not specified, Cox, Gamma set as the reference")
})

test_that("specifying dropbig includes a slot with dropped point estimates", {
  data("frailty")
  ms <- multisimsum(data = frailty, par = "par", true = c(trt = -0.50, fv = 0.75), estvarname = "b", se = "se", methodvar = "model", by = "fv_dist", dropbig = TRUE)
  expect_false(is.null(ms$big_estvarname))
})

test_that("specifying dropbig includes a slot with dropped standard errors", {
  data("frailty")
  ms <- multisimsum(data = frailty, par = "par", true = c(trt = -0.50, fv = 0.75), estvarname = "b", se = "se", methodvar = "model", by = "fv_dist", dropbig = TRUE)
  expect_false(is.null(ms$big_se))
})

test_that("running multisimsum on frailty return summaries of the correct dimension", {
  data("frailty")
  ms <- multisimsum(data = frailty, par = "par", true = c(trt = -0.50, fv = 0.75), estvarname = "b", se = "se", methodvar = "model", by = "fv_dist")
  expect_equal(dim(ms$summ), expected = c(224, 6))
})

test_that("multisimsum with mcse option returns mcse", {
  data("frailty")
  ms <- multisimsum(data = frailty, par = "par", true = c(trt = -0.50, fv = 0.75), estvarname = "b", se = "se", methodvar = "model", by = "fv_dist", mcse = TRUE)
  expect_true("mcse" %in% names(ms$summ))
})

test_that("multisimsum without mcse option does not returns mcse", {
  data("frailty")
  ms <- multisimsum(data = frailty, par = "par", true = c(trt = -0.50, fv = 0.75), estvarname = "b", se = "se", methodvar = "model", by = "fv_dist", mcse = FALSE)
  expect_false("mcse" %in% names(ms$summ))
})

test_that("multisimsum with by factors returns error when 'by' name is not a variable in data", {
  expect_error({
    data("frailty")
    ms <- multisimsum(data = frailty, par = "par", true = c(trt = -0.50, fv = 0.75), estvarname = "b", se = "se", methodvar = "model", by = "not_in_data")
  })
})

test_that("multisimsum with by factors works fine", {
  data("frailty")
  ms <- multisimsum(data = frailty, par = "par", true = c(trt = -0.50, fv = 0.75), estvarname = "b", se = "se", methodvar = "model", by = "fv_dist")
})

test_that("multisimsum with by factors returns a data.frame with results", {
  data("frailty")
  ms <- multisimsum(data = frailty, par = "par", true = c(trt = -0.50, fv = 0.75), estvarname = "b", se = "se", methodvar = "model", by = "fv_dist")
  expect_s3_class(object = ms$summ, class = "data.frame")
})

test_that("na.pair argument accepts only scalar boolean values", {
  data("frailty")
  expect_error(object = multisimsum(data = frailty, par = "par", true = c(trt = -0.50, fv = 0.75), estvarname = "b", se = "se", methodvar = "model", by = "fv_dist", na.pair = "Yes"))
  expect_error(object = multisimsum(data = frailty, par = "par", true = c(trt = -0.50, fv = 0.75), estvarname = "b", se = "se", methodvar = "model", by = "fv_dist", na.pair = 1))
  expect_error(object = multisimsum(data = frailty, par = "par", true = c(trt = -0.50, fv = 0.75), estvarname = "b", se = "se", methodvar = "model", by = "fv_dist", na.pair = c(TRUE, TRUE)))
})

test_that("na.pair argument works as expected", {
  data("frailty")
  frailty <- frailty[!is.na(frailty$b) & !is.na(frailty$se), ]
  x1 <- frailty
  x1[1, "b"] <- NA
  ms1 <- multisimsum(data = x1, par = "par", true = c(trt = -0.50, fv = 0.75), estvarname = "b", se = "se")
  x2 <- frailty
  x2[1, "b"] <- NA
  x2[1, "se"] <- NA
  ms2 <- multisimsum(data = x2, par = "par", true = c(trt = -0.50, fv = 0.75), estvarname = "b", se = "se", na.pair = FALSE)
  expect_equal(object = get_data(ms1), expected = get_data(ms2))
})

test_that("multisimsum works with missing data and default arguments", {
  data("frailty")
  x <- frailty
  set.seed(180126)
  x[which(rnorm(nrow(x)) > 2), "b"] <- NA
  x[which(rnorm(nrow(x)) > 2), "se"] <- NA
  multisimsum(data = frailty, par = "par", true = c(trt = -0.50, fv = 0.75), estvarname = "b", se = "se", methodvar = "model", by = "fv_dist")
})
