context("get_data")

test_that("get_data.simsum returns a data.frame", {
  data("MIsim")
  x <- simsum(data = MIsim, estvarname = "b", true = 0.5, se = "se", methodvar = "method", mcse = TRUE)
  expect_s3_class(object = get_data(x), class = "data.frame")
})

test_that("get_data.simsum asking for a subset of summary statistics only", {
  data("MIsim")
  x <- simsum(data = MIsim, estvarname = "b", true = 0.5, se = "se", methodvar = "method", mcse = TRUE)
  gd <- get_data(x, sstat = c("nsim", "bias", "cover"))
  gd
  expect_equal(object = nrow(gd), expected = 3 * length(unique(MIsim$method)))
})

test_that("get_data.simsum asking for bias returns only bias", {
  data("MIsim")
  x <- simsum(data = MIsim, estvarname = "b", true = 0.5, se = "se", methodvar = "method", mcse = TRUE)
  expect_true(object = all(get_data(x, sstat = "bias")$stat == "bias"))
})

test_that("get_data.summary.simsum returns a data.frame", {
  data("MIsim")
  x <- simsum(data = MIsim, estvarname = "b", true = 0.5, se = "se", methodvar = "method", mcse = TRUE)
  s <- summary(x)
  expect_s3_class(object = get_data(s), class = "data.frame")
})

test_that("get_data.summary.simsum asking for a subset of summary statistics only", {
  data("MIsim")
  x <- simsum(data = MIsim, estvarname = "b", true = 0.5, se = "se", methodvar = "method", mcse = TRUE)
  s <- summary(x)
  gd <- get_data(s, sstat = c("nsim", "bias", "cover"))
  gd
  expect_equal(object = nrow(gd), expected = 3 * length(unique(MIsim$method)))
})

test_that("get_data.summary.simsum asking for bias returns only bias", {
  data("MIsim")
  x <- simsum(data = MIsim, estvarname = "b", true = 0.5, se = "se", methodvar = "method", mcse = TRUE)
  s <- summary(x)
  expect_true(object = all(get_data(x, sstat = "bias")$stat == "bias"))
})

test_that("get_data.multisimsum returns a data.frame", {
  data("frailty")
  x <- multisimsum(data = frailty, par = "par", true = c(trt = -0.50, fv = 0.75), estvarname = "b", se = "se", methodvar = "model", by = "fv_dist")
  expect_s3_class(object = get_data(x), class = "data.frame")
})

test_that("get_data.multisimsum asking for a subset of summary statistics only", {
  data("frailty")
  x <- multisimsum(data = frailty, par = "par", true = c(trt = -0.50, fv = 0.75), estvarname = "b", se = "se", methodvar = "model", by = "fv_dist")
  gd <- get_data(x, sstat = c("nsim", "bias", "cover"))
  gd
  expect_equal(object = nrow(gd), expected = 3 * length(unique(frailty$model)) * length(unique(frailty$fv_dist)) * length(unique(frailty$par)))
})

test_that("get_data.multisimsum asking for bias returns only bias", {
  data("frailty")
  x <- multisimsum(data = frailty, par = "par", true = c(trt = -0.50, fv = 0.75), estvarname = "b", se = "se", methodvar = "model", by = "fv_dist")
  expect_true(object = all(get_data(x, sstat = "bias")$stat == "bias"))
})

test_that("get_data.summary.multisimsum returns a data.frame", {
  data("frailty")
  x <- multisimsum(data = frailty, par = "par", true = c(trt = -0.50, fv = 0.75), estvarname = "b", se = "se", methodvar = "model", by = "fv_dist")
  s <- summary(x)
  expect_s3_class(object = get_data(s), class = "data.frame")
})

test_that("get_data.summary.multisimsum asking for a subset of summary statistics only", {
  data("frailty")
  x <- multisimsum(data = frailty, par = "par", true = c(trt = -0.50, fv = 0.75), estvarname = "b", se = "se", methodvar = "model", by = "fv_dist")
  s <- summary(x)
  gd <- get_data(s, sstat = c("nsim", "bias", "cover"))
  gd
  expect_equal(object = nrow(gd), expected = 3 * length(unique(frailty$model)) * length(unique(frailty$fv_dist)) * length(unique(frailty$par)))
})

test_that("get_data.summary.multisimsum asking for bias returns only bias", {
  data("frailty")
  x <- multisimsum(data = frailty, par = "par", true = c(trt = -0.50, fv = 0.75), estvarname = "b", se = "se", methodvar = "model", by = "fv_dist")
  s <- summary(x)
  expect_true(object = all(get_data(x, sstat = "bias")$stat == "bias"))
})
