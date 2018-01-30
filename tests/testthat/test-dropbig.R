context("dropbig")

test_that("dropbig works ok and prints fine", {
  data("MIsim")
  # when dropbig = TRUE
  x <- simsum(data = MIsim, estvarname = "b", true = 0.5, se = "se", methodvar = "method", mcse = TRUE, dropbig = TRUE, max = 3, semax = 1.5)
  print(dropbig(x))
  # when dropbig = FALSE
  x <- simsum(data = MIsim, estvarname = "b", true = 0.5, se = "se", methodvar = "method", mcse = TRUE, dropbig = FALSE)
  print(dropbig(x))
  # with huuuuge limits
  x <- simsum(data = MIsim, estvarname = "b", true = 0.5, se = "se", methodvar = "method", mcse = TRUE, dropbig = TRUE, max = 10, semax = 10)
  print(dropbig(x))
  # with by factors
  data("relhaz")
  x <- simsum(data = relhaz, estvarname = "theta", true = -0.50, se = "se", methodvar = "model", by = c("baseline", "n"), dropbig = TRUE, max = 3, semax = 1.5)
  print(dropbig(x))
  # for multisimsum
  data("frailty")
  ms <- multisimsum(data = frailty, par = "par", true = c(trt = -0.50, fv = 0.75), estvarname = "b", se = "se", methodvar = "model", by = "fv_dist", dropbig = FALSE)
  print(dropbig(ms))
  ms <- multisimsum(data = frailty, par = "par", true = c(trt = -0.50, fv = 0.75), estvarname = "b", se = "se", methodvar = "model", by = "fv_dist", dropbig = TRUE)
  print(dropbig(ms))
  ms <- multisimsum(data = frailty, par = "par", true = c(trt = -0.50, fv = 0.75), estvarname = "b", se = "se", methodvar = "model", by = "fv_dist", dropbig = TRUE, max = 6, semax = 3)
  print(dropbig(ms))
})

test_that("dropbig returns a data.frame when dropbig = TRUE", {
  data("MIsim")
  x <- simsum(data = MIsim, estvarname = "b", true = 0.5, se = "se", methodvar = "method", mcse = TRUE, dropbig = TRUE, max = 3, semax = 1.5)
  d <- dropbig(x)
  expect_s3_class(object = d$big_estvarname, class = "data.frame")
  expect_s3_class(object = d$big_se, class = "data.frame")
  data("frailty")
  ms <- multisimsum(data = frailty, par = "par", true = c(trt = -0.50, fv = 0.75), estvarname = "b", se = "se", methodvar = "model", by = "fv_dist", dropbig = TRUE, max = 6, semax = 3)
  d <- dropbig(ms)
  expect_s3_class(object = d$big_estvarname, class = "data.frame")
  expect_s3_class(object = d$big_se, class = "data.frame")
})

test_that("dropbig returns NULL when dropbig = FALSE", {
  data("MIsim")
  x <- simsum(data = MIsim, estvarname = "b", true = 0.5, se = "se", methodvar = "method", mcse = TRUE, dropbig = FALSE)
  d <- dropbig(x)
  expect_null(object = d)
  data("frailty")
  ms <- multisimsum(data = frailty, par = "par", true = c(trt = -0.50, fv = 0.75), estvarname = "b", se = "se", methodvar = "model", by = "fv_dist", dropbig = FALSE)
  d <- dropbig(ms)
  expect_null(object = d)
})
