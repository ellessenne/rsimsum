context("dropbig")

test_that("dropbig works ok and prints fine", {
  data("MIsim")
  # when dropbig = TRUE
  x <- simsum(data = MIsim, estvarname = "b", true = 0.5, se = "se", methodvar = "method", mcse = TRUE, dropbig = TRUE, max = 3, semax = 1.5)
  dropbig(x)
  # when dropbig = FALSE
  x <- simsum(data = MIsim, estvarname = "b", true = 0.5, se = "se", methodvar = "method", mcse = TRUE, dropbig = FALSE)
  dropbig(x)
})

test_that("dropbig returns a data.frame when dropbig = TRUE", {
  data("MIsim")
  x <- simsum(data = MIsim, estvarname = "b", true = 0.5, se = "se", methodvar = "method", mcse = TRUE, dropbig = TRUE, max = 3, semax = 1.5)
  d <- dropbig(x)
  expect_s3_class(object = d$big_estvarname, class = "data.frame")
  expect_s3_class(object = d$big_se, class = "data.frame")
})

test_that("dropbig returns NULL when dropbig = FALSE", {
  data("MIsim")
  x <- simsum(data = MIsim, estvarname = "b", true = 0.5, se = "se", methodvar = "method", mcse = TRUE, dropbig = FALSE)
  d <- dropbig(x)
  expect_null(object = d)
})
