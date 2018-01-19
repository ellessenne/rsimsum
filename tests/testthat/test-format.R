context("format.simsum")

test_that("format.simsum returns a simsum object", {
  data("MIsim")
  x <- simsum(data = MIsim, estvarname = "b", true = 0.5, se = "se", methodvar = "method")
  fx <- format(x, digits = 2)
  expect_s3_class(object = fx, class = "simsum")
})

test_that("the summ slot of a formatted simsum object is a data.frame", {
	data("MIsim")
	x <- simsum(data = MIsim, estvarname = "b", true = 0.5, se = "se", methodvar = "method")
	fx <- format(x, digits = 2)
	expect_s3_class(object = fx$summ, class = "data.frame")
})

test_that("all columns of a formatted object are string-type columns", {
	data("MIsim")
	x <- simsum(data = MIsim, estvarname = "b", true = 0.5, se = "se", methodvar = "method")
	fx <- format(x, digits = 2)
	classes <- sapply(X = names(fx$summ), FUN = function(x) class(fx$summ[[x]]))
	expect_true(object = all(classes == "character"))
})

context("format.summary.simsum")

test_that("format.summary.simsum returns a summary.simsum object", {
  data("MIsim")
  x <- simsum(data = MIsim, estvarname = "b", true = 0.5, se = "se", methodvar = "method")
  fx <- format(summary(x), digits = 2)
  expect_s3_class(object = fx, class = "summary.simsum")
})

test_that("the summ slot of a formatted summary.simsum object is a data.frame", {
	data("MIsim")
	x <- simsum(data = MIsim, estvarname = "b", true = 0.5, se = "se", methodvar = "method")
	fx <- format(summary(x), digits = 2)
	expect_s3_class(object = fx$summ, class = "data.frame")
})

test_that("all columns of a formatted object are string-type columns", {
	data("MIsim")
	x <- simsum(data = MIsim, estvarname = "b", true = 0.5, se = "se", methodvar = "method")
	fx <- format(summary(x), digits = 2)
	classes <- sapply(X = names(fx$summ), FUN = function(x) class(fx$summ[[x]]))
	expect_true(object = all(classes == "character"))
})
