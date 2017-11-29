context("simsum")

test_that("simsum returns an object of class simsum", {
	data("MIsim")
	s = simsum(data = MIsim, estvarname = "b", true = 0.5, se = "se", methodvar = "method", ref = "CC")
	# test
	expect_s3_class(s, "simsum")
})

test_that("summ slot of a simsum object is a data.frame", {
	data("MIsim")
	s = simsum(data = MIsim, estvarname = "b", true = 0.5, se = "se", methodvar = "method", ref = "CC")
	# test
	expect_s3_class(s$summ, "data.frame")
})

test_that("not passing estvarname throws an error", {
	expect_error({
		data("MIsim")
		s = simsum(data = MIsim, true = 0.5, se = "se", methodvar = "method", ref = "CC")
	}, 'argument "estvarname" is missing, with no default')
})

test_that("not passing true throws an error", {
	expect_error({
		data("MIsim")
		s = simsum(data = MIsim, estvarname = "b", se = "se", methodvar = "method", ref = "CC")
	}, 'argument "true" is missing, with no default')
})

test_that("not passing se throws an error", {
	expect_error({
		data("MIsim")
		s = simsum(data = MIsim, estvarname = "b", true = 0.5, methodvar = "method", ref = "CC")
	}, 'argument "se" is missing, with no default')
})

test_that("specifying ref and not methodvar throws a warning", {
	expect_warning({
		data("MIsim")
		s = simsum(data = MIsim, estvarname = "b", true = 0.5, se = "se", ref = "CC")
	}, "`ref` is specified while `methodvar` is not; `ref` will be ignored")
})

test_that("specifying methodvar and not ref shows a message", {
	expect_message({
		data("MIsim")
		s = simsum(data = MIsim, estvarname = "b", true = 0.5, se = "se", methodvar = "method")
	}, "`ref` was not specified, CC set as the reference")
})

test_that("running simsum on MIsim return summaries of the correct dimension", {
	data("MIsim")
	s = simsum(data = MIsim, estvarname = "b", true = 0.5, se = "se", methodvar = "method")
	expect_equal(dim(s$summ), expected = c(42, 4))
})

test_that("simsum with mcse option returns mcse", {
	data("MIsim")
	s = simsum(data = MIsim, estvarname = "b", true = 0.5, se = "se", methodvar = "method")
	expect_true("mcse" %in% names(s$summ))
})

test_that("simsum without mcse option returns mcse", {
	data("MIsim")
	s = simsum(data = MIsim, estvarname = "b", true = 0.5, se = "se", methodvar = "method", mcse = FALSE)
	expect_false("mcse" %in% names(s$summ))
})
