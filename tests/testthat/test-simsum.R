context("simsum")

test_that("simsum prints ok", {
  data("MIsim")
  data("relhaz")
  expect_output(print(simsum(data = MIsim, estvarname = "b", true = 0.5, se = "se", methodvar = "method")))
  expect_output(print(simsum(data = MIsim, estvarname = "b", true = 0.5, se = "se")))
  expect_output(print(simsum(data = MIsim, estvarname = "b", true = 0.5, se = "se", mcse = FALSE)))
  expect_output(print(simsum(data = relhaz, estvarname = "theta", true = -0.5, se = "se", methodvar = "model", by = c("n", "baseline"))))
  expect_output(print(simsum(data = relhaz, estvarname = "theta", true = -0.5, se = "se", by = c("n", "baseline"))))
})

test_that("simsum returns an object of class simsum", {
  data("MIsim")
  s <- simsum(data = MIsim, estvarname = "b", true = 0.5, se = "se", methodvar = "method", ref = "CC")
  # test
  expect_s3_class(s, "simsum")
})

test_that("summ slot of a simsum object is a data.frame", {
  data("MIsim")
  s <- simsum(data = MIsim, estvarname = "b", true = 0.5, se = "se", methodvar = "method", ref = "CC")
  # test
  expect_s3_class(s$summ, "data.frame")
})

test_that("not passing estvarname throws an error", {
  expect_error({
    data("MIsim")
    s <- simsum(data = MIsim, true = 0.5, se = "se", methodvar = "method", ref = "CC")
  }, 'argument "estvarname" is missing, with no default')
})

test_that("not passing true throws an error", {
  expect_error({
    data("MIsim")
    s <- simsum(data = MIsim, estvarname = "b", se = "se", methodvar = "method", ref = "CC")
  }, 'argument "true" is missing, with no default')
})

test_that("not passing se throws an error", {
  expect_error({
    data("MIsim")
    s <- simsum(data = MIsim, estvarname = "b", true = 0.5, methodvar = "method", ref = "CC")
  }, 'argument "se" is missing, with no default')
})

test_that("specifying ref and not methodvar throws a warning", {
  expect_warning({
    data("MIsim")
    s <- simsum(data = MIsim, estvarname = "b", true = 0.5, se = "se", ref = "CC")
  }, "`ref` is specified while `methodvar` is not; `ref` will be ignored")
})

test_that("specifying methodvar and not ref shows a message", {
  expect_message({
    data("MIsim")
    s <- simsum(data = MIsim, estvarname = "b", true = 0.5, se = "se", methodvar = "method")
  }, "`ref` was not specified, CC set as the reference")
})

test_that("specifying dropbig includes a slot with dropped point estimates", {
  data("MIsim")
  s <- simsum(data = MIsim, estvarname = "b", true = 0.5, se = "se", methodvar = "method", dropbig = TRUE)
  expect_false(is.null(s$big_estvarname))
})

test_that("specifying dropbig includes a slot with dropped standard errors", {
  data("MIsim")
  s <- simsum(data = MIsim, estvarname = "b", true = 0.5, se = "se", methodvar = "method", dropbig = TRUE)
  expect_false(is.null(s$big_se))
})

test_that("running simsum on MIsim return summaries of the correct dimension", {
  data("MIsim")
  s <- simsum(data = MIsim, estvarname = "b", true = 0.5, se = "se", methodvar = "method")
  expect_equal(dim(s$summ), expected = c(42, 4))
})

test_that("simsum with mcse option returns mcse", {
  data("MIsim")
  s <- simsum(data = MIsim, estvarname = "b", true = 0.5, se = "se", methodvar = "method")
  expect_true("mcse" %in% names(s$summ))
})

test_that("simsum without mcse option does not returns mcse", {
  data("MIsim")
  s <- simsum(data = MIsim, estvarname = "b", true = 0.5, se = "se", methodvar = "method", mcse = FALSE)
  expect_false("mcse" %in% names(s$summ))
})

test_that("simsum with by factors returns error when 'by' name is not a variable in data", {
  expect_error({
    data("relhaz")
    simsum(data = relhaz, estvarname = "theta", true = -0.5, se = "se", methodvar = "model", by = "by")
  })
})

test_that("simsum with by factors works fine", {
  data("relhaz")
  s <- simsum(data = relhaz, estvarname = "theta", true = -0.5, se = "se", methodvar = "model", by = "n")
  s <- simsum(data = relhaz, estvarname = "theta", true = -0.5, se = "se", methodvar = "model", by = "baseline")
  s <- simsum(data = relhaz, estvarname = "theta", true = -0.5, se = "se", methodvar = "model", by = c("n", "baseline"))
})

test_that("simsum with by factors returns a data.frame with results", {
  data("relhaz")
  s <- simsum(data = relhaz, estvarname = "theta", true = -0.5, se = "se", methodvar = "model", by = c("n", "baseline"))
  expect_s3_class(object = s$summ, class = "data.frame")
})

test_that("na.pair argument accepts only scalar boolean values", {
  data("MIsim")
  expect_error(object = simsum(data = MIsim, estvarname = "b", true = 0.5, se = "se", methodvar = "method", na.pair = "Yes"))
  expect_error(object = simsum(data = MIsim, estvarname = "b", true = 0.5, se = "se", methodvar = "method", na.pair = 1))
  expect_error(object = simsum(data = MIsim, estvarname = "b", true = 0.5, se = "se", methodvar = "method", na.pair = c(TRUE, TRUE)))
})

test_that("na.pair argument works as expected", {
  data("MIsim")
  x1 <- MIsim
  x1[1, "b"] <- NA
  s1 <- simsum(data = x1, estvarname = "b", true = 0.5, se = "se", methodvar = "method", na.pair = TRUE)
  x2 <- MIsim
  x2[1, "b"] <- NA
  x2[1, "se"] <- NA
  s2 <- simsum(data = x2, estvarname = "b", true = 0.5, se = "se", methodvar = "method", na.pair = FALSE)
  expect_equal(object = get_data(s1), expected = get_data(s2))
})

test_that("simsum works with missing data and default arguments", {
  data("MIsim")
  x <- MIsim
  set.seed(180123)
  x[which(rnorm(nrow(x)) > 2), "b"] <- NA
  x[which(rnorm(nrow(x)) > 2), "se"] <- NA
  simsum(data = x, estvarname = "b", true = 0.5, se = "se", methodvar = "method")
})

test_that("simsum with x = FALSE does not return data", {
  data("MIsim")
  s <- simsum(data = MIsim, estvarname = "b", true = 0.5, se = "se", methodvar = "method", x = FALSE)
  expect_null(object = s$data)
})

test_that("simsum with x = TRUE returns the original dataset (setting all data processing arguments to FALSE is required)", {
  data("MIsim")
  s <- simsum(data = MIsim, estvarname = "b", true = 0.5, se = "se", methodvar = "method", x = TRUE, dropbig = FALSE, na.rm = FALSE, na.pair = FALSE)
  expect_s3_class(object = s$data, class = "data.frame")
  expect_equal(object = s$data, expected = na.omit(MIsim))
})

test_that("simsum with custom ci.limits works as expected", {
	data("MIsim")
	s <- simsum(data = MIsim, estvarname = "b", true = 0.5, se = "se", methodvar = "method", ci.limits = c(-Inf, Inf))
	expect_true(object = all(s$summ$est[s$summ$stat == "cover"] == 1))
	s <- simsum(data = MIsim, estvarname = "b", true = 0.5, se = "se", methodvar = "method", ci.limits = c(Inf, -Inf))
	expect_true(object = all(s$summ$est[s$summ$stat == "cover"] == 0))
})
