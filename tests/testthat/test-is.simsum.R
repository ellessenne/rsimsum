context("is.simsum")

test_that("is.simsum works as expected", {
  data("MIsim")
  x <- simsum(data = MIsim, estvarname = "b", true = 0.5, se = "se", methodvar = "method")
  expect_true(object = is.simsum(x))
  expect_false(object = is.simsum(MIsim))
})
