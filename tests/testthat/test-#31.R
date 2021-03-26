### New tests for PR #31
testthat::context("#31")

testthat::test_that("Splitting and not-splitting 'method' in MIsim leads to the same results", {
  data(MIsim, package = "rsimsum")
  data(MIsim2, package = "rsimsum")
  s1 <- simsum(data = MIsim, estvarname = "b", true = 0.5, se = "se", methodvar = "method")
  s2 <- simsum(data = MIsim2, estvarname = "b", true = 0.5, se = "se", methodvar = c("m1", "m2"))
  testthat::expect_identical(object = tidy(s1)$stat, expected = tidy(s2)$stat)
  testthat::expect_identical(object = tidy(s1)$est, expected = tidy(s2)$est)
  testthat::expect_identical(object = tidy(s1)$mcse, expected = tidy(s2)$mcse)
})
