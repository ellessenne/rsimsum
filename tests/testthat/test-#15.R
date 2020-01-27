### New tests for issue #15
testthat::context("#15")

testthat::test_that("simsum with 'true' as a column returns the same results as using a scalar value if each row has the same value", {
  data(tt, package = "rsimsum")
  tt$true <- -1

  s1 <- simsum(data = tt, estvarname = "diff", true = "true", se = "se", ci.limits = c("lower", "upper"), methodvar = "method", by = "dgm")
  s2 <- simsum(data = tt, estvarname = "diff", true = -1, se = "se", ci.limits = c("lower", "upper"), methodvar = "method", by = "dgm")
  testthat::expect_identical(object = get_data(s1), expected = get_data(s2))

  s1 <- simsum(data = tt, estvarname = "diff", true = "true", se = "se", methodvar = "method", by = "dgm")
  s2 <- simsum(data = tt, estvarname = "diff", true = -1, se = "se", methodvar = "method", by = "dgm")
  testthat::expect_identical(object = get_data(s1), expected = get_data(s2))
})

testthat::test_that("simsum with 'true' as a column returns different results compared to using a scalar value if each row has a different value", {
  data(tt, package = "rsimsum")
  tt$true <- stats::rnorm(n = nrow(tt), mean = -1, sd = 1)

  s1 <- simsum(data = tt, estvarname = "diff", true = "true", se = "se", ci.limits = c("lower", "upper"), methodvar = "method", by = "dgm")
  s2 <- simsum(data = tt, estvarname = "diff", true = -1, se = "se", ci.limits = c("lower", "upper"), methodvar = "method", by = "dgm")
  testthat::expect_false(object = identical(get_data(s1), get_data(s2)))

  s1 <- simsum(data = tt, estvarname = "diff", true = "true", se = "se", methodvar = "method", by = "dgm")
  s2 <- simsum(data = tt, estvarname = "diff", true = -1, se = "se", methodvar = "method", by = "dgm")
  testthat::expect_false(object = identical(get_data(s1), get_data(s2)))
})
