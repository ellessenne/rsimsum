testthat::test_that("#60", {
  # See https://github.com/ellessenne/rsimsum/issues/60
  set.seed(135)
  test <- data.frame(
    dataset_id = 1:5,
    method = 1,
    estimate = rnorm(5, mean = 1),
    se = rnorm(5, 1 / sqrt(5), 0.05),
    true = 1
  )
  ss <- simsum(
    data = test,
    estvarname = "estimate",
    se = "se",
    true = "true",
    methodvar = "method",
    x = TRUE,
    ref = "1"
  )
  a <- summary(ss, stat = "rbias") |>
    tidy()
  testthat::expect_equal(
    object = a$est,
    expected = -0.1846501,
    tolerance = 1e-6
  )
  testthat::expect_equal(
    object = a$mcse,
    expected = 0.4455973,
    tolerance = 1e-6
  )
  #
  test2 <- test
  test2[1, "se"] <- NA
  ss2 <- simsum(
    data = test2,
    estvarname = "estimate",
    se = "se",
    true = "true",
    methodvar = "method",
    x = TRUE,
    ref = "1"
  )
  b <- summary(ss2, stat = "rbias") |>
    tidy()
  testthat::expect_equal(
    object = b$est,
    expected = -0.1195449,
    tolerance = 1e-6
  )
  testthat::expect_equal(
    object = b$mcse,
    expected = 0.5690903,
    tolerance = 1e-6
  )
})
