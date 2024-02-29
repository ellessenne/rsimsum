testthat::test_that("#55", {
  data("tt", package = "rsimsum")

  s <- simsum(data = tt, estvarname = "diff", se = "se", true = -1, x = TRUE)
  testthat::expect_s3_class(object = autoplot(s, type = "zip"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(s, type = "zip", zip_ci_colours = "blue"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(s, type = "zip", zip_ci_colours = c("green", "red")), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(s, type = "zip", zip_ci_colours = c("green", "red", "blue")), class = c("gg", "ggplot"))
  testthat::expect_error(object = autoplot(s, type = "zip", zip_ci_colours = c("green", "red", "blue", "yellow")))
  testthat::expect_error(object = autoplot(s, type = "zip", zip_ci_colours = 1))
  testthat::expect_error(object = autoplot(s, type = "zip", zip_ci_colours = TRUE))
})
