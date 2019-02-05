testthat::context("autoplot")

data("MIsim", package = "rsimsum")
data("relhaz", package = "rsimsum")
single <- rsimsum::simsum(data = MIsim, estvarname = "b", true = 0.5, se = "se", methodvar = "method", ref = "CC", x = T)
multi <- rsimsum::simsum(data = relhaz, estvarname = "theta", true = -0.5, se = "se", methodvar = "model", by = c("n", "baseline"), x = TRUE)
singlesum <- summary(single)
multisum <- summary(multi)

testthat::test_that("output from autoplot is of class gg, ggplot", {
  # simsum object, no 'by'
  testthat::expect_s3_class(object = autoplot(single), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(single, type = "forest"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(single, type = "lolly"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(single, type = "zip"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(single, type = "est"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(single, type = "se"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(single, type = "est_ba"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(single, type = "se_ba"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(single, type = "est_ridge"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(single, type = "se_ridge"), class = c("gg", "ggplot"))
  # simsum object, with 'by'
  testthat::expect_s3_class(object = autoplot(multi), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(multi, type = "forest"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(multi, type = "lolly"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(multi, type = "zip"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(multi, type = "est"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(multi, type = "se"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(multi, type = "est_ba"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(multi, type = "se_ba"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(multi, type = "est_ridge"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(multi, type = "se_ridge"), class = c("gg", "ggplot"))
  # summary.simsum object, no 'by'
  testthat::expect_s3_class(object = autoplot(singlesum), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(singlesum, type = "forest"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(singlesum, type = "lolly"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(singlesum, type = "zip"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(singlesum, type = "est"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(singlesum, type = "se"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(singlesum, type = "est_ba"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(singlesum, type = "se_ba"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(singlesum, type = "est_ridge"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(singlesum, type = "se_ridge"), class = c("gg", "ggplot"))
  # summary.simsum object, with 'by'
  testthat::expect_s3_class(object = autoplot(multisum), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(multisum, type = "forest"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(multisum, type = "lolly"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(multisum, type = "zip"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(multisum, type = "est"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(multisum, type = "se"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(multisum, type = "est_ba"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(multisum, type = "se_ba"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(multisum, type = "est_ridge"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(multisum, type = "se_ridge"), class = c("gg", "ggplot"))
})

testthat::test_that("argument checks works throws errors when appropriate", {
  # simsum object, no 'by'
  testthat::expect_error(object = print(autoplot(single, type = "megacool_plot")))
  testthat::expect_error(object = print(autoplot(single, type = 1)))
  testthat::expect_error(object = print(autoplot(single, type = TRUE)))
  testthat::expect_error(object = print(autoplot(single, stats = "summary")))
  testthat::expect_error(object = print(autoplot(single, stats = "BIAS")))
  testthat::expect_error(object = print(autoplot(single, stats = 1)))
  testthat::expect_error(object = print(autoplot(single, stats = TRUE)))
  testthat::expect_error(object = print(autoplot(single, target = "top")))
  testthat::expect_error(object = print(autoplot(single, target = TRUE)))
  testthat::expect_error(object = print(autoplot(single, fitted = NULL)))
  testthat::expect_error(object = print(autoplot(single, fitted = 1)))
  testthat::expect_error(object = print(autoplot(single, fitted = "yes")))
  testthat::expect_error(object = print(autoplot(single, scales = "both")))
  testthat::expect_error(object = print(autoplot(single, scales = "none")))
  testthat::expect_error(object = print(autoplot(single, scales = "either")))
  # simsum object, with 'by'
  testthat::expect_error(object = print(autoplot(multi, type = "megacool_plot")))
  testthat::expect_error(object = print(autoplot(multi, type = 1)))
  testthat::expect_error(object = print(autoplot(multi, type = TRUE)))
  testthat::expect_error(object = print(autoplot(multi, stats = "summary")))
  testthat::expect_error(object = print(autoplot(multi, stats = "BIAS")))
  testthat::expect_error(object = print(autoplot(multi, stats = 1)))
  testthat::expect_error(object = print(autoplot(multi, stats = TRUE)))
  testthat::expect_error(object = print(autoplot(multi, target = "top")))
  testthat::expect_error(object = print(autoplot(multi, target = TRUE)))
  testthat::expect_error(object = print(autoplot(multi, fitted = NULL)))
  testthat::expect_error(object = print(autoplot(multi, fitted = 1)))
  testthat::expect_error(object = print(autoplot(multi, fitted = "yes")))
  testthat::expect_error(object = print(autoplot(multi, scales = "both")))
  testthat::expect_error(object = print(autoplot(multi, scales = "none")))
  testthat::expect_error(object = print(autoplot(multi, scales = "either")))
  # summary.simsum object, no 'by'
  testthat::expect_error(object = print(autoplot(singlesum, type = "megacool_plot")))
  testthat::expect_error(object = print(autoplot(singlesum, type = 1)))
  testthat::expect_error(object = print(autoplot(singlesum, type = TRUE)))
  testthat::expect_error(object = print(autoplot(singlesum, stats = "summary")))
  testthat::expect_error(object = print(autoplot(singlesum, stats = "BIAS")))
  testthat::expect_error(object = print(autoplot(singlesum, stats = 1)))
  testthat::expect_error(object = print(autoplot(singlesum, stats = TRUE)))
  testthat::expect_error(object = print(autoplot(singlesum, target = "top")))
  testthat::expect_error(object = print(autoplot(singlesum, target = TRUE)))
  testthat::expect_error(object = print(autoplot(singlesum, fitted = NULL)))
  testthat::expect_error(object = print(autoplot(singlesum, fitted = 1)))
  testthat::expect_error(object = print(autoplot(singlesum, fitted = "yes")))
  testthat::expect_error(object = print(autoplot(singlesum, scales = "both")))
  testthat::expect_error(object = print(autoplot(singlesum, scales = "none")))
  testthat::expect_error(object = print(autoplot(singlesum, scales = "either")))
  # summary.simsum object, with 'by'
  testthat::expect_error(object = print(autoplot(multisum, type = "megacool_plot")))
  testthat::expect_error(object = print(autoplot(multisum, type = 1)))
  testthat::expect_error(object = print(autoplot(multisum, type = TRUE)))
  testthat::expect_error(object = print(autoplot(multisum, stats = "summary")))
  testthat::expect_error(object = print(autoplot(multisum, stats = "BIAS")))
  testthat::expect_error(object = print(autoplot(multisum, stats = 1)))
  testthat::expect_error(object = print(autoplot(multisum, stats = TRUE)))
  testthat::expect_error(object = print(autoplot(multisum, target = "top")))
  testthat::expect_error(object = print(autoplot(multisum, target = TRUE)))
  testthat::expect_error(object = print(autoplot(multisum, fitted = NULL)))
  testthat::expect_error(object = print(autoplot(multisum, fitted = 1)))
  testthat::expect_error(object = print(autoplot(multisum, fitted = "yes")))
  testthat::expect_error(object = print(autoplot(multisum, scales = "both")))
  testthat::expect_error(object = print(autoplot(multisum, scales = "none")))
  testthat::expect_error(object = print(autoplot(multisum, scales = "either")))
})

testthat::test_that("autoplot with target", {
  # simsum object, no 'by'
  testthat::expect_s3_class(object = autoplot(single, target = 1), class = c("gg", "ggplot"))
  # simsum object, with 'by'
  testthat::expect_s3_class(object = autoplot(multi, target = 1), class = c("gg", "ggplot"))
  # summary.simsum object, no 'by'
  testthat::expect_s3_class(object = autoplot(singlesum, target = 1), class = c("gg", "ggplot"))
  # summary.simsum object, with 'by'
  testthat::expect_s3_class(object = autoplot(multisum, target = 1), class = c("gg", "ggplot"))
})

testthat::test_that("autoplot with stats", {
  # simsum object, no 'by'
  testthat::expect_s3_class(object = autoplot(single, stats = "cover"), class = c("gg", "ggplot"))
  # simsum object, with 'by'
  testthat::expect_s3_class(object = autoplot(multi, stats = "cover"), class = c("gg", "ggplot"))
  # summary.simsum object, no 'by'
  testthat::expect_s3_class(object = autoplot(singlesum, stats = "cover"), class = c("gg", "ggplot"))
  # summary.simsum object, with 'by'
  testthat::expect_s3_class(object = autoplot(multisum, stats = "cover"), class = c("gg", "ggplot"))
})

testthat::test_that("autoplot with target, stats", {
  # simsum object, no 'by'
  testthat::expect_s3_class(object = autoplot(single, target = 0.50, stats = "cover"), class = c("gg", "ggplot"))
  # simsum object, with 'by'
  testthat::expect_s3_class(object = autoplot(multi, target = 0.50, stats = "cover"), class = c("gg", "ggplot"))
  # summary.simsum object, no 'by'
  testthat::expect_s3_class(object = autoplot(singlesum, target = 0.50, stats = "cover"), class = c("gg", "ggplot"))
  # summary.simsum object, with 'by'
  testthat::expect_s3_class(object = autoplot(multisum, target = 0.50, stats = "cover"), class = c("gg", "ggplot"))
})

testthat::test_that("autoplot with fitted", {
  # simsum object, no 'by'
  testthat::expect_s3_class(object = autoplot(single, fitted = TRUE), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(single, fitted = FALSE), class = c("gg", "ggplot"))
  # simsum object, with 'by'
  testthat::expect_s3_class(object = autoplot(multi, fitted = TRUE), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(multi, fitted = FALSE), class = c("gg", "ggplot"))
  # summary.simsum object, no 'by'
  testthat::expect_s3_class(object = autoplot(singlesum, fitted = TRUE), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(singlesum, fitted = FALSE), class = c("gg", "ggplot"))
  # summary.simsum object, with 'by'
  testthat::expect_s3_class(object = autoplot(multisum, fitted = TRUE), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(multisum, fitted = FALSE), class = c("gg", "ggplot"))
})

testthat::test_that("autoplot with scales", {
  # simsum object, no 'by'
  testthat::expect_s3_class(object = autoplot(single, scales = "fixed"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(single, scales = "free"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(single, scales = "free_x"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(single, scales = "free_y"), class = c("gg", "ggplot"))
  # simsum object, with 'by'
  testthat::expect_s3_class(object = autoplot(multi, scales = "fixed"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(multi, scales = "free"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(multi, scales = "free_x"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(multi, scales = "free_y"), class = c("gg", "ggplot"))
  # summary.simsum object, no 'by'
  testthat::expect_s3_class(object = autoplot(singlesum, scales = "fixed"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(singlesum, scales = "free"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(singlesum, scales = "free_x"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(singlesum, scales = "free_y"), class = c("gg", "ggplot"))
  # summary.simsum object, with 'by'
  testthat::expect_s3_class(object = autoplot(multisum, scales = "fixed"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(multisum, scales = "free"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(multisum, scales = "free_x"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(multisum, scales = "free_y"), class = c("gg", "ggplot"))
})
