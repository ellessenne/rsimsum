testthat::context("autoplot")

data("MIsim", package = "rsimsum")
data("relhaz", package = "rsimsum")
single <- rsimsum::simsum(data = MIsim, estvarname = "b", true = 0.5, se = "se", methodvar = "method", ref = "CC", x = T)
multi <- rsimsum::simsum(data = relhaz, estvarname = "theta", true = -0.5, se = "se", methodvar = "model", by = c("n", "baseline"), x = TRUE)
singlesum <- summary(single)
multisum <- summary(multi)
data("nlp", package = "rsimsum")
nlps <- rsimsum::simsum(data = nlp, estvarname = "b", true = 0, se = "se", methodvar = "model", by = c("baseline", "ss", "esigma"))
nlpssum <- summary(nlps)

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
  testthat::expect_s3_class(object = autoplot(single, type = "heat"), class = c("gg", "ggplot"))
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
  testthat::expect_s3_class(object = autoplot(multi, type = "heat"), class = c("gg", "ggplot"))
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
  testthat::expect_s3_class(object = autoplot(singlesum, type = "heat"), class = c("gg", "ggplot"))
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
  testthat::expect_s3_class(object = autoplot(multisum, type = "heat"), class = c("gg", "ggplot"))
  # nested loop plot
  testthat::expect_s3_class(object = autoplot(nlps, type = "nlp"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(nlpssum, type = "nlp"), class = c("gg", "ggplot"))
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
  testthat::expect_error(object = print(autoplot(single, top = 0)))
  testthat::expect_error(object = print(autoplot(single, top = "Yes!")))
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
  testthat::expect_error(object = print(autoplot(multi, top = 0)))
  testthat::expect_error(object = print(autoplot(multi, top = "Yes!")))
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
  testthat::expect_error(object = print(autoplot(singlesum, top = 0)))
  testthat::expect_error(object = print(autoplot(singlesum, top = "Yes!")))
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
  testthat::expect_error(object = print(autoplot(multisum, top = 0)))
  testthat::expect_error(object = print(autoplot(multisum, top = "Yes!")))
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

testthat::test_that("autoplot with top", {
  # nested loop plot
  testthat::expect_s3_class(object = autoplot(nlps, type = "nlp", top = TRUE), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(nlps, type = "nlp", top = FALSE), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(nlpssum, type = "nlp", top = TRUE), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(nlpssum, type = "nlp", top = FALSE), class = c("gg", "ggplot"))
})

testthat::test_that("inferring target", {
  # simsum object, no 'by'
  testthat::expect_s3_class(object = autoplot(single, stats = "bias"), class = c("gg", "ggplot"))
  # simsum object, with 'by'
  testthat::expect_s3_class(object = autoplot(multi, stats = "bias"), class = c("gg", "ggplot"))
  # summary.simsum object, no 'by'
  testthat::expect_s3_class(object = autoplot(singlesum, stats = "bias"), class = c("gg", "ggplot"))
  # summary.simsum object, with 'by'
  testthat::expect_s3_class(object = autoplot(multisum, stats = "bias"), class = c("gg", "ggplot"))
  # simsum object, no 'by'
  testthat::expect_s3_class(object = autoplot(single, stats = "cover"), class = c("gg", "ggplot"))
  # simsum object, with 'by'
  testthat::expect_s3_class(object = autoplot(multi, stats = "cover"), class = c("gg", "ggplot"))
  # summary.simsum object, no 'by'
  testthat::expect_s3_class(object = autoplot(singlesum, stats = "cover"), class = c("gg", "ggplot"))
  # summary.simsum object, with 'by'
  testthat::expect_s3_class(object = autoplot(multisum, stats = "cover"), class = c("gg", "ggplot"))
  # simsum object, no 'by'
  testthat::expect_s3_class(object = autoplot(single, stats = "becover"), class = c("gg", "ggplot"))
  # simsum object, with 'by'
  testthat::expect_s3_class(object = autoplot(multi, stats = "becover"), class = c("gg", "ggplot"))
  # summary.simsum object, no 'by'
  testthat::expect_s3_class(object = autoplot(singlesum, stats = "becover"), class = c("gg", "ggplot"))
  # summary.simsum object, with 'by'
  testthat::expect_s3_class(object = autoplot(multisum, stats = "becover"), class = c("gg", "ggplot"))
  # simsum object, no 'by'
  testthat::expect_s3_class(object = autoplot(single, stats = "power"), class = c("gg", "ggplot"))
  # simsum object, with 'by'
  testthat::expect_s3_class(object = autoplot(multi, stats = "power"), class = c("gg", "ggplot"))
  # summary.simsum object, no 'by'
  testthat::expect_s3_class(object = autoplot(singlesum, stats = "power"), class = c("gg", "ggplot"))
  # summary.simsum object, with 'by'
  testthat::expect_s3_class(object = autoplot(multisum, stats = "power"), class = c("gg", "ggplot"))
  # simsum object, no 'by'
  testthat::expect_s3_class(object = autoplot(single, stats = "thetamean"), class = c("gg", "ggplot"))
  # simsum object, with 'by'
  testthat::expect_s3_class(object = autoplot(multi, stats = "thetamean"), class = c("gg", "ggplot"))
  # summary.simsum object, no 'by'
  testthat::expect_s3_class(object = autoplot(singlesum, stats = "thetamean"), class = c("gg", "ggplot"))
  # summary.simsum object, with 'by'
  testthat::expect_s3_class(object = autoplot(multisum, stats = "thetamean"), class = c("gg", "ggplot"))
  # simsum object, no 'by'
  testthat::expect_s3_class(object = autoplot(single, stats = "thetamedian"), class = c("gg", "ggplot"))
  # simsum object, with 'by'
  testthat::expect_s3_class(object = autoplot(multi, stats = "thetamedian"), class = c("gg", "ggplot"))
  # summary.simsum object, no 'by'
  testthat::expect_s3_class(object = autoplot(singlesum, stats = "thetamedian"), class = c("gg", "ggplot"))
  # summary.simsum object, with 'by'
  testthat::expect_s3_class(object = autoplot(multisum, stats = "thetamedian"), class = c("gg", "ggplot"))
})

testthat::test_that("zip with t critical values", {
  data("MIsim", package = "rsimsum")
  data("relhaz", package = "rsimsum")
  single <- rsimsum::simsum(data = MIsim, estvarname = "b", true = 0.5, se = "se", methodvar = "method", ref = "CC", x = TRUE, control = list(df = 10))
  multi <- rsimsum::simsum(data = relhaz, estvarname = "theta", true = -0.5, se = "se", methodvar = "model", by = c("n", "baseline"), x = TRUE, control = list(df = 10))
  singlesum <- summary(single)
  multisum <- summary(multi)
  # simsum object, no 'by'
  testthat::expect_s3_class(object = autoplot(single, type = "zip"), class = c("gg", "ggplot"))
  # simsum object, with 'by'
  testthat::expect_s3_class(object = autoplot(multi, type = "zip"), class = c("gg", "ggplot"))
  # summary.simsum object, no 'by'
  testthat::expect_s3_class(object = autoplot(singlesum, type = "zip"), class = c("gg", "ggplot"))
  # summary.simsum object, with 'by'
  testthat::expect_s3_class(object = autoplot(multisum, type = "zip"), class = c("gg", "ggplot"))
})

data("frailty", package = "rsimsum")
ms <- rsimsum::multisimsum(
  data = frailty,
  par = "par", true = c(trt = -0.50, fv = 0.75),
  estvarname = "b", se = "se", methodvar = "model",
  x = TRUE
)
sms <- summary(ms)
ms2 <- rsimsum::multisimsum(
  data = frailty,
  par = "par", true = c(trt = -0.50, fv = 0.75),
  estvarname = "b", se = "se", methodvar = "model",
  by = "fv_dist",
  x = TRUE
)
sms2 <- summary(ms2)

testthat::test_that("output from autoplot is of class gg, ggplot", {
  # multisimsum object, no 'by'
  testthat::expect_s3_class(object = autoplot(ms, par = "trt"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(ms, par = "trt", type = "forest"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(ms, par = "trt", type = "lolly"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(ms, par = "trt", type = "zip"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(ms, par = "trt", type = "est"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(ms, par = "trt", type = "se"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(ms, par = "trt", type = "est_ba"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(ms, par = "trt", type = "se_ba"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(ms, par = "trt", type = "est_ridge"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(ms, par = "trt", type = "se_ridge"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(ms, par = "trt", type = "heat"), class = c("gg", "ggplot"))
  # multisimsum object, with 'by'
  testthat::expect_s3_class(object = autoplot(ms2, par = "trt"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(ms2, par = "trt", type = "forest"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(ms2, par = "trt", type = "lolly"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(ms2, par = "trt", type = "zip"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(ms2, par = "trt", type = "est"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(ms2, par = "trt", type = "se"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(ms2, par = "trt", type = "est_ba"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(ms2, par = "trt", type = "se_ba"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(ms2, par = "trt", type = "est_ridge"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(ms2, par = "trt", type = "se_ridge"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(ms2, par = "trt", type = "heat"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(ms2, par = "trt", type = "nlp"), class = c("gg", "ggplot"))
  # summary.multisimsum object, no 'by'
  testthat::expect_s3_class(object = autoplot(sms, par = "trt"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(sms, par = "trt", type = "forest"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(sms, par = "trt", type = "lolly"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(sms, par = "trt", type = "zip"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(sms, par = "trt", type = "est"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(sms, par = "trt", type = "se"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(sms, par = "trt", type = "est_ba"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(sms, par = "trt", type = "se_ba"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(sms, par = "trt", type = "est_ridge"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(sms, par = "trt", type = "se_ridge"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(sms, par = "trt", type = "heat"), class = c("gg", "ggplot"))
  # summary.multisimsum object, with 'by'
  testthat::expect_s3_class(object = autoplot(sms2, par = "trt"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(sms2, par = "trt", type = "forest"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(sms2, par = "trt", type = "lolly"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(sms2, par = "trt", type = "zip"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(sms2, par = "trt", type = "est"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(sms2, par = "trt", type = "se"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(sms2, par = "trt", type = "est_ba"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(sms2, par = "trt", type = "se_ba"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(sms2, par = "trt", type = "est_ridge"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(sms2, par = "trt", type = "se_ridge"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(sms2, par = "trt", type = "heat"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(sms2, par = "trt", type = "nlp"), class = c("gg", "ggplot"))
})

testthat::test_that("putting wrong 'par' (or no 'par' at all) throws an error", {
  # multisimsum object, no 'by'
  testthat::expect_error(object = autoplot(ms))
  testthat::expect_error(object = autoplot(ms, par = "42"))
  testthat::expect_error(object = autoplot(ms, par = 42))
  testthat::expect_error(object = autoplot(ms, par = TRUE))
  # multisimsum object, with 'by'
  testthat::expect_error(object = autoplot(ms2))
  testthat::expect_error(object = autoplot(ms2, par = "42"))
  testthat::expect_error(object = autoplot(ms2, par = 42))
  testthat::expect_error(object = autoplot(ms2, par = TRUE))
  # summary.multisimsum object, no 'by'
  testthat::expect_error(object = autoplot(sms))
  testthat::expect_error(object = autoplot(sms, par = "42"))
  testthat::expect_error(object = autoplot(sms, par = 42))
  testthat::expect_error(object = autoplot(sms, par = TRUE))
  # summary.multisimsum object, with 'by'
  testthat::expect_error(object = autoplot(sms2))
  testthat::expect_error(object = autoplot(sms2, par = "42"))
  testthat::expect_error(object = autoplot(sms2, par = 42))
  testthat::expect_error(object = autoplot(sms2, par = TRUE))
})


testthat::test_that("nlp with no 'by' factors throw an error", {
  testthat::expect_error(object = autoplot(ms, par = "trt", type = "nlp"), regexp = "Nested loop plot not meaningful")
})
