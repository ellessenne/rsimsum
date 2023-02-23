test_that("MCSE of relative error is the same as in Stata's simsum", {
  data("MIsim", package = "rsimsum")
  s <- simsum(data = MIsim, estvarname = "b", true = 0.5, se = "se", methodvar = "method", ref = "CC")
  s <- summary(s)
  expect_equal(
    object = s$summ[s$summ$stat == "relerror", ]$mcse,
    expected = c(2.205481529236, 2.332338094711, 2.269521713257),
    tolerance = 1e-6
  )
})
