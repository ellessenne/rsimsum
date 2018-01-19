context("data")

test_that("loading MIsim works", {
  data("MIsim", package = "rsimsum")
  expect_s3_class(object = MIsim, class = "data.frame")
})

test_that("loading relhaz works", {
  data("relhaz", package = "rsimsum")
  expect_s3_class(object = relhaz, class = "data.frame")
})
