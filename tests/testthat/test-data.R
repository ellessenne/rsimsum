context("data")

test_that("loading MIsim works", {
  data("MIsim", package = "rsimsum")
  expect_s3_class(object = MIsim, class = "data.frame")
})

test_that("loading relhaz works", {
  data("relhaz", package = "rsimsum")
  expect_s3_class(object = relhaz, class = "data.frame")
})

test_that("loading frailty works", {
  data("frailty", package = "rsimsum")
  expect_s3_class(object = frailty, class = "data.frame")
})

test_that("loading nlp works", {
  data("nlp", package = "rsimsum")
  expect_s3_class(object = nlp, class = "data.frame")
})

test_that("loading tt works", {
  data("tt", package = "rsimsum")
  expect_s3_class(object = tt, class = "data.frame")
  expect_equal(object = dim(tt), expected = c(4000, 8))
})
