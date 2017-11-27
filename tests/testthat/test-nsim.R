context("nsim")

test_that("nsim returns a single, double value", {
	expect_length(
		nsim(alpha = 0.05, sigma = 1, delta = 1, power = 0.5),
		n = 1)
	expect_type(
		nsim(alpha = 0.05, sigma = 1, delta = 1, power = 0.5),
		type = "double")
})

test_that("alpha is in the appropriate range", {
	expect_error(
		nsim(alpha = 1.5, sigma = 1, delta = 1, power = 0.5),
		"Variable 'alpha': All elements must be <= 1",
		fixed = TRUE)
	expect_error(
		nsim(alpha = -0.1, sigma = 1, delta = 1, power = 0.5),
		"Variable 'alpha': All elements must be >= 0",
		fixed = TRUE)
})

test_that("sigma is greater than zero", {
	expect_error(
		nsim(alpha = 0.05, sigma = -0.1, delta = 1, power = -0.1),
		"Variable 'sigma': All elements must be >= 0",
		fixed = TRUE)
})

test_that("delta is greater than zero", {
	expect_error(
		nsim(alpha = 0.05, sigma = 1, delta = -0.1, power = -0.1),
		"Variable 'delta': All elements must be >= 0",
		fixed = TRUE)
})

test_that("power is in the appropriate range", {
	expect_error(
		nsim(alpha = 0.05, sigma = 1, delta = 1, power = 1.5),
		"Variable 'power': All elements must be <= 1",
		fixed = TRUE)
	expect_error(
		nsim(alpha = 0.05, sigma = 1, delta = 1, power = -0.1),
		"Variable 'power': All elements must be >= 0",
		fixed = TRUE)
})
