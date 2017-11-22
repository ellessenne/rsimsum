#' summary.simsum
#'
#' @title Summarising simsum objects
#'
#' @description The `summary()` method for objects of class `simsum` returns confidence intervals for performance measures based on Monte Carlo standard errors.
#'
#' @param x An object of class `simsum`.
#' @param level Significance level for confidence intervals based on Monte Carlo standard errors. Ignored if a `simsum` object is obtained with `mcse = FALSE`.
#'
#' @return An object of class `summary.simsum`.
#' @seealso [simsum()], [print.summary.simsum()]
#' @export
#'
#' @examples
#' data("MIsim")
#' x = simsum(data = MIsim, estvarname = "b", true = 0.5, se = "se", methodvar = "method", mcse = TRUE)
#' xs = summary(x)
#' xs


summary.simsum <- function(x, level = 0.95) {
	### Check arguments
	arg_checks = makeAssertCollection()

	# `x` must be an object of class `simsum`
	assert_class(x, classes = "simsum", add = arg_checks)

	# `level` must be a numeric value
	assert_number(level, lower = 0, upper = 1, add = arg_checks)

	### Report if there are any errors
	if (!arg_checks$isEmpty()) reportAssertions(arg_checks)

	### Compute confidence intervals if Monte Carlo standard errors are available
	if (x$mcse) {
		x$summ$lower = x$summ$coef - qnorm(1 - (1 - level) / 2) * x$summ$mcse
		x$summ$upper = x$summ$coef + qnorm(1 - (1 - level) / 2) * x$summ$mcse
	}

	### Return object of class summary.simsum
	class(x) = c("list", "summary.simsum")
	return(x)
}
