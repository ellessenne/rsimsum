#' summary.simsum
#'
#' @title Summarising simsum objects
#' @description The `summary()` method for objects of class `simsum` returns confidence intervals for performance measures based on Monte Carlo standard errors.
#' @param object An object of class `simsum`.
#' @param ci_level Significance level for confidence intervals based on Monte Carlo standard errors. Ignored if a `simsum` object is obtained with `mcse = FALSE`.
#' @param ... Ignored.
#' @return An object of class `summary.simsum`.
#' @seealso [simsum()], [print.summary.simsum()]
#' @note Confidence intervals are constructed using quantiles from a normal distribution with the specified `ci_level` confidence level, and are therefore symmetric by definition.
#' @export
#'
#' @examples
#' data("MIsim")
#' object = simsum(data = MIsim, estvarname = "b", true = 0.5, se = "se",
#' methodvar = "method", mcse = TRUE)
#' xs = summary(object)
#' xs


summary.simsum <- function(object, ci_level = 0.95, ...) {
	### Check arguments
	arg_checks = checkmate::makeAssertCollection()

	# `level` must be a numeric value
	checkmate::assert_number(ci_level, lower = 0, upper = 1, add = arg_checks)

	### Report if there are any errors
	if (!arg_checks$isEmpty()) checkmate::reportAssertions(arg_checks)

	### Compute confidence intervals if Monte Carlo standard errors are available
	if (object$mcse) {
		object$summ$lower = object$summ$coef - stats::qnorm(1 - (1 - ci_level) / 2) * object$summ$mcse
		object$summ$upper = object$summ$coef + stats::qnorm(1 - (1 - ci_level) / 2) * object$summ$mcse
	}

	### Add ci_level slot
	object$ci_level = ci_level

	### Return object of class summary.simsum
	class(object) = c("list", "summary.simsum")
	return(object)
}
