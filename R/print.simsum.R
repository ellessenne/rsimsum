#' print.simsum
#'
#' @title Print method for simsum objects
#' @param x An object of class `simsum`
#' @export
#' @examples
#' data("MIsim")
#' x = simsum(data = MIsim, estvarname = "b", true = 0.5, se = "se", methodvar = "method", mcse = TRUE)
#' x

print.simsum <- function(x) {
	### Check arguments
	arg_checks = checkmate::makeAssertCollection()

	# `x` must be an object of class `simsum`
	checkmate::assert_class(x, classes = "simsum", add = arg_checks)

	### Report if there are any errors
	if (!arg_checks$isEmpty()) checkmate::reportAssertions(arg_checks)

	### Print call to `simsum`
	cat("\nCall:\n\t", paste(deparse(x$call), sep = "\n", collapse = "\n"), "\n", sep = "")

	### Print `methodvar` (if any), possible methods, and reference method
	if (!is.null(x$methodvar)) {
		cat("\nMethod variable:", x$methodvar, "\n")
		methods = unique(x$summ$method)
		cat("\tUnique methods:", paste(methods, collapse = ", "), "\n")
		cat("\tReference method:", x$ref, "\n")
	} else {
		cat("\nMethod variable: none\n")
	}

	### Print `by` factors (if any)
	if (!is.null(x$by)) {
		cat("By factors:", paste(x$by, collapse = ", "), "\n")
	} else {
		cat("By factors: none\n")
	}
}
