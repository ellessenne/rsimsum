#' format.simsum
#'
#' @title Format simsum objects
#'
#' @description Format an object of class `simsum` for pretty printing.
#'
#' @param x An object of class `simsum`.
#' @param digits Number of significant digits. Defaults to 4.
#'
#' @seealso [simsum()], [format.summary.simsum()]
#'
#' @return An object of class `simsum` with its `summ` slot formatted for pretty printing.
format.simsum <- function(x, digits) {
	x$summ$coef = sprintf(paste0("%.", digits, "f"), x$summ$coef)
	if (x$mcse) {
		x$summ$mcse = sprintf(paste0("%.", digits, "f"), x$summ$mcse)
		if ("summary.simsum" %in% class(x)) {
			x$summ$lower = sprintf(paste0("%.", digits, "f"), x$summ$lower)
			x$summ$upper = sprintf(paste0("%.", digits, "f"), x$summ$upper)
		}
	}
	x$summ$stat[x$summ$stat == "bsims"] = "Non-missing point estimates"
	x$summ$stat[x$summ$stat == "sesims"] = "Non-missing standard errors"
	x$summ$stat[x$summ$stat == "bias"] = "Bias in point estimate"
	x$summ$stat[x$summ$stat == "esd"] = "Empirical standard error"
	x$summ$stat[x$summ$stat == "mse"] = "Mean squared error"
	x$summ$stat[x$summ$stat == "relprec"] = paste("% gain in precision relative to method", x$ref)
	x$summ$stat[x$summ$stat == "modelse"] = paste(ifelse(x$modelsemethod == "rmse", "RMS", "Mean"), "model-based standard error")
	x$summ$stat[x$summ$stat == "relerror"] = "Relative % error in standard error"
	x$summ$stat[x$summ$stat == "cover"] = paste("Coverage of nominal", sprintf("%.0f%%", 100 * (x$level)), "confidence interval")
	x$summ$stat[x$summ$stat == "power"] = paste("Power of", sprintf("%.0f%%", 100 * (1 - x$level)), "level test")
	rownames(x$summ) = NULL
	return(x)
}

#' format.summary.simsum
#'
#' @title Format summary.simsum objects
#'
#' @description Format an object of class `summary.simsum` for pretty printing.
#'
#' @param x An object of class `summary.simsum`.
#' @param digits Number of significant digits. Defaults to 4.
#'
#' @seealso [simsum()], [format.simsum()]
#'
#' @return An object of class `summary.simsum` with its `summ` slot formatted for pretty printing.

format.summary.simsum <- function(x, digits) format.simsum(x = x, digits = digits)
