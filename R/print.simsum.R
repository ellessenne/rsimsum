#' @title print.simsum
#' @description Print method for simsum objects
#' @param x An object of class `simsum`.
#' @param ... Ignored.
#' @export
#'
#' @examples
#' data("MIsim")
#' x <- simsum(
#'   data = MIsim, estvarname = "b", true = 0.5, se = "se",
#'   methodvar = "method"
#' )
#' x
#'
#' MIsim$true <- 0.5
#' x <- simsum(data = MIsim, estvarname = "b", true = "true", se = "se")
#' x
print.simsum <- function(x, ...) {
  cat("Summary of a simulation study with a single estimand.\n")
  if (!is.null(x$true)) {
    if (is.character(x$true)) {
      cat("True value of the estimand from column", paste0("'", x$true, "'"), "\n")
    } else {
      cat("True value of the estimand:", x$true, "\n")
    }
  } else {
    cat("True value of the estimand not defined: bias, coverage, and mean squared error were not computed.\n")
  }

  ### Print `methodvar` (if any), possible methods, and reference method
  if (!is.null(x$methodvar)) {
    cat("\nMethod variable:", x$methodvar, "\n")
    methods <- unique(x$summ[[x$methodvar]])
    cat("\tUnique methods:", paste(methods, collapse = ", "), "\n")
    cat("\tReference method:", x$ref, "\n")
  } else {
    cat("\nMethod variable: none\n")
  }

  ### Print `by` factors (if any)
  if (!is.null(x$by)) {
    cat("\nBy factors:", paste(x$by, collapse = ", "), "\n")
  } else {
    cat("\nBy factors: none\n")
  }

  ### Print whether Monte Carlo SEs were computed or not
  if (x$control$mcse) {
    cat("\nMonte Carlo standard errors were computed.\n")
  } else {
    cat("\nMonte Carlo standard errors were not computed.\n")
  }
}
