#' @title print.simsum
#' @description Print method for simsum objects
#' @param x An object of class `simsum`.
#' @param ... Ignored.
#' @export
#'
#' @examples
#' data("MIsim")
#' x <- simsum(data = MIsim, estvarname = "b", true = 0.5, se = "se",
#'             methodvar = "method", mcse = TRUE)
#' x

print.simsum <- function(x, ...) {
  ### Print call to `simsum`
  cat("\nCall:\n\t", paste(deparse(x$call), sep = "\n", collapse = "\n"), "\n", sep = "")

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
  if (x$mcse) {
    cat("\nMonte Carlo standard errors were computed.\n")
  } else {
    cat("\nMonte Carlo standard errors were not computed.\n")
  }
}
