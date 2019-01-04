#' @title print.multisimsum
#' @description Print method for multisimsum objects
#' @param x An object of class `multisimsum`.
#' @param ... Ignored.
#' @export
#'
#' @examples
#' data(frailty)
#' ms <- multisimsum(
#'   data = frailty, par = "par", true = c(
#'     trt = -0.50,
#'     fv = 0.75
#'   ), estvarname = "b", se = "se", methodvar = "model",
#'   by = "fv_dist"
#' )
#' ms
print.multisimsum <- function(x, ...) {
  ### Print `par`, possible estimands
  cat("\nEstimands variable:", x$par, "\n")
  estimands <- unique(x$summ[[x$par]])
  cat("\tUnique estimands:", paste(estimands, collapse = ", "), "\n")
  cat("\tTrue values:", paste(estimands, "=", x$true[estimands], collapse = ", "), "\n")

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
