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
#'
#' data("frailty", package = "rsimsum")
#' frailty$true <- ifelse(frailty$par == "trt", -0.50, 0.75)
#' ms <- multisimsum(data = frailty, par = "par", estvarname = "b", true = "true")
#' ms
print.multisimsum <- function(x, ...) {
  ### Print `par`, possible estimands
  cat("\nEstimands variable:", x$par, "\n")
  estimands <- unique(x$summ[[x$par]])
  cat("\tUnique estimands:", paste(estimands, collapse = ", "), "\n")
  if (!is.null(x$true)) {
    if (rlang::is_named(x$true)) {
      cat("\tTrue values:", paste(estimands, "=", x$true[estimands], collapse = ", "), "\n")
    } else {
      if (is.character(x$true)) {
        cat("\tTrue values from column", paste0("'", x$true, "'"), "\n")
      } else {
        cat("\tTrue values fixed at value", x$true, "\n")
      }
    }
  } else {
    cat("\tTrue value of the estimands not defined: bias, relative bias, coverage, and mean squared error were not computed.\n")
  }

  ### Print `methodvar` (if any), possible methods, and reference method
  if (!is.null(x$methodvar)) {
    if (length(x$methodvar) > 1) {
      cat("\nColumns identifying methods:", paste(x$methodvar, collapse = ", "), "\n")
      reftable <- .compact_method_columns(data = tidy.multisimsum(x), methodvar = x$methodvar)$data[[":methodvar"]]
      cat("\tUnique methods:", paste(unique(reftable), collapse = ", "), "\n")
    } else {
      cat("\nMethod variable:", x$methodvar, "\n")
      methods <- unique(x$summ[[x$methodvar]])
      cat("\tUnique methods:", paste(methods, collapse = ", "), "\n")
      cat("\tReference method:", x$ref, "\n")
    }
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
