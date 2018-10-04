#' @title print.miss
#' @description Print method for `miss` objects
#' @param x An object of class `miss`.
#' @param fmt Format string passed to [base::sprintf()] to format proportions of missing values when printed.
#' @param ... Ignored.
#' @export
#' @examples
#' library(rsimsum)
#' data("frailty", package = "rsimsum")
#' m <- miss(
#'   data = frailty, estvarname = "b", se = "se", par = "par",
#'   methodvar = "model", by = "fv_dist"
#' )
#' print(m)
#' print(m, fmt = "%.2f")
print.miss <- function(x, fmt = "%.4f", ...) {
  ### Check arguments
  arg_checks <- checkmate::makeAssertCollection()

  # `fmt` must be a single string value
  checkmate::assert_string(fmt, add = arg_checks)

  ### Report if there are any errors
  if (!arg_checks$isEmpty()) {
    checkmate::reportAssertions(arg_checks)
  }

  ### Format output
  x[["ovsumm"]][[paste0("missing_", x[["estvarname"]])]] <- sprintf(fmt = fmt, x[["ovsumm"]][[paste0("missing_", x[["estvarname"]])]])
  x[["ovsumm"]][[paste0("missing_", x[["se"]])]] <- sprintf(fmt = fmt, x[["ovsumm"]][[paste0("missing_", x[["se"]])]])
  if (!is.null(x[["par"]])) {
    x[["bypsumm"]][[paste0("missing_", x[["estvarname"]])]] <- sprintf(fmt = fmt, x[["bypsumm"]][[paste0("missing_", x[["estvarname"]])]])
    x[["bypsumm"]][[paste0("missing_", x[["se"]])]] <- sprintf(fmt = fmt, x[["bypsumm"]][[paste0("missing_", x[["se"]])]])
  }
  if (!is.null(x[["methodvar"]])) {
    x[["bymsumm"]][[paste0("missing_", x[["estvarname"]])]] <- sprintf(fmt = fmt, x[["bymsumm"]][[paste0("missing_", x[["estvarname"]])]])
    x[["bymsumm"]][[paste0("missing_", x[["se"]])]] <- sprintf(fmt = fmt, x[["bymsumm"]][[paste0("missing_", x[["se"]])]])
  }
  if (!is.null(x[["by"]])) {
    x[["bybsumm"]][[paste0("missing_", x[["estvarname"]])]] <- sprintf(fmt = fmt, x[["bybsumm"]][[paste0("missing_", x[["estvarname"]])]])
    x[["bybsumm"]][[paste0("missing_", x[["se"]])]] <- sprintf(fmt = fmt, x[["bybsumm"]][[paste0("missing_", x[["se"]])]])
  }

  ### Print call to `miss`
  cat("\nCall:\n\t", paste(deparse(x$call), sep = "\n", collapse = "\n"), "\n", sep = "")

  ### Print `overall` missingness
  cat("\nProportion of missingness overall:\n")
  print(x[["ovsumm"]], row.names = FALSE)

  ### Print `par` (if any)
  if (!is.null(x[["par"]])) {
    cat("\nEstimands variable:", x[["par"]], "\nProportions of missingness by estimands:\n")
    print(x[["bypsumm"]], row.names = FALSE)
  } else {
    cat("\nEstimands variable: none\n")
  }

  ### Print `methodvar` (if any)
  if (!is.null(x[["methodvar"]])) {
    cat("\nMethod variable:", x[["methodvar"]], "\nProportions of missingness by method:\n")
    print(x[["bymsumm"]], row.names = FALSE)
  } else {
    cat("\nMethod variable: none\n")
  }

  ### Print `by` factors (if any)
  if (!is.null(x[["by"]])) {
    cat("\nBy factors:", paste(x[["by"]], collapse = ", "), "\nProportions of missingness by 'by' factors:\n")
    print(x[["bybsumm"]], row.names = FALSE)
  } else {
    cat("\nBy factors: none\n")
  }
}
