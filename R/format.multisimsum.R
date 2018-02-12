#' @title Format multisimsum objects
#' @description Format an object of class `multisimsum` for pretty printing.
#' @param x An object of class `multisimsum`.
#' @param digits Number of significant digits. Defaults to 4.
#' @seealso [multisimsum()], [format.summary.multisimsum()]
#' @return An object of class `multisimsum` with its `summ` slot formatted for pretty printing.

format.multisimsum <- function(x, digits) {
  x$summ$est <- sprintf(paste0("%.", digits, "f"), x$summ$est)
  if (x$mcse) {
    x$summ$mcse <- sprintf(paste0("%.", digits, "f"), x$summ$mcse)
    if ("summary.multisimsum" %in% class(x)) {
      x$summ$lower <- sprintf(paste0("%.", digits, "f"), x$summ$lower)
      x$summ$upper <- sprintf(paste0("%.", digits, "f"), x$summ$upper)
    }
  }
  x$summ$stat[x$summ$stat == "nsim"] <- "Simulations with non-missing estimates/SEs"
  x$summ$stat[x$summ$stat == "thetamean"] <- "Average point estimate"
  x$summ$stat[x$summ$stat == "thetamedian"] <- "Median point estimate"
  x$summ$stat[x$summ$stat == "se2mean"] <- "Average standard error"
  x$summ$stat[x$summ$stat == "se2median"] <- "Median standard error"
  x$summ$stat[x$summ$stat == "bias"] <- "Bias in point estimate"
  x$summ$stat[x$summ$stat == "empse"] <- "Empirical standard error"
  x$summ$stat[x$summ$stat == "mse"] <- "Mean squared error"
  x$summ$stat[x$summ$stat == "relprec"] <- paste("% gain in precision relative to method", x$ref)
  x$summ$stat[x$summ$stat == "modelse"] <- "Model-based standard error"
  x$summ$stat[x$summ$stat == "relerror"] <- "Relative % error in standard error"
  x$summ$stat[x$summ$stat == "cover"] <- paste("Coverage of nominal", sprintf("%.0f%%", 100 * (x$level)), "CI")
  x$summ$stat[x$summ$stat == "bccover"] <- paste("Bias corrected coverage of nominal", sprintf("%.0f%%", 100 * (x$level)), "CI")
  x$summ$stat[x$summ$stat == "power"] <- paste("Power of", sprintf("%.0f%%", 100 * (1 - x$level)), "level test")
  rownames(x$summ) <- NULL
  return(x)
}

#' format.summary.multisimsum
#'
#' @title Format summary.multisimsum objects
#'
#' @description Format an object of class `summary.multisimsum` for pretty printing.
#'
#' @param x An object of class `summary.multisimsum`.
#' @param digits Number of significant digits. Defaults to 4.
#'
#' @seealso [multisimsum()], [format.multisimsum()]
#'
#' @return An object of class `summary.multisimsum` with its `summ` slot formatted for pretty printing.

format.summary.multisimsum <- function(x, digits) format.multisimsum(x = x, digits = digits)
